use std::{future::Future, time::Instant};

use futures::{stream::FuturesUnordered, StreamExt};
use tokio::sync::RwLock;
use tower_lsp::{jsonrpc::Result, lsp_types::*, Client, LanguageServer, LspService, Server};

use vine::{
  compiler::Compiler,
  components::parser::VineParser,
  features::cfg::Config,
  structures::{
    ast::{visit::VisitMut, Item, ItemKind, Span},
    checkpoint::Checkpoint,
    core::{Core, CoreArenas},
    diag::{Diag, FileInfo},
  },
};

struct Backend {
  core: &'static Core<'static>,
  compiler: RwLock<Compiler<'static>>,
  client: Client,
  entrypoints: Vec<String>,
}

impl Backend {
  async fn refresh(&self) {
    let mut compiler = self.compiler.write().await;
    compiler.revert(&Checkpoint::default());
    self.core.files.write().unwrap().clear();

    for glob in &self.entrypoints {
      for entry in glob::glob(glob).unwrap() {
        let e = entry.unwrap();
        compiler.loader.load_mod(&e);
      }
    }

    let start = Instant::now();
    let diags = match compiler.compile(()) {
      Ok(_) => Vec::new(),
      Err(diags) => diags,
    };
    eprintln!("compiled in {:?}", start.elapsed());

    self.report(self.core, diags).await;
  }

  fn report(
    &self,
    core: &Core<'_>,
    mut diags: Vec<Diag<'_>>,
  ) -> impl Future<Output = ()> + Send + '_ {
    diags.sort_by_key(|d| Some(d.span()?.file));
    let mut diags = diags.into_iter().peekable();
    while diags.peek().is_some_and(|x| x.span().is_none()) {
      diags.next();
    }
    let futures = FuturesUnordered::new();
    for (i, file) in core.files().iter().enumerate() {
      let mut out = Vec::new();
      while diags.peek().is_some_and(|x| x.span().is_some_and(|x| x.file == i)) {
        let diag = diags.next().unwrap();
        let span = diag.span().unwrap();
        out.push(Diagnostic {
          range: _get_range(file, span),
          severity: Some(DiagnosticSeverity::ERROR),
          code: None,
          code_description: None,
          source: Some("vine".into()),
          message: diag.to_string(),
          related_information: None,
          tags: None,
          data: None,
        });
      }
      let uri = Url::from_file_path(file.path.as_ref().unwrap()).unwrap();
      futures.push(self.client.publish_diagnostics(uri, out, None));
    }
    futures.collect::<()>()
  }
}

fn get_range<'core>(core: &'core Core<'core>, span: Span) -> Range {
  _get_range(&core.files()[span.file], span)
}

fn _get_range(file: &FileInfo, span: Span) -> Range {
  let start = file.get_pos(span.start);
  let end = file.get_pos(span.end);
  Range {
    start: Position { line: start.line as u32, character: start.col as u32 },
    end: Position { line: end.line as u32, character: end.col as u32 },
  }
}

fn get_location<'core>(core: &'core Core<'core>, span: Span) -> Location {
  Location {
    uri: Url::from_file_path(core.files()[span.file].path.as_ref().unwrap()).unwrap(),
    range: get_range(core, span),
  }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
  async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
    assert!(params.capabilities.general.is_some_and(|x| x
      .position_encodings
      .is_some_and(|x| x.contains(&PositionEncodingKind::UTF8))));
    Ok(InitializeResult {
      server_info: None,
      capabilities: ServerCapabilities {
        position_encoding: Some(PositionEncodingKind::UTF8),
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        workspace: Some(WorkspaceServerCapabilities {
          workspace_folders: Some(WorkspaceFoldersServerCapabilities {
            supported: Some(true),
            change_notifications: Some(OneOf::Left(true)),
          }),
          file_operations: None,
        }),
        document_symbol_provider: Some(OneOf::Left(true)),
        workspace_symbol_provider: Some(OneOf::Left(true)),
        definition_provider: Some(OneOf::Left(true)),
        references_provider: Some(OneOf::Left(true)),
        ..ServerCapabilities::default()
      },
    })
  }

  async fn symbol(&self, params: WorkspaceSymbolParams) -> Result<Option<Vec<SymbolInformation>>> {
    let compiler = self.compiler.read().await;
    let query = params.query.to_lowercase();
    let mut results = Vec::from_iter(
      [].into_iter()
        .chain(compiler.chart.structs.values().map(|x| (x.name, SymbolKind::STRUCT, x.span)))
        .chain(compiler.chart.enums.values().map(|x| (x.name, SymbolKind::ENUM, x.span)))
        .chain(compiler.chart.concrete_fns.values().map(|x| (x.name, SymbolKind::FUNCTION, x.span)))
        .chain(
          compiler.chart.concrete_consts.values().map(|x| (x.name, SymbolKind::CONSTANT, x.span)),
        )
        .chain(compiler.chart.traits.values().map(|x| (x.name, SymbolKind::INTERFACE, x.span)))
        .filter(|(name, _, _)| name.0 .0.to_lowercase().contains(&query))
        .map(|(name, kind, span)| SymbolInformation {
          name: name.0 .0.to_owned(),
          kind,
          tags: None,
          #[allow(deprecated)]
          deprecated: None,
          location: get_location(compiler.core, span),
          container_name: None,
        }),
    );
    results.sort_by(|a, b| a.name.cmp(&b.name));
    Ok(Some(results))
  }

  async fn document_symbol(
    &self,
    params: DocumentSymbolParams,
  ) -> Result<Option<DocumentSymbolResponse>> {
    let _compiler = self.compiler.read().await;
    let files = self.core.files.read().unwrap();
    let Some(file) = files.iter().find(|x| {
      x.path.as_ref().and_then(|x| x.to_str()).is_some_and(|x| x == params.text_document.uri.path())
    }) else {
      dbg!(":(");
      return Ok(None);
    };
    let Ok(ast) = VineParser::parse(self.core, &file.src, 0) else {
      return Ok(None);
    };
    struct Symbols<'a>(&'a FileInfo, Vec<DocumentSymbol>);
    impl<'t> VisitMut<'static, 't> for Symbols<'_> {
      fn visit_item(&mut self, item: &'t mut Item<'static>) {
        let (kind, name) = match &item.kind {
          ItemKind::Fn(i) => (SymbolKind::FUNCTION, i.name),
          ItemKind::Const(i) => (SymbolKind::CONSTANT, i.name),
          ItemKind::Struct(i) => (SymbolKind::STRUCT, i.name),
          ItemKind::Enum(i) => (SymbolKind::ENUM, i.name),
          ItemKind::Type(i) => (SymbolKind::INTERFACE, i.name),
          ItemKind::Mod(i) => (SymbolKind::MODULE, i.name),
          ItemKind::Trait(i) => (SymbolKind::INTERFACE, i.name),
          ItemKind::Impl(i) => (SymbolKind::CLASS, i.name),
          ItemKind::Use(_) | ItemKind::Taken => return,
        };
        let mut children = Symbols(self.0, Vec::new());
        children._visit_item(item);
        self.1.push(DocumentSymbol {
          name: name.0 .0.to_owned(),
          kind,
          detail: None,
          tags: None,
          #[allow(deprecated)]
          deprecated: None,
          range: _get_range(self.0, item.span),
          selection_range: _get_range(self.0, item.span),
          children: Some(children.1),
        });
      }
    }
    let mut symbols = Symbols(file, Vec::new());
    symbols.visit(&mut { ast });
    Ok(Some(DocumentSymbolResponse::Nested(symbols.1)))
  }

  async fn goto_definition(
    &self,
    params: GotoDefinitionParams,
  ) -> Result<Option<GotoDefinitionResponse>> {
    let compiler = self.compiler.read().await;
    let files = self.core.files();
    let Some((file, info)) = files.iter().enumerate().find(|x| {
      x.1
        .path
        .as_ref()
        .and_then(|x| x.to_str())
        .is_some_and(|x| x == params.text_document_position_params.text_document.uri.path())
    }) else {
      dbg!(":( 0");
      return Ok(None);
    };
    let span = Span {
      file,
      start: info.line_starts[params.text_document_position_params.position.line as usize]
        + params.text_document_position_params.position.character as usize,
      end: info.line_starts[params.text_document_position_params.position.line as usize]
        + params.text_document_position_params.position.character as usize,
    };
    let Some((src_span, dests)) = compiler.resolutions.definitions.range(..=&span).next_back()
    else {
      dbg!(":( 1");
      return Ok(None);
    };
    if !(src_span.file == span.file && src_span.start <= span.start && src_span.end >= span.end) {
      dbg!(span, src_span);
      return Ok(None);
    }
    Ok(Some(GotoDefinitionResponse::Array(
      dests.iter().map(|x| get_location(self.core, *x)).collect(),
    )))
  }

  async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
    let compiler = self.compiler.read().await;
    let files = self.core.files();
    let Some((file, info)) = files.iter().enumerate().find(|x| {
      x.1
        .path
        .as_ref()
        .and_then(|x| x.to_str())
        .is_some_and(|x| x == params.text_document_position.text_document.uri.path())
    }) else {
      dbg!(":( 0");
      return Ok(None);
    };
    let span = Span {
      file,
      start: info.line_starts[params.text_document_position.position.line as usize]
        + params.text_document_position.position.character as usize,
      end: info.line_starts[params.text_document_position.position.line as usize]
        + params.text_document_position.position.character as usize,
    };
    let Some((src_span, dests)) = compiler.resolutions.references.range(..=&span).next_back()
    else {
      dbg!(":( 1");
      return Ok(None);
    };
    if !(src_span.file == span.file && src_span.start <= span.start && src_span.end >= span.end) {
      dbg!(span, src_span);
      return Ok(None);
    }
    Ok(Some(dests.iter().map(|x| get_location(self.core, *x)).collect()))
  }

  async fn initialized(&self, _: InitializedParams) {
    self.refresh().await
  }

  async fn did_save(&self, _: DidSaveTextDocumentParams) {
    self.refresh().await
  }

  async fn shutdown(&self) -> Result<()> {
    Ok(())
  }
}

#[allow(clippy::absolute_paths)]
#[tokio::main]
pub async fn lsp(entrypoints: Vec<String>) {
  let stdin = tokio::io::stdin();
  let stdout = tokio::io::stdout();

  let arenas = &*Box::leak(Box::new(CoreArenas::default()));
  let core = &*Box::leak(Box::new(Core::new(arenas, true)));

  let (service, socket) = LspService::new(|client| Backend {
    core,
    compiler: RwLock::new(Compiler::new(core, Config::default())),
    client,
    entrypoints,
  });
  Server::new(stdin, stdout, socket).serve(service).await;
}
