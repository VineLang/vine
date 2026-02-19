use std::{
  collections::{BTreeMap, HashMap},
  env::current_dir,
  fs,
  path::{Path, PathBuf},
  time::Instant,
};

use futures::{StreamExt, stream::FuturesUnordered};
use tokio::sync::RwLock;
use tower_lsp::{Client, LanguageServer, LspService, Server, jsonrpc::Result, lsp_types::*};

use vine::{
  compiler::Compiler,
  components::{
    loader::{FileId, Loader, RealFS},
    parser::Parser,
  },
  structures::{
    ast::{Ident, Item, ItemKind, Span, visit::VisitMut},
    checkpoint::Checkpoint,
    diag::Diag,
  },
  tools::fmt::Formatter,
};
use vine_util::idx::IdxVec;

struct Backend {
  client: Client,
  entrypoints: Vec<String>,
  docs: RwLock<HashMap<Url, String>>,
  lsp: RwLock<Lsp>,
  checkpoint: Checkpoint,
}

impl Backend {
  async fn refresh(&self) {
    let mut lsp = self.lsp.write().await;
    let lsp = &mut *lsp;
    lsp.compiler.revert(&self.checkpoint);
    lsp.file_paths.truncate(self.checkpoint.files.0);

    let mut loader = Loader::new(&mut lsp.compiler, RealFS, Some(&mut lsp.file_paths));
    for glob in &self.entrypoints {
      for entry in glob::glob(glob).unwrap() {
        let path = entry.unwrap();
        if let Some(name) = RealFS::detect_name(&path) {
          loader.load_mod(name, path);
        }
      }
    }

    for id in lsp.file_paths.keys_from(self.checkpoint.files) {
      let path = &mut lsp.file_paths[id];
      if let Ok(path_) = fs::canonicalize(&path) {
        *path = path_;
      }
    }

    let start = Instant::now();
    _ = lsp.compiler.check(());
    eprintln!("compiled in {:?}", start.elapsed());

    let mut diags_by_file = HashMap::<FileId, (Vec<&Diag>, Vec<&Diag>)>::new();
    for diag in &lsp.compiler.diags.errors {
      let Some(span) = diag.span() else { continue };
      diags_by_file.entry(span.file).or_default().0.push(diag);
    }
    for diag in &lsp.compiler.diags.warnings {
      let Some(span) = diag.span() else { continue };
      diags_by_file.entry(span.file).or_default().1.push(diag);
    }

    let futures = FuturesUnordered::new();
    for file_id in lsp.compiler.files.keys() {
      let mut out = Vec::new();
      if let Some((errors, warnings)) = diags_by_file.get(&file_id) {
        for diag in errors {
          out.push(lsp.diag_to_diagnostic(diag, DiagnosticSeverity::ERROR));
        }
        for diag in warnings {
          out.push(lsp.diag_to_diagnostic(diag, DiagnosticSeverity::WARNING));
        }
      }
      let uri = lsp.file_to_uri(file_id);
      futures.push(self.client.publish_diagnostics(uri, out, None));
    }
    futures.collect::<()>().await;
  }
}

struct Lsp {
  compiler: Compiler,
  file_paths: IdxVec<FileId, PathBuf>,
}

impl Lsp {
  fn workspace_symbols(&self, params: WorkspaceSymbolParams) -> Option<Vec<SymbolInformation>> {
    let query = params.query.to_lowercase();
    let chart = &self.compiler.chart;
    let mut results = []
      .into_iter()
      .chain(chart.structs.values().map(|x| (SymbolKind::STRUCT, &x.name, x.span, x.def)))
      .chain(chart.enums.values().map(|x| (SymbolKind::ENUM, &x.name, x.span, x.def)))
      .chain(chart.concrete_fns.values().map(|x| (SymbolKind::FUNCTION, &x.name, x.span, x.def)))
      .chain(chart.concrete_consts.values().map(|x| (SymbolKind::CONSTANT, &x.name, x.span, x.def)))
      .chain(chart.traits.values().map(|x| (SymbolKind::INTERFACE, &x.name, x.span, x.def)))
      .filter(|(_, name, _, _)| name.0.to_lowercase().contains(&query))
      .map(|(kind, name, span, def)| SymbolInformation {
        name: name.0.clone(),
        kind,
        tags: None,
        #[allow(deprecated)]
        deprecated: None,
        location: self.span_to_location(span),
        container_name: Some(chart.defs[chart.defs[def].parent.unwrap()].name.0.clone()),
      })
      .collect::<Vec<_>>();
    results.sort_by(|a, b| a.name.cmp(&b.name));
    Some(results)
  }

  fn document_symbols(&self, params: DocumentSymbolParams) -> Option<DocumentSymbolResponse> {
    let file = self.uri_to_file_id(params.text_document.uri)?;
    let ast = Parser::parse(file, &self.compiler.files[file].src).ok()?;
    struct Visitor<'a> {
      lsp: &'a Lsp,
      symbols: &'a mut Vec<DocumentSymbol>,
    }
    impl<'t> VisitMut<'t> for Visitor<'_> {
      fn visit_item(&mut self, item: &'t mut Item) {
        let (kind, name) = match &item.kind {
          ItemKind::Fn(i) => (SymbolKind::FUNCTION, i.name.clone()),
          ItemKind::Const(i) => (SymbolKind::CONSTANT, i.name.clone()),
          ItemKind::Struct(i) => (SymbolKind::STRUCT, i.name.clone()),
          ItemKind::Enum(i) => (SymbolKind::ENUM, i.name.clone()),
          ItemKind::Type(i) => (SymbolKind::INTERFACE, i.name.clone()),
          ItemKind::Mod(i) => (SymbolKind::MODULE, i.name.clone()),
          ItemKind::Trait(i) => (SymbolKind::INTERFACE, i.name.clone()),
          ItemKind::Impl(i) => {
            (SymbolKind::CLASS, i.name.clone().unwrap_or_else(|| Ident("impl".into())))
          }
          ItemKind::Use(_) | ItemKind::OuterMod | ItemKind::Taken => return,
        };
        let mut children = Vec::new();
        Visitor { symbols: &mut children, ..*self }._visit_item(item);
        self.symbols.push(DocumentSymbol {
          name: name.0.to_owned(),
          kind,
          detail: None,
          tags: None,
          #[allow(deprecated)]
          deprecated: None,
          range: self.lsp.span_to_range(item.span),
          selection_range: self.lsp.span_to_range(item.name_span),
          children: Some(children),
        });
      }
    }
    let mut symbols = Vec::new();
    Visitor { lsp: self, symbols: &mut symbols }.visit(&mut { ast });
    Some(DocumentSymbolResponse::Nested(symbols))
  }

  fn definitions(&self, params: GotoDefinitionParams) -> Option<GotoDefinitionResponse> {
    let span = self.document_position_to_span(params.text_document_position_params)?;
    let (_, defs) = self.lookup_span(&self.compiler.annotations.definitions, span)?;
    Some(GotoDefinitionResponse::Array(defs.iter().map(|&x| self.span_to_location(x)).collect()))
  }

  fn references(&self, params: ReferenceParams) -> Option<Vec<Location>> {
    let span = self.document_position_to_span(params.text_document_position)?;
    let (_, refs) =
      self.lookup_span(&self.compiler.annotations.references, span).or_else(|| {
        let (_, def) = self.get_def(span)?;
        self.lookup_span(&self.compiler.annotations.references, def)
      })?;
    Some(refs.iter().map(|&x| self.span_to_location(x)).collect())
  }

  fn occurrences(&self, params: DocumentHighlightParams) -> Option<Vec<DocumentHighlight>> {
    let span = self.document_position_to_span(params.text_document_position_params)?;
    let file = span.file;
    let span = self.get_def(span).map(|x| x.1).unwrap_or(span);
    let (span, refs) = self.lookup_span(&self.compiler.annotations.references, span)?;
    Some(
      refs
        .iter()
        .copied()
        .chain([span])
        .filter(|span| span.file == file)
        .map(|span| DocumentHighlight { range: self.span_to_range(span), kind: None })
        .collect(),
    )
  }

  fn hover(&self, params: HoverParams) -> Option<Hover> {
    let span = self.document_position_to_span(params.text_document_position_params)?;
    let (span, hover) =
      self.lookup_span(&self.compiler.annotations.hovers, span).or_else(|| {
        let (span, def) = self.get_def(span)?;
        let (_, hover) = self.lookup_span(&self.compiler.annotations.hovers, def)?;
        Some((span, hover))
      })?;
    let mut str = String::new();
    if !hover.signatures.is_empty() {
      str += "```vi\n";
      for sig in &hover.signatures {
        str += sig;
        str += "\n";
      }
      str += "```\n";
    }
    for doc in &hover.docs {
      let doc = doc.strip_prefix("///").unwrap();
      let doc = doc.strip_prefix(" ").unwrap_or(doc);
      str += doc;
      str += "\n";
    }
    Some(Hover {
      contents: HoverContents::Markup(MarkupContent { kind: MarkupKind::Markdown, value: str }),
      range: Some(self.span_to_range(span)),
    })
  }

  fn lookup_span<'a, T>(&self, map: &'a BTreeMap<Span, T>, span: Span) -> Option<(Span, &'a T)> {
    let (&found, value) = map.range(..=&span).next_back()?;
    (found.file == span.file && found.start <= span.start && found.end >= span.end)
      .then_some((found, value))
  }

  fn get_def(&self, span: Span) -> Option<(Span, Span)> {
    let (span, defs) = self.lookup_span(&self.compiler.annotations.definitions, span)?;
    (defs.len() == 1).then(|| (span, *defs.iter().next().unwrap()))
  }

  fn file_to_uri(&self, file: FileId) -> Url {
    let mut path = current_dir().unwrap();
    path.push(&self.file_paths[file]);
    Url::from_file_path(&path).unwrap()
  }

  fn uri_to_file_id(&self, uri: Url) -> Option<FileId> {
    Some(self.file_paths.iter().find(|(_, path)| **path == AsRef::<Path>::as_ref(&uri.path()))?.0)
  }

  fn byte_to_position(&self, file: FileId, byte: usize) -> Position {
    let pos = self.compiler.files[file].get_pos(byte);
    Position { line: pos.line as u32, character: pos.col as u32 }
  }

  fn span_to_range(&self, span: Span) -> Range {
    Range {
      start: self.byte_to_position(span.file, span.start),
      end: self.byte_to_position(span.file, span.end),
    }
  }

  fn position_to_byte(&self, file: FileId, position: Position) -> Option<usize> {
    Some(
      self.compiler.files[file].line_starts.get(position.line as usize)?
        + position.character as usize,
    )
  }

  fn position_to_span(&self, file: FileId, position: Position) -> Option<Span> {
    let byte = self.position_to_byte(file, position)?;
    Some(Span { file, start: byte, end: byte })
  }

  fn document_position_to_span(&self, doc_pos: TextDocumentPositionParams) -> Option<Span> {
    let file = self.uri_to_file_id(doc_pos.text_document.uri)?;
    self.position_to_span(file, doc_pos.position)
  }

  fn span_to_location(&self, span: Span) -> Location {
    Location { uri: self.file_to_uri(span.file), range: self.span_to_range(span) }
  }

  fn diag_to_diagnostic(&self, diag: &Diag, severity: DiagnosticSeverity) -> Diagnostic {
    let span = diag.span().unwrap();
    Diagnostic {
      range: self.span_to_range(span),
      severity: Some(severity),
      code: None,
      code_description: None,
      source: Some("vine".into()),
      message: diag.to_string(),
      related_information: None,
      tags: None,
      data: None,
    }
  }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
  async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
    let supports_utf8 = params.capabilities.general.is_some_and(|x| {
      x.position_encodings.is_some_and(|x| x.contains(&PositionEncodingKind::UTF8))
    });
    Ok(InitializeResult {
      server_info: None,
      capabilities: ServerCapabilities {
        position_encoding: Some(if supports_utf8 {
          PositionEncodingKind::UTF8
        } else {
          // TODO(#411): handle properly
          PositionEncodingKind::UTF16
        }),
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        document_formatting_provider: Some(OneOf::Left(true)),
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
        document_highlight_provider: Some(OneOf::Left(true)),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        ..ServerCapabilities::default()
      },
    })
  }

  async fn initialized(&self, _: InitializedParams) {
    self.refresh().await
  }

  async fn did_save(&self, _: DidSaveTextDocumentParams) {
    self.refresh().await
  }

  async fn did_open(&self, params: DidOpenTextDocumentParams) {
    let uri = params.text_document.uri;
    let text = params.text_document.text;

    self.docs.write().await.insert(uri, text);
  }

  async fn did_change(&self, params: DidChangeTextDocumentParams) {
    let uri = params.text_document.uri;
    if let Some(change) = params.content_changes.into_iter().last() {
      self.docs.write().await.insert(uri, change.text);
    }
  }

  async fn did_close(&self, params: DidCloseTextDocumentParams) {
    let uri = params.text_document.uri;
    self.docs.write().await.remove(&uri);
  }

  async fn symbol(&self, params: WorkspaceSymbolParams) -> Result<Option<Vec<SymbolInformation>>> {
    Ok(self.lsp.read().await.workspace_symbols(params))
  }

  async fn document_symbol(
    &self,
    params: DocumentSymbolParams,
  ) -> Result<Option<DocumentSymbolResponse>> {
    Ok(self.lsp.read().await.document_symbols(params))
  }

  async fn goto_definition(
    &self,
    params: GotoDefinitionParams,
  ) -> Result<Option<GotoDefinitionResponse>> {
    Ok(self.lsp.read().await.definitions(params))
  }

  async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
    Ok(self.lsp.read().await.references(params))
  }

  async fn document_highlight(
    &self,
    params: DocumentHighlightParams,
  ) -> Result<Option<Vec<DocumentHighlight>>> {
    Ok(self.lsp.read().await.occurrences(params))
  }

  async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
    Ok(self.lsp.read().await.hover(params))
  }

  async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
    let uri = params.text_document.uri;

    let src = {
      let docs = self.docs.read().await;
      match docs.get(&uri) {
        Some(text) => text.clone(),
        None => return Ok(None),
      }
    };

    let Ok(formatted) = Formatter::fmt(&src) else {
      return Ok(None);
    };
    if formatted == src {
      return Ok(Some(vec![]));
    }
    Ok(Some(vec![TextEdit {
      range: Range { start: Position::new(0, 0), end: Position::new(u32::MAX, u32::MAX) },
      new_text: formatted,
    }]))
  }

  async fn shutdown(&self) -> Result<()> {
    Ok(())
  }
}

#[allow(clippy::absolute_paths)]
#[tokio::main]
pub async fn lsp(
  mut compiler: Compiler,
  mut file_paths: IdxVec<FileId, PathBuf>,
  entrypoints: Vec<String>,
) {
  let stdin = tokio::io::stdin();
  let stdout = tokio::io::stdout();

  _ = compiler.check(());
  let checkpoint = compiler.checkpoint();

  for path in file_paths.values_mut() {
    if let Ok(path_) = fs::canonicalize(&path) {
      *path = path_;
    }
  }

  let (service, socket) = LspService::new(|client| Backend {
    client,
    entrypoints,
    docs: RwLock::new(HashMap::new()),
    lsp: RwLock::new(Lsp { compiler, file_paths }),
    checkpoint,
  });
  Server::new(stdin, stdout, socket).serve(service).await;
}
