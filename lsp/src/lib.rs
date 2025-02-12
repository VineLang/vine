use std::future::Future;

use futures::{stream::FuturesUnordered, StreamExt};
use tower_lsp::{jsonrpc::Result, lsp_types::*, Client, LanguageServer, LspService, Server};

use vine::{
  chart::Chart,
  charter::Charter,
  checker::{ChartTypes, Checker},
  core::{Core, CoreArenas},
  diag::Diag,
  loader::Loader,
  resolver::Resolver,
};

#[derive(Debug)]
struct Backend {
  client: Client,
  entrypoints: Vec<String>,
}

impl Backend {
  fn refresh(&self) -> impl Future<Output = ()> + Send + '_ {
    let arenas = &CoreArenas::default();
    let core = &Core::new(arenas);

    let mut loader = Loader::new(core);
    for glob in &self.entrypoints {
      for entry in glob::glob(glob).unwrap() {
        let e = entry.unwrap();
        loader.load_mod(e);
      }
    }

    if core.has_diags() {
      return self.report(core, core.take_diags());
    }

    let root = loader.finish();

    let chart = &mut Chart::default();
    Charter { core, chart }.chart_root(root);
    Resolver { core, chart }.resolve_all();
    Checker::new(core, chart, &mut ChartTypes::default()).check_all();

    self.report(core, core.take_diags())
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
        let start = file.get_pos(span.start);
        let end = file.get_pos(span.end);
        out.push(Diagnostic {
          range: Range {
            start: Position { line: start.line as u32, character: start.col as u32 },
            end: Position { line: end.line as u32, character: end.col as u32 },
          },
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

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
  async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
    Ok(InitializeResult {
      server_info: None,
      capabilities: ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        workspace: Some(WorkspaceServerCapabilities {
          workspace_folders: Some(WorkspaceFoldersServerCapabilities {
            supported: Some(true),
            change_notifications: Some(OneOf::Left(true)),
          }),
          file_operations: None,
        }),
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

  async fn shutdown(&self) -> Result<()> {
    Ok(())
  }
}

#[allow(clippy::absolute_paths)]
#[tokio::main]
pub async fn lsp(entrypoints: Vec<String>) {
  let stdin = tokio::io::stdin();
  let stdout = tokio::io::stdout();

  let (service, socket) = LspService::new(|client| Backend { client, entrypoints });
  Server::new(stdin, stdout, socket).serve(service).await;
}
