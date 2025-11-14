use std::{collections::HashMap, future::Future, sync::Arc, time::Instant};

use futures::{StreamExt, stream::FuturesUnordered};
use tokio::sync::RwLock;
use tower_lsp::{Client, LanguageServer, LspService, Server, jsonrpc::Result, lsp_types::*};

use vine::{
  compiler::Compiler, components::loader::Loader, features::cfg::Config, structures::diag::Diag,
  tools::fmt::Formatter,
};

#[derive(Debug)]
struct Backend {
  client: Client,
  entrypoints: Vec<String>,
  docs: Arc<RwLock<HashMap<Url, String>>>,
}

impl Backend {
  fn refresh(&self) -> impl Future<Output = ()> + Send + '_ {
    let mut compiler = Compiler::new(true, Config::default());

    for glob in &self.entrypoints {
      for entry in glob::glob(glob).unwrap() {
        let e = entry.unwrap();
        compiler.loader.load_mod(&e, &mut compiler.diags);
      }
    }

    let start = Instant::now();
    let diags = match compiler.compile(()) {
      Ok(_) => Vec::new(),
      Err(diags) => diags,
    };
    eprintln!("compiled in {:?}", start.elapsed());

    self.report(&compiler.loader, diags)
  }

  fn report(
    &self,
    loader: &Loader,
    mut diags: Vec<Diag>,
  ) -> impl Future<Output = ()> + Send + use<'_> {
    diags.sort_by_key(|d| Some(d.span()?.file));
    let mut diags = diags.into_iter().peekable();
    while diags.peek().is_some_and(|x| x.span().is_none()) {
      diags.next();
    }
    let futures = FuturesUnordered::new();
    for (i, file) in loader.files.iter() {
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
        document_formatting_provider: Some(OneOf::Left(true)),
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
pub async fn lsp(entrypoints: Vec<String>) {
  let stdin = tokio::io::stdin();
  let stdout = tokio::io::stdout();

  let (service, socket) = LspService::new(|client| Backend {
    client,
    entrypoints,
    docs: Arc::new(RwLock::new(HashMap::new())),
  });
  Server::new(stdin, stdout, socket).serve(service).await;
}
