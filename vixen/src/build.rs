use std::{
  collections::HashMap,
  fs::FileTimes,
  io::{self},
  path::{Path, PathBuf},
  sync::Arc,
  time::SystemTime,
};

use anyhow::{Error, Result, anyhow};
use async_fs as afs;
use blocking::unblock;
use std::fs;
use tempdir::TempDir;
use tokio::sync::Mutex;
use vine_util::unwrap_vec;

use futures::{
  FutureExt, StreamExt,
  future::{BoxFuture, Shared},
  stream::FuturesUnordered,
};

use crate::{Build, Fetch, Job, JobId, JobSet, Jobs, React, Store, Task, hash::Blake3};

#[derive(Clone)]
pub struct Spawner<'a> {
  #[allow(clippy::type_complexity)]
  pub(crate) spawned: Arc<Mutex<HashMap<JobId, Shared<BoxFuture<'a, Result<PathBuf, Reported>>>>>>,
  pub(crate) errors: &'a Mutex<Vec<Error>>,
  pub(crate) tmp_dir: &'a TempDir,
}

#[derive(Debug, Clone, Copy)]
pub struct Reported(());

enum ErrorOrReported {
  Error(Error),
  Reported(Reported),
}

impl<T: Into<anyhow::Error>> From<T> for ErrorOrReported {
  fn from(error: T) -> ErrorOrReported {
    ErrorOrReported::Error(error.into())
  }
}

impl From<Reported> for ErrorOrReported {
  fn from(reported: Reported) -> ErrorOrReported {
    ErrorOrReported::Reported(reported)
  }
}

impl<'a> Spawner<'a> {
  pub fn spawn(
    &self,
    jobs: Arc<Jobs>,
    job_id: JobId,
  ) -> impl Send + Future<Output = Result<PathBuf, Reported>> {
    async move {
      let self_ = self.clone();
      {
        self
          .spawned
          .lock()
          .await
          .entry(job_id)
          .or_insert_with(move || {
            async move {
              match self_._spawn(jobs, job_id).await {
                Ok(path) => Ok(path),
                Err(ErrorOrReported::Reported(reported)) => Err(reported),
                Err(ErrorOrReported::Error(error)) => {
                  self_.errors.lock().await.push(error);
                  Err(Reported(()))
                }
              }
            }
            .boxed()
            .shared()
          })
          .clone()
      }
      .await
    }
  }

  async fn _spawn(&self, jobs: Arc<Jobs>, job_id: JobId) -> Result<PathBuf, ErrorOrReported> {
    let job = &jobs.jobs[&job_id];
    let (dir, hash) = match &job.task {
      Task::Store { hash } => ("store", &hash.0),
      Task::Fetch { hash, .. } => ("fetch", hash),
      Task::Build { .. } => ("build", &job_id.0),
      Task::React { .. } => ("react", &job_id.0),
    };
    let output = AsRef::<Path>::as_ref(dir).join(hash.encode(&mut [0; _]));
    let output_ = output.clone();
    match unblock(move || fs::set_times(output_, FileTimes::new().set_accessed(SystemTime::now())))
      .await
    {
      Ok(()) => match &job.task {
        Task::Store { .. } | Task::Fetch { .. } | Task::Build { .. } => Ok(output),
        Task::React { .. } => Ok(PathBuf::from(afs::read_to_string(output).await?)),
      },
      Err(err) if matches!(err.kind(), io::ErrorKind::NotFound) => match &job.task {
        Task::Store { .. } => Err(anyhow!("missing store file"))?,
        Task::Fetch { hash, task } => {
          let tmp = self.tmp_path(&job_id);
          let inputs = self.get_inputs(&jobs, job).await?;
          task.fetch(inputs, &tmp).await?;
          if Blake3::hash_file(&tmp).await? != *hash {
            Err(anyhow!("invalid hash"))?
          }
          _ = afs::rename(tmp, &output).await;
          Ok(output)
        }
        Task::Build { task } => {
          let tmp = self.tmp_path(&job_id);
          let inputs = self.get_inputs(&jobs, job).await?;
          task.build(inputs, &tmp).await?;
          _ = afs::rename(tmp, &output).await;
          Ok(output)
        }
        Task::React { task } => {
          let inputs = self.get_inputs(&jobs, job).await?;
          let JobSet { root, jobs } = task.react(inputs, &Store { dir: "store".into() }).await?;
          let path = self.spawn(Arc::new(jobs), root).await?;
          _ = afs::write(output, path.as_os_str().as_encoded_bytes()).await;
          Ok(path)
        }
      },
      Err(err) => Err(err)?,
    }
  }

  async fn get_inputs(&self, jobs: &Arc<Jobs>, job: &Job) -> Result<Vec<PathBuf>, Reported> {
    let mut inputs = vec![None; job.inputs.len()];
    let mut children = job
      .inputs
      .iter()
      .enumerate()
      .map(|(i, &job_id)| {
        let jobs = jobs.clone();
        async move { Ok::<_, Reported>((i, self.spawn(jobs, job_id).await?)) }
      })
      .collect::<FuturesUnordered<_>>();
    while let Some(child) = children.next().await {
      let (i, path) = child?;
      inputs[i] = Some(path);
    }
    Ok(unwrap_vec(inputs))
  }

  fn tmp_path(&self, job_id: &JobId) -> PathBuf {
    self.tmp_dir.path().join(job_id.0.encode(&mut [0; _]))
  }
}
