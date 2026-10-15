#![feature(fs_set_times)]

pub mod build;
pub mod hash;

use macro_rules_attribute::derive;

use anyhow::Result;
use async_fs as afs;
use derive_more::From;
use serde::{Deserialize, Serialize, Serializer, ser::SerializeStruct};
use serde_json::{from_str as from_json, to_string as to_json};
use std::{
  collections::HashMap,
  env::{self, set_current_dir},
  path::{Path, PathBuf},
  sync::Arc,
};
use tempdir::TempDir;
use vine_util::for_enum;

use crate::{build::Spawner, hash::Blake3};

mod example;
use example::*;

#[derive(Debug, Clone, Copy, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub struct JobId(Blake3);

#[derive(Debug, Serialize)]
pub struct JobSet {
  pub root: JobId,
  pub jobs: Jobs,
}

impl JobSet {
  pub fn new(f: impl FnOnce(&mut Jobs) -> JobId) -> JobSet {
    let mut jobs = Jobs::default();
    let root = f(&mut jobs);
    jobs.root(root)
  }
}

#[derive(Debug, Default)]
pub struct Jobs {
  jobs: HashMap<JobId, Job>,
}

impl Serialize for Jobs {
  fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    self.jobs.serialize(serializer)
  }
}

impl Jobs {
  pub fn root(self, root: JobId) -> JobSet {
    JobSet { root, jobs: self }
  }

  pub fn store(&mut self, hash: StoredHash) -> JobId {
    self.task(Task::Store { hash }, [])
  }

  pub fn fetch(&mut self, task: impl Into<TaskFetch>, hash: Blake3) -> JobId {
    self.task(Task::Fetch { hash, task: task.into() }, [])
  }

  pub fn build(
    &mut self,
    task: impl Into<TaskBuild>,
    inputs: impl IntoIterator<Item = JobId>,
  ) -> JobId {
    self.task(Task::Build { task: task.into() }, inputs)
  }

  pub fn react(
    &mut self,
    task: impl Into<TaskReact>,
    inputs: impl IntoIterator<Item = JobId>,
  ) -> JobId {
    self.task(Task::React { task: task.into() }, inputs)
  }

  pub fn task(&mut self, task: Task, inputs: impl IntoIterator<Item = JobId>) -> JobId {
    let inputs = inputs.into_iter().collect::<Vec<_>>();
    #[derive(Hash)]
    enum JobHash<'a> {
      Hash(&'a Blake3),
      Build(&'a TaskBuild, &'a Vec<JobId>),
      React(&'a TaskReact, &'a Vec<JobId>),
    }
    let job_id = JobId(Blake3::hash_value(&match &task {
      Task::Store { hash: StoredHash(hash) } | Task::Fetch { hash, .. } => JobHash::Hash(hash),
      Task::Build { task } => JobHash::Build(task, &inputs),
      Task::React { task } => JobHash::React(task, &inputs),
    }));
    self.jobs.insert(job_id, Job { task, inputs });
    job_id
  }
}

#[derive(Debug)]
struct Job {
  task: Task,
  inputs: Vec<JobId>,
}

impl Serialize for Job {
  fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    let mut job = serializer.serialize_struct("Job", 3)?;
    job.serialize_field("name", &self.task.name())?;
    job.serialize_field("task", &self.task)?;
    job.serialize_field("inputs", &self.inputs)?;
    job.end()
  }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum Task {
  Store { hash: StoredHash },
  Fetch { hash: Blake3, task: TaskFetch },
  Build { task: TaskBuild },
  React { task: TaskReact },
}

impl Task {
  pub fn name(&self) -> String {
    match self {
      Task::Store { .. } => "store".into(),
      Task::Fetch { task, .. } => task.name(),
      Task::Build { task } => task.name(),
      Task::React { task } => task.name(),
    }
  }
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, From, for_enum!)]
pub enum TaskFetch {
  Get(Get),
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, From, for_enum!)]
pub enum TaskBuild {
  Concat(Concat),
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, From, for_enum!)]
pub enum TaskReact {
  Main(Main),
  GetAll(GetAll),
}

pub trait Fetch {
  fn name(&self) -> String;
  fn fetch(&self, inputs: Vec<PathBuf>, output: &Path) -> impl Send + Future<Output = Result<()>>;
}

pub trait Build {
  fn name(&self) -> String;
  fn build(&self, inputs: Vec<PathBuf>, output: &Path) -> impl Send + Future<Output = Result<()>>;
}

pub trait React {
  fn name(&self) -> String;
  fn react(
    &self,
    inputs: Vec<PathBuf>,
    store: &Store,
  ) -> impl Send + Future<Output = Result<JobSet>>;
}

impl Fetch for TaskFetch {
  fn name(&self) -> String {
    for_enum!(Variant in TaskFetch {
      match self {
        $(TaskFetch::$Variant(self_) => self_.name()),*
      }
    })
  }

  async fn fetch(&self, inputs: Vec<PathBuf>, output: &Path) -> Result<()> {
    for_enum!(Variant in TaskFetch {
      match self {
        $(TaskFetch::$Variant(self_) => self_.fetch(inputs, output).await),*
      }
    })
  }
}

impl Build for TaskBuild {
  fn name(&self) -> String {
    for_enum!(Variant in TaskBuild {
      match self {
        $(TaskBuild::$Variant(self_) => self_.name()),*
      }
    })
  }

  async fn build(&self, inputs: Vec<PathBuf>, output: &Path) -> Result<()> {
    for_enum!(Variant in TaskBuild {
      match self {
        $(TaskBuild::$Variant(self_) => self_.build(inputs, output).await),*
      }
    })
  }
}

impl React for TaskReact {
  fn name(&self) -> String {
    for_enum!(Variant in TaskReact {
      match self {
        $(TaskReact::$Variant(self_) => self_.name()),*
      }
    })
  }

  async fn react(&self, inputs: Vec<PathBuf>, store: &Store) -> Result<JobSet> {
    for_enum!(Variant in TaskReact {
      match self {
        $(TaskReact::$Variant(self_) => self_.react(inputs, store).await),*
      }
    })
  }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, Hash)]
pub struct StoredHash(pub Blake3);

pub struct Store {
  dir: PathBuf,
}

impl Store {
  pub async fn store(&self, bytes: &[u8]) -> Result<StoredHash> {
    let hash = Blake3::hash(bytes);
    afs::write(self.dir.join(hash.encode(&mut [0; _])), bytes).await?;
    Ok(StoredHash(hash))
  }
}

#[tokio::main]
async fn main() -> Result<()> {
  let mut args = env::args().peekable();
  _ = args.next();
  if args.peek().is_some_and(|arg| arg == "_nix") {
    _ = args.next();
    let output = PathBuf::from(env::var("out").unwrap());
    if args.peek().is_some_and(|arg| arg == "_init") {
      _ = args.next();
      afs::create_dir(&output).await?;
      let job_set = init(args.collect(), &Store { dir: output.clone() }).await?;
      afs::write(output.join("job_set"), to_json(&job_set)?).await?;
    } else {
      let task = from_json::<Task>(&args.next().unwrap())?;
      let inputs = args.map(PathBuf::from).collect::<Vec<_>>();
      match task {
        Task::Store { hash } => {
          let store = PathBuf::from(env::var("store").unwrap());
          afs::copy(store.join(hash.0.encode(&mut [0; _])), &output).await?;
        }
        Task::Fetch { task, .. } => {
          task.fetch(inputs, &output).await?;
        }
        Task::Build { task } => {
          task.build(inputs, &output).await?;
        }
        Task::React { task } => {
          afs::create_dir(&output).await?;
          let job_set = task.react(inputs, &Store { dir: output.clone() }).await?;
          afs::write(output.join("job_set"), to_json(&job_set)?).await?;
        }
      }
    }
  } else {
    let build_dir = PathBuf::from("_/vixen");
    let store_dir = build_dir.join("store");
    afs::create_dir_all(&store_dir).await?;
    let JobSet { jobs, root } = init(args.collect(), &Store { dir: store_dir }).await?;
    set_current_dir(&build_dir)?;
    for subdir in ["fetch", "build", "react", "tmp"] {
      afs::create_dir_all(subdir).await?;
    }
    let errors = Default::default();
    let tmp_dir = &TempDir::new_in("tmp", "")?;
    let spawner = Spawner { spawned: Default::default(), errors: &errors, tmp_dir };
    let result = spawner.spawn(Arc::new(jobs), root).await;
    drop(spawner);
    match result {
      Ok(result) => {
        println!("{}", build_dir.join(result).display());
      }
      Err(_) => {
        for error in errors.into_inner() {
          eprintln!("{:?}\n", error);
        }
        anyhow::bail!("build failed")
      }
    }
  }
  Ok(())
}
