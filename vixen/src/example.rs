use anyhow::Result;
use async_fs as afs;
use serde::{Deserialize, Serialize};
use serde_json::from_str as from_json;
use std::path::{Path, PathBuf};

use crate::{Build, Fetch, JobSet, Jobs, React, Store, TaskBuild, hash::Blake3};

pub async fn init(args: Vec<String>, store: &Store) -> Result<JobSet> {
  let file = store.store(&afs::read(&args[0]).await?).await?;
  Ok(JobSet::new(|jobs: &mut Jobs| {
    let file = jobs.store(file);
    jobs.react(Main, [file])
  }))
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash)]
pub struct Get {
  url: String,
}

impl Fetch for Get {
  fn name(&self) -> String {
    format!("get-{}", self.url)
  }

  async fn fetch(&self, _: Vec<PathBuf>, output: &Path) -> Result<()> {
    let result = reqwest::get(&self.url).await?.bytes().await?;
    afs::write(&output, result).await?;
    Ok(())
  }
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash)]
pub struct GetAll {
  then: Box<TaskBuild>,
}

impl React for GetAll {
  fn name(&self) -> String {
    "get-all".into()
  }

  async fn react(&self, inputs: Vec<PathBuf>, _: &Store) -> Result<JobSet> {
    let files = from_json::<Vec<File>>(&afs::read_to_string(&inputs[0]).await?)?;
    Ok(JobSet::new(|jobs: &mut Jobs| {
      let inputs = files
        .into_iter()
        .map(|file| jobs.fetch(Get { url: file.url }, file.hash))
        .collect::<Vec<_>>();
      jobs.build((*self.then).clone(), inputs)
    }))
  }
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash)]
pub struct Concat;

impl Build for Concat {
  fn name(&self) -> String {
    "concat".into()
  }

  async fn build(&self, inputs: Vec<PathBuf>, output: &Path) -> Result<()> {
    let mut text = String::new();
    for input in inputs {
      text += &afs::read_to_string(&input).await?;
    }
    afs::write(output, &text).await?;
    Ok(())
  }
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash)]
pub struct Main;

impl React for Main {
  fn name(&self) -> String {
    "main".into()
  }

  async fn react(&self, inputs: Vec<PathBuf>, _: &Store) -> Result<JobSet> {
    let file = from_json::<File>(&afs::read_to_string(&inputs[0]).await?)?;
    Ok(JobSet::new(|jobs: &mut Jobs| {
      let file = jobs.fetch(Get { url: file.url }, file.hash);
      jobs.react(GetAll { then: Box::new(Concat.into()) }, [file])
    }))
  }
}

#[derive(Deserialize)]
struct File {
  url: String,
  hash: Blake3,
}
