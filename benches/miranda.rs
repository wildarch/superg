use regex::Regex;
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};
use std::time::Duration;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Stats {
    pub runtime: Duration,
    pub cycles: usize,
    pub result: i32,
}

pub fn run_miranda(file_path: &Path, command: &str) -> Stats {
    let miranda_path = std::env::var("MIRANDA_PATH").expect("'MIRANDA_PATH' must be set");
    let mut mira = Command::new("./mira")
        .arg(file_path)
        .current_dir(miranda_path)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .spawn()
        .expect("error starting miranda");
    {
        let mut stdin = mira.stdin.take().unwrap();
        writeln!(stdin, "{}", command).unwrap();
        // stdin dropped and closed here
    }

    let output = mira.wait_with_output().expect("error running miranda");

    let stdout = String::from_utf8(output.stdout).unwrap();

    let stats_regex = Regex::new("Nanos: (\\d+)\nCycles: (\\d+)\n(\\d+)").unwrap();
    let caps = match stats_regex.captures(&stdout) {
        Some(caps) => caps,
        None => panic!("stats not found, stdout: {}", stdout),
    };
    let nanos: u64 = caps.get(1).unwrap().as_str().parse().unwrap();
    let cycles = caps.get(2).unwrap().as_str().parse().unwrap();
    let result = caps.get(3).unwrap().as_str().parse().unwrap();
    Stats {
        runtime: Duration::from_nanos(nanos),
        cycles,
        result,
    }
}
