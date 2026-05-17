#![allow(clippy::multiple_crate_versions)]
use anyhow::{Context, Result};
use ignore::WalkBuilder;
use std::fs;
use std::io::{BufRead, BufReader, Write};
use std::path::Path;
fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.is_empty() {
        process_path(Path::new("."));
    } else {
        for arg in args {
            process_path(Path::new(&arg));
        }
    }
    Ok(())
}
fn process_path(root: &Path) {
    let walker = WalkBuilder::new(root).require_git(false).build();
    for result in walker {
        match result {
            Ok(entry) => {
                let path = entry.path();
                if path.is_file() {
                    if let Err(e) = remove_empty_lines(path) {
                        let path_display = path.display();
                        eprintln!("Error processing {path_display}: {e}");
                    }
                }
            }
            Err(err) => eprintln!("Error walking path: {err}"),
        }
    }
}
fn remove_empty_lines(path: &Path) -> Result<()> {
    let path_display = path.display();
    let data = fs::read(path).with_context(|| format!("Failed to read file: {path_display}"))?;
    if content_inspector::inspect(&data).is_binary() {
        return Ok(());
    }
    let reader = BufReader::new(&data[..]);
    let mut new_lines = Vec::new();
    for line_result in reader.lines() {
        let line = line_result?;
        if !line.trim().is_empty() {
            new_lines.push(line);
        }
    }
    let mut output = Vec::new();
    for line in new_lines {
        writeln!(output, "{line}")?;
    }
    if output != data {
        fs::write(path, output).with_context(|| format!("Failed to write file: {path_display}"))?;
    }
    Ok(())
}
#[cfg(test)]
mod tests {
    use super::*;
    use std::env;
    #[test]
    fn test_main_and_process_path() -> Result<()> {
        use tempfile::tempdir;
        let dir = tempdir()?;
        let root = dir.path();
        let file1_path = root.join("test.txt");
        fs::write(&file1_path, "line1\n\nline2\n   \nline3\n")?;
        let gitignore_path = root.join(".gitignore");
        fs::write(&gitignore_path, "ignored.txt\n")?;
        let ignored_path = root.join("ignored.txt");
        fs::write(&ignored_path, "should be ignored\n\n")?;
        let binary_path = root.join("binary.bin");
        fs::write(&binary_path, [0, 15, 255, 0, 1, 2, 3])?;
        let previous_dir = env::current_dir()?;
        env::set_current_dir(root)?;
        let result = main();
        env::set_current_dir(previous_dir)?;
        result?;
        let content1 = fs::read_to_string(&file1_path)?;
        assert_eq!(content1, "line1\nline2\nline3\n");
        let content_ignored = fs::read_to_string(&ignored_path)?;
        assert_eq!(content_ignored, "should be ignored\n\n");
        let content_binary = fs::read(&binary_path)?;
        assert_eq!(content_binary, vec![0, 15, 255, 0, 1, 2, 3]);
        Ok(())
    }
}
