#![allow(clippy::multiple_crate_versions)]
use anyhow::{Context, Result};
use ignore::WalkBuilder;
use std::fs;
use std::io::{BufRead, BufReader, Write};
use std::path::Path;
fn main() -> Result<()> {
    let input_paths: Vec<String> = std::env::args().skip(1).collect();
    if input_paths.is_empty() {
        process_root_path(Path::new("."));
    } else {
        for input_path in input_paths {
            process_root_path(Path::new(&input_path));
        }
    }
    Ok(())
}
fn process_root_path(root: &Path) {
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
    let output = strip_empty_lines_from_bytes(&data)?;
    if output != data {
        fs::write(path, output).with_context(|| format!("Failed to write file: {path_display}"))?;
    }
    Ok(())
}
fn strip_empty_lines_from_bytes(data: &[u8]) -> Result<Vec<u8>> {
    let reader = BufReader::new(data);
    let mut output = Vec::new();
    for line_result in reader.lines() {
        let line = line_result?;
        if !line.trim().is_empty() {
            writeln!(output, "{line}")?;
        }
    }
    Ok(output)
}
#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::{Arbitrary, Gen, QuickCheck, TestResult};
    use std::env;
    #[derive(Clone, Debug)]
    struct LogicalLine(String);
    impl Arbitrary for LogicalLine {
        fn arbitrary(g: &mut Gen) -> Self {
            let line = String::arbitrary(g)
                .chars()
                .filter(|character| *character != '\n' && *character != '\r')
                .collect();
            Self(line)
        }
    }
    fn render_lines(lines: &[LogicalLine]) -> Vec<u8> {
        let mut rendered = Vec::new();
        for line in lines {
            rendered.extend_from_slice(line.0.as_bytes());
            rendered.push(b'\n');
        }
        rendered
    }
    fn expected_non_empty_lines(lines: &[LogicalLine]) -> Vec<u8> {
        let mut rendered = Vec::new();
        for line in lines {
            if !line.0.trim().is_empty() {
                rendered.extend_from_slice(line.0.as_bytes());
                rendered.push(b'\n');
            }
        }
        rendered
    }
    #[test]
    fn test_main_and_process_root_path() -> Result<()> {
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
    #[test]
    fn quickcheck_strip_empty_lines_matches_filtered_sequence() {
        fn property(lines: Vec<LogicalLine>) -> TestResult {
            let input = render_lines(&lines);
            match strip_empty_lines_from_bytes(&input) {
                Ok(actual) => TestResult::from_bool(actual == expected_non_empty_lines(&lines)),
                Err(_) => TestResult::error("strip_empty_lines_from_bytes returned an error"),
            }
        }
        QuickCheck::new()
            .tests(100)
            .quickcheck(property as fn(Vec<LogicalLine>) -> TestResult);
    }
    #[test]
    fn quickcheck_strip_empty_lines_is_idempotent() {
        fn property(lines: Vec<LogicalLine>) -> TestResult {
            let input = render_lines(&lines);
            match strip_empty_lines_from_bytes(&input) {
                Ok(first_pass) => match strip_empty_lines_from_bytes(&first_pass) {
                    Ok(second_pass) => TestResult::from_bool(first_pass == second_pass),
                    Err(_) => {
                        TestResult::error("second strip_empty_lines_from_bytes returned an error")
                    }
                },
                Err(_) => TestResult::error("first strip_empty_lines_from_bytes returned an error"),
            }
        }
        QuickCheck::new()
            .tests(100)
            .quickcheck(property as fn(Vec<LogicalLine>) -> TestResult);
    }
}
