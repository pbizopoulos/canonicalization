#![allow(clippy::multiple_crate_versions)]
use anyhow::{Context as _, Result};
use ignore::WalkBuilder;
use std::fs;
use std::path::Path;
fn main() -> Result<()> {
    let input_paths: Vec<String> = std::env::args().skip(1).collect();
    return run(input_paths);
}
fn run(input_paths: Vec<String>) -> Result<()> {
    if input_paths.is_empty() {
        process_root_path(Path::new("."))?;
    } else {
        for input_path in input_paths {
            process_root_path(Path::new(&input_path))?;
        }
    }
    return Ok(());
}
fn process_root_path(root: &Path) -> Result<()> {
    let walker = WalkBuilder::new(root).require_git(false).build();
    for result in walker {
        let entry = result.with_context(|| format!("Failed to walk path: {}", root.display()))?;
        let path = entry.path();
        if path.is_file() {
            remove_empty_lines(path)?;
        }
    }
    return Ok(());
}
fn remove_empty_lines(path: &Path) -> Result<()> {
    let path_display = path.display();
    let data = fs::read(path).with_context(|| format!("Failed to read file: {path_display}"))?;
    if content_inspector::inspect(&data).is_binary() {
        return Ok(());
    }
    let output = strip_empty_lines_from_bytes(&data);
    if output != data {
        fs::write(path, output).with_context(|| format!("Failed to write file: {path_display}"))?;
    }
    return Ok(());
}
fn strip_empty_lines_from_bytes(data: &[u8]) -> Vec<u8> {
    let mut output = Vec::with_capacity(data.len());
    for line in data.split_inclusive(|byte| return *byte == b'\n') {
        let without_newline = line.strip_suffix(b"\n").unwrap_or(line);
        let contents = without_newline
            .strip_suffix(b"\r")
            .unwrap_or(without_newline);
        let is_empty =
            core::str::from_utf8(contents).is_ok_and(|text| return text.trim().is_empty());
        if !is_empty {
            output.extend_from_slice(contents);
            output.push(b'\n');
        }
    }
    return output;
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
                .filter(|character| return *character != '\n' && *character != '\r')
                .collect();
            return Self(line);
        }
    }
    fn render_lines(lines: &[LogicalLine]) -> Vec<u8> {
        let mut rendered = Vec::new();
        for line in lines {
            rendered.extend_from_slice(line.0.as_bytes());
            rendered.push(b'\n');
        }
        return rendered;
    }
    fn expected_non_empty_lines(lines: &[LogicalLine]) -> Vec<u8> {
        let mut rendered = Vec::new();
        for line in lines {
            if !line.0.trim().is_empty() {
                rendered.extend_from_slice(line.0.as_bytes());
                rendered.push(b'\n');
            }
        }
        return rendered;
    }
    #[test]
    fn test_process_root_path_removes_empty_lines_from_text_files() -> Result<()> {
        use tempfile::tempdir;
        let dir = tempdir()?;
        let root = dir.path();
        let file1_path = root.join("test.txt");
        fs::write(&file1_path, "line1\n\nline2\n   \nline3\n")?;
        process_root_path(root)?;
        let content1 = fs::read_to_string(&file1_path)?;
        assert_eq!(content1, "line1\nline2\nline3\n");
        return Ok(());
    }
    #[test]
    fn test_process_root_path_respects_gitignore_and_skips_binary_files() -> Result<()> {
        use tempfile::tempdir;
        let dir = tempdir()?;
        let root = dir.path();
        let gitignore_path = root.join(".gitignore");
        fs::write(&gitignore_path, "ignored.txt\n")?;
        let ignored_path = root.join("ignored.txt");
        fs::write(&ignored_path, "should be ignored\n\n")?;
        let binary_path = root.join("binary.bin");
        fs::write(&binary_path, [0, 15, 255, 0, 1, 2, 3])?;
        process_root_path(root)?;
        let content_ignored = fs::read_to_string(&ignored_path)?;
        assert_eq!(content_ignored, "should be ignored\n\n");
        let content_binary = fs::read(&binary_path)?;
        assert_eq!(content_binary, vec![0, 15, 255, 0, 1, 2, 3]);
        return Ok(());
    }
    #[test]
    fn test_main_processes_current_directory_when_no_args() -> Result<()> {
        use tempfile::tempdir;
        let dir = tempdir()?;
        let root = dir.path();
        let file_path = root.join("test.txt");
        fs::write(&file_path, "line1\n\nline2\n")?;
        let previous_dir = env::current_dir()?;
        env::set_current_dir(root)?;
        let result = run(Vec::new());
        env::set_current_dir(previous_dir)?;
        result?;
        let content = fs::read_to_string(&file_path)?;
        assert_eq!(content, "line1\nline2\n");
        return Ok(());
    }
    #[test]
    fn test_run_propagates_missing_root_failures() {
        let missing = env::temp_dir().join("remove-empty-lines-definitely-missing-root");
        assert!(process_root_path(&missing).is_err());
    }
    #[test]
    fn test_byte_transform_preserves_non_utf8_data_without_a_decode_error() {
        assert_eq!(
            strip_empty_lines_from_bytes(&[0xff, b'\n', b' ', b'\r', b'\n']),
            vec![0xff, b'\n']
        );
    }
    #[test]
    fn quickcheck_strip_empty_lines_matches_filtered_sequence() {
        fn property(lines: Vec<LogicalLine>) -> TestResult {
            let input = render_lines(&lines);
            let actual = strip_empty_lines_from_bytes(&input);
            return TestResult::from_bool(actual == expected_non_empty_lines(&lines));
        }
        QuickCheck::new()
            .tests(100)
            .quickcheck(property as fn(Vec<LogicalLine>) -> TestResult);
    }
    #[test]
    fn quickcheck_strip_empty_lines_is_idempotent() {
        fn property(lines: Vec<LogicalLine>) -> TestResult {
            let input = render_lines(&lines);
            let first_pass = strip_empty_lines_from_bytes(&input);
            let second_pass = strip_empty_lines_from_bytes(&first_pass);
            return TestResult::from_bool(first_pass == second_pass);
        }
        QuickCheck::new()
            .tests(100)
            .quickcheck(property as fn(Vec<LogicalLine>) -> TestResult);
    }
}
