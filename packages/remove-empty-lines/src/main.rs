#![allow(clippy::multiple_crate_versions)]
use anyhow::{Context as _, Result};
use ignore::WalkBuilder;
use std::env;
use std::fs;
use std::path::Path;
fn main() -> Result<()> {
    let mut paths = env::args_os().skip(1);
    if let Some(path) = paths.next() {
        process_path(Path::new(&path))?;
        for path in paths {
            process_path(Path::new(&path))?;
        }
    } else {
        process_path(Path::new("."))?;
    }
    return Ok(());
}
fn process_path(path: &Path) -> Result<()> {
    for result in WalkBuilder::new(path).require_git(false).build() {
        let entry = result.with_context(|| format!("Failed to walk path: {}", path.display()))?;
        if entry
            .file_type()
            .is_some_and(|file_type| return file_type.is_file())
        {
            process_file(entry.path())?;
        }
    }
    return Ok(());
}
fn process_file(path: &Path) -> Result<()> {
    let path_display = path.display();
    let contents =
        fs::read(path).with_context(|| format!("Failed to read file: {path_display}"))?;
    if content_inspector::inspect(&contents).is_binary() {
        return Ok(());
    }
    let updated_contents = remove_empty_lines(&contents);
    if updated_contents != contents {
        fs::write(path, updated_contents)
            .with_context(|| format!("Failed to write file: {path_display}"))?;
    }
    return Ok(());
}
fn remove_empty_lines(contents: &[u8]) -> Vec<u8> {
    let mut updated_contents = Vec::with_capacity(contents.len());
    for line in contents.split_inclusive(|byte| return *byte == b'\n') {
        let without_newline = line.strip_suffix(b"\n").unwrap_or(line);
        let line_contents = without_newline
            .strip_suffix(b"\r")
            .unwrap_or(without_newline);
        if !core::str::from_utf8(line_contents).is_ok_and(|text| return text.trim().is_empty()) {
            updated_contents.extend_from_slice(line);
        }
    }
    return updated_contents;
}
#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::{Arbitrary, Gen, QuickCheck, TestResult};
    use std::process::Command;
    use tempfile::tempdir;
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
    fn respects_gitignore_and_skips_binary_files() -> Result<()> {
        use std::os::unix::fs::symlink;
        let dir = tempdir()?;
        let external_dir = tempdir()?;
        let root = dir.path();
        let gitignore_path = root.join(".gitignore");
        fs::write(&gitignore_path, "ignored.txt\n")?;
        let ignored_path = root.join("ignored.txt");
        fs::write(&ignored_path, "should be ignored\n\n")?;
        let binary_path = root.join("binary.bin");
        fs::write(&binary_path, [0, 15, 255, 0, 1, 2, 3])?;
        let external_path = external_dir.path().join("external.txt");
        fs::write(&external_path, "outside\n\n")?;
        symlink(&external_path, root.join("external.txt"))?;
        process_path(root)?;
        let content_ignored = fs::read_to_string(&ignored_path)?;
        assert_eq!(content_ignored, "should be ignored\n\n");
        let content_binary = fs::read(&binary_path)?;
        assert_eq!(content_binary, vec![0, 15, 255, 0, 1, 2, 3]);
        assert_eq!(fs::read_to_string(external_path)?, "outside\n\n");
        return Ok(());
    }
    #[test]
    fn processes_current_directory_when_no_arguments() -> Result<()> {
        let Some(executable) = env::var_os("PACKAGE_E2E_EXECUTABLE") else {
            return Ok(());
        };
        let dir = tempdir()?;
        let root = dir.path();
        let file_path = root.join("test.txt");
        fs::write(&file_path, "line1\n\nline2\n   \nline3\n")?;
        let output = Command::new(executable).current_dir(root).output()?;
        assert!(output.status.success());
        assert!(output.stdout.is_empty());
        assert!(output.stderr.is_empty());
        let content = fs::read_to_string(&file_path)?;
        assert_eq!(content, "line1\nline2\nline3\n");
        return Ok(());
    }
    #[test]
    fn propagates_missing_path_failures() {
        let missing = env::temp_dir().join("remove-empty-lines-definitely-missing-root");
        assert!(process_path(&missing).is_err());
    }
    #[test]
    fn preserves_non_utf8_data_when_removing_empty_lines() {
        assert_eq!(
            remove_empty_lines(&[0xff, b'\n', b' ', b'\r', b'\n']),
            vec![0xff, b'\n']
        );
    }
    #[test]
    fn removes_only_empty_lines() {
        assert_eq!(
            remove_empty_lines(b"first\r\n \t\r\nlast"),
            b"first\r\nlast"
        );
    }
    #[test]
    fn quickcheck_removing_empty_lines_matches_filtered_sequence() {
        #[expect(clippy::needless_pass_by_value)]
        fn property(lines: Vec<LogicalLine>) -> TestResult {
            let input = render_lines(&lines);
            let actual = remove_empty_lines(&input);
            return TestResult::from_bool(actual == expected_non_empty_lines(&lines));
        }
        QuickCheck::new()
            .tests(100)
            .quickcheck(property as fn(Vec<LogicalLine>) -> TestResult);
    }
}
