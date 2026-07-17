use std::env;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
const USAGE: &str = "Usage: check-home-gitmodules";
fn main() -> ExitCode {
    if env::args_os().len() != 1 {
        eprintln!("{USAGE}");
        return ExitCode::FAILURE;
    }
    let Some(home_directory) = env::var_os("HOME") else {
        eprintln!("HOME is not set");
        return ExitCode::FAILURE;
    };
    let home_directory = PathBuf::from(home_directory);
    match check_home_gitmodules(&home_directory) {
        Ok(invalid_path_entries) if invalid_path_entries.is_empty() => return ExitCode::SUCCESS,
        Ok(invalid_path_entries) => {
            for path_entry in invalid_path_entries {
                println!("{path_entry}: must be exactly <host>/<owner>/<repo>");
            }
            return ExitCode::FAILURE;
        }
        Err(error) if error.kind() == io::ErrorKind::NotFound => {
            println!(
                "missing file: {}",
                home_directory.join(".gitmodules").display()
            );
            return ExitCode::FAILURE;
        }
        Err(error) => {
            eprintln!(
                "failed to read {}: {error}",
                home_directory.join(".gitmodules").display()
            );
            return ExitCode::FAILURE;
        }
    }
}
fn check_home_gitmodules(home_directory: &Path) -> io::Result<Vec<String>> {
    let contents = fs::read_to_string(home_directory.join(".gitmodules"))?;
    return Ok(parse_gitmodule_path_entries(&contents)
        .into_iter()
        .filter(|path_entry| return !is_compatible_path_entry(path_entry))
        .collect());
}
fn parse_gitmodule_path_entries(contents: &str) -> Vec<String> {
    let mut path_entries = Vec::new();
    for line in contents.lines() {
        let trimmed_line = line.trim();
        let Some((key, raw_path_entry)) = trimmed_line.split_once('=') else {
            continue;
        };
        if key.trim() != "path" {
            continue;
        }
        let path_entry = raw_path_entry.trim();
        if !path_entry.is_empty() && !path_entries.iter().any(|entry| return entry == path_entry) {
            path_entries.push(path_entry.to_owned());
        }
    }
    return path_entries;
}
fn is_compatible_path_entry(path_entry: &str) -> bool {
    if path_entry.starts_with('/') {
        return false;
    }
    let components: Vec<&str> = path_entry.split('/').collect();
    return components.len() == 3
        && components.iter().all(|component| {
            return !component.is_empty() && *component != "." && *component != "..";
        });
}
#[cfg(test)]
mod tests {
    use super::*;
    use core::sync::atomic::{AtomicUsize, Ordering};
    static TEMP_DIRECTORY_COUNTER: AtomicUsize = AtomicUsize::new(0);
    struct TestDirectory(PathBuf);
    impl TestDirectory {
        fn new() -> io::Result<Self> {
            let counter = TEMP_DIRECTORY_COUNTER.fetch_add(1, Ordering::Relaxed);
            let path = env::temp_dir().join(format!(
                "check-home-gitmodules-test-{}-{counter}",
                std::process::id()
            ));
            fs::create_dir(&path)?;
            return Ok(Self(path));
        }
    }
    impl Drop for TestDirectory {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }
    #[test]
    fn parses_paths_and_preserves_first_occurrences() {
        let contents = concat!(
            "[submodule \"one\"]\n",
            "  path = github.com/owner/one  \n",
            "  url = git@example.test:owner/one\n",
            "path = github.com/owner/one\n",
            "pathname = gitlab.com/owner/ignored\n",
            "path = gitlab.com/owner/two\n",
            "path =    \n",
        );
        assert_eq!(
            parse_gitmodule_path_entries(contents),
            vec!["github.com/owner/one", "gitlab.com/owner/two"]
        );
    }
    #[test]
    fn validates_exactly_three_non_empty_relative_components() {
        for valid_path in ["github.com/owner/repository", "host/a/b"] {
            assert!(is_compatible_path_entry(valid_path));
        }
        for invalid_path in [
            "repository",
            "owner/repository",
            "host/owner/repository/extra",
            "/host/owner",
            "host//repository",
            "host/./repository",
            "host/../repository",
        ] {
            assert!(!is_compatible_path_entry(invalid_path));
        }
    }
    #[test]
    fn reports_a_missing_gitmodules_file() -> io::Result<()> {
        let home_directory = TestDirectory::new()?;
        let error = check_home_gitmodules(&home_directory.0).expect_err("file should be missing");
        assert_eq!(error.kind(), io::ErrorKind::NotFound);
        return Ok(());
    }
    #[test]
    fn accepts_empty_and_valid_gitmodules_files() -> io::Result<()> {
        let home_directory = TestDirectory::new()?;
        let gitmodules_path = home_directory.0.join(".gitmodules");
        fs::write(&gitmodules_path, "")?;
        assert!(check_home_gitmodules(&home_directory.0)?.is_empty());
        fs::write(
            gitmodules_path,
            "[submodule \"example\"]\n  path = github.com/owner/repository\n",
        )?;
        assert!(check_home_gitmodules(&home_directory.0)?.is_empty());
        return Ok(());
    }
    #[test]
    fn returns_invalid_entries_in_source_order() -> io::Result<()> {
        let home_directory = TestDirectory::new()?;
        fs::write(
            home_directory.0.join(".gitmodules"),
            concat!(
                "path = owner/one\n",
                "path = github.com/owner/valid\n",
                "path = too/many/path/components\n",
            ),
        )?;
        assert_eq!(
            check_home_gitmodules(&home_directory.0)?,
            vec!["owner/one", "too/many/path/components"]
        );
        return Ok(());
    }
}
