use std::collections::BTreeMap;
use std::env;
use std::ffi::OsString;
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{self, Command, ExitStatus, Stdio};
const MAIN_USAGE: &str = "usage: git home-submodule init\n   or: git home-submodule add <repository>\n   or: git home-submodule check [--fix]\n";
const MAIN_HELP: &str = "usage: git home-submodule init\n   or: git home-submodule add <repository>\n   or: git home-submodule check [--fix]\n\nManage repositories as canonical submodules of the home directory.\n\ninit\n    For a new home repository, equivalent to:\n        git init ~\n        printf '*\\n!.gitignore\\n!.gitmodules\\n' > ~/.gitignore\n    A compatible existing .gitignore is preserved and completed.\n\nadd <repository>\n    Add an HTTPS or SSH repository at its canonical path, equivalent to:\n        git -C ~ submodule add --force <repository> <host>/<repository-path>\n\ncheck [--fix]\n    Check .gitmodules paths; with --fix, move and update mismatches.\n";
const ADD_USAGE: &str = "usage: git home-submodule add <repository>\n";
const DEFAULT_GITIGNORE: &str = "*\n!.gitignore\n!.gitmodules\n";
const USAGE_EXIT_CODE: i32 = 129;
#[derive(Clone, Debug, Eq, PartialEq)]
enum RepositoryUrlError {
    Syntax(String),
    Validation(String),
}
impl fmt::Display for RepositoryUrlError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Syntax(message) | Self::Validation(message) => formatter.write_str(message),
        }
    }
}
impl std::error::Error for RepositoryUrlError {}
#[derive(Debug)]
enum CliFailure {
    Fatal(String),
    Check(Vec<String>),
    Git(ExitStatus),
    Usage,
}
impl CliFailure {
    fn fatal(diagnostic: impl Into<String>) -> Self {
        Self::Fatal(diagnostic.into())
    }
}
#[derive(Debug, Default)]
struct RawSubmoduleFields {
    paths: Vec<String>,
    urls: Vec<String>,
}
#[derive(Debug, Eq, PartialEq)]
struct SubmoduleRecord {
    section: String,
    path: String,
    url: String,
}
fn main() {
    let arguments: Vec<OsString> = env::args_os().skip(1).collect();
    let home_directory = env::var_os("HOME").map_or_else(
        || {
            eprintln!("fatal: HOME is not set");
            process::exit(128);
        },
        PathBuf::from,
    );
    process::exit(run_cli(&arguments, &home_directory));
}
fn run_cli(arguments: &[OsString], home_directory: &Path) -> i32 {
    let result = match arguments {
        [] => {
            eprint!("{MAIN_USAGE}");
            return 1;
        }
        [argument] if argument == "-h" || argument == "--help" => {
            print!("{MAIN_HELP}");
            return 0;
        }
        [_, argument] if argument == "-h" || argument == "--help" => {
            eprint!("{MAIN_USAGE}");
            return 1;
        }
        [command] if command == "init" => initialize_home_repository(home_directory),
        [command, repository_url] if command == "add" => repository_url.to_str().map_or_else(
            || Err(CliFailure::fatal("repository URL must be valid UTF-8")),
            |url| add_repository(home_directory, url),
        ),
        [command] if command == "check" => check_home_gitmodules(home_directory, false),
        [command, fix] if command == "check" && fix == "--fix" => {
            check_home_gitmodules(home_directory, true)
        }
        _ => {
            eprint!("{MAIN_USAGE}");
            return USAGE_EXIT_CODE;
        }
    };
    match result {
        Ok(()) => 0,
        Err(CliFailure::Fatal(diagnostic)) => {
            eprintln!("fatal: {diagnostic}");
            128
        }
        Err(CliFailure::Check(diagnostics)) => {
            for diagnostic in diagnostics {
                eprintln!("error: {diagnostic}");
            }
            1
        }
        Err(CliFailure::Git(exit_status)) => exit_status.code().unwrap_or(1),
        Err(CliFailure::Usage) => USAGE_EXIT_CODE,
    }
}
fn parse_repository_url(repository_url: &str) -> Result<String, RepositoryUrlError> {
    let (hostname, repository_path) =
        if let Some(location) = repository_url.strip_prefix("https://") {
            split_url_path(repository_url, location)?
        } else if let Some(location) = repository_url.strip_prefix("ssh://git@") {
            split_url_path(repository_url, location)?
        } else if let Some(location) = repository_url.strip_prefix("git+ssh://git@") {
            split_url_path(repository_url, location)?
        } else if let Some(location) = repository_url.strip_prefix("git@") {
            location.split_once(':').ok_or_else(|| {
                syntax_url_error(repository_url, "unsupported or incomplete repository URL")
            })?
        } else {
            return Err(syntax_url_error(
                repository_url,
                "unsupported repository URL",
            ));
        };
    let repository_path = repository_path.trim_end_matches('/');
    if hostname.is_empty() || repository_path.is_empty() {
        return Err(syntax_url_error(
            repository_url,
            "unsupported or incomplete repository URL",
        ));
    }
    validate_component("hostname", hostname)?;
    let repository_path = repository_path
        .strip_suffix(".git")
        .unwrap_or(repository_path);
    for component in repository_path.split('/') {
        validate_component("repository path component", component)?;
    }
    Ok(format!("{hostname}/{repository_path}"))
}
fn split_url_path<'a>(
    repository_url: &str,
    location: &'a str,
) -> Result<(&'a str, &'a str), RepositoryUrlError> {
    location
        .split_once('/')
        .ok_or_else(|| syntax_url_error(repository_url, "unsupported or incomplete repository URL"))
}
fn syntax_url_error(repository_url: &str, diagnostic: &str) -> RepositoryUrlError {
    RepositoryUrlError::Syntax(format!("{diagnostic}: {repository_url}"))
}
fn validate_component(component_kind: &str, component: &str) -> Result<(), RepositoryUrlError> {
    let message = if component.is_empty() {
        format!("{component_kind} name must not be empty")
    } else if component == "." || component == ".." {
        format!("{component_kind} name must not be '.' or '..'")
    } else if !component
        .chars()
        .all(|character| character.is_ascii_alphanumeric() || matches!(character, '.' | '-' | '_'))
    {
        format!("{component_kind} name must contain only ASCII letters, digits, '.', '-', or '_'")
    } else {
        return Ok(());
    };
    Err(RepositoryUrlError::Validation(message))
}
fn add_repository(home_directory: &Path, repository_url: &str) -> Result<(), CliFailure> {
    let canonical_path = match parse_repository_url(repository_url) {
        Ok(path) => path,
        Err(RepositoryUrlError::Syntax(_)) => {
            eprint!("{ADD_USAGE}");
            return Err(CliFailure::Usage);
        }
        Err(RepositoryUrlError::Validation(message)) => return Err(CliFailure::fatal(message)),
    };
    let mut command = Command::new("git");
    command
        .arg("-C")
        .arg(home_directory)
        .args(["submodule", "add", "--force"])
        .arg(repository_url)
        .arg(canonical_path);
    let status = command
        .status()
        .map_err(|error| CliFailure::fatal(format!("failed to execute git: {error}")))?;
    if !status.success() {
        return Err(CliFailure::Git(status));
    }
    Ok(())
}
fn initialize_home_repository(home_directory: &Path) -> Result<(), CliFailure> {
    let gitignore_path = home_directory.join(".gitignore");
    let updated_gitignore = match fs::symlink_metadata(&gitignore_path) {
        Ok(metadata) if metadata.file_type().is_file() => {
            let contents = fs::read_to_string(&gitignore_path).map_err(|error| {
                CliFailure::fatal(format!("{}: {error}", gitignore_path.display()))
            })?;
            let lines: Vec<&str> = contents.lines().collect();
            let compatible = lines.first() == Some(&"*")
                && lines.iter().skip(1).all(|line| line.starts_with('!'));
            if !compatible {
                return Err(CliFailure::fatal(format!(
                    "{}: existing file must start with * and subsequent lines must start with !",
                    gitignore_path.display()
                )));
            }
            let missing_rules: Vec<&str> = ["!.gitignore", "!.gitmodules"]
                .into_iter()
                .filter(|required_rule| !lines.contains(required_rule))
                .collect();
            if missing_rules.is_empty() {
                None
            } else {
                let mut updated = contents;
                if !updated.ends_with('\n') {
                    updated.push('\n');
                }
                for missing_rule in missing_rules {
                    updated.push_str(missing_rule);
                    updated.push('\n');
                }
                Some(updated)
            }
        }
        Ok(_) => {
            return Err(CliFailure::fatal(format!(
                "{}: existing path must be a regular file",
                gitignore_path.display()
            )));
        }
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            Some(DEFAULT_GITIGNORE.to_owned())
        }
        Err(error) => {
            return Err(CliFailure::fatal(format!(
                "{}: {error}",
                gitignore_path.display()
            )));
        }
    };
    let status = Command::new("git")
        .arg("init")
        .arg(home_directory)
        .env_remove("GIT_DIR")
        .env_remove("GIT_WORK_TREE")
        .env_remove("GIT_COMMON_DIR")
        .env_remove("GIT_OBJECT_DIRECTORY")
        .status()
        .map_err(|error| CliFailure::fatal(format!("failed to execute git: {error}")))?;
    if !status.success() {
        return Err(CliFailure::Git(status));
    }
    if let Some(contents) = updated_gitignore {
        fs::write(&gitignore_path, contents)
            .map_err(|error| CliFailure::fatal(format!("{}: {error}", gitignore_path.display())))?;
    }
    Ok(())
}
fn check_home_gitmodules(home_directory: &Path, fix: bool) -> Result<(), CliFailure> {
    let gitmodules_path = home_directory.join(".gitmodules");
    match fs::symlink_metadata(&gitmodules_path) {
        Ok(metadata) if metadata.file_type().is_file() => {}
        Ok(_) => {
            return Err(CliFailure::Check(vec![format!(
                "not a regular file: {}",
                gitmodules_path.display()
            )]));
        }
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            return Err(CliFailure::Check(vec![format!(
                "missing file: {}",
                gitmodules_path.display()
            )]));
        }
        Err(error) => {
            return Err(CliFailure::Check(vec![format!(
                "{}: {error}",
                gitmodules_path.display()
            )]));
        }
    }
    let output = Command::new("git")
        .args(["config", "get", "--file"])
        .arg(&gitmodules_path)
        .args([
            "--null",
            "--show-names",
            "--all",
            "--regexp",
            "^submodule\\..*",
        ])
        .stderr(Stdio::inherit())
        .output()
        .map_err(|error| {
            CliFailure::Check(vec![format!("failed to execute git config: {error}")])
        })?;
    let exit_code = output.status.code().unwrap_or(1);
    if !(output.status.success()
        || exit_code == 1 && output.stdout.is_empty() && output.stderr.is_empty())
    {
        return Err(CliFailure::Git(output.status));
    }
    let raw_submodules = parse_raw_submodule_fields(&output.stdout)
        .map_err(|diagnostic| CliFailure::Check(vec![diagnostic]))?;
    let (submodules, mut diagnostics) = validate_submodule_records(raw_submodules);
    let mut path_fixes = Vec::new();
    let mut configured_path_sections = BTreeMap::new();
    let mut canonical_path_sections = BTreeMap::new();
    for SubmoduleRecord {
        section,
        path: configured_path,
        url: configured_url,
    } in submodules
    {
        if let Some(existing_section) = configured_path_sections.get(&configured_path) {
            diagnostics.push(format!(                "submodule \"{section}\": path '{configured_path}' is already used by submodule \"{existing_section}\""            ));
            continue;
        }
        configured_path_sections.insert(configured_path.clone(), section.clone());
        if validate_canonical_repository_path(&configured_path).is_err()
            && (!fix || validate_repository_path_components(&configured_path).is_err())
        {
            diagnostics.push(format!(                "submodule \"{section}\": invalid path '{configured_path}'; expected <host>/<repository-path> with valid components"            ));
            continue;
        }
        match parse_repository_url(&configured_url) {
            Ok(expected_path) => {
                if let Some(existing_section) = canonical_path_sections.get(&expected_path) {
                    diagnostics.push(format!(                        "submodule \"{section}\": URL resolves to canonical path '{expected_path}', already used by submodule \"{existing_section}\""                    ));
                    continue;
                }
                canonical_path_sections.insert(expected_path.clone(), section.clone());
                if configured_path != expected_path {
                    if fix {
                        path_fixes.push((section, configured_path, expected_path));
                    } else {
                        diagnostics.push(format!(                            "submodule \"{section}\": path '{configured_path}' does not match URL '{configured_url}'; expected '{expected_path}'"                        ));
                    }
                }
            }
            Err(error) => diagnostics.push(format!("submodule \"{section}\": {error}")),
        }
    }
    for (section, _, expected_path) in &path_fixes {
        if let Some(existing_section) = configured_path_sections.get(expected_path) {
            diagnostics.push(format!(                "submodule \"{section}\": canonical path '{expected_path}' is currently used by submodule \"{existing_section}\""            ));
        }
    }
    if !diagnostics.is_empty() {
        return Err(CliFailure::Check(diagnostics));
    }
    for (section, configured_path, expected_path) in path_fixes {
        fix_submodule_path(home_directory, &section, &configured_path, &expected_path)?;
    }
    Ok(())
}
fn validate_submodule_records(
    submodules: BTreeMap<String, RawSubmoduleFields>,
) -> (Vec<SubmoduleRecord>, Vec<String>) {
    let mut records = Vec::new();
    let mut diagnostics = Vec::new();
    for (section, fields) in submodules {
        match (fields.paths.as_slice(), fields.urls.as_slice()) {
            ([path], [url]) => records.push(SubmoduleRecord {
                section,
                path: path.clone(),
                url: url.clone(),
            }),
            (paths, urls) => {
                if paths.len() != 1 {
                    diagnostics.push(format!(
                        "submodule \"{section}\": must have exactly one path (found {})",
                        paths.len()
                    ));
                }
                if urls.len() != 1 {
                    diagnostics.push(format!(
                        "submodule \"{section}\": must have exactly one URL (found {})",
                        urls.len()
                    ));
                }
            }
        }
    }
    (records, diagnostics)
}
fn fix_submodule_path(
    home_directory: &Path,
    section: &str,
    configured_path: &str,
    expected_path: &str,
) -> Result<(), CliFailure> {
    let mut target_parent = home_directory.join(expected_path);
    target_parent.set_file_name("");
    fs::create_dir_all(&target_parent).map_err(|error| {
        CliFailure::fatal(format!(
            "cannot create '{}': {error}",
            target_parent.display()
        ))
    })?;
    let status = Command::new("git")
        .arg("-C")
        .arg(home_directory)
        .args(["mv", "--"])
        .arg(configured_path)
        .arg(expected_path)
        .status()
        .map_err(|error| CliFailure::fatal(format!("failed to execute git mv: {error}")))?;
    if !status.success() {
        return Err(CliFailure::Git(status));
    }
    let status = Command::new("git")
        .args(["config", "set", "--file"])
        .arg(home_directory.join(".gitmodules"))
        .arg(format!("submodule.{section}.path"))
        .arg(expected_path)
        .status()
        .map_err(|error| {
            CliFailure::fatal(format!("failed to update .gitmodules path: {error}"))
        })?;
    if !status.success() {
        return Err(CliFailure::Git(status));
    }
    Ok(())
}
fn parse_raw_submodule_fields(
    bytes: &[u8],
) -> Result<BTreeMap<String, RawSubmoduleFields>, String> {
    let mut submodules: BTreeMap<String, RawSubmoduleFields> = BTreeMap::new();
    for record in bytes
        .split(|byte| *byte == 0)
        .filter(|record| !record.is_empty())
    {
        let separator = record
            .iter()
            .position(|byte| *byte == b'\n')
            .ok_or_else(|| "malformed git config output: missing key/value separator".to_owned())?;
        let key = std::str::from_utf8(&record[..separator])
            .map_err(|error| format!("git config key is not valid UTF-8: {error}"))?;
        let value = std::str::from_utf8(&record[separator + 1..])
            .map_err(|error| format!("git config value for '{key}' is not valid UTF-8: {error}"))?;
        let Some(remainder) = key.strip_prefix("submodule.") else {
            continue;
        };
        let (section, field) = match remainder.rsplit_once('.') {
            Some(parts) if !parts.0.is_empty() => parts,
            _ => continue,
        };
        let fields = submodules.entry(section.to_owned()).or_default();
        match field {
            "path" => fields.paths.push(value.to_owned()),
            "url" => fields.urls.push(value.to_owned()),
            _ => {}
        }
    }
    Ok(submodules)
}
fn validate_canonical_repository_path(path: &str) -> Result<(), RepositoryUrlError> {
    if path.split('/').count() < 2 {
        return Err(RepositoryUrlError::Validation(
            "repository path must include a host and repository path".to_owned(),
        ));
    }
    validate_repository_path_components(path)
}
fn validate_repository_path_components(path: &str) -> Result<(), RepositoryUrlError> {
    for component in path.split('/') {
        validate_component("path component", component)?;
    }
    Ok(())
}
#[cfg(test)]
mod tests {
    use super::*;
    use std::error::Error;
    use std::io;
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::time::{SystemTime, UNIX_EPOCH};
    type TestResult = Result<(), Box<dyn Error>>;
    static TEMPORARY_DIRECTORY_COUNTER: AtomicU64 = AtomicU64::new(0);
    struct TemporaryDirectory(PathBuf);
    impl TemporaryDirectory {
        fn new(label: &str) -> io::Result<Self> {
            let timestamp = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map_err(io::Error::other)?
                .as_nanos();
            let counter = TEMPORARY_DIRECTORY_COUNTER.fetch_add(1, Ordering::Relaxed);
            let path = env::temp_dir().join(format!(
                "git-home-submodule-{label}-{}-{timestamp}-{counter}",
                process::id()
            ));
            fs::create_dir(&path)?;
            Ok(Self(path))
        }
        fn path(&self) -> &Path {
            &self.0
        }
    }
    impl Drop for TemporaryDirectory {
        fn drop(&mut self) {
            let _result = fs::remove_dir_all(&self.0);
        }
    }
    fn run_installed(
        home_directory: &Path,
        arguments: &[&str],
    ) -> io::Result<std::process::Output> {
        let executable = env::var_os("PACKAGE_E2E_EXECUTABLE")
            .ok_or_else(|| io::Error::other("PACKAGE_E2E_EXECUTABLE is not set"))?;
        Command::new(executable)
            .args(arguments)
            .env("HOME", home_directory)
            .output()
    }
    fn run_git_fixture(home_directory: &Path, arguments: &[&str]) -> TestResult {
        let output = Command::new("git")
            .arg("-C")
            .arg(home_directory)
            .args(arguments)
            .output()?;
        if !output.status.success() {
            return Err(format!(
                "git fixture command failed: {}",
                String::from_utf8_lossy(&output.stderr)
            )
            .into());
        }
        Ok(())
    }
    #[test]
    fn prints_concise_top_level_help() -> TestResult {
        if env::var_os("PACKAGE_E2E_EXECUTABLE").is_none() {
            return Ok(());
        }
        let home = TemporaryDirectory::new("help")?;
        let output = run_installed(home.path(), &["-h"])?;
        assert!(output.status.success());
        assert_eq!(String::from_utf8(output.stdout)?, MAIN_HELP);
        assert!(output.stderr.is_empty());
        assert_eq!(
            run_installed(home.path(), &["check-gitmodules"])?
                .status
                .code(),
            Some(USAGE_EXIT_CODE)
        );
        Ok(())
    }
    #[test]
    fn parses_supported_urls_into_the_same_canonical_path() -> TestResult {
        for repository_url in [
            "https://github.com/owner/demo.git",
            "ssh://git@github.com/owner/demo.git",
            "git+ssh://git@github.com/owner/demo.git//",
            "git@github.com:owner/demo.git",
        ] {
            assert_eq!(
                parse_repository_url(repository_url)?,
                "github.com/owner/demo"
            );
        }
        assert_eq!(
            parse_repository_url("https://git.example.test/group/nested/demo.git/")?,
            "git.example.test/group/nested/demo"
        );
        Ok(())
    }
    #[test]
    fn rejects_unsupported_and_unsafe_urls() {
        for repository_url in [
            "../demo.git",
            "http://github.com/owner/demo.git",
            "https://github.com/",
            "https://github.com/owner/../demo.git",
            "git@github.com:owner//demo.git",
            "https://github.com/ownér/demo.git",
        ] {
            assert!(parse_repository_url(repository_url).is_err());
        }
    }
    #[test]
    fn separates_raw_submodule_parsing_from_cardinality_validation() -> TestResult {
        let raw = parse_raw_submodule_fields(            b"submodule.demo.path\nhost/owner/demo\0submodule.demo.path\nhost/owner/other\0submodule.demo.url\nhttps://host/owner/demo.git\0",        )?;
        let (records, diagnostics) = validate_submodule_records(raw);
        assert!(records.is_empty());
        assert_eq!(diagnostics.len(), 1);
        assert!(diagnostics[0].contains("exactly one path (found 2)"));
        assert!(parse_raw_submodule_fields(b"submodule.demo.path\0").is_err());
        Ok(())
    }
    #[test]
    fn rejects_unsafe_canonical_paths_centrally() {
        for path in [
            "host",
            "host/../demo",
            "/owner/demo",
            "host/owner/demo name",
        ] {
            assert!(validate_canonical_repository_path(path).is_err(), "{path}");
        }
        assert!(validate_canonical_repository_path("host/owner/demo").is_ok());
    }
    #[test]
    fn initializes_and_reinitializes_a_compatible_home_repository() -> TestResult {
        if env::var_os("PACKAGE_E2E_EXECUTABLE").is_none() {
            return Ok(());
        }
        let home = TemporaryDirectory::new("init")?;
        let initial = run_installed(home.path(), &["init"])?;
        assert!(initial.status.success());
        assert!(home.path().join(".git").is_dir());
        assert_eq!(
            fs::read_to_string(home.path().join(".gitignore"))?,
            DEFAULT_GITIGNORE
        );
        fs::write(home.path().join(".gitignore"), "*\n!github.com/")?;
        let repeated = run_installed(home.path(), &["init"])?;
        assert!(repeated.status.success());
        assert_eq!(
            fs::read_to_string(home.path().join(".gitignore"))?,
            "*\n!github.com/\n!.gitignore\n!.gitmodules\n"
        );
        Ok(())
    }
    #[test]
    fn rejects_conflicting_initialization_without_side_effects() -> TestResult {
        let home = TemporaryDirectory::new("init-conflict")?;
        fs::write(home.path().join(".gitignore"), "*.tmp\n")?;
        let failure = initialize_home_repository(home.path())
            .expect_err("conflicting initialization must fail");
        assert!(matches!(failure, CliFailure::Fatal(_)));
        assert!(!home.path().join(".git").exists());
        Ok(())
    }
    #[test]
    fn rejects_a_non_regular_gitignore_without_side_effects() -> TestResult {
        let home = TemporaryDirectory::new("init-non-regular-gitignore")?;
        fs::create_dir(home.path().join(".gitignore"))?;
        let failure = initialize_home_repository(home.path())
            .expect_err("a non-regular .gitignore must fail");
        assert!(matches!(failure, CliFailure::Fatal(_)));
        assert!(!home.path().join(".git").exists());
        Ok(())
    }
    #[test]
    fn checks_matching_submodule_urls_and_paths() -> TestResult {
        let home = TemporaryDirectory::new("check")?;
        fs::write(
            home.path().join(".gitmodules"),
            "[submodule \"https\"]\n path = github.com/owner/https-demo\n url = https://github.com/owner/https-demo.git\n[submodule \"ssh\"]\n path = git.example.test/group/ssh-demo\n url = git+ssh://git@git.example.test/group/ssh-demo.git//\n",
        )?;
        check_home_gitmodules(home.path(), false)
            .map_err(|failure| format!("check failed: {failure:?}"))?;
        Ok(())
    }
    #[test]
    fn reports_all_invalid_entry_categories() -> TestResult {
        let home = TemporaryDirectory::new("check-invalid")?;
        fs::write(
            home.path().join(".gitmodules"),
            "[submodule \"missing-url\"]\n path = github.com/owner/demo\n[submodule \"missing-path\"]\n url = https://github.com/owner/missing.git\n[submodule \"relative\"]\n path = github.com/owner/relative\n url = ../relative.git\n[submodule \"unsafe\"]\n path = github.com/owner/../unsafe\n url = https://github.com/owner/unsafe.git\n[submodule \"mismatch\"]\n path = github.com/other/demo\n url = git@github.com:owner/demo.git\n[submodule \"duplicate\"]\n path = github.com/owner/duplicate\n path = github.com/owner/other\n url = https://github.com/owner/duplicate.git\n url = git@github.com:owner/duplicate.git\n",
        )?;
        let failure =
            check_home_gitmodules(home.path(), false).expect_err("invalid entries must fail");
        let CliFailure::Check(diagnostics) = failure else {
            return Err("invalid entries must produce check diagnostics".into());
        };
        let diagnostics = diagnostics.join("\n");
        for expected in [
            "must have exactly one URL",
            "must have exactly one path",
            "unsupported repository URL",
            "invalid path",
            "expected 'github.com/owner/demo'",
        ] {
            assert!(
                diagnostics.contains(expected),
                "missing diagnostic: {expected}"
            );
        }
        Ok(())
    }
    #[test]
    fn validates_every_entry_before_fixing_paths() -> TestResult {
        let home = TemporaryDirectory::new("check-fix-invalid")?;
        let gitmodules = "[submodule \"mismatch\"]\n path = old/demo\n url = https://example.test/owner/demo.git\n[submodule \"invalid\"]\n path = example.test/owner/invalid\n url = ../invalid.git\n";
        fs::write(home.path().join(".gitmodules"), gitmodules)?;
        let failure = check_home_gitmodules(home.path(), true)
            .expect_err("invalid entries must prevent fixes");
        assert!(matches!(failure, CliFailure::Check(_)));
        assert_eq!(
            fs::read_to_string(home.path().join(".gitmodules"))?,
            gitmodules
        );
        Ok(())
    }
    #[test]
    fn rejects_duplicate_canonical_destinations() -> TestResult {
        let home = TemporaryDirectory::new("check-duplicate-destination")?;
        fs::write(
            home.path().join(".gitmodules"),
            "[submodule \"first\"]\n path = example.test/owner/demo\n url = https://example.test/owner/demo.git\n[submodule \"second\"]\n path = another/path\n url = git@example.test:owner/demo.git\n",
        )?;
        let failure = check_home_gitmodules(home.path(), false)
            .expect_err("canonical destinations must be unique");
        let CliFailure::Check(diagnostics) = failure else {
            return Err("duplicate destinations must produce check diagnostics".into());
        };
        assert!(diagnostics[0].contains("already used by submodule"));
        Ok(())
    }
    #[test]
    fn rejects_ambiguous_fix_paths_before_mutating() -> TestResult {
        for (label, gitmodules, expected_diagnostic) in [
            (
                "duplicate-source",
                "[submodule \"first\"]\n path = old/demo\n url = https://example.test/owner/first.git\n[submodule \"second\"]\n path = old/demo\n url = https://example.test/owner/second.git\n",
                "already used by submodule",
            ),
            (
                "occupied-target",
                "[submodule \"first\"]\n path = old/demo\n url = https://example.test/owner/first.git\n[submodule \"second\"]\n path = example.test/owner/first\n url = https://example.test/owner/second.git\n",
                "currently used by submodule",
            ),
        ] {
            let home = TemporaryDirectory::new(label)?;
            fs::write(home.path().join(".gitmodules"), gitmodules)?;
            let failure =
                check_home_gitmodules(home.path(), true).expect_err("ambiguous fixes must fail");
            let CliFailure::Check(diagnostics) = failure else {
                return Err("ambiguous fixes must produce check diagnostics".into());
            };
            assert!(
                diagnostics
                    .iter()
                    .any(|diagnostic| diagnostic.contains(expected_diagnostic))
            );
            assert_eq!(
                fs::read_to_string(home.path().join(".gitmodules"))?,
                gitmodules
            );
        }
        Ok(())
    }
    #[test]
    fn treats_an_empty_file_as_valid_but_requires_gitmodules_to_be_a_regular_file() -> TestResult {
        use std::os::unix::fs::symlink;
        let home = TemporaryDirectory::new("check-empty")?;
        let gitmodules_path = home.path().join(".gitmodules");
        assert!(check_home_gitmodules(home.path(), false).is_err());
        fs::create_dir(&gitmodules_path)?;
        assert!(check_home_gitmodules(home.path(), false).is_err());
        fs::remove_dir(&gitmodules_path)?;
        let symlink_target = home.path().join("gitmodules-target");
        fs::write(&symlink_target, "")?;
        symlink(&symlink_target, &gitmodules_path)?;
        assert!(check_home_gitmodules(home.path(), false).is_err());
        fs::remove_file(&gitmodules_path)?;
        fs::write(&gitmodules_path, "")?;
        check_home_gitmodules(home.path(), false)
            .map_err(|failure| format!("empty check failed: {failure:?}"))?;
        Ok(())
    }
    #[test]
    fn fixes_and_checks_a_mismatched_path_through_the_installed_cli() -> TestResult {
        if env::var_os("PACKAGE_E2E_EXECUTABLE").is_none() {
            return Ok(());
        }
        let home = TemporaryDirectory::new("check-fix-success")?;
        assert!(run_installed(home.path(), &["init"])?.status.success());
        fs::write(
            home.path().join(".gitignore"),
            "*\n!.gitignore\n!.gitmodules\n!old/\n!old/demo/\n!old/demo/file.txt\n!example.test/\n!example.test/owner/\n!example.test/owner/demo/\n!example.test/owner/demo/file.txt\n",
        )?;
        fs::create_dir_all(home.path().join("old/demo"))?;
        fs::write(home.path().join("old/demo/file.txt"), "fixture\n")?;
        fs::write(
            home.path().join(".gitmodules"),
            "[submodule \"demo\"]\n path = old/demo\n url = https://example.test/owner/demo.git\n",
        )?;
        run_git_fixture(
            home.path(),
            &["add", ".gitignore", ".gitmodules", "old/demo/file.txt"],
        )?;
        let output = run_installed(home.path(), &["check", "--fix"])?;
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
        assert!(!home.path().join("old/demo").exists());
        assert_eq!(
            fs::read_to_string(home.path().join("example.test/owner/demo/file.txt"))?,
            "fixture\n"
        );
        let gitmodules = fs::read_to_string(home.path().join(".gitmodules"))?;
        assert!(
            gitmodules
                .lines()
                .any(|line| line.trim() == "path = example.test/owner/demo")
        );
        assert!(
            !gitmodules
                .lines()
                .any(|line| line.trim() == "path = old/demo")
        );
        let check_output = run_installed(home.path(), &["check"])?;
        assert!(
            check_output.status.success(),
            "{}",
            String::from_utf8_lossy(&check_output.stderr)
        );
        Ok(())
    }
}
