use std::collections::BTreeMap;
use std::env;
use std::ffi::OsString;
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{self, Command, ExitStatus, Stdio};
const MAIN_USAGE: &str = "usage: git home-submodule add <repository>\n   or: git home-submodule check [--fix]\n   or: git home-submodule init\n";
const MAIN_HELP: &str = "GIT-HOME-SUBMODULE(1)\n\nNAME\n    git-home-submodule - Manage canonical submodules in an allowlisted home directory\n\nSYNOPSIS\n    git home-submodule add <repository>\n    git home-submodule check [--fix]\n    git home-submodule init\n\nDESCRIPTION\n    Manages repositories as submodules of a Git superproject in $HOME.\n\n    Every repository is placed at its canonical\n    <hostname>/<repository-path> location. Repository URLs must use HTTPS or\n    SSH, and the home directory's .gitignore acts as an explicit allowlist.\n\nCOMMANDS\n    add <repository>\n        Add <repository> as a submodule at its canonical path. HTTPS and SSH\n        repository URLs are supported.\n\n    check [--fix]\n        Check that every path in $HOME/.gitmodules matches the canonical path\n        derived from its repository URL.\n\n        With --fix, move mismatched submodules and update their paths in\n        .gitmodules.\n\n    init\n        Initialize $HOME as a Git superproject with an allowlist .gitignore.\n        The rules !.gitignore and !.gitmodules are required; additional rules\n        must start with !.\n";
const ADD_USAGE: &str = "usage: git home-submodule add <repository>\n";
const DEFAULT_GITIGNORE: &str = "*\n!.gitignore\n!.gitmodules\n";
const USAGE_EXIT_CODE: i32 = 129;
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum RepositoryUrlErrorKind {
    Syntax,
    Validation,
}
#[derive(Clone, Debug, Eq, PartialEq)]
struct RepositoryUrlError {
    kind: RepositoryUrlErrorKind,
    message: String,
}
impl fmt::Display for RepositoryUrlError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        return formatter.write_str(&self.message);
    }
}
impl std::error::Error for RepositoryUrlError {}
#[derive(Debug)]
struct CliFailure {
    exit_code: i32,
    diagnostics: Vec<String>,
}
impl CliFailure {
    fn fatal(diagnostic: impl Into<String>) -> Self {
        return Self {
            exit_code: 128,
            diagnostics: vec![format!("fatal: {}", diagnostic.into())],
        };
    }
    fn check(diagnostics: Vec<String>) -> Self {
        return Self {
            exit_code: 1,
            diagnostics: diagnostics
                .into_iter()
                .map(|diagnostic| format!("error: {diagnostic}"))
                .collect(),
        };
    }
    fn git(exit_status: ExitStatus) -> Self {
        return Self {
            exit_code: exit_status.code().unwrap_or(1),
            diagnostics: Vec::new(),
        };
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
        Err(failure) => {
            for diagnostic in failure.diagnostics {
                eprintln!("{diagnostic}");
            }
            failure.exit_code
        }
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
    let mut path_components: Vec<String> = repository_path.split('/').map(str::to_owned).collect();
    if let Some(repository_name) = path_components.last_mut() {
        if let Some(name_without_suffix) = repository_name.strip_suffix(".git") {
            *repository_name = name_without_suffix.to_owned();
        }
    }
    for component in &path_components {
        validate_component("repository path component", component)?;
    }
    return Ok(format!("{hostname}/{}", path_components.join("/")));
}
fn split_url_path<'a>(
    repository_url: &str,
    location: &'a str,
) -> Result<(&'a str, &'a str), RepositoryUrlError> {
    return location.split_once('/').ok_or_else(|| {
        syntax_url_error(repository_url, "unsupported or incomplete repository URL")
    });
}
fn syntax_url_error(repository_url: &str, diagnostic: &str) -> RepositoryUrlError {
    return RepositoryUrlError {
        kind: RepositoryUrlErrorKind::Syntax,
        message: format!("{diagnostic}: {repository_url}"),
    };
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
    return Err(RepositoryUrlError {
        kind: RepositoryUrlErrorKind::Validation,
        message,
    });
}
fn add_repository(home_directory: &Path, repository_url: &str) -> Result<(), CliFailure> {
    return add_repository_with_environment(home_directory, repository_url, &[]);
}
fn add_repository_with_environment(
    home_directory: &Path,
    repository_url: &str,
    environment: &[(OsString, OsString)],
) -> Result<(), CliFailure> {
    let canonical_path = match parse_repository_url(repository_url) {
        Ok(path) => path,
        Err(error) if error.kind == RepositoryUrlErrorKind::Syntax => {
            eprint!("{ADD_USAGE}");
            return Err(CliFailure {
                exit_code: USAGE_EXIT_CODE,
                diagnostics: Vec::new(),
            });
        }
        Err(error) => return Err(CliFailure::fatal(error.message)),
    };
    let mut command = Command::new("git");
    command
        .arg("-C")
        .arg(home_directory)
        .args(["submodule", "add", "--force"])
        .arg(repository_url)
        .arg(canonical_path)
        .envs(environment.iter().cloned());
    let status = command
        .status()
        .map_err(|error| CliFailure::fatal(format!("failed to execute git: {error}")))?;
    if status.success() {
        return Ok(());
    }
    return Err(CliFailure::git(status));
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
            let mut updated = contents.clone();
            for required_rule in ["!.gitignore", "!.gitmodules"] {
                if !lines.contains(&required_rule) {
                    if !updated.ends_with('\n') {
                        updated.push('\n');
                    }
                    updated.push_str(required_rule);
                    updated.push('\n');
                }
            }
            (updated != contents).then_some(updated)
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
        return Err(CliFailure::git(status));
    }
    if let Some(contents) = updated_gitignore {
        fs::write(&gitignore_path, contents)
            .map_err(|error| CliFailure::fatal(format!("{}: {error}", gitignore_path.display())))?;
    }
    return Ok(());
}
fn check_home_gitmodules(home_directory: &Path, fix: bool) -> Result<(), CliFailure> {
    let gitmodules_path = home_directory.join(".gitmodules");
    if !gitmodules_path.is_file() {
        return Err(CliFailure::check(vec![format!(
            "missing file: {}",
            gitmodules_path.display()
        )]));
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
            CliFailure::check(vec![format!("failed to execute git config: {error}")])
        })?;
    let exit_code = output.status.code().unwrap_or(1);
    if !(output.status.success()
        || exit_code == 1 && output.stdout.is_empty() && output.stderr.is_empty())
    {
        return Err(CliFailure::git(output.status));
    }
    let raw_submodules = parse_raw_submodule_fields(&output.stdout)
        .map_err(|diagnostic| CliFailure::check(vec![diagnostic]))?;
    let (submodules, mut diagnostics) = validate_submodule_records(raw_submodules);
    for submodule in submodules {
        let section = submodule.section;
        let configured_path = submodule.path;
        let configured_url = submodule.url;
        if validate_canonical_repository_path(&configured_path).is_err()
            && (!fix || validate_repository_path_components(&configured_path).is_err())
        {
            diagnostics.push(format!(                "submodule \"{section}\": invalid path '{configured_path}'; expected <host>/<repository-path> with valid components"            ));
            continue;
        }
        match parse_repository_url(&configured_url) {
            Ok(expected_path) => {
                if configured_path != expected_path {
                    if fix {
                        fix_submodule_path(home_directory, &configured_path, &expected_path)?;
                    } else {
                        diagnostics.push(format!(                            "submodule \"{section}\": path '{configured_path}' does not match URL '{configured_url}'; expected '{expected_path}'"                        ));
                    }
                }
            }
            Err(error) => diagnostics.push(format!("submodule \"{section}\": {}", error.message)),
        }
    }
    if diagnostics.is_empty() {
        return Ok(());
    }
    return Err(CliFailure::check(diagnostics));
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
    return (records, diagnostics);
}
fn fix_submodule_path(
    home_directory: &Path,
    configured_path: &str,
    expected_path: &str,
) -> Result<(), CliFailure> {
    let target = home_directory.join(expected_path);
    let target_parent = target.parent().ok_or_else(|| {
        CliFailure::fatal(format!(
            "cannot determine parent directory for '{}'",
            target.display()
        ))
    })?;
    fs::create_dir_all(target_parent).map_err(|error| {
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
    if status.success() {
        return Ok(());
    }
    return Err(CliFailure::git(status));
}
fn parse_raw_submodule_fields(
    bytes: &[u8],
) -> Result<BTreeMap<String, RawSubmoduleFields>, String> {
    let mut submodules = BTreeMap::new();
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
        let fields = submodules
            .entry(section.to_owned())
            .or_insert_with(RawSubmoduleFields::default);
        match field {
            "path" => fields.paths.push(value.to_owned()),
            "url" => fields.urls.push(value.to_owned()),
            _ => {}
        }
    }
    return Ok(submodules);
}
fn validate_canonical_repository_path(path: &str) -> Result<(), RepositoryUrlError> {
    if path.split('/').count() < 2 {
        return Err(RepositoryUrlError {
            kind: RepositoryUrlErrorKind::Validation,
            message: "repository path must include a host and repository path".to_owned(),
        });
    }
    return validate_repository_path_components(path);
}
fn validate_repository_path_components(path: &str) -> Result<(), RepositoryUrlError> {
    for component in path.split('/') {
        validate_component("path component", component)?;
    }
    return Ok(());
}
#[cfg(test)]
mod tests {
    use super::*;
    use std::error::Error;
    use std::ffi::OsStr;
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
            return Ok(Self(path));
        }
        fn path(&self) -> &Path {
            return &self.0;
        }
    }
    impl Drop for TemporaryDirectory {
        fn drop(&mut self) {
            let _result = fs::remove_dir_all(&self.0);
        }
    }
    fn run_git<I, S>(arguments: I) -> io::Result<()>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        let status = Command::new("git").args(arguments).status()?;
        if status.success() {
            return Ok(());
        }
        return Err(io::Error::other(format!(
            "git fixture command exited with {status}"
        )));
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
        return Ok(());
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
        return Ok(());
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
        let home = TemporaryDirectory::new("init")?;
        initialize_home_repository(home.path())
            .map_err(|failure| format!("init failed: {failure:?}"))?;
        assert!(home.path().join(".git").is_dir());
        assert_eq!(
            fs::read_to_string(home.path().join(".gitignore"))?,
            DEFAULT_GITIGNORE
        );
        fs::write(home.path().join(".gitignore"), "*\n!github.com/")?;
        initialize_home_repository(home.path())
            .map_err(|failure| format!("reinit failed: {failure:?}"))?;
        assert_eq!(
            fs::read_to_string(home.path().join(".gitignore"))?,
            "*\n!github.com/\n!.gitignore\n!.gitmodules\n"
        );
        return Ok(());
    }
    #[test]
    fn rejects_conflicting_initialization_without_side_effects() -> TestResult {
        let home = TemporaryDirectory::new("init-conflict")?;
        fs::write(home.path().join(".gitignore"), "*.tmp\n")?;
        let failure = initialize_home_repository(home.path())
            .expect_err("conflicting initialization must fail");
        assert_eq!(failure.exit_code, 128);
        assert!(!home.path().join(".git").exists());
        return Ok(());
    }
    #[test]
    fn rejects_a_non_regular_gitignore_without_side_effects() -> TestResult {
        let home = TemporaryDirectory::new("init-non-regular-gitignore")?;
        fs::create_dir(home.path().join(".gitignore"))?;
        let failure = initialize_home_repository(home.path())
            .expect_err("a non-regular .gitignore must fail");
        assert_eq!(failure.exit_code, 128);
        assert!(!home.path().join(".git").exists());
        return Ok(());
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
        return Ok(());
    }
    #[test]
    fn reports_incomplete_unsupported_unsafe_and_mismatched_entries() -> TestResult {
        let home = TemporaryDirectory::new("check-invalid")?;
        fs::write(
            home.path().join(".gitmodules"),
            "[submodule \"missing-url\"]\n path = github.com/owner/demo\n[submodule \"missing-path\"]\n url = https://github.com/owner/missing.git\n[submodule \"relative\"]\n path = github.com/owner/relative\n url = ../relative.git\n[submodule \"unsafe\"]\n path = github.com/owner/../unsafe\n url = https://github.com/owner/unsafe.git\n[submodule \"mismatch\"]\n path = github.com/other/demo\n url = git@github.com:owner/demo.git\n[submodule \"duplicate\"]\n path = github.com/owner/duplicate\n path = github.com/owner/other\n url = https://github.com/owner/duplicate.git\n url = git@github.com:owner/duplicate.git\n",
        )?;
        let failure =
            check_home_gitmodules(home.path(), false).expect_err("invalid entries must fail");
        let diagnostics = failure.diagnostics.join("\n");
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
        return Ok(());
    }
    #[test]
    fn treats_an_empty_file_as_valid_but_requires_gitmodules_to_exist() -> TestResult {
        let home = TemporaryDirectory::new("check-empty")?;
        assert!(check_home_gitmodules(home.path(), false).is_err());
        fs::write(home.path().join(".gitmodules"), "")?;
        check_home_gitmodules(home.path(), false)
            .map_err(|failure| format!("empty check failed: {failure:?}"))?;
        return Ok(());
    }
    #[test]
    fn exposes_check_and_its_fix_option() -> TestResult {
        let home = TemporaryDirectory::new("check-command")?;
        fs::write(home.path().join(".gitmodules"), "")?;
        assert_eq!(run_cli(&[OsString::from("check")], home.path()), 0);
        assert_eq!(
            run_cli(
                &[OsString::from("check"), OsString::from("--fix"),],
                home.path(),
            ),
            0
        );
        assert_eq!(
            run_cli(&[OsString::from("check-gitmodules")], home.path()),
            129
        );
        return Ok(());
    }
    #[test]
    fn fixes_a_mismatched_submodule_path() -> TestResult {
        let workspace = TemporaryDirectory::new("check-fix")?;
        let seed = workspace.path().join("seed");
        let home = workspace.path().join("home");
        fs::create_dir(&seed)?;
        fs::create_dir(&home)?;
        run_git([OsStr::new("init"), OsStr::new("--quiet"), seed.as_os_str()])?;
        run_git([
            OsStr::new("-C"),
            seed.as_os_str(),
            OsStr::new("config"),
            OsStr::new("user.email"),
            OsStr::new("test@example.test"),
        ])?;
        run_git([
            OsStr::new("-C"),
            seed.as_os_str(),
            OsStr::new("config"),
            OsStr::new("user.name"),
            OsStr::new("Test"),
        ])?;
        fs::write(seed.join("README"), "fixture\n")?;
        run_git([
            OsStr::new("-C"),
            seed.as_os_str(),
            OsStr::new("add"),
            OsStr::new("README"),
        ])?;
        run_git([
            OsStr::new("-C"),
            seed.as_os_str(),
            OsStr::new("commit"),
            OsStr::new("--quiet"),
            OsStr::new("-m"),
            OsStr::new("fixture"),
        ])?;
        run_git([OsStr::new("init"), OsStr::new("--quiet"), home.as_os_str()])?;
        run_git([
            OsStr::new("-c"),
            OsStr::new("protocol.file.allow=always"),
            OsStr::new("-C"),
            home.as_os_str(),
            OsStr::new("submodule"),
            OsStr::new("add"),
            OsStr::new("--quiet"),
            seed.as_os_str(),
            OsStr::new("demo"),
        ])?;
        run_git([
            OsStr::new("config"),
            OsStr::new("--file"),
            home.join(".gitmodules").as_os_str(),
            OsStr::new("submodule.demo.url"),
            OsStr::new("https://new.example.test/owner/demo.git"),
        ])?;
        run_git([
            OsStr::new("-C"),
            home.as_os_str(),
            OsStr::new("add"),
            OsStr::new(".gitmodules"),
        ])?;
        let source = home.join("demo");
        check_home_gitmodules(&home, true).map_err(|failure| format!("fix failed: {failure:?}"))?;
        let target = home.join("new.example.test/owner/demo");
        assert!(!source.exists());
        assert_eq!(fs::read_to_string(target.join("README"))?, "fixture\n");
        assert!(
            fs::read_to_string(home.join(".gitmodules"))?
                .contains("path = new.example.test/owner/demo")
        );
        check_home_gitmodules(&home, false)
            .map_err(|failure| format!("post-fix check failed: {failure:?}"))?;
        return Ok(());
    }
    #[test]
    fn adds_a_local_fixture_at_the_canonical_destination() -> TestResult {
        let workspace = TemporaryDirectory::new("add")?;
        let seed = workspace.path().join("seed");
        let remote_root = workspace.path().join("remotes");
        let remote = remote_root.join("owner/demo.git");
        let home = workspace.path().join("home");
        fs::create_dir_all(&seed)?;
        fs::create_dir_all(
            remote
                .parent()
                .ok_or_else(|| io::Error::other("remote has no parent"))?,
        )?;
        fs::create_dir(&home)?;
        run_git([OsStr::new("init"), OsStr::new("--quiet"), seed.as_os_str()])?;
        run_git([
            OsStr::new("-C"),
            seed.as_os_str(),
            OsStr::new("config"),
            OsStr::new("user.email"),
            OsStr::new("test@example.test"),
        ])?;
        run_git([
            OsStr::new("-C"),
            seed.as_os_str(),
            OsStr::new("config"),
            OsStr::new("user.name"),
            OsStr::new("Test"),
        ])?;
        fs::write(seed.join("README"), "fixture\n")?;
        run_git([
            OsStr::new("-C"),
            seed.as_os_str(),
            OsStr::new("add"),
            OsStr::new("README"),
        ])?;
        run_git([
            OsStr::new("-C"),
            seed.as_os_str(),
            OsStr::new("commit"),
            OsStr::new("--quiet"),
            OsStr::new("-m"),
            OsStr::new("fixture"),
        ])?;
        run_git([
            OsStr::new("clone"),
            OsStr::new("--quiet"),
            OsStr::new("--bare"),
            seed.as_os_str(),
            remote.as_os_str(),
        ])?;
        initialize_home_repository(&home)
            .map_err(|failure| format!("fixture init failed: {failure:?}"))?;
        let global_config = workspace.path().join("gitconfig");
        fs::write(
            &global_config,
            format!(
                "[protocol \"file\"]\n allow = always\n[url \"file://{}/\"]\n insteadOf = https://example.test/\n",
                remote_root.display()
            ),
        )?;
        let environment = [
            (
                OsString::from("GIT_CONFIG_GLOBAL"),
                global_config.into_os_string(),
            ),
            (OsString::from("GIT_CONFIG_NOSYSTEM"), OsString::from("1")),
        ];
        add_repository_with_environment(&home, "https://example.test/owner/demo.git", &environment)
            .map_err(|failure| format!("add failed: {failure:?}"))?;
        assert!(home.join("example.test/owner/demo").is_dir());
        let gitmodules = fs::read_to_string(home.join(".gitmodules"))?;
        assert!(gitmodules.contains("path = example.test/owner/demo"));
        assert!(gitmodules.contains("url = https://example.test/owner/demo.git"));
        return Ok(());
    }
}
