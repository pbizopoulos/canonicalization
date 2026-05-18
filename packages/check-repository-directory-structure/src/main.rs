#![allow(clippy::multiple_crate_versions)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::needless_pass_by_value)]
use clap::Parser;
use git2::{Repository, StatusOptions};
use regex::Regex;
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(default_value = "flake.nix")]
    flake_path: String,
}
fn is_valid_domain_name(name: &str) -> bool {
    if name != name.to_lowercase() {
        return false;
    }
    if name.starts_with('.') || name.ends_with('.') || name.contains("..") {
        return false;
    }
    let Ok(ascii_name) = idna::domain_to_ascii(name) else {
        return false;
    };
    ascii_name.contains('.') && ascii_name.split('.').all(|label| !label.is_empty())
}
fn is_dash_case(name: &str) -> bool {
    let re = Regex::new(r"^[a-z0-9]+([-.][a-z0-9]+)*$").unwrap();
    re.is_match(name)
}
fn should_ignore_untracked_path(path: &str) -> bool {
    path == ".codex" || path == ".agents"
}
fn package_root(rel_path: &Path) -> Option<PathBuf> {
    let components: Vec<_> = rel_path
        .components()
        .map(|component| component.as_os_str().to_str())
        .collect::<Option<Vec<_>>>()?;
    match components.as_slice() {
        ["packages", package_name, ..] => Some(PathBuf::from(format!("packages/{package_name}"))),
        _ => None,
    }
}
fn host_root(rel_path: &Path) -> Option<PathBuf> {
    let components: Vec<_> = rel_path
        .components()
        .map(|component| component.as_os_str().to_str())
        .collect::<Option<Vec<_>>>()?;
    match components.as_slice() {
        ["hosts", host_name, ..] => Some(PathBuf::from(format!("hosts/{host_name}"))),
        _ => None,
    }
}
use std::time::{SystemTime, UNIX_EPOCH};
fn main() {
    let args = Cli::parse();
    let flake_path = args.flake_path;
    let lock_path = std::env::temp_dir().join("validate_repository_directory_structure.lock");
    let lock_file = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(&lock_path)
        .expect("Failed to open lock file");
    let mut lock = fd_lock::RwLock::new(lock_file);
    let mut _guard = lock.write().expect("Failed to acquire write lock");
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs();
    let last_run_str = std::fs::read_to_string(&lock_path).unwrap_or_default();
    let last_run: u64 = last_run_str.trim().parse().unwrap_or(0);
    if now - last_run < 5 {
        std::process::exit(0);
    }
    match validate_repository_directory_structure(flake_path) {
        #[allow(clippy::ignored_unit_patterns)]
        Ok(()) => {
            std::fs::write(&lock_path, now.to_string()).unwrap();
            std::process::exit(0)
        }
        Err(warnings) => {
            println!("{}", warnings.join("\n"));
            std::process::exit(1);
        }
    }
}
fn validate_repository_directory_structure(flake_path: String) -> Result<(), Vec<String>> {
    if std::env::var("NIX_BUILD_TOP").is_ok() {
        return Ok(());
    }
    let mut warnings = Vec::new();
    let dir_path = Path::new(&flake_path)
        .canonicalize()
        .expect("Failed to canonicalize path");
    let Ok(repo) = Repository::discover(&dir_path) else {
        return Ok(());
    };
    let working_dir = repo.workdir().expect("No working directory for repository");
    let working_dir_display = working_dir.display();
    let branch_warning = format!("{working_dir_display}: should have 'main' as the active branch");
    let mut status_options = StatusOptions::new();
    status_options.include_untracked(true);
    let statuses = match repo.statuses(Some(&mut status_options)) {
        Ok(s) => s,
        Err(_) => return Ok(()),
    };
    for entry in statuses.iter() {
        if entry.status().is_wt_new() && !entry.path().is_some_and(should_ignore_untracked_path) {
            warnings.push(format!(
                "{}: is untracked",
                working_dir.join(entry.path().unwrap()).display()
            ));
        }
    }
    let head = repo.head();
    if let Ok(head) = head {
        match head.shorthand() {
            Some(branch_name) => {
                if branch_name != "main" {
                    warnings.push(branch_warning.clone());
                }
            }
            None => {
                warnings.push(branch_warning.clone());
            }
        }
        let branches = repo
            .branches(Some(git2::BranchType::Local))
            .expect("Failed to get branches");
        if branches.count() != 1 {
            warnings.push(format!(
                "{working_dir_display}: should have only one branch"
            ));
        }
    }
    let dir_name_str = working_dir.file_name().unwrap().to_str().unwrap();
    if dir_name_str != dir_name_str.to_lowercase()
        || (!is_valid_domain_name(dir_name_str) && !is_dash_case(dir_name_str))
    {
        warnings.push(format!(
            "{working_dir_display}: should be lower-case and valid FQDN or in dash-case"
        ));
    }
    let mut paths = Vec::new();
    for entry in WalkDir::new(working_dir).into_iter().filter_entry(|e| {
        let path = e.path();
        if path == working_dir {
            return true;
        }
        let rel_path = path.strip_prefix(working_dir).unwrap();
        for component in rel_path.components() {
            let s = component.as_os_str().to_str().unwrap();
            if s == "tmp"
                || s == "prm"
                || s == "target"
                || s == "CSharpier"
                || s == "build"
                || s == "_build"
                || s == "deps"
                || s == "node_modules"
                || s == ".nuxt"
                || s == ".svelte-kit"
                || s == "result"
            {
                return false;
            }
        }
        true
    }) {
        let entry = entry.expect("Failed to read directory entry");
        if entry.path() != working_dir {
            paths.push(entry.path().to_path_buf());
        }
    }
    paths.sort();
    let all_rel_paths: HashSet<_> = paths
        .iter()
        .map(|path| path.strip_prefix(working_dir).unwrap().to_path_buf())
        .collect();
    let mut dir_and_file_names = HashSet::new();
    for path in &paths {
        let rel_path = path.strip_prefix(working_dir).unwrap();
        let is_leaf = path.is_file() || !paths.iter().any(|p| p.parent() == Some(path));
        if is_leaf {
            dir_and_file_names.insert(rel_path.to_path_buf());
        }
    }
    let names_allowed = [
        r"\.git(/.*)?",
        r"\.github/workflows/workflow\.yml",
        r"\.agents(/.*)?",
        r"\.codex(/.*)?",
        r"\.gitignore",
        r"CITATION\.bib",
        r"LICENSE",
        r"README",
        r"checks/[^/]+/default\.nix",
        r"flake\.lock",
        r"flake\.nix",
        r"formatter\.nix",
        r"hosts/[^/]+/configuration\.nix",
        r"hosts/[^/]+/hardware-configuration\.nix",
        r"packages/[^/]+/\.gitignore",
        r"packages/[^/]+/Main\.hs",
        r"packages/[^/]+/Cargo\.toml",
        r"packages/[^/]+/default\.nix",
        r"packages/[^/]+/index\.html",
        r"packages/[^/]+/manage\.py",
        r"packages/[^/]+/main\.(c|py|sh|tf)",
        r"packages/[^/]+/ms\.tex",
        r"packages/[^/]+/style\.css",
        r"packages/[^/]+/script\.js",
        r"prm/[^/]+",
        r"result",
        r"secrets/secrets\.age",
        r"secrets/secrets\.env\.example",
        r"secrets/secrets\.nix",
    ];
    let file_dependencies = [
        (
            r"packages/[^/]+/Cargo\.toml",
            vec![
                r"packages/[^/]+/Cargo\.lock",
                r"packages/[^/]+/src/main\.rs",
            ],
        ),
        (
            r"packages/[^/]+/index\.html",
            vec![r"packages/[^/]+/script\.js", r"packages/[^/]+/style\.css"],
        ),
        (
            r"packages/[^/]+/main\.tf",
            vec![
                r"packages/[^/]+/\.gitignore",
                r"packages/[^/]+/\.terraform(/.*)?",
                r"packages/[^/]+/\.terraform\.lock\.hcl",
                r"packages/[^/]+/prm/.*",
            ],
        ),
        (r"packages/[^/]+/ms\.tex", vec![r"packages/[^/]+/ms\.bib"]),
        (
            r"packages/[^/]+/manage\.py",
            vec![
                r"packages/[^/]+/[^/]+/__init__\.py",
                r"packages/[^/]+/[^/]+/(apps|auth_backends|context_processors|forms|models|settings|throttle|urls|views|wsgi)\.py",
                r"packages/[^/]+/[^/]+/migrations(/.*)?",
                r"packages/[^/]+/[^/]+/tests(/.*)?",
                r"packages/[^/]+/templates(/.*)?",
                r"packages/[^/]+/static(/.*)?",
            ],
        ),
        (
            r"packages/[^/]+/main\.py",
            vec![r"packages/[^/]+/prm(/.*)?"],
        ),
    ];
    let compiled_names_allowed: Vec<Regex> = names_allowed
        .iter()
        .map(|p| Regex::new(&format!("^{p}$")).unwrap())
        .collect();
    let compiled_file_dependencies: Vec<(Regex, Vec<Regex>)> = file_dependencies
        .iter()
        .map(|(trigger, patterns)| {
            (
                Regex::new(&format!("^{trigger}$")).unwrap(),
                patterns
                    .iter()
                    .map(|dep| Regex::new(&format!("^{dep}$")).unwrap())
                    .collect(),
            )
        })
        .collect();
    let mut allowed_patterns = compiled_names_allowed;
    for path in &dir_and_file_names {
        let path_str = path.to_str().unwrap();
        for (trigger_re, deps) in &compiled_file_dependencies {
            if trigger_re.is_match(path_str) {
                allowed_patterns.extend(deps.iter().cloned());
            }
        }
    }
    let mut final_warnings = warnings;
    let package_roots: HashSet<_> = all_rel_paths
        .iter()
        .filter_map(|path| package_root(path))
        .collect();
    for package_root in &package_roots {
        let default_nix = package_root.join("default.nix");
        if !all_rel_paths.contains(&default_nix) {
            final_warnings.push(format!(
                "{}: is missing required default.nix",
                working_dir.join(package_root).display()
            ));
        }
    }
    let host_roots: HashSet<_> = all_rel_paths
        .iter()
        .filter_map(|path| host_root(path))
        .collect();
    for host_root in host_roots {
        let configuration_nix = host_root.join("configuration.nix");
        if !all_rel_paths.contains(&configuration_nix) {
            final_warnings.push(format!(
                "{}: is missing required configuration.nix",
                working_dir.join(host_root).display()
            ));
        }
    }
    for package_root in &package_roots {
        let package_name = package_root
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or_default();
        if all_rel_paths.contains(&package_root.join("Main.hs")) {
            allowed_patterns.push(
                Regex::new(&format!(
                    "^{}/{}\\.cabal$",
                    package_root.display(),
                    package_name
                ))
                .unwrap(),
            );
        }
        if all_rel_paths.contains(&package_root.join("Main.hs"))
            && !all_rel_paths.contains(&package_root.join(format!("{package_name}.cabal")))
        {
            final_warnings.push(format!(
                "{}: is missing required {} for Main.hs package",
                working_dir.join(package_root).display(),
                format!("{package_name}.cabal")
            ));
        }
        let expected_cabal_name = format!("{package_name}.cabal");
        for rel_path in &all_rel_paths {
            let Ok(package_relative_path) = rel_path.strip_prefix(package_root) else {
                continue;
            };
            let Some(file_name) = package_relative_path
                .file_name()
                .and_then(|name| name.to_str())
            else {
                continue;
            };
            if file_name.ends_with(".cabal") && file_name != expected_cabal_name {
                final_warnings.push(format!(
                    "{}: cabal file must be named {}",
                    working_dir.join(rel_path).display(),
                    expected_cabal_name
                ));
            }
        }
    }
    let mut sorted_names: Vec<_> = dir_and_file_names.into_iter().collect();
    sorted_names.sort();
    for name in sorted_names {
        let name_str = name.to_str().unwrap();
        if !allowed_patterns.iter().any(|re| re.is_match(name_str)) {
            if std::env::var("DEBUG").as_deref() == Ok("1") {
                for re in &allowed_patterns {
                    eprintln!("Pattern: {}", re.as_str());
                }
            }
            final_warnings.push(format!(
                "{}: is not allowed",
                working_dir.join(name).display()
            ));
        }
    }
    if final_warnings.is_empty() {
        Ok(())
    } else {
        Err(final_warnings)
    }
}
#[allow(dead_code)]
fn run_tests() {
    test_is_valid_domain_name_standalone();
    test_is_dash_case_standalone();
    test_check_repository_directory_structure_standalone();
}
fn test_check_repository_directory_structure_standalone() {
    std::env::remove_var("NIX_BUILD_TOP");
    use std::fs;
    use std::process::Command;
    let temp_dir = std::env::temp_dir().join("test-repo-structure-standalone");
    if temp_dir.exists() {
        fs::remove_dir_all(&temp_dir).unwrap();
    }
    fs::create_dir_all(&temp_dir).unwrap();
    Command::new("git")
        .arg("init")
        .arg("-b")
        .arg("main")
        .current_dir(&temp_dir)
        .output()
        .expect("Failed to init git");
    Command::new("git")
        .args(["config", "user.email", "test@example.com"])
        .current_dir(&temp_dir)
        .output()
        .unwrap();
    Command::new("git")
        .args(["config", "user.name", "Test User"])
        .current_dir(&temp_dir)
        .output()
        .unwrap();
    fs::write(temp_dir.join("flake.nix"), "test").unwrap();
    Command::new("git")
        .arg("add")
        .arg("flake.nix")
        .current_dir(&temp_dir)
        .output()
        .expect("Failed to add flake.nix");
    Command::new("git")
        .arg("commit")
        .arg("-m")
        .arg("initial commit")
        .current_dir(&temp_dir)
        .output()
        .expect("Failed to commit");
    let flake_path = temp_dir.join("flake.nix");
    let result = validate_repository_directory_structure(flake_path.to_str().unwrap().to_string());
    assert!(
        result.is_ok(),
        "Expected Ok, but got Err: {:?}",
        result.err()
    );
    fs::write(temp_dir.join("unallowed.txt"), "test").unwrap();
    let result = validate_repository_directory_structure(flake_path.to_str().unwrap().to_string());
    assert!(result.is_err());
    fs::remove_file(temp_dir.join("unallowed.txt")).unwrap();
    fs::create_dir_all(temp_dir.join("templates/my-template/packages/my-pkg")).unwrap();
    fs::write(
        temp_dir.join("templates/my-template/packages/my-pkg/default.nix"),
        "test",
    )
    .unwrap();
    Command::new("git")
        .args(["add", "templates"])
        .current_dir(&temp_dir)
        .output()
        .unwrap();
    let result = validate_repository_directory_structure(flake_path.to_str().unwrap().to_string());
    assert!(result.is_err());
    fs::remove_dir_all(temp_dir.join("templates")).unwrap();
    fs::create_dir_all(temp_dir.join("packages/no-default")).unwrap();
    fs::write(temp_dir.join("packages/no-default/main.py"), "test").unwrap();
    let result = validate_repository_directory_structure(flake_path.to_str().unwrap().to_string());
    assert!(result.is_err());
    fs::remove_dir_all(temp_dir.join("packages/no-default")).unwrap();
    fs::remove_dir(temp_dir.join("packages")).unwrap();
    fs::create_dir_all(temp_dir.join("hosts/my-host")).unwrap();
    fs::write(temp_dir.join("hosts/my-host/configuration.nix"), "test").unwrap();
    fs::write(
        temp_dir.join("hosts/my-host/hardware-configuration.nix"),
        "test",
    )
    .unwrap();
    Command::new("git")
        .args(["add", "hosts"])
        .current_dir(&temp_dir)
        .output()
        .unwrap();
    let result = validate_repository_directory_structure(flake_path.to_str().unwrap().to_string());
    assert!(
        result.is_ok(),
        "Expected Ok for hosts/configuration.nix, but got Err: {:?}",
        result.err()
    );
    fs::write(temp_dir.join("hosts/my-host/.gitignore"), "test").unwrap();
    let result = validate_repository_directory_structure(flake_path.to_str().unwrap().to_string());
    assert!(result.is_err());
    fs::remove_file(temp_dir.join("hosts/my-host/.gitignore")).unwrap();
    fs::create_dir_all(temp_dir.join("hosts/only-hardware")).unwrap();
    fs::write(
        temp_dir.join("hosts/only-hardware/hardware-configuration.nix"),
        "test",
    )
    .unwrap();
    let result = validate_repository_directory_structure(flake_path.to_str().unwrap().to_string());
    assert!(result.is_err());
    fs::remove_dir_all(&temp_dir).unwrap();
    println!("test validate_repository_directory_structure ... ok");
}
fn test_is_valid_domain_name_standalone() {
    assert!(is_valid_domain_name("google.com"));
    assert!(is_valid_domain_name("a.b.co"));
    assert!(!is_valid_domain_name("google"));
    assert!(!is_valid_domain_name("google."));
    assert!(!is_valid_domain_name(".com"));
    println!("test is_valid_domain_name ... ok");
}
fn test_is_dash_case_standalone() {
    assert!(is_dash_case("my-package"));
    assert!(is_dash_case("my.package"));
    assert!(is_dash_case("package123"));
    assert!(!is_dash_case("My-Package"));
    assert!(!is_dash_case("my_package"));
    println!("test is_dash_case ... ok");
}
#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::path::Path;
    use std::process::Command;
    fn init_temp_repo(path: &Path) {
        if path.exists() {
            fs::remove_dir_all(path).unwrap();
        }
        fs::create_dir_all(path).unwrap();
        Command::new("git")
            .arg("init")
            .arg("-b")
            .arg("main")
            .current_dir(path)
            .output()
            .expect("Failed to init git");
        Command::new("git")
            .args(["config", "user.email", "test@example.com"])
            .current_dir(path)
            .output()
            .unwrap();
        Command::new("git")
            .args(["config", "user.name", "Test User"])
            .current_dir(path)
            .output()
            .unwrap();
        fs::write(path.join("flake.nix"), "test").unwrap();
        Command::new("git")
            .arg("add")
            .arg("flake.nix")
            .current_dir(path)
            .output()
            .expect("Failed to add flake.nix");
        Command::new("git")
            .arg("commit")
            .arg("-m")
            .arg("initial commit")
            .current_dir(path)
            .output()
            .expect("Failed to commit");
    }
    #[test]
    fn test_is_valid_domain_name() {
        assert!(is_valid_domain_name("google.com"));
        assert!(is_valid_domain_name("a.b.co"));
        assert!(!is_valid_domain_name("google"));
        assert!(!is_valid_domain_name("google."));
        assert!(!is_valid_domain_name(".com"));
    }
    #[test]
    fn test_is_dash_case() {
        assert!(is_dash_case("my-package"));
        assert!(is_dash_case("my.package"));
        assert!(is_dash_case("package123"));
        assert!(!is_dash_case("My-Package"));
        assert!(!is_dash_case("my_package"));
    }
    #[test]
    fn test_should_ignore_untracked_path() {
        assert!(should_ignore_untracked_path(".codex"));
        assert!(should_ignore_untracked_path(".agents"));
        assert!(!should_ignore_untracked_path(
            "packages/rust-template/default.nix"
        ));
    }
    #[test]
    fn test_package_root() {
        assert_eq!(
            package_root(Path::new("packages/rust-template/default.nix")),
            Some(PathBuf::from("packages/rust-template"))
        );
        assert_eq!(
            package_root(Path::new(
                "templates/example/packages/rust-template/default.nix"
            )),
            None
        );
        assert_eq!(
            package_root(Path::new("hosts/template/configuration.nix")),
            None
        );
    }
    #[test]
    fn test_host_root() {
        assert_eq!(
            host_root(Path::new("hosts/template/configuration.nix")),
            Some(PathBuf::from("hosts/template"))
        );
        assert_eq!(
            host_root(Path::new("packages/rust-template/default.nix")),
            None
        );
    }
    #[test]
    fn test_check_repository_directory_structure() {
        std::env::remove_var("NIX_BUILD_TOP");
        let temp_dir = std::env::temp_dir().join("test-repo-structure");
        init_temp_repo(&temp_dir);
        let flake_path = temp_dir.join("flake.nix");
        let result =
            validate_repository_directory_structure(flake_path.to_str().unwrap().to_string());
        assert!(
            result.is_ok(),
            "Expected Ok, but got Err: {:?}",
            result.err()
        );
        fs::write(temp_dir.join("unallowed.txt"), "test").unwrap();
        let result =
            validate_repository_directory_structure(flake_path.to_str().unwrap().to_string());
        assert!(result.is_err());
        fs::remove_file(temp_dir.join("unallowed.txt")).unwrap();
        fs::create_dir_all(temp_dir.join("templates/my-template/packages/my-pkg")).unwrap();
        fs::write(
            temp_dir.join("templates/my-template/packages/my-pkg/default.nix"),
            "test",
        )
        .unwrap();
        fs::write(
            temp_dir.join("templates/my-template/packages/my-pkg/.gitignore"),
            "test",
        )
        .unwrap();
        Command::new("git")
            .args(["add", "templates"])
            .current_dir(&temp_dir)
            .output()
            .unwrap();
        let result =
            validate_repository_directory_structure(flake_path.to_str().unwrap().to_string());
        assert!(result.is_err());
        fs::remove_dir_all(temp_dir.join("templates")).unwrap();
        fs::create_dir_all(temp_dir.join("packages/no-default")).unwrap();
        fs::write(temp_dir.join("packages/no-default/main.py"), "test").unwrap();
        let result =
            validate_repository_directory_structure(flake_path.to_str().unwrap().to_string());
        assert!(result.is_err());
        fs::remove_dir_all(temp_dir.join("packages/no-default")).unwrap();
        fs::remove_dir(temp_dir.join("packages")).unwrap();
        fs::create_dir_all(temp_dir.join("hosts/my-host")).unwrap();
        fs::write(temp_dir.join("hosts/my-host/configuration.nix"), "test").unwrap();
        fs::write(
            temp_dir.join("hosts/my-host/hardware-configuration.nix"),
            "test",
        )
        .unwrap();
        Command::new("git")
            .args(["add", "hosts"])
            .current_dir(&temp_dir)
            .output()
            .unwrap();
        let result =
            validate_repository_directory_structure(flake_path.to_str().unwrap().to_string());
        assert!(
            result.is_ok(),
            "Expected Ok for hosts/configuration.nix, but got Err: {:?}",
            result.err()
        );
        fs::write(temp_dir.join("hosts/my-host/.gitignore"), "test").unwrap();
        let result =
            validate_repository_directory_structure(flake_path.to_str().unwrap().to_string());
        assert!(result.is_err());
        fs::remove_file(temp_dir.join("hosts/my-host/.gitignore")).unwrap();
        fs::create_dir_all(temp_dir.join("hosts/only-hardware")).unwrap();
        fs::write(
            temp_dir.join("hosts/only-hardware/hardware-configuration.nix"),
            "test",
        )
        .unwrap();
        let result =
            validate_repository_directory_structure(flake_path.to_str().unwrap().to_string());
        assert!(result.is_err());
        fs::remove_dir_all(&temp_dir).unwrap();
    }
    #[test]
    fn test_python_package_layout_matches_repository_conventions() {
        std::env::remove_var("NIX_BUILD_TOP");
        let temp_dir = std::env::temp_dir().join("test-repo-structure-python");
        init_temp_repo(&temp_dir);
        let package_root = temp_dir.join("packages/python_template");
        fs::create_dir_all(&package_root).unwrap();
        for (relative_path, contents) in [
            (".gitignore", "tmp/\n"),
            ("default.nix", "{}"),
            ("main.py", "print('Hello World')\n"),
        ] {
            fs::write(package_root.join(relative_path), contents).unwrap();
        }
        Command::new("git")
            .args(["add", "packages"])
            .current_dir(&temp_dir)
            .output()
            .unwrap();
        let flake_path = temp_dir.join("flake.nix");
        let result =
            validate_repository_directory_structure(flake_path.to_str().unwrap().to_string());
        assert!(
            result.is_ok(),
            "Expected Ok for python_template layout, but got Err: {:?}",
            result.err()
        );
        fs::write(package_root.join("extra.py"), "print('extra')\n").unwrap();
        let result =
            validate_repository_directory_structure(flake_path.to_str().unwrap().to_string());
        assert!(
            result.is_err(),
            "Expected Err for non-whitelisted Python file, but got Ok",
        );
        fs::remove_dir_all(&temp_dir).unwrap();
    }
    #[test]
    fn test_rust_package_layout_matches_repository_conventions() {
        std::env::remove_var("NIX_BUILD_TOP");
        let temp_dir = std::env::temp_dir().join("test-repo-structure-rust");
        init_temp_repo(&temp_dir);
        let package_root = temp_dir.join("packages/rust-template");
        fs::create_dir_all(package_root.join("src")).unwrap();
        for (relative_path, contents) in [
            (".gitignore", "target/\n"),
            ("default.nix", "{}"),
            (
                "Cargo.toml",
                "[package]\nname = \"rust-template\"\nversion = \"0.1.0\"\n",
            ),
            ("Cargo.lock", "# lock\n"),
            ("src/main.rs", "fn main() {}\n"),
        ] {
            fs::write(package_root.join(relative_path), contents).unwrap();
        }
        Command::new("git")
            .args(["add", "packages"])
            .current_dir(&temp_dir)
            .output()
            .unwrap();
        let flake_path = temp_dir.join("flake.nix");
        let result =
            validate_repository_directory_structure(flake_path.to_str().unwrap().to_string());
        assert!(
            result.is_ok(),
            "Expected Ok for rust-template layout, but got Err: {:?}",
            result.err()
        );
        fs::write(package_root.join("src/lib.rs"), "pub fn x() {}\n").unwrap();
        let result =
            validate_repository_directory_structure(flake_path.to_str().unwrap().to_string());
        assert!(
            result.is_err(),
            "Expected Err for non-whitelisted Rust source file, but got Ok",
        );
        fs::remove_dir_all(&temp_dir).unwrap();
    }
}
