use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};
fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            eprintln!("{err}");
            ExitCode::from(1)
        }
    }
}
fn run() -> Result<(), String> {
    let config = parse_args()?;
    let target_dir = Path::new(&config.target_directory);
    let available_templates = get_available_templates()?;
    let templates_to_copy: Vec<PathBuf> = available_templates
        .into_iter()
        .filter_map(|(name, path)| config.selected_templates.contains(&name).then_some(path))
        .collect();
    if !target_dir.exists() {
        log_progress(format_args!(
            "Creating target directory: {}",
            target_dir.display()
        ));
        fs::create_dir_all(target_dir)
            .map_err(|err| format!("Failed to create target directory: {err}"))?;
    }
    log_progress(format_args!(
        "Initializing git repository in {}",
        target_dir.display()
    ));
    run_command(
        Command::new("git")
            .args(["init", "-b", "main"])
            .current_dir(target_dir),
        "git init",
    )?;
    let root_dir = get_root_dir()?;
    let flake_nix_src = root_dir.join("flake.nix");
    if flake_nix_src.exists() {
        let dest = target_dir.join("flake.nix");
        let _ = fs::remove_file(&dest);
        fs::copy(&flake_nix_src, &dest)
            .map_err(|err| format!("Failed to copy flake.nix: {err}"))?;
        let mut permissions = fs::metadata(&dest)
            .map_err(|err| format!("Failed to read copied flake.nix metadata: {err}"))?
            .permissions();
        permissions.set_readonly(false);
        fs::set_permissions(&dest, permissions)
            .map_err(|err| format!("Failed to make copied flake.nix writable: {err}"))?;
        let content = fs::read_to_string(&dest)
            .map_err(|err| format!("Failed to read copied flake.nix: {err}"))?;
        let replacement =
            "inputs = {\n    canonicalization.url = \"github:pbizopoulos/canonicalization\";";
        let new_content = content.replacen("inputs = {", replacement, 1);
        fs::write(&dest, new_content)
            .map_err(|err| format!("Failed to write modified flake.nix: {err}"))?;
    }
    let checks_src_root = root_dir.join("checks");
    for template_path in templates_to_copy {
        let template_name = template_path
            .file_name()
            .and_then(|s| s.to_str())
            .ok_or_else(|| format!("Invalid template path: {}", template_path.display()))?;
        let dest_path = target_dir.join("packages").join(template_name);
        log_progress(format_args!(
            "Copying template {template_name} to {}",
            dest_path.display()
        ));
        copy_dir_contents(&template_path, &dest_path)?;
        let checks_src = checks_src_root.join(template_name);
        if checks_src.exists() {
            let checks_dest = target_dir.join("checks").join(template_name);
            log_progress(format_args!(
                "Copying checks {template_name} to {}",
                checks_dest.display()
            ));
            copy_dir_contents(&checks_src, &checks_dest)?;
        }
    }
    if target_dir.join("flake.nix").exists() {
        log_progress(format_args!("Running nix fmt in target directory..."));
        run_command(
            Command::new("nix").arg("fmt").current_dir(target_dir),
            "nix fmt",
        )
        .ok();
    }
    Ok(())
}
#[derive(Debug)]
struct Config {
    target_directory: String,
    selected_templates: Vec<String>,
}
fn parse_args() -> Result<Config, String> {
    parse_args_from(env::args())
}
fn parse_args_from<I>(args: I) -> Result<Config, String>
where
    I: IntoIterator<Item = String>,
{
    let mut args = args.into_iter();
    let program_name = args.next().unwrap_or_else(|| "default".to_string());
    let target_directory = args
        .next()
        .ok_or_else(|| format!("Usage: {program_name} <directory> [--templates rust,python]"))?;
    let mut selected_templates = Vec::new();
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-t" | "--templates" => {
                let value = args
                    .next()
                    .ok_or_else(|| "Missing value for --templates".to_string())?;
                selected_templates.extend(
                    value
                        .split(',')
                        .filter(|item| !item.is_empty())
                        .map(std::string::ToString::to_string),
                );
            }
            value if value.starts_with("--templates=") => {
                let value = &value["--templates=".len()..];
                selected_templates.extend(
                    value
                        .split(',')
                        .filter(|item| !item.is_empty())
                        .map(std::string::ToString::to_string),
                );
            }
            other => {
                return Err(format!("Unrecognized argument: {other}"));
            }
        }
    }
    Ok(Config {
        target_directory,
        selected_templates,
    })
}
fn run_command(command: &mut Command, label: &str) -> Result<(), String> {
    let status = command
        .status()
        .map_err(|err| format!("Failed to run {label}: {err}"))?;
    if status.success() {
        Ok(())
    } else {
        Err(format!("{label} exited with status {status}"))
    }
}
fn log_progress(message: std::fmt::Arguments<'_>) {
    if env::var("CANONICALIZATION_VERBOSE").as_deref() == Ok("1") {
        println!("{message}");
    }
}
fn copy_dir_contents(src: &Path, dest: &Path) -> Result<(), String> {
    if dest.exists() {
        fs::remove_dir_all(dest)
            .map_err(|err| format!("Failed to remove {}: {err}", dest.display()))?;
    }
    fs::create_dir_all(dest)
        .map_err(|err| format!("Failed to create {}: {err}", dest.display()))?;
    copy_dir_recursive(src, dest)?;
    let _ = set_permissions_recursive(dest);
    Ok(())
}
fn copy_dir_recursive(src: &Path, dest: &Path) -> Result<(), String> {
    for entry in
        fs::read_dir(src).map_err(|err| format!("Failed to read {}: {err}", src.display()))?
    {
        let entry = entry.map_err(|err| format!("Failed to read directory entry: {err}"))?;
        let path = entry.path();
        let dest_path = dest.join(entry.file_name());
        let file_type = entry
            .file_type()
            .map_err(|err| format!("Failed to read file type for {}: {err}", path.display()))?;
        if file_type.is_dir() {
            fs::create_dir_all(&dest_path)
                .map_err(|err| format!("Failed to create {}: {err}", dest_path.display()))?;
            copy_dir_recursive(&path, &dest_path)?;
        } else if file_type.is_file() {
            fs::copy(&path, &dest_path)
                .map_err(|err| format!("Failed to copy {}: {err}", path.display()))?;
        }
    }
    Ok(())
}
fn get_available_templates() -> Result<Vec<(String, PathBuf)>, String> {
    let root_dir = get_root_dir()?;
    let packages_dir = root_dir.join("packages");
    let mut templates = Vec::new();
    if packages_dir.exists() {
        for entry in fs::read_dir(&packages_dir)
            .map_err(|err| format!("Failed to read {}: {err}", packages_dir.display()))?
        {
            let entry = entry.map_err(|err| format!("Failed to read directory entry: {err}"))?;
            let path = entry.path();
            if path.is_dir() {
                if let Some(name) = path.file_name().and_then(|s| s.to_str()) {
                    if let Some(flag) = name
                        .strip_suffix("_template")
                        .or_else(|| name.strip_suffix("-template"))
                    {
                        templates.push((flag.to_string(), path));
                    }
                }
            }
        }
    }
    templates.sort_by(|a, b| a.0.cmp(&b.0));
    Ok(templates)
}
fn get_root_dir() -> Result<PathBuf, String> {
    if let Ok(root) = env::var("CANONICALIZATION_ROOT") {
        return Ok(PathBuf::from(root));
    }
    let mut current_dir =
        env::current_dir().map_err(|err| format!("Failed to get current directory: {err}"))?;
    loop {
        if current_dir.join("flake.nix").exists() {
            return Ok(current_dir);
        }
        if let Some(parent) = current_dir.parent() {
            current_dir = parent.to_path_buf();
        } else {
            return Err("Could not find root directory (containing flake.nix)".to_string());
        }
    }
}
#[cfg(unix)]
fn set_permissions_recursive(path: &Path) -> Result<(), String> {
    use std::os::unix::fs::PermissionsExt;
    let metadata = fs::metadata(path)
        .map_err(|err| format!("Failed to read metadata for {}: {err}", path.display()))?;
    if metadata.is_dir() {
        fs::set_permissions(path, fs::Permissions::from_mode(0o755))
            .map_err(|err| format!("Failed to set permissions on {}: {err}", path.display()))?;
        for entry in
            fs::read_dir(path).map_err(|err| format!("Failed to read {}: {err}", path.display()))?
        {
            let entry = entry.map_err(|err| format!("Failed to read directory entry: {err}"))?;
            set_permissions_recursive(&entry.path())?;
        }
    } else {
        fs::set_permissions(path, fs::Permissions::from_mode(0o644))
            .map_err(|err| format!("Failed to set permissions on {}: {err}", path.display()))?;
    }
    Ok(())
}
#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{SystemTime, UNIX_EPOCH};
    fn unique_temp_dir(prefix: &str) -> PathBuf {
        let ts = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system clock before unix epoch")
            .as_nanos();
        let dir = env::temp_dir().join(format!("{prefix}-{ts}"));
        fs::create_dir_all(&dir).expect("failed to create temp directory");
        dir
    }
    #[test]
    fn parse_args_from_requires_directory() {
        let err = parse_args_from(Vec::<String>::new()).expect_err("expected missing directory");
        assert!(err.contains("Usage: default <directory>"));
    }
    #[test]
    fn parse_args_from_supports_templates_formats() {
        let long = parse_args_from(vec![
            "default".to_string(),
            "target".to_string(),
            "--templates".to_string(),
            "rust,python".to_string(),
        ])
        .expect("failed to parse --templates");
        assert_eq!(long.target_directory, "target");
        assert_eq!(long.selected_templates, vec!["rust", "python"]);
        let equals = parse_args_from(vec![
            "default".to_string(),
            "target".to_string(),
            "--templates=go,rust".to_string(),
        ])
        .expect("failed to parse --templates=<...>");
        assert_eq!(equals.selected_templates, vec!["go", "rust"]);
    }
    #[test]
    fn parse_args_from_rejects_unknown_flags() {
        let err = parse_args_from(vec![
            "default".to_string(),
            "target".to_string(),
            "--unknown".to_string(),
        ])
        .expect_err("expected unrecognized argument");
        assert_eq!(err, "Unrecognized argument: --unknown");
    }
    #[test]
    fn get_available_templates_reads_template_suffixes() {
        let root = unique_temp_dir("default-templates");
        fs::write(root.join("flake.nix"), "{}").expect("failed to write flake.nix");
        let packages = root.join("packages");
        fs::create_dir_all(packages.join("rust-template")).expect("failed to create rust-template");
        fs::create_dir_all(packages.join("python_template"))
            .expect("failed to create python_template");
        fs::create_dir_all(packages.join("not-a-templ")).expect("failed to create non-template");
        env::set_var("CANONICALIZATION_ROOT", &root);
        let templates = get_available_templates().expect("failed to read templates");
        env::remove_var("CANONICALIZATION_ROOT");
        assert_eq!(templates.len(), 2);
        assert_eq!(templates[0].0, "python");
        assert_eq!(templates[1].0, "rust");
        fs::remove_dir_all(root).expect("failed to remove temp root");
    }
    #[test]
    fn copy_dir_contents_copies_nested_files() {
        let root = unique_temp_dir("default-copy");
        let src = root.join("src");
        let dest = root.join("dest");
        fs::create_dir_all(src.join("nested")).expect("failed to create nested src dir");
        fs::write(src.join("nested/one.txt"), "1").expect("failed to write nested file");
        fs::write(src.join("two.txt"), "2").expect("failed to write root file");
        copy_dir_contents(&src, &dest).expect("copy failed");
        assert_eq!(
            fs::read_to_string(dest.join("nested/one.txt")).expect("failed to read copied nested"),
            "1"
        );
        assert_eq!(
            fs::read_to_string(dest.join("two.txt")).expect("failed to read copied file"),
            "2"
        );
        fs::remove_dir_all(root).expect("failed to remove temp root");
    }
}
#[cfg(not(unix))]
fn set_permissions_recursive(_path: &Path) -> Result<(), String> {
    Ok(())
}
