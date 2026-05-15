{
  pkgs ? import <nixpkgs> { },
}:
let
  launcher = pkgs.writeShellScript pname ''
    set -euo pipefail
    export PATH="${
      pkgs.lib.makeBinPath [
        pkgs.coreutils
        pkgs.findutils
        pkgs.gnugrep
        pkgs.gnused
        pkgs.postgresql
        (pkgs.python313.withPackages (_: pythonDeps))
      ]
    }:$PATH"
    package_root="@packageRoot@"
    export DJANGO_SETTINGS_MODULE="${pname}.settings"
    export PYTHONPATH="$package_root''${PYTHONPATH:+:$PYTHONPATH}"
    script_name="''${0##*/}"
    mode="serve"
    if [ "$script_name" = "${pname}-manage" ]; then
      mode="manage"
    fi
    db_port=5432
    db_user=postgres
    pgsystem_user=""
    if [ "$(id -u)" -eq 0 ]; then
      for candidate in postgres nobody; do
        if id "$candidate" >/dev/null 2>&1; then
          pgsystem_user="$candidate"
          break
        fi
      done
      if [ -z "$pgsystem_user" ]; then
        echo "No unprivileged system user available" >&2
        exit 1
      fi
    fi
    run_pg() {
      if [ -z "$pgsystem_user" ]; then
        "$@"
        return
      fi
      env PATH="$PATH" su -s /bin/sh -c '
        export PATH="$1"
        shift
        exec "$@"
      ' "$pgsystem_user" -- sh "$PATH" "$@"
    }
    prepare_pg_dirs() {
      mkdir -p "$pgdata" "$pghost"
      if [ -n "$pgsystem_user" ]; then
        chown -R "$pgsystem_user" "$pgdata" "$pghost"
      fi
      chmod 700 "$pgdata" "$pghost"
    }
    init_db() {
      prepare_pg_dirs
      if [ ! -f "$pgdata/PG_VERSION" ]; then
        run_pg initdb --username="$db_user" --auth=trust -D "$pgdata" >/dev/null
      fi
    }
    start_db() {
      init_db
      if run_pg pg_ctl -D "$pgdata" status >/dev/null 2>&1; then
        return
      fi
      run_pg pg_ctl -D "$pgdata" -l "$1" -o "-k '$pghost' -p '$db_port'" start >/dev/null
      run_pg pg_isready -h "$pghost" -p "$db_port" -U "$db_user" >/dev/null
    }
    create_db() {
      run_pg psql -h "$pghost" -p "$db_port" -U "$db_user" -d postgres -tAc \
        "select 1 from pg_database where datname = '${pname}'" | grep -q 1 && return
      run_pg createdb -h "$pghost" -p "$db_port" -U "$db_user" "${pname}"
    }
    state_root="''${XDG_STATE_HOME:-/tmp}/${pname}"
    mkdir -p "$state_root"
    if [ -z "''${SECRET_KEY:-}" ]; then
      secret_key_file="$state_root/secret_key"
      if [ -f "$secret_key_file" ]; then
        export SECRET_KEY="$(cat "$secret_key_file")"
      else
        umask 077
        export SECRET_KEY="$(head -c 32 /dev/urandom | base64 | tr -d '\n')"
        printf '%s\n' "$SECRET_KEY" > "$secret_key_file"
      fi
    fi
    pgdata="$state_root/.postgres"
    pghost="$state_root/.pgsocket"
    export DATABASE_NAME="${pname}"
    export DB_HOST="$pghost"
    export DB_PORT="$db_port"
    export DB_USER="$db_user"
    export STATIC_ROOT="''${STATIC_ROOT:-$state_root/staticfiles}"
    start_db "$pgdata/postgres.log"
    trap 'if run_pg pg_ctl -D "$pgdata" status >/dev/null 2>&1; then run_pg pg_ctl -D "$pgdata" stop >/dev/null 2>&1; fi' EXIT
    create_db >/dev/null 2>&1
    if [ "''${DEBUG:-0}" = "1" ]; then
      cd "$package_root"
      exec python3 "$package_root/manage.py" test
    fi
    if [ "$mode" = "manage" ]; then
      exec python3 "$package_root/manage.py" "$@"
    fi
    python3 "$package_root/manage.py" migrate --noinput
    python3 "$package_root/manage.py" collectstatic --noinput >/dev/null
    exec gunicorn \
      --bind "127.0.0.1:8000" \
      --chdir "$package_root" \
      "${pname}.wsgi:application"
  '';
  pname = baseNameOf ./.;
  pythonDeps = with pkgs.python313Packages; [
    django
    gunicorn
    psycopg
    whitenoise
  ];
in
pkgs.python313Packages.buildPythonPackage rec {
  inherit pname;
  installCheckPhase = ''
    runHook preInstallCheck
    test -x "$out/bin/${pname}"
    mkdir -p "$PWD/coverage"
    coverage_data_file="$PWD/coverage/.coverage"
    HOME="$(mktemp -d)" \
      SECRET_KEY="django-insecure-template-secret-key" \
      DATABASE_ENGINE="sqlite" \
      EMAIL_BACKEND="django.core.mail.backends.locmem.EmailBackend" \
      DATABASE_NAME="$PWD/coverage/db.sqlite3" \
      DEBUG=1 \
      coverage run --data-file="$coverage_data_file" --branch --source=. ./manage.py test
    coverage report --data-file="$coverage_data_file"
    HOME="$(mktemp -d)" \
      SECRET_KEY="django-insecure-template-secret-key" \
      DATABASE_ENGINE="sqlite" \
      EMAIL_BACKEND="django.core.mail.backends.locmem.EmailBackend" \
      DATABASE_NAME="$PWD/coverage/db.sqlite3" \
      DEBUG=1 \
      pyinstrument ./manage.py test
    runHook postInstallCheck
  '';
  installPhase = ''
    mkdir -p "$out/lib/${pname}"
    cp -r ./. "$out/lib/${pname}"
    install -Dm755 ${launcher} "$out/bin/${pname}"
    substituteInPlace "$out/bin/${pname}" \
      --replace-fail "@packageRoot@" "$out/lib/${pname}"
    ln -s "${pname}" "$out/bin/${pname}-manage"
  '';
  meta.mainProgram = pname;
  nativeInstallCheckInputs = [
    pkgs.python313Packages.coverage
    pkgs.python313Packages.pyinstrument
  ];
  propagatedBuildInputs = pythonDeps;
  pyproject = false;
  src = ./.;
  version = "0.0.0";
}
