#!/usr/bin/env bash
set -euo pipefail
current_dir=$(dirname "$(realpath "$0")")
repository_dir=$(git -C "$current_dir" rev-parse --show-toplevel)
cd "${repository_dir}/prm"
eval "$(nix run github:ryantm/agenix -- --rekey)"
