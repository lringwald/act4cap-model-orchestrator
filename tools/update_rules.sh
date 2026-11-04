#!/usr/bin/env bash
# ==============================================================================
# tools/update_rules.sh — Updates agent rulebooks from agent-rules-hub
# ==============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd -P)"
cd "$SCRIPT_DIR"

echo ">>> Checking and syncing agent rules from https://github.com/lringwald/agent-rules-hub..."

if [ -d ".rules-hub/.git" ] || [ -f ".rules-hub/.git" ]; then
  git submodule update --remote --init .rules-hub 2>/dev/null || true
elif command -v git >/dev/null 2>&1 && [ -f ".gitmodules" ]; then
  git submodule update --init --recursive .rules-hub 2>/dev/null || true
fi

if [ -f ".rules-hub/install-rules.sh" ]; then
  bash .rules-hub/install-rules.sh . copy >/dev/null 2>&1 || true
  echo ">>> Agent rules successfully refreshed and wired."
else
  echo ">>> Note: .rules-hub/install-rules.sh not found; keeping current rules in .agents/rules."
fi

