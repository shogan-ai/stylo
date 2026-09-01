#!/bin/sh
# Point this repository's git hooks at the committed hooks/ directory.
set -e
cd "$(dirname "$0")/.."
git config core.hooksPath hooks
