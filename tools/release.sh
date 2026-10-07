#!/bin/bash
# Usage: tools/release.sh VERSION
#
# Creates the release commit and tag for VERSION, on top of the current commit
# but without moving the current branch. The release commit adds the generated
# files which are not committed otherwise, so that the release tarball doesn't
# need ppxlib to build. Run dune-release on the tag afterwards.
set -euo pipefail

version=${1:?usage: $0 VERSION}
cd "$(git rev-parse --show-toplevel)"

generated=(lib/parsing/traversals.ml vendor/oxcaml-frontend/ast_mapper.ml)

if [[ -n $(git status --porcelain --untracked-files=no) ]]; then
  echo "error: uncommitted changes" >&2
  exit 1
fi
if git rev-parse -q --verify "refs/tags/$version" >/dev/null; then
  echo "error: tag $version already exists" >&2
  exit 1
fi
for f in "${generated[@]}"; do
  if [[ -e $f ]]; then
    echo "error: $f exists, remove it to make sure it's regenerated" >&2
    exit 1
  fi
done

# the files are generated into _build since they are absent from the source tree
dune build "${generated[@]}"

orig=$(git symbolic-ref -q --short HEAD || git rev-parse HEAD)
git switch --detach --quiet
trap 'git switch --quiet "$orig"; rm -f "${generated[@]}"' EXIT
for f in "${generated[@]}"; do
  cp "_build/default/$f" "$f"
  chmod u+w "$f"
  git add "$f"
done
git commit --quiet --no-verify -m "release $version: ship generated files"
git tag -a "$version" -m "$version"
echo "Tagged $version ($(git rev-parse --short HEAD)); $orig is untouched."
echo "Next: dune-release distrib / publish ... (tag $version)"
