#!/usr/bin/env bash

# Intended to work with stack. Would need changes to work with cabal.
# Assumes that ormolu[1], hlint[2] and refactor[3] are on the
# PATH.

# Also assumes there's a stack.yaml file in the top directory -
# so if there isn't, you probably want to copy (or symlink)
# stack-lts-13.30.yaml to stack.yaml.

# [1] ormolu: https://hackage.haskell.org/package/ormolu
# [2] hlint: https://hackage.haskell.org/package/hlint
# [3] refactor: https://hackage.haskell.org/package/apply-refact

PODMAN_VERSION="3.2.3"

set -euo pipefail
set -x

stack build \
  --local-bin-path binaries --copy-bins --fast \
  podman-codegen:exe:podman-codegen

./binaries/podman-codegen $PODMAN_VERSION \
    | ormolu --start-line 7 > src/Podman/Types.hs

hlint --refactor --refactor-options=-i src/Podman/Types.hs

