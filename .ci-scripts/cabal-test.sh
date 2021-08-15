#!/usr/bin/env bash

# we assume a build has already been done

set -euo pipefail
set -x

PATH=$HOME/.local/bin:$HOME/.cabal/bin:$PATH

# check we have cabal and ghc
cabal --version
ghc --version

cabal new-build --enable-tests
cabal new-test --jobs=1
