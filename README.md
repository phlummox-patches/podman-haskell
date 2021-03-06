# podman

[![Hackage](https://img.shields.io/hackage/v/podman.svg)](https://hackage.haskell.org/package/podman)
[![build](https://github.com/phlummox-patches/podman-haskell/actions/workflows/build.yml/badge.svg)](https://github.com/phlummox-patches/podman-haskell/actions/workflows/build.yml)

A client library for [podman](https://podman.io)

**NB:** This fork of <https://github.com/softwarefactory-project/podman-haskell>
is experimental! (Although it has been tested somewhat against the Alpine build
of podman 3.2.3. See the GitHub actions [workflow file](.github/workflows/build.yml).)
If you want something that is maintained and tested, use the original.

For development tips, see [HACKING.md](HACKING.md).

## Overview and scope

At a high level, the scope of podman-haskell is to provide a REST client for
the podman API based on the swagger description.
The goal is to validate the definitions and enable using the podman service in
Haskell application.

For the compat API, use the [docker client](https://hackage.haskell.org/package/docker) instead.

## Features

- JSON decoder for API endpoints.
- HTTP client helper functions.

## Documentation

Haddock documentation for the original project is available at
<https://docs.softwarefactory-project.io/podman-haskell/>.

Haddock documentation for this repository is available at
<https://phlummox-patches.github.io/podman-haskell/>.

## Contribute

Contributions are most welcome, for example the project needs help to:

- Add missing endpoint (e.g. Pod, Image).
- Add test case json sample.
- Report and fix mismatchs in the upstream podman swagger documentation.
- Higher level functions to simplify usage.

To work on this project you need a Haskell toolchain, for example on fedora:

```
$ sudo dnf install -y ghc cabal-install && cabal update
```

Then grab a copy of the swagger file:

```
$ curl -OL https://storage.googleapis.com/libpod-master-releases/swagger-latest.yaml
```

Run tests:

```
$ cabal build --enable-tests --enable-benchmarks --write-ghc-environment-files=always -O0 all
$ cabal test --enable-tests --test-show-details=direct -O0
$ cabal haddock
```

Update the types by running the `podman-codegen` utility.

Validate the API using the `podman-quickcheck` and the `podman-demo` utility.
