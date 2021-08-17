
-- |
-- Copyright: (c) 2021 Red Hat
-- SPDX-License-Identifier: Apache-2.0
-- Maintainer: Tristan de Cacqueray <tdecacqu@redhat.com>
--
-- See "Podman.Tutorial" to learn how to use this library.
--
-- Here is the recommended way to import this library:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Podman
--
-- This module re-exports the rest of the library.
--
-- "Podman.Types" provides data types generated from the swagger definitions.
--
-- "Podman.Internal" provides utility function to further decode API response.
-- Internal is exposed for testing purpose and it shouldn't be used.
--
-- = Notes
--
-- Some notes on the API. And on Podman generally. (Particularly on ttys, and attaching
-- and detaching from them, since that's quite fiddly.)
--
-- * When creating a container, '_specGeneratorstdin' is equivalent to "podman container create -i ...".
--   However, it /doesn't/ automatically allocate a tty. You need
--   '_specGeneratorterminal' for that (the equivalent of passing "-t" to "podman container create").
-- * If attaching to a container, a tty needs to be allocated if you want to /detach/
--   again using the "detach keys". See
--   <http://docs.podman.io/en/latest/markdown/podman-attach.1.html container attach man page>
--   for details of "container attach", and
--   <https://github.com/containers/podman/issues/6990 this "wontfix" issue> which
--   says detaching from a non-tty container is not supported.
-- * The Podman API notes on
--   <https://docs.podman.io/en/latest/_static/api.html#operation/ContainerAttachLibpod "containers/{ctr_name}/attach">
--   are not very complete. It looks like the Docker Engine API docs have
--   additional information (see
--   <https://docs.docker.com/engine/api/v1.41/#operation/ContainerAttach>). Namely:
--
--     * When attaching to a container, the HTTP connection is "hijacked" so as to
--       transport stdin, stdout, and stderr on the same socket.
--
--     * The response from the daemon for an attach request is:
--
--           @
--           HTTP/1.1 200 OK
--           Content-Type: application/vnd.docker.raw-stream
--
--           [STREAM]
--           @
--
--          After the headers and two new lines, the TCP connection can now be used
--          for raw, bidirectional communication between the client and server.
--
--     * The client will (should?) send @Upgrade: tcp@ and @Connection: Upgrade@
--       headers in its request.
--
--     * If that happens server will (should?) respond with
--       @HTTP/1.1 101 UPGRADED@, and @Connection: Upgrade@ and @Upgrade: tcp@
--       headers.
--
--     * Those "upgrade" headers are supposed to be a hint to any HTTP proxies about
--       what's going on; but I suspect a fair few proxies just won't be able to cope
--       with the socket being hijacked in this way.
--
--     * See the Docker API documentation for more info (e.g. on the stream
--       format).

module Podman
  ( -- * Client
    PodmanClient,
    withClient,
    Result,
    module Podman.Api,
    module Podman.Types,

    -- * re-exports
    Text,
  )
where

import Data.Text (Text)
import Podman.Api
import Podman.Client
import Podman.Types
