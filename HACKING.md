# Development/hacking tips

## Regenerating `src/Podman/Types.hs`

[`src/Podman/Types.hs`](src/Podman/Types.hs) is generated from a
[Swagger][[swagger] API specification; the specifications for different Podman
API versions can be downloaded from some random [Google Cloud Storage
bucket][gs-bucket] (because of course, what a totally obvious place to put
your important specifications).

[`podman-codegen`](podman-codegen) is the program that does the
code generation, and there's a (Stack-only) script to run it:
[`regenerate-podman-types.sh`](regenerate-podman-types.sh).
See the script for more details -- to get a pretty version of the
generated code, you need Ormolu, HLint and Refactor installed,
but you can comment those bits out if you want it to run quicker
and don't care about the code formatting.

See also the GitHub Actions workflows under `.github/workflows`
for examples of installing those tools and running the script.

[swagger]: https://en.wikipedia.org/wiki/Swagger_(software)
[gs-bucket]: https://storage.googleapis.com/libpod-master-releases/

## Podman API documentation

Podman's main API documentation page (generated from the same
Swagger specification as `src/Podman/Types.hs`) is at
<https://docs.podman.io/en/latest/_static/api.html>. Apparently
it's hosted by [Read the Docs](http://readthedocs.org), but inexplicably omits
the standard (and very useful) Read the Docs feature of being able
to select historical versions of the documentation.

Instead, pick the version of the Podman library you're interested
in, take a guess at what URL it might be at
(e.g. <http://docs.podman.io/en/v3.0.1/_static/api-static.html>),
and see if it works.

(Actually, there is another way -- go to
<https://readthedocs.org/projects/podman/>, click through to
the version you're interested in, and look for a link near the
bottom of the page to "our API documentation Reference".)


## Starting podman as a server

You can start podman running as a server on a free TCP port by running e.g.

```
$ podman --log-level=debug system service --time=0 tcp:0.0.0.0:3000
```

(Or you could use a Unix domain socket to listen on -- see the Podman
docco on how to do that.)

YMMV, but I found running a server from Ubuntu a bit flaky -- I
had more replicable results running the server on
[Alpine Linux][alpine] version 3.14, which has `podman` version 3.2.3
[in its repositories][pod-v-3.2.3]. You may as well install
`podman-remote` and `podman-rc` (which contains an `/etc/init.d`
service for root-ful podan) as well.

[alpine]: https://www.alpinelinux.org
[pod-v-3.2.3]: https://pkgs.alpinelinux.org/packages?name=podman*&branch=v3.14&arch=x86_64

Probably, you could run the server out of a Docker container,
but I created a Vagrant libvirt box with podman installed
(see [here][vagrant-podman]), which might be useful.

[vagrant-podman]: https://github.com/phlummox/vagrant-podman

## Intercepting client/server traffic

If you want to inspect what's being sent between client and server, HTTP
is much easier to eavesdrop on than Unix domain sockets.

Some options:

- Interpose [`mitmproxy`][mitmproxy] as a reverse proxy in front of the
  actual server.

  This is fairly straightforward. Assuming you've got Python 3 installed (I
  haven't tried Python 2), install `mitmproxy` with

  ```
  $ pip install mitmproxy
  ```

  and ensure `~/.local/bin` is on your PATH.

  Let's suppose podman is being served on port 3000. To run a reverse proxy
  on port 3001 (plus a web UI on port 8080), run

  ```
  $ mitmweb --web-port 8080 \
      --listen-host 0.0.0.0 --listen-port 3001 \
      --mode reverse:http://localhost:3000
  ```

  But note that `mitmproxy` doesn't handle the TCP streams
  from a `container attach` command well -- that seemed to
  cause errors in the web UI.

- Run [`wireshark`][wireshark] (or `tcpdump`) to capture the traffic and then inspect it.

[mitmproxy]: https://mitmproxy.org
[wireshark]: https://www.wireshark.org

## Specifying server URLs (for TCP)

Note that the Podman command-line tool uses an odd and not terribly
consistent syntax for specifying server URLs.

If you're *running* a server, you use a URL of the format
`tcp:127.0.0.1:3000`; e.g.

```
$ podman system service --time=0 tcp:0.0.0.0:3000
```

But if you're *connecting* to a server using `podman-remote`,
you need forward slashes after the "`tcp`":

```
$ podman-remote --url tcp://localhost:3000/ ps -a
```

And this Haskell library uses neither of those -- it uses
plain old HTTP URLs -- e.g. `http://localhost:300/`.

(And of course you can also serve on or connect to Unix
domain sockets, but I tend to use TCP sockets, since the
software I'm developing runs the Podman server on a different
host to the client.)

