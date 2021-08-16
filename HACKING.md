# Development/hacking tips

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



