# Bluebook

Renderer / web-server for man-pages present on (I assume) Unix-like systems.

## Usage

### Web Server

```console
bluebook serve
```

Launches a web server where the local man-pages can be browsed.

![](./screenshots/platform.1.png)

#### Configuration

- `PORT`: port to listen on, default is `3000`
- `APPROOT`: base to use for URLs, default is none
- `MANPATH`: used to define where to search for man-pages, default is

  ```
  ${XDG_DATA_DIR:-$HOME/.local/share}/man:/usr/local/share/man:/usr/share/man
  ```

### CLI

```console
bluebook write
```

Converts man-pages into a directory of local HTML files.

```
Usage: bluebook write [-o|--out PATH] [--web-links] [--app-root PATH|URL] 
                      [--man-path PATH [--man-path PATH]]

  Write local man-pages

Available options:
  -o,--out PATH            Write man-pages into this directory
  --web-links              Render links for later serving, not for file://
  --app-root PATH|URL      Render links with the following root
  --man-path PATH          Include man-pages from this path
  --man-path PATH          Include man-pages from this path
  -h,--help                Show this help text
```

## Docker Image

We ship Docker images from CI. When run, they will serve all man-pages from the
Ubuntu 20.04 based image. More man-pages you can be added in a few ways:

**Build your own image**:

```dockerfile
FROM pbrisbin:bluebook:edge

# From some official package
RUN apt-get update -y && apt-get install -y {package-with-docs}

# Or copy in locally installed
COPY /usr/local/share/man/ /usr/local/share/man/
COPY /usr/share/man/ /usr/share/man/
```

**Mount your local files**:

```console
docker run --rm \
  --volume "$HOME/.local/share/man:/root/.local/share/man:ro" \
  --publish 3000 \
  pbrisbin/bluebook:edge bluebook serve
```

The image can also be used to produce local HTML files:

```console
docker run --rm \
  --volume "$PWD"/dist:/dist
  pbrisbin/bluebook:edge bluebook write --out /dist
```

Note that the files will come out `root`-owned.

## Motivation

**Why not use [die.net](https://linux.die.net/man/), or
[kernel.org](https://www.kernel.org/doc/man-pages/),
[man7](https://man7.org/linux/man-pages/index.html)?**

At work, we write a handful of CLI tools that are thoroughly documented through
man-pages that get installed with the tool. It is super useful to have these
also be online-browsable, particularly with references to other pages linked and
traversable. Unfortunately, this rules out sites such as above as they'd never
contain our man-pages.

**Why not use [existing tool]?**

I could not find a tool that did the minimal things I need:

- Convert local man-pages to HTML
- Turn headers into linkable anchors
- Turn text like `foo(1)` into links to `man1/foo.1.html`

If you know of such a tool, do let me know!

## Development

We use the Haskell tool Stack:

```console
stack setup
stack build --dependencies-only
stack build --fast --pedantic --test --file-watch
```

---

[LICENSE](./LICENSE) | [CHANGELOG](./CHANGELOG.md)
