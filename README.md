# BLUEBOOK(1)

## NAME

bluebook - nicely render man-pages to html, with cross-linking

## SYNOPSIS

**bluebook** [**\--out**=*PATH*]

## DESCRIPTION

Bluebook converts man-pages to html. In so doing, it applies styles and
conversions to make the pages useful for browsing. This includes cross-linking
references to other man-pages, and converting any bare URLs to actual HTML
links.

The files Bluebook creates are meant to be served by a web-server, not browsed
directly on the file-system. See *EXAMPLES*.

## OPTIONS

**\-o**, **\--out** *\<PATH\>*

> Write html files in this directory, default is *./dist*.

## ENVIRONMENT

**MANPATH**

> `:`-separated list of directories to search for man-pages. Default is,
>
> ```
> ${XDG_DATA_DIR:-$HOME/.local/share}/man:/usr/local/share/man:/usr/share/man
> ```

## NOTES

You may wonder,

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

## EXAMPLES

Convert all your system's man-pages into the `./dist` directory,

```
bluebook
```

Serve them,

```
docker run \
  --detach \
  --publish 8080:80 \
  --volume "$PWD"/dist:/usr/share/nginx/html:ro \
  nginx
```

And browse them,

```
$BROWSER http://localhost:8080
```

---

[LICENSE](./LICENSE) | [CHANGELOG](./CHANGELOG.md)
