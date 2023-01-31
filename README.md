## NAME

bluebook - nicely render man-pages to html, with cross-linking

## SYNOPSIS

**bluebook** [**\--out**=*PATH*]

## DESCRIPTION

Bluebook converts man-pages to html. In so doing, it applies styles and
conversions to make the pages useful for browsing. This includes cross-linking
references to other man-pages it knows about.

## OPTIONS

**\-o**, **\--out** _<PATH\>_

Write html files in this directory, default is _./dist_.

## ENVIRONMENT

**MANPATH**

> `:`-separated list of directories to search for man-pages. Default is,
>
> ```
> ${XDG_DATA_DIR:-$HOME/.local/share}/man:/usr/local/share/man:/usr/share/man
> ```

## NOTES

**Why not use die.net, kernel.org, or man7.org?**

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

## CAVEATS

The files Bluebook creates are meant to be served by a web-server, not browsed
directly on the file-system. For example, we render links to *directory/* and
expect *directory/index.html* to be served. See _EXAMPLES_ for an easy way to
achieve this behavior in a local context.

## EXAMPLES

Convert all your system's man-pages into the `./dist` directory, serve them, and
browse them:

```
bluebook
docker run -d -p 8080:80 -v "$PWD"/dist:/usr/share/nginx/html:ro nginx
$BROWSER http://localhost:8080
```

Use Bluebook in a GitHub Action to build a static site of man-pages for deploy:

```
- uses: pbrisbin/setup-tool-action@v1
  with:
    name: bluebook
    version: 1.1.0.0
    url: 'https://github.com/pbrisbin/{name}/releases/download/v{version}/{name}-{os}-{arch}'
    no-extract: "true"

- uses: actions/cache@v3
  with:
    path: |
      ./.shake
      ./_site
    key: ...

- name: Generate HTML
  run: bluebook -o _site

- name: Deploy
  run: ...
```
