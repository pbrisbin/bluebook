% BLUEBOOK(1) User Manual
%
% January 2023

## NAME

bluebook - nicely render man-pages to html, with cross-linking

## SYNOPSIS

**bluebook** [*option*]... [*target*]...

## DESCRIPTION

Bluebook converts man-pages to html. In so doing, it applies styles and
conversions to make the pages useful for browsing. This includes cross-linking
references to other man-pages it knows about.

## OPTIONS

Bluebook comprises a small set of _Shake_ rules for producing its artifacts. As
such, the **bluebook** executable's option parser is provided by Shake.

To see all available options, run **bluebook --help**. The following lists
commonly-needed and/or overridden options only.

**\-C**, **\--directory**=_DIRECTORY_

> Change into _DIRECTORY_ before producing artifacts. _DIRECTORY_ must exist
> already.

The following options represent **bluebook**'s default behavior:

**\--color**

> Colorize output. Use **\--no-color** to disable.

**\--digest-and-input**

> Consider files changed based on their modtime and digest for inputs. Use
> another **\--digest** option to override.

**\-j**, **\--jobs**[=*N*]

> Process _N_ targets concurrently (defaulted to number of CPUs). Use
> **\--jobs** again to override (with value _1_ to disable).

**\--verbose**

> Increase verbosity. Use **\--quiet** to override, or pass it again to increase
> further.

## ENVIRONMENT

**HTML_ROOT**

> Prefix for all internal links. Default is `/`. The value must end in a slash
> and one will be appended automatically if necessary.

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
directly on the file-system. For example, we render links to _directory/_ and
expect _directory/index.html_ to be served. See _EXAMPLES_ for an easy way to
achieve this behavior in a local context.

## EXAMPLES

Convert all your system's man-pages into the `./dist` directory, serve them, and
browse them:

```
bluebook -C dist --color -j -p -V
docker run -d -p 8080:80 -v "$PWD"/dist:/usr/share/nginx/html:ro nginx
$BROWSER http://localhost:8080
```

Convert a single man-page and view its HTML in a Browser (navigation links won't
work, but the single page will):

```
MANPATH=/path/to/man bluebook -C /tmp man1/page.1.html
$BROWSER /tmp/man1/page.1.html
```

Use Bluebook in a GitHub Action to build a static site of man-pages for deploy:

```
- uses: pbrisbin/setup-tool-action@v1
  with:
    name: bluebook
    version: 1.1.0.0
    url: "https://github.com/pbrisbin/{name}/releases/download/v{version}/{name}-{os}-{arch}"
    no-extract: "true"

- uses: actions/cache@v3
  with:
    path: _site
    key: ${{ runner.os }}-site

- name: Generate HTML
  run: bluebook -C _site

- name: Deploy
  run: ...
```

# SEE ALSO

- https://shakebuild.com/manual
- https://pandoc.org/MANUAL.html
- https://hub.docker.com/_/nginx
