colorout
========

*colorout* is an R package that colorizes R output when running in a Unix
(e.g. Linux and OS X) terminal emulator; it does not work on Microsoft Windows.
The relevant code is written in C and, thus, it runs very quickly and you
should not note any slowdown in R output.

R's `stdout` is parsed and numbers, negative numbers, dates in the standard
format, strings, indices, and R constants are identified and wrapped by special
ansi escape codes that are interpreted by terminal emulators as commands to
colorize the output.

R's `stderr` is also parsed to identify the expressions "warning" and "error"
and their translations to many languages. If these expressions are found, the
output is colorized accordingly; otherwise, it is colorized as "stderror"
(magenta, by default).

Screenshot:

![Screenshot](https://raw.githubusercontent.com/jalvesaq/colorout/master/man/figures/screenshot.png "Screenshot")

### Installation

You can install the latest release version of the package from
[R-multiverse](https://community.r-multiverse.org/):

```r
install.packages('colorout', repos = 'https://community.r-multiverse.org')
```

You can install the development version by running the following commands in a terminal
emulator:

```sh
git clone https://github.com/jalvesaq/colorout.git
R CMD INSTALL colorout
```

Some people prefer to use
[devtools](http://cran.r-project.org/web/packages/devtools/index.html) to
install packages from github.

_NOTES_:

  - The package cannot be on CRAN because it changes code already loaded by R
    and this is prohibited by the *CRAN Repository Policy*. The package
    replaces the functions that output results and messages to R Console, and
    this is necessary because we cannot colorize the output without replacing
    these functions.

  - Because the package is not on CRAN, it is better to load it with
    `require()` rather than `library()` to avoid error in your `~/.Rprofile`
    after upgrading R.

  - You should load `colorout` only when you can actually see R's output, not
    when running R non-interactively (see example on the next section).
    This will avoid unnecessary error messages in some circumstances (in the
    [languageserver](https://github.com/REditorSupport/languageserver)
    log, for instance).

### Customization

You can customize the colors according to your preference, guided by the
color table made by the command `show256Colors()`.
You can also set the colors to any arbitrary string. In this case, it is
up to you to set valid values.

You can also call the function `addPattern()` to set your own patterns to be
colorized.

Example of how to load `colorout` from your `~/.Rprofile`:

```r
if (interactive() || isatty(stdout())) {
    options(colorout.verbose = 1)
    if (require("colorout", quietly = FALSE)) {
        # Gruvbox color scheme by @sjlva
        colorout::setOutputColors(
            index    = "\x1b[38;2;215;153;33m",
            normal   = "\x1b[38;2;235;219;178m",
            number   = "\x1b[38;2;211;134;155m",
            negnum   = "\x1b[38;2;104;157;106m",
            zero     = "\x1b[38;2;69;133;136m",
            infinite = "\x1b[38;2;250;189;47m",
            string   = "\x1b[38;2;184;187;38m",
            date     = "\x1b[38;2;254;128;25m",
            const    = "\x1b[38;2;250;189;47m",
            true     = "\x1b[38;2;142;192;124m",
            false    = "\x1b[38;2;251;73;52m",
            warn     = "\x1b[38;2;250;189;47m",
            stderror = "\x1b[38;2;204;36;29m",
            error    = "\x1b[38;2;204;36;29m",
            verbose  = TRUE
        )
    }
}
```

Try different colors and, when happy with your colorscheme, change
`colorout.verbose` to `0`, `quietly` to `TRUE`, and `verbose` to `FALSE`. If
you don't understand the color codes above, see
<https://github.com/jalvesaq/colorout/issues/27#issuecomment-1692518129>.
