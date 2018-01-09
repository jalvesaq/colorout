colorout
========

*colorout* is an R package that colorizes R output when running in a Unix
(e.g. Linux and OS X) terminal emulator; it does not work on Microsoft Windows.
The relevant code is written in C and, thus, it runs very quickly and you
should not note any slowdown in R output. R's `stdout` is parsed and numbers,
negative numbers, dates in the standard format, strings, and R constants are
identified and wrapped by special ansi scape codes that are interpreted by
terminal emulators as commands to colorize the output. R's `stderr` is also
parsed to identify the expressions "warning" and "error" and their
translations to many languages. If these expressions are found, the output is
colorized accordingly; otherwise, it is colorized as "stderror" (blue, by
default). You can customize the colors according to your taste, guided by the
color table made by the command `show256Colors()`.

The package cannot be on CRAN because it changes code already loaded by R and
this is prohibited by the *CRAN Repository Policy*. The package replaces the
functions that output results and messages to R Console, and this is necessary
because we cannot colorize the output without replacing these functions.

You can intall the package by running the following commands in a terminal
emulator:

```
git clone https://github.com/jalvesaq/colorout.git
cd colorout
git checkout <version.tag>
cd ..
R CMD INSTALL colorout
```

You can find the available version tags
[here](https://github.com/cpfaff/colorout/releases).

Some people prefer to use
[devtools](http://cran.r-project.org/web/packages/devtools/index.html) to
install packages from github.

Recently released versions are available at
https://github.com/jalvesaq/colorout/releases and older versions are at
https://drive.google.com/open?id=0ByMBQcSs9G7KYkotRGpRYjlLVDg

Screenshot:

![Screenshot](https://raw.githubusercontent.com/jalvesaq/colorout/master/man/figures/screenshot.png "Screenshot")
