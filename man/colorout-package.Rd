\name{colorout-package}
\alias{colorout-package}
\alias{colorout}
\docType{package}
\title{
Colorize R output on terminal emulators
}
\description{
  Colorize R output when it is running on a Unix terminal emulator. The
  library also works within RStudio, but there is no support for other
  Graphical User Interfaces, such as Windows RGui, RKward, JGR, Rcmdr
  that cannot display ANSI colors. It also did not work with Emacs with ess.
  The functions of this package only work if R is compiled for Unix systems
  (e.g., Linux and OS X) and it is running interactively in a terminal
  emulator. The terminal emulator might have been called by a text editor,
  such as Vim, Gedit, Kate or Geany.

  Homepage: \url{https://github.com/jalvesaq/colorout}

  Screenshot:

  \figure{screenshot.png}
}
\details{

  The terminal must support Select Graphic Rendition (SGR, also known as ANSI
  escape codes or sequences), otherwise you may see garbage like this:

  \preformatted{
    > rnorm(5)
    [32m[ [33m1 [32m] [0m [32m  [33m0.07574585 [32m [0m [32m
    [33m0.88167822 [32m [0m [32m  [33m0.60788656 [32m [0m [32m
    [33m1.13590951 [32m [0m [32m  [33m1.07758879 [32m [0m [32m [0m
  }

  The package contains a C library with a function that replaces the default
  Rstd_WriteConsoleEx which, when enabled, is responsible for printing
  messages in the Console when R is running in interactive mode.
  R should be built against libreadline since libedit may cause issues.

  The function that enables the colorization of R output is \link{ColorOut},
  and it is called automatically when the package is loaded. However, it will
  do nothing if \code{Sys.getenv("TERM")} returns either \code{""} or
  \code{"dumb"}. The output will not be colorized also if the result of either
  \code{interactive()} or \code{isatty(stdout())} is \code{FALSE}. You can
  change this behavior by putting in your \samp{~/.Rprofile} one or more of
  the following options:

  \preformatted{
    options(colorout.verbose = 0)
    options(colorout.anyterm = TRUE)
    options(colorout.dumb = TRUE)
    options(colorout.noninteractive = TRUE)
    options(colorout.notatty = TRUE)
  }

  With \code{colorout.anyterm == TRUE}, the package will not check the
  \samp{TERM} environment variable, and it will also not test whether R is
  running interactively. If your terminal emulator is capable of displaying
  ANSI colors but is not being recognized you may put the following in your
  \samp{~/.Rprofile}:

  \preformatted{
    if(interactive()){
        options(colorout.anyterm = TRUE)
        library(colorout)
    }
  }

  And of course, you can use any arbitrary condition to decide whether to load
  colorout or not. For instance:

  \preformatted{
    if(isatty(stdout())){
        options(colorout.anyterm = TRUE)
        library(colorout)
    }
  }

  If \code{colorout.verbose > 0}, the package will display a warning if the
  output is not going to be colorized.

  A 256 color scheme is used by default. If your terminal emulator only
  supports 8 colors, you will have to set your own color scheme. Please, see
  the example at \link{setOutputColors}.
}

\seealso{
\link{ColorOut}, \link{noColorOut}, \link{setOutputColors},
\link{show256Colors} and \link{setZero}.
The package \pkg{setwidth} updates the value of \code{options("width")}
automatically when the terminal is resized.
}
\author{
Jakson Alves de Aquino \email{jalvesaq@gmail.com} and
Dominique-Laurent Couturier \email{dlc48@medschl.cam.ac.uk}.
}
\keyword{ package }
