\name{colorout-package}
\alias{colorout-package}
\alias{colorout}
\docType{package}
\title{
Colorize R output on terminal emulators
}
\description{
  Colorize R output when it is running on a terminal emulator. There is no
  support for Graphical User Interfaces, such as Windows RGui, RStudio,
  RKward, JGR, Rcmdr and other interfaces which have their own engine to
  display R output. The functions of this package only work if R is compiled
  for Unix systems and it is running interactively in a terminal emulator.
  The terminal emulator might have been called by a text editor, such as Vim,
  Gedit, Kate or Geany.
  Screenshot: http://www.lepem.ufc.br/jaa/R_color_output.png
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

  The function that enables the colorization of R output is \link{ColorOut},
  and it is called automatically when the package is loaded. However, it will
  do nothing if \code{Sys.getenv("TERM")} returns either \code{""} or
  \code{"dumb"}. The output will not be colorized also if the result of
  \code{interactive()} is \code{FALSE}. You can change this behavior by
  putting in your \samp{~/.Rprofile} one or more of the following options:

  \preformatted{
    options(colorout.verbose = 0)
    options(colorout.anyterm = TRUE)
    options(colorout.emacs = TRUE)
    options(colorout.dumb = TRUE)
  }

  With \code{colorout.anyterm == TRUE}, the package will not check the
  \samp{TERM} environment variable, and it will also not test whether R is
  running interactively. If your terminal emulator is capable of displaying
  ANSI colors but is not being recognized you may put the following in your
  \samp{~/.Rprofile}:
  
  \preformatted{
    if(interactive())
        options(colorout.anyterm = TRUE)
    library(colorout)
  }

  If \code{colorout.verbose > 0}, the package will display a warning if the
  output is not going to be colorized.

  If \code{colorout.emacs == TRUE}, the package will look for the environment
  variable \samp{INSIDE_EMACS} and if it is not empty the package will work
  regardless of the value of the \samp{TERM} environment variable. Finally, if
  the value of \samp{TERM} is \samp{"dumb"}, and the colorization of R output
  was not already enabled by \samp{colorout.emacs == TRUE}, then you will get
  colored output only if \samp{colorout.dumb == TRUE}. Emacs/ESS users wanting
  to try this package should set the option \samp{colorout.emacs = TRUE}
  before loading the package. For example, the following code could be put in
  the \samp{~/.Rprofile}:

  \preformatted{
    options(colorout.emacs = TRUE)
    library(colorout)
  }

  Emacs/ESS users should do in the buffer where R is running:

  \preformatted{
    M-x ansi-color-for-comint-mode-on
  }

  Or, put in their \samp{~/.emacs}:

  \preformatted{
    (require 'ansi-color)
    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  }

  Notes: (1) Emacs terminal does not support 256 colors. (2) When the
  colorizing of R output is enabled, the ESS autocompletion of R object names
  and function arguments stops working on the R buffer.
}

\seealso{
\link{ColorOut}, \link{noColorOut}, \link{setOutputColors},
\link{setOutputColors256}, \link{show256Colors}. The package \pkg{setwidth}
updates the value of \code{options("width")} automatically when the terminal
is resized.
}
\author{
Jakson Alves de Aquino <jalvesaq@gmail.com>
}
\keyword{ package }
