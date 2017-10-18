\name{ColorOut}
\alias{ColorOut}
\title{Colorize R output in terminal emulator}
\description{
  Colorize output of R running in a terminal emulator. The function is called
  automatically when the package is loaded.
}
\usage{
ColorOut()
}

\value{
  NULL.
}

\details{
  The library works within RStudio, but there is no support for Graphical User
  Interfaces, that does not support ANSI colors. If running in a terminal
  emulator, it must support ANSI escape codes. Please, read
  \link{colorout-package} for more details.

  Emacs/ESS users should read \link{colorout-package} to know how to enable
  and use the package.
}

\author{Jakson A. Aquino \email{jalvesaq@gmail.com}}

