\name{setOutputColors}
\alias{setOutputColors}
\title{Set the colors to be used on R output}
\description{
  Set the output colors to be used if R is running in a terminal emulator.
}
\usage{
setOutputColors(normal = 2, number = 3, negnum = 3, date = 3, string = 6, const = 5,
                stderror = 4, warn = c(1, 0, 1), error = c(1, 7),
                verbose = TRUE)
}
\arguments{
  \item{normal}{Formating and color of normal text.}
  \item{number}{Formating and color of numbers.}
  \item{negnum}{Formating and color of negative numbers.}
  \item{date}{Formating and color of dates (output in the format
    \samp{yyyy-mm-dd}}
  \item{string}{Formating and color of quoted text.}
  \item{const}{Formating and color of \code{TRUE}, \code{FALSE}, \code{NULL},
    \code{NA}, \code{NaN} and \code{Inf}.}
  \item{stderror}{Formating and color of text sent to stderr.}
  \item{warn}{Formating and color of warnings.}
  \item{error}{Formating and color of errors.}
  \item{verbose}{Logical value indicating whether to print colored words
    showing the result of the setup.}
}
\details{
  The function takes numeric vectors of at most three elements each. The
  three numbers indicate, respectively, formating, background color and
  foreground color. If a vector has only one element, the number will be used
  to set the foreground color; if it has two elements, they will be used to
  set the background and foreground colors.

  The table below shows valid values and their meanings as both formating and
  color codes (some formating codes do not work properly in some terminal
  emulators):
 
  \tabular{lll}{
    Value \tab Formating         \tab Color            \cr
    0     \tab No formating      \tab Default or black \cr
    1     \tab Bold or bright    \tab Red              \cr
    2     \tab Faint             \tab Green            \cr
    3     \tab Italic or inverse \tab Yellow           \cr
    4     \tab Underline         \tab Blue             \cr
    5     \tab Blink slowly      \tab Magenta          \cr
    6     \tab Blink quickly     \tab Cyan             \cr
    7     \tab Invert            \tab White            \cr
  }

  Messages are colored as errors and warnings if they start with \code{"Error"}
  or \code{"Warning"} (or their translations, if not running in an English
  locale).
 
}
\value{
  NULL.
}

\author{Jakson A. Aquino \email{jalvesaq@gmail.com}}

\examples{
# Color scheme for white background:
setOutputColors(4, 5, 1, 5, c(1, 0, 5), 5, 1)
}
