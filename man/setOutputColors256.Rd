\name{setOutputColors256}
\alias{setOutputColors256}
\title{Set the colors to be used on R output}
\description{
  Set the output colors to be used if R is running in a terminal emulator. The
  terminal must be capable of displaying 256 colors.
}
\usage{
setOutputColors256(normal = 40, number = 214, negnum = 209, date = 179,
                   string = 85, const = 35, stderror = 33, warn = c(1, 0, 1),
                   error = c(1, 15), verbose = TRUE)
}
\arguments{
  \item{normal}{Formating and color of normal text.}
  \item{number}{Formating and color of numbers.}
  \item{negnum}{Formating and color of negative numbers.}
  \item{date}{Formating and color of dates (output in the format
    \samp{yyyy-mm-dd}).}
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

  The table below shows valid formating values (some formating codes do not
  work properly in some terminal emulators):
 
  \tabular{lll}{
    Value \tab Formating         \cr
    0     \tab No formating      \cr
    1     \tab Bold or bright    \cr
    2     \tab Faint             \cr
    3     \tab Italic or inverse \cr
    4     \tab Underline         \cr
    5     \tab Blink slowly      \cr
    6     \tab Blink quickly     \cr
    7     \tab Invert            \cr
  }

  The values of ANSI escape codes for 256 colors are different than the ones
  used in the function \link{setOutputColors}. Run \link{show256Colors} to
  to get a table of the 256 colors and their codes.

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
setOutputColors256(normal = c(0, 0, 18), number = c(0, 0, 90),
                   negnum = c(0, 0, 88), date = c(0, 0, 53),
                   string = c(0, 0, 22), const = c(0, 0, 94), stderror = 52)

# Color schemes for black background:
setOutputColors256(normal = 39, number = 51, negnum = 183, date = 43,
                   string = 79, const = 75)
setOutputColors256(202, 214, 209, 184, 79, 179)
}
