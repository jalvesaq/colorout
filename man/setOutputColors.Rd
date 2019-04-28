\name{setOutputColors}
\alias{setOutputColors}
\title{Set the Colors to Be Used on R Output}
\description{
  Set the output colors to be used if R is running in a terminal emulator. The
  terminal must be capable of displaying 256 colors.
  See a screenshot at \link{colorout-package}.
}
\usage{
setOutputColors(normal = 40, negnum = 209, zero = 226,
                number = 214, date = 179, string = 85,
                const = 35, false = 203, true = 78,
                infinite = 39, index = 30, stderror = 213,
                warn = c(1, 16, 196), error = c(160, 231),
                verbose = TRUE, zero.limit = NA)
}
\arguments{
  \item{normal}{Formating and color of normal text.}
  \item{negnum}{Formating and color of negative numbers.}
  \item{zero}{Formating and color of values being close to 0.}
  \item{number}{Formating and color of positive numbers.}
  \item{date}{Formating and color of dates and hours.
    For dates, the output format should be \samp{YYYYxMMxDD} or \samp{DDxMMxYYYY},
    where the separator x may equal '-' or '/'.
    For hours, the output format should be \samp{HH:MM:SS}.}
  \item{string}{Formating and color of quoted text.}
  \item{const}{Formating and color of \code{NULL},
    \code{NA} and \code{NaN}.}
  \item{false}{Formating and color of \code{FALSE}.}
  \item{true}{Formating and color of \code{TRUE}.}
  \item{infinite}{Formating and color of  \code{Inf} and \code{-Inf}.}
  \item{index}{Formating and color of vector indexes.}
  \item{stderror}{Formating and color of text sent to stderr.}
  \item{warn}{Formating and color of warnings.}
  \item{error}{Formating and color of errors.}
  \item{verbose}{Logical value indicating whether to print colored words
    showing the result of the setup.}
  \item{zero.limit}{The limit below which a value is considered close to 0. If set,
    the colorizing of zero and values near zero is enabled. You can also use
    the function \code{\link{setZero}} to set this value at any time and
    \code{\link{unsetZero}} to disable the colorizing of values near zero.}
}
\details{
  The function takes numeric vectors of at most three elements each. The
  three numbers indicate, respectively, formating, background color and
  foreground color. If a vector has only one element, the number will be used
  to set the foreground color; if it has two elements, they will be used to
  set the background and foreground colors.

  If you pass a string as a color, it will be used as is. Thus, you should
  pass valid strings (please, see the true color example below). The string
  must have at most 63 characters.

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

  If the \samp{TERM} environment variable is \code{"fbterm"}, fbterm escape
  codes will be used instead of the ANSI ones and the format values will be
  ignored.
}
\value{
  NULL.
}

\author{Jakson A. Aquino \email{jalvesaq@gmail.com}}

\examples{
# Color scheme for terminal emulator with only 8 colors:
setOutputColors(normal = 2, negnum = 3, zero = 3, number = 3,
                date = 3, string = 6, const = 5, false = 5,
                true = 2, infinite = 5, index = 2, stderror = 4,
                warn = c(1, 0, 1), error = c(1, 7),
                verbose = TRUE, zero.limit = NA)

# Color scheme for white background:
setOutputColors(normal = c(0, 0, 18), number = c(0, 0, 90),
                negnum = c(0, 0, 88), date = c(0, 0, 53),
                string = c(0, 0, 22), const = c(0, 0, 94), stderror = 52)

# Color schemes for black background:
setOutputColors(normal = 39, negnum = 183, zero = 114, number = 51,
                date = 43, string = 79, const = 75, zero.limit = 0.1)
setOutputColors(normal = 202, number = 214, zero = 220, negnum = 209,
                date = 184, string = 172, const = 179, zero.limit = 0.01)
setOutputColors(normal = 202, number = 214, zero = NA, negnum = 209,
                date = 184, string = 172, const = 179, zero.limit = 0.01)

# True color example:
# The color code starts with "\x1b[" and ends with "m"
# 38;2 means "foreground"
# 48;2 means "background"
# valid values for red, green, and blue are integers between 0 and 255
setOutputColors(normal   = "\x1b[38;2;0;200;0m",     # red = 0; green = 200; blue = 0
                negnum   = "\x1b[38;2;255;200;0m",
                zero     = "\x1b[38;2;255;255;0m",
                number   = "\x1b[38;2;200;255;75m",
                date     = "\x1b[38;2;155;155;255m",
                string   = "\x1b[38;2;0;255;175m",
                const    = "\x1b[38;2;0;255;255m",
                false    = "\x1b[38;2;255;125;125m",
                true     = "\x1b[38;2;125;255;125m",
                infinite = "\x1b[38;2;75;75;255m",
                index    = "\x1b[38;2;00;150;80m",
                stderror = "\x1b[38;2;255;0;255m",
                warn     = "\x1b[38;2;255;0;0m",
                error    = "\x1b[38;2;255;255;255;48;2;255;0;0m",
                zero.limit = 0.01)
}
