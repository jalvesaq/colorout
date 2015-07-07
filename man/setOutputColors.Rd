\name{setOutputColors}
\alias{setOutputColors}
\title{Set the colors to be used on R output}
\description{
  Set the output colors to be used if R is running in a terminal emulator.
}
\usage{
setOutputColors(normal = 2, negnum = 3, zero = 3, number = 3,
                date = 3, string = 6, const = 5, false = 5,
                true = 2, infinite = 5, stderror = 4,
                warn = c(1, 0, 1), error = c(1, 7),
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
setOutputColors(normal = 4, negnum = 3,  zero = 0, number = 4,
                date = 2, string = 2, const = 2, false = 1,
                true = 5, infinite = 4, stderror = c(1, 0, 5),
                zero.limit = 0.01) 
}
