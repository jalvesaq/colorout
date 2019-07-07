\name{addPattern}
\alias{addPattern}
\title{Add custom pattern to be colorized}
\description{
  Enable colorizing of custom patterns.
}
\usage{
addPattern(pattern, color)
}

\arguments{
  \item{pattern}{String representing the pattern to be colorized. The patterns
    are literal strings. The only type of regular expression accepted is a
    range of ASCII characters surrounded by square brackets and separated by a
    hyphen (see section Examples below).}
  \item{color}{Formating and color of the pattern (see the Details section in
    \code{\link{setOutputColors}}).}
}

\value{
  NULL.
}


\author{Jakson A. Aquino \email{jalvesaq@gmail.com}}

\examples{
# Slash separated date (YYYY-MM-DD):
addPattern("[1-2][0-9][0-9][0-9]/[0-1][0-9]/[0-3][0-9]", 179)

# Date in Year-quarter format:
addPattern("[1-2][0-9][0-9][0-9] Q[0-4]", 179)

# Date in "Month Year" format (Not perfect; it will wrongly colorize strings
# such as"Not 2019"):
addPattern("[A-S][a-u][b-z] [1-2][0-9][0-9][0-9]", 179)
}
