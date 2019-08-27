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
    are literal strings. The only two types of regular expression accepted are
    (1) a range of ASCII characters surrounded by square brackets and
    separated by a hyphen and (2) either an ASCII character or a range of
    ASCII characters followed by an asterisk (see section Examples below).}
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

# If all columns in a data.frame start with the letter 'q' followed by a
# number end, in some cases, by other letters, colorize them:
addPattern("q[0-9]*", 123)
addPattern("q[0-9]*[a-z]*", 123)
}
