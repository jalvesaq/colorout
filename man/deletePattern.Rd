\name{deletePattern}
\alias{deletePattern}
\title{Delete custom pattern that was being colorized}
\description{
  Do not colorize previously defined string pattern.
}
\usage{
deletePattern(pattern)
}

\arguments{
  \item{pattern}{String representing the pattern to be colorized. The patterns
    are literal strings.}
}

\value{
  NULL.
}

\seealso{
    \code{\link{addPattern}}.
}

\author{Jakson A. Aquino \email{jalvesaq@gmail.com}}

\examples{
# Slash separated date (YYYY-MM-DD):
deletePattern("[1-2][0-9][0-9][0-9]/[0-1][0-9]/[0-3][0-9]")

# Delete all user defined patterns:
deletePattern(listPatterns())
}
