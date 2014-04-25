\name{show256Colors}
\alias{show256Colors}
\title{Create and show a table with 256 colors}
\description{
  Create and show a table with 256 colors and their ANSI escape codes.
  The table should be displayed in your browser.
  You have to hover the mouse over the colors to see their codes.
}
\usage{
show256Colors(outfile = "/tmp/table256.html")
}

\arguments{
  \item{outfile}{String. Path to file where the table will be saved.}
}

\value{
  NULL.
}

\author{Jakson A. Aquino \email{jalvesaq@gmail.com}, based on code from Todd
  Larason (\samp{256colors.pl}) and Ben Fritz (\samp{2html.vim}).}
