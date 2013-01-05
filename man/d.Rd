\name{d}
\alias{d}
\title{Create regex for repeated digits}
\usage{
  d(lo, hi, optional = FALSE)
}
\arguments{
  \item{lo}{Minimum number of digits to match.}

  \item{hi}{Optional maximum number of digits to match.}

  \item{optional}{If \code{TRUE}, the digits are optional.}
}
\value{
  A character vector of regexes.
}
\description{
  Creates a regex string for repeated digits.
}
\note{
  If \code{hi} is omitted, the returned regex will only
  match the exact number of digits given by \code{lo}.
}
\examples{
\dontrun{
d(3)
d(3, 4)
d(1:5, 6)
}
}

