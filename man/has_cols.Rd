\name{assert_has_cols}
\alias{assert_has_cols}
\alias{assert_has_rows}
\alias{has_cols}
\alias{has_rows}
\title{Does the input have rows/columns?}
\usage{
  assert_has_cols(x)

  assert_has_rows(x)

  has_cols(x)

  has_rows(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{has_rows} and \code{has_cols} return \code{TRUE} if
  \code{nrow} and \code{ncol} respectively return a value
  that is non-null and positive.  The \code{assert_*}
  functions return nothing but throw an error if the
  corresponding \code{has_*} function returns \code{FALSE}.
}
\description{
  Checks to see if the input has rows/columns.
}
\examples{
assert_has_rows(data.frame(x = 1:10))
assert_has_cols(matrix())
}
\seealso{
  \code{\link{ncol}}.
}

