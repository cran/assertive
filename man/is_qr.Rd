\name{assert_is_qr}
\alias{assert_is_qr}
\alias{is_qr}
\title{Is the input a QR decomposition of a matrix?}
\usage{
  assert_is_qr(x)

  is_qr(x, .xname = get_name_in_parent(x))
}
\arguments{
  \item{x}{Input to check.}

  \item{.xname}{Not intended to be used directly.}
}
\value{
  \code{is_qr} wraps \code{is.qr}, providing more
  information on failure.  \code{assert_is_qr} returns
  nothing but throws an error if \code{is_qr} returns
  \code{FALSE}.
}
\description{
  Checks to see if the input is a QR decomposition of a
  matrix.
}
\examples{
assert_is_qr(qr(matrix(rnorm(25), nrow = 5)))
}
\seealso{
  \code{\link[base]{is.qr}}.
}

