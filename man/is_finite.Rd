\name{assert_all_are_finite}
\alias{assert_all_are_finite}
\alias{assert_all_are_infinite}
\alias{assert_any_are_finite}
\alias{assert_any_are_infinite}
\alias{is_finite}
\alias{is_infinite}
\title{Are the inputs (in)finite?}
\usage{
  assert_all_are_finite(x)

  assert_any_are_finite(x)

  assert_all_are_infinite(x)

  assert_any_are_infinite(x)

  is_finite(x)

  is_infinite(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{is_finite} wraps \code{is.finite}, showing the
  names of the inputs in the answer. \code{is_infinite}
  works likewise for \code{is.infinite}.  The
  \code{assert_*} functions return nothing but throw an
  error if the corresponding \code{is_*} function returns
  \code{FALSE}.
}
\description{
  Checks to see if the inputs are (in)finite.
}
\examples{
assert_all_are_finite(1:10)
assert_any_are_finite(c(1, Inf))
}
\seealso{
  \code{\link[base]{is.finite}}
}

