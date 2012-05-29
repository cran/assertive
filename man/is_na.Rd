\name{assert_all_are_not_na}
\alias{assert_all_are_not_na}
\alias{assert_any_are_not_na}
\alias{is_na}
\alias{is_not_na}
\title{Is the input present/missing?}
\usage{
  assert_all_are_not_na(x)

  assert_any_are_not_na(x)

  is_na(x)

  is_not_na(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{is_na} wraps \code{is.na}, showing the names of the
  inputs in the answer.  \code{is_not_na} is the negation.
  The \code{assert_*} functions return nothing but throw an
  error if the corresponding \code{is_*} function returns
  \code{FALSE}.
}
\description{
  Checks to see if the input is (not) NA.
}
\examples{
assert_all_are_not_na(1:10)
assert_any_are_not_na(c(NA, 1))
}
\seealso{
  \code{\link[base]{is.na}}
}

