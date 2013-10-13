\name{assert_is_a_number}
\alias{assert_is_a_number}
\alias{assert_is_numeric}
\alias{is_a_number}
\alias{is_numeric}
\title{Is the input numeric?}
\usage{
  assert_is_a_number(x)

  assert_is_numeric(x)

  is_a_number(x, .xname = get_name_in_parent(x))

  is_numeric(x, .xname = get_name_in_parent(x))
}
\arguments{
  \item{x}{Input to check.}

  \item{.xname}{Not intended to be used directly.}
}
\value{
  \code{is_numeric} wraps \code{is.numeric}, providing more
  information on failure. \code{is_a_number} returns
  \code{TRUE} if the input is numeric and scalar.  The
  \code{assert_*} functions return nothing but throw an
  error if the corresponding \code{is_*} function returns
  \code{FALSE}.
}
\description{
  Checks to see if the input is numeric.
}
\examples{
assert_is_numeric(1:10)
assert_is_a_number(pi)
assert_is_a_number(1L)
assert_is_a_number(NA_real_)
\dontrun{
#These examples should fail.
assert_is_numeric(c(TRUE, FALSE))
assert_is_a_number(1:10)
assert_is_a_number(numeric())
}
}
\seealso{
  \code{\link[base]{is.numeric}} and
  \code{\link{is_scalar}}.
}

