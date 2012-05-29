\name{assert_is_empty}
\alias{assert_is_empty}
\alias{assert_is_non_empty}
\alias{assert_is_scalar}
\alias{is_empty}
\alias{is_non_empty}
\alias{is_scalar}
\title{Is the input empty/scalar?}
\usage{
  assert_is_empty(x)

  assert_is_non_empty(x)

  assert_is_scalar(x)

  is_empty(x, .xname = get_name_in_parent(x))

  is_non_empty(x, .xname = get_name_in_parent(x))

  is_scalar(x, .xname = get_name_in_parent(x))
}
\arguments{
  \item{x}{Input to check.}

  \item{.xname}{Not intended to be used directly.}
}
\value{
  \code{is_empty} returns \code{TRUE} if the input has
  length zero.  \code{is_scalar} returns \code{TRUE} if the
  input has length one.  The \code{assert_*} functions
  return nothing but throw an error if the corresponding
  \code{is_*} function returns \code{FALSE}.
}
\description{
  Checks to see if the input has length zero/one.
}
\examples{
assert_is_empty(NULL)
assert_is_empty(numeric())
assert_is_non_empty(1:10)
assert_is_non_empty(NA)
assert_is_scalar(1)
assert_is_scalar("Multiple words in a single string are scalar.")
assert_is_scalar(NA)
}
\seealso{
  \code{\link{length}}.
}

