\name{assert_all_are_false}
\alias{assert_all_are_false}
\alias{assert_all_are_true}
\alias{assert_any_are_false}
\alias{assert_any_are_true}
\alias{assert_is_identical_to_false}
\alias{assert_is_identical_to_true}
\alias{is_false}
\alias{is_identical_to_false}
\alias{is_identical_to_true}
\alias{is_true}
\title{Is the input TRUE?}
\usage{
  assert_all_are_false(x)

  assert_any_are_false(x)

  assert_is_identical_to_false(x, allow_attributes = FALSE)

  assert_is_identical_to_true(x, allow_attributes = FALSE)

  assert_all_are_true(x)

  assert_any_are_true(x)

  is_false(x)

  is_identical_to_false(x, allow_attributes = FALSE,
    .xname = get_name_in_parent(x))

  is_identical_to_true(x, allow_attributes = FALSE,
    .xname = get_name_in_parent(x))

  is_true(x)
}
\arguments{
  \item{x}{Input to check.}

  \item{allow_attributes}{If \code{TRUE}, a scalar value of
  \code{TRUE} with attributes is allowed.}

  \item{.xname}{Not intended to be used directly.}
}
\value{
  The \code{is*} functions return \code{TRUE} if the input
  is \code{TRUE}/ \code{FALSE}. The \code{assert_*}
  functions return nothing but throw an error if the
  corresponding \code{is_*} function returns \code{FALSE}.
}
\description{
  Checks to see if the input is \code{TRUE}.
}
\note{
  \code{is_identical_to_true} wraps the base function
  \code{isTRUE}, providing more information on failure.
  Likewise, \code{is_identical_to_false} checks that the
  input is identical to FALSE.  If \code{allow_attributes}
  is \code{TRUE}, a scalar value of \code{TRUE} with
  attributes is allowed. \code{is_true} and \code{is_false}
  are vectorised, returning \code{TRUE} when the inputs are
  \code{TRUE} and \code{FALSE} respectively. Note that in
  version 0.1-4 and prior,
  \code{is_identical_to_true/false} was named
  \code{is_true/false} and the vectorised versions were not
  present.
}
\examples{
assert_is_identical_to_true(TRUE)
assert_is_identical_to_false(FALSE)
assert_is_identical_to_true(c(truth = TRUE), allow_attributes = TRUE)
assert_is_identical_to_false(matrix(FALSE), allow_attributes = TRUE)
x <- c(TRUE, FALSE, NA)
is_true(x)
is_false(x)
assert_any_are_true(x)
assert_any_are_false(x)
\dontrun{
#These tests should fail:
assert_is_true(c(truth = TRUE))
assert_is_false(matrix(FALSE))
assert_all_are_true(x)
assert_all_are_false(x)
}
}
\seealso{
  \code{\link[base]{isTRUE}}.
}

