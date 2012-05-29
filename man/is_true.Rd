\name{assert_is_false}
\alias{assert_is_false}
\alias{assert_is_true}
\alias{is_false}
\alias{is_true}
\title{Is the input TRUE?}
\usage{
  assert_is_false(x, allow_attributes = FALSE)

  assert_is_true(x, allow_attributes = FALSE)

  is_false(x, allow_attributes = FALSE,
    .xname = get_name_in_parent(x))

  is_true(x, allow_attributes = FALSE,
    .xname = get_name_in_parent(x))
}
\arguments{
  \item{x}{Input to check.}

  \item{allow_attributes}{If \code{TRUE}, a scalar value of
  \code{TRUE} with attributes is allowed.}

  \item{.xname}{Not intended to be used directly.}
}
\value{
  \code{TRUE} if the input is identical to \code{TRUE}. The
  \code{assert_*} functions return nothing but throw an
  error if the corresponding \code{is_*} function returns
  \code{FALSE}.
}
\description{
  Checks to see if the input is \code{TRUE}.
}
\examples{
assert_is_true(TRUE)
assert_is_false(FALSE)
assert_is_true(c(truth = TRUE), allow_attributes = TRUE)
assert_is_false(assertive:::false("This has an attribute"), allow_attributes = TRUE)
\dontrun{
#These tests should fail:
assert_is_true(c(truth = TRUE))
assert_is_false(assertive:::false("This has an attribute"))
}
}
\seealso{
  \code{\link[base]{isTRUE}}.
}

