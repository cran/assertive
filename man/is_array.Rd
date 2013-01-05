\name{assert_is_array}
\alias{assert_is_array}
\alias{assert_is_matrix}
\alias{is_array}
\alias{is_matrix}
\title{Is the input an array or matrix?}
\usage{
  assert_is_array(x)

  assert_is_matrix(x)

  is_array(x, .xname = get_name_in_parent(x))

  is_matrix(x, .xname = get_name_in_parent(x))
}
\arguments{
  \item{x}{Input to check.}

  \item{.xname}{Not intended to be used directly.}
}
\value{
  \code{is_array} and \code{is_matrix} wrap
  \code{is.array}, and \code{is.matrix} respectively,
  providing more information on failure.  The
  \code{assert_*} functions return nothing but throw an
  error if the corresponding \code{is_*} function returns
  \code{FALSE}.
}
\description{
  Checks to see if the input is an array or matrix.
}
\examples{
assert_is_array(array())
assert_is_array(matrix())
assert_is_matrix(matrix())
\dontrun{
#These examples should fail.
assert_is_matrix(array())
}
}

