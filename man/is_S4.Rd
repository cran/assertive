\name{assert_is_S4}
\alias{assert_is_S4}
\alias{is_S4}
\title{Is the input an S4 object?}
\usage{
  assert_is_S4(x)

  is_S4(x, .xname = get_name_in_parent(x))
}
\arguments{
  \item{x}{Input to check.}

  \item{.xname}{Not intended to be used directly.}
}
\value{
  \code{is_S4} wraps \code{isS4}, providing more
  information on failure.  \code{assert_is_S4} returns
  nothing but throws an error if \code{is_S4} returns
  \code{FALSE}.
}
\description{
  Checks to see if the input is an S4 object.
}
\examples{
assert_is_S4(getClass("MethodDefinition"))
\dontrun{
assert_is_S4(1:10)
}
}
\seealso{
  \code{\link[base]{isS4}}.
}

