\name{assert_is_list}
\alias{assert_is_list}
\alias{is_list}
\title{Is the input a list?}
\usage{
  assert_is_list(x)

  is_list(x, .xname = get_name_in_parent(x))
}
\arguments{
  \item{x}{Input to check.}

  \item{.xname}{Not intended to be used directly.}
}
\value{
  \code{is_list} wraps \code{is.list}, providing more
  information on failure.
}
\description{
  Checks to see if the input is a list.
}
\examples{
assert_is_list(list(1,2,3))
\dontrun{
#These examples should fail.
assert_is_list(1:10)
}
}
\seealso{
  \code{\link[base]{is.list}}.
}

