\name{assert_is_R}
\alias{assert_is_R}
\alias{is_R}
\title{Are you running R?}
\usage{
  assert_is_R()

  is_R()
}
\value{
  \code{is_R} wraps \code{is.R}, providing more information
  on failure.  \code{assert_is_R} returns nothing but
  throws an error if \code{is_R} returns \code{FALSE}.
}
\description{
  Checks to see you are running R.
}
\examples{
assert_is_R()
}
\seealso{
  \code{\link[base]{is.R}}.
}

