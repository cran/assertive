\name{assert_is_r}
\alias{assert_is_r}
\alias{is_r}
\title{Are you running R?}
\usage{
  assert_is_r()

  is_r()
}
\value{
  \code{is_r} wraps \code{is.R}, providing more information
  on failure.  \code{assert_is_r} returns nothing but
  throws an error if \code{is_R} returns \code{FALSE}.
}
\description{
  Checks to see you are running R.
}
\examples{
\dontrun{
assert_is_r()
}
}
\seealso{
  \code{\link[base]{is.R}}.
}

