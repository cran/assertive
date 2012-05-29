\name{has_arg}
\alias{has_arg}
\title{Does the current call have an argument?}
\usage{
  has_arg(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{has_arg} wraps \code{hasArg}, providing more
  information on failure.
}
\description{
  Checks to see if the current call has an argument with
  the name given in the input.
}
\note{
  There is currently no corresponding \code{assert_has_arg}
  function, because evaluating in the correct call is hard.
}
\examples{
assert_has_rows(data.frame(x = 1:10))
assert_has_cols(matrix())
}
\seealso{
  \code{\link{ncol}}.
}

