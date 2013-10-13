\name{assert_has_dims}
\alias{assert_has_dims}
\alias{has_dims}
\title{Does the input have dimensions?}
\usage{
  assert_has_dims(x)

  has_dims(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{has_dims} returns\code{TRUE} if \code{dim} is
  non-null. \code{assert_has_dims} returns nothing but
  throws an error if \code{has_dims} is not \code{TRUE}.
}
\description{
  Checks to see if the input has dimensions.
}
\seealso{
  \code{\link{dim}}.
}

