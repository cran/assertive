\name{assert_has_duplicates}
\alias{assert_has_duplicates}
\alias{assert_has_no_duplicates}
\alias{has_duplicates}
\alias{has_no_duplicates}
\title{Does the input have duplicates?}
\usage{
  assert_has_duplicates(x)

  assert_has_no_duplicates(x)

  has_duplicates(x)

  has_no_duplicates(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{has_duplicates} returns \code{TRUE}
  if\code{anyDuplicated} is \code{TRUE}.
  \code{assert_has_duplicates} returns nothing but throws
  an error if \code{has_duplicates} is not \code{TRUE}.
  \code{has_no_duplicates} is the negation of
  \code{has_duplicates}.
}
\description{
  Checks to see if the input has duplicates.
}
\seealso{
  \code{\link{anyDuplicated}}.
}

