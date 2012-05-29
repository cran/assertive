\name{false}
\alias{false}
\title{FALSE, with a cause of failure.}
\usage{
  false(...)
}
\arguments{
  \item{...}{Passed to sprintf to create a cause of failure
  message.}
}
\value{
  \code{FALSE} with the attribute \code{cause}, as provided
  in the input.
}
\description{
  Always returns the value \code{FALSE}, with a cause
  attribute.
}
\seealso{
  \code{\link{cause}} and \code{\link{na}}.
}

