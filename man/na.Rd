\name{na}
\alias{na}
\title{NA, with a cause of failure.}
\usage{
  na(...)
}
\arguments{
  \item{...}{Passed to sprintf to create a cause of failure
  message.}
}
\value{
  \code{NA} with the attribute \code{cause}, as provided in
  the input.
}
\description{
  Always returns the value (logical) \code{NA}, with a
  cause attribute.
}
\seealso{
  \code{\link{cause}} and \code{\link{false}}.
}

