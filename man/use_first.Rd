\name{use_first}
\alias{use_first}
\title{Only use the first element of a vector.}
\usage{
  use_first(x)
}
\arguments{
  \item{x}{Input that should be scalar.}
}
\value{
  If \code{x} is scalar, it is returned unchanged,
  otherwise only the first element is returned, with a
  warning.
}
\description{
  If the input is not scalar, then only the first element
  is returned, with a warning.
}

