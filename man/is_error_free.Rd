\name{is_error_free}
\alias{is_error_free}
\title{Does the code run without throwing an error?}
\usage{
  is_error_free(x)
}
\arguments{
  \item{x}{Code to check.}
}
\value{
  \code{TRUE} if the code runs without throwing an error.
}
\description{
  Call the code inside a try block and report if an error
  was thrown.
}
\note{
  Note that this has the side effect of running the code
  contained in \code{x}.
}

