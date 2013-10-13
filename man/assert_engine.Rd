\name{assert_engine}
\alias{assert_engine}
\title{Throws an error if a condition isn't met.}
\usage{
  assert_engine(x, predicate, msg, what = c("all", "any"),
    ...)
}
\arguments{
  \item{x}{Input to check.  If missing, pass no args to
  \code{predicate}.}

  \item{predicate}{Function that returns a logical value
  (possibly a vector).}

  \item{msg}{The error message, in the event of failure.}

  \item{what}{Either 'all' or 'any', to reduce vectorised
  tests to a single value.}

  \item{...}{Passed to the \code{predicate} function.}
}
\value{
  \code{FALSE} with the attribute \code{message}, as
  provided in the input.
}
\description{
  The workhorse of the package.  If a condition isn't met,
  then an error is thrown.
}

