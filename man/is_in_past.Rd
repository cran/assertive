\name{assert_all_are_in_future}
\alias{assert_all_are_in_future}
\alias{assert_all_are_in_past}
\alias{assert_any_are_in_future}
\alias{assert_any_are_in_past}
\alias{is_in_future}
\alias{is_in_past}
\title{Is the input in the past/future?}
\usage{
  assert_all_are_in_future(x)

  assert_any_are_in_future(x)

  assert_all_are_in_past(x)

  assert_any_are_in_past(x)

  is_in_future(x)

  is_in_past(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  The \code{is_*} function return \code{TRUE} if the input
  is a time in the future/past.  The \code{assert_*}
  functions return nothing but throw an error if the
  corresponding \code{is_*} function returns \code{FALSE}.
}
\description{
  Checks to see if the input is a time in the past/future.
}
\details{
  The current time is determined by \code{Sys.time}, and
  the input is coerced to \code{POSIXct} format if
  necessary.
}
\examples{
assert_is_empty(NULL)
assert_is_empty(numeric())
assert_is_non_empty(1:10)
assert_is_non_empty(NA)
assert_is_scalar(1)
assert_is_scalar("Multiple words in a single string are scalar.")
assert_is_scalar(NA)
}
\seealso{
  \code{\link{Sys.time}}.
}

