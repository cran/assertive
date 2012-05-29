\name{assert_all_are_in_closed_range}
\alias{assert_all_are_in_closed_range}
\alias{assert_all_are_in_left_open_range}
\alias{assert_all_are_in_open_range}
\alias{assert_all_are_in_range}
\alias{assert_all_are_in_right_open_range}
\alias{assert_all_are_negative}
\alias{assert_all_are_non_negative}
\alias{assert_all_are_non_positive}
\alias{assert_all_are_percentages}
\alias{assert_all_are_positive}
\alias{assert_all_are_proportions}
\alias{assert_any_are_in_closed_range}
\alias{assert_any_are_in_left_open_range}
\alias{assert_any_are_in_open_range}
\alias{assert_any_are_in_range}
\alias{assert_any_are_in_right_open_range}
\alias{assert_any_are_negative}
\alias{assert_any_are_non_negative}
\alias{assert_any_are_non_positive}
\alias{assert_any_are_percentages}
\alias{assert_any_are_positive}
\alias{assert_any_are_proportions}
\alias{is_in_closed_range}
\alias{is_in_left_open_range}
\alias{is_in_open_range}
\alias{is_in_range}
\alias{is_in_right_open_range}
\alias{is_negative}
\alias{is_non_negative}
\alias{is_non_positive}
\alias{is_percentage}
\alias{is_positive}
\alias{is_proportion}
\title{Is the input in range?}
\usage{
  assert_all_are_in_closed_range(x, lower = -Inf,
    upper = Inf)

  assert_any_are_in_closed_range(x, lower = -Inf,
    upper = Inf)

  assert_all_are_in_left_open_range(x, lower = -Inf,
    upper = Inf)

  assert_any_are_in_left_open_range(x, lower = -Inf,
    upper = Inf)

  assert_all_are_in_open_range(x, lower = -Inf,
    upper = Inf)

  assert_any_are_in_open_range(x, lower = -Inf,
    upper = Inf)

  assert_all_are_in_range(x, lower = -Inf, upper = Inf,
    lower_is_strict = FALSE, upper_is_strict = FALSE)

  assert_any_are_in_range(x, lower = -Inf, upper = Inf,
    lower_is_strict = FALSE, upper_is_strict = FALSE)

  assert_all_are_in_right_open_range(x, lower = -Inf,
    upper = Inf)

  assert_any_are_in_right_open_range(x, lower = -Inf,
    upper = Inf)

  assert_all_are_negative(x)

  assert_any_are_negative(x)

  assert_all_are_non_negative(x)

  assert_any_are_non_negative(x)

  assert_all_are_non_positive(x)

  assert_any_are_non_positive(x)

  assert_all_are_percentages(x, lower_is_strict = FALSE,
    upper_is_strict = FALSE)

  assert_any_are_percentages(x, lower_is_strict = FALSE,
    upper_is_strict = FALSE)

  assert_all_are_positive(x)

  assert_any_are_positive(x)

  assert_all_are_proportions(x, lower_is_strict = FALSE,
    upper_is_strict = FALSE)

  assert_any_are_proportions(x, lower_is_strict = FALSE,
    upper_is_strict = FALSE)

  is_in_closed_range(x, lower = -Inf, upper = Inf)

  is_in_left_open_range(x, lower = -Inf, upper = Inf)

  is_in_open_range(x, lower = -Inf, upper = Inf)

  is_in_range(x, lower = -Inf, upper = Inf,
    lower_is_strict = FALSE, upper_is_strict = FALSE)

  is_in_right_open_range(x, lower = -Inf, upper = Inf)

  is_negative(x)

  is_non_negative(x)

  is_non_positive(x)

  is_percentage(x, lower_is_strict = FALSE,
    upper_is_strict = FALSE)

  is_positive(x)

  is_proportion(x, lower_is_strict = FALSE,
    upper_is_strict = FALSE)
}
\arguments{
  \item{x}{Input to check.}

  \item{lower}{Lower bound for the interval.}

  \item{upper}{Upper bound for the interval.}

  \item{lower_is_strict}{If \code{TRUE}, the lower bound is
  open (strict) otherwise it is closed.}

  \item{upper_is_strict}{If \code{TRUE}, the upper bound is
  open (strict) otherwise it is closed.}
}
\value{
  The \code{is_*} function return \code{TRUE} if the input
  is within an interval.  The \code{assert_*} functions
  return nothing but throw an error if the corresponding
  \code{is_*} function returns \code{FALSE}.
}
\description{
  Checks to see if the input is within an numeric interval.
}
\note{
  \code{is_in_range} provides the most flexibility in
  determining if values are within a numeric interval.  The
  other functions restrict the input arguments for
  convience in common cases.  For example,
  \code{is_percentage} forces the interval to be from 0 to
  100.
}
\examples{
assert_all_are_positive(1:10)
assert_all_are_non_negative(0:10)
assert_any_are_positive(c(-1, 1))
assert_all_are_percentages(c(0, 50, 100))
assert_all_are_proportions(c(0, 0.5, 1))
assert_all_are_in_left_open_range(1 + .Machine$double.eps, lower = 1)
}

