\name{assert_all_numbers_whole_numbers}
\alias{assert_all_numbers_whole_numbers}
\alias{assert_any_numbers_whole_numbers}
\alias{is_whole_number}
\title{Is the input a whole number?}
\usage{
  assert_all_numbers_whole_numbers(x,
    tol = .Machine$double.eps)

  assert_any_numbers_whole_numbers(x,
    tol = .Machine$double.eps)

  is_whole_number(x, tol = 100 * .Machine$double.eps)
}
\arguments{
  \item{x}{Input to check.}

  \item{tol}{Differences smaller than \code{tol} are not
  considered.}
}
\value{
  \code{TRUE} if the input is a whole number.
}
\description{
  Checks that the (probably floating point) input is a
  whole number.
}
\note{
  The term whole number is used to distinguish from integer
  in that the input \code{x} need not have type
  \code{integer}.  In fact it is expected that \code{x}
  will be \code{numeric}.
}

