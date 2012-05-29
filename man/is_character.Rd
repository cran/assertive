\name{assert_is_a_non_empty_string}
\alias{assert_all_are_numeric_strings}
\alias{assert_all_strings_are_missing_or_empty}
\alias{assert_all_strings_are_not_missing_nor_empty}
\alias{assert_any_are_numeric_strings}
\alias{assert_any_strings_are_missing_or_empty}
\alias{assert_any_strings_are_not_missing_nor_empty}
\alias{assert_is_a_non_empty_string}
\alias{assert_is_a_string}
\alias{assert_is_an_empty_string}
\alias{assert_is_character}
\alias{is_a_non_empty_string}
\alias{is_a_string}
\alias{is_an_empty_string}
\alias{is_character}
\alias{is_missing_or_empty_character}
\alias{is_not_missing_nor_empty_character}
\alias{is_numeric_string}
\title{Is the input of type character?}
\usage{
  assert_is_a_non_empty_string(x)

  assert_is_a_string(x)

  assert_is_an_empty_string(x)

  assert_all_are_numeric_strings(x)

  assert_any_are_numeric_strings(x)

  assert_all_strings_are_missing_or_empty(x)

  assert_any_strings_are_missing_or_empty(x)

  assert_all_strings_are_not_missing_nor_empty(x)

  assert_any_strings_are_not_missing_nor_empty(x)

  assert_is_character(x)

  is_a_non_empty_string(x, .xname = get_name_in_parent(x))

  is_a_string(x, .xname = get_name_in_parent(x))

  is_an_empty_string(x, .xname = get_name_in_parent(x))

  is_missing_or_empty_character(x)

  is_not_missing_nor_empty_character(x)

  is_numeric_string(x)

  is_character(x, .xname = get_name_in_parent(x))
}
\arguments{
  \item{x}{Input to check.}

  \item{.xname}{Not intended to be used directly.}
}
\value{
  \code{is_character} wraps \code{is.character}, providing
  more information on failure. \code{is_a_string} returns
  \code{TRUE} if the input is character and scalar.
  \code{is_an_empty_string} returns \code{TRUE} if the
  input is \code{""}.  \code{is_numeric_string} is
  vectorised, returning \code{TRUE} when the inputs are not
  \code{NA} after conversion to character and then numeric.
  \code{is_missing_or_empty_string} is also vectorised,
  returning \code{TRUE} when the input is \code{""} or
  \code{NA}. The \code{assert_*} functions return nothing
  but throw an error if the corresponding \code{is_*}
  function returns \code{FALSE}.
}
\description{
  Checks to see if the input is of type character.
}
\examples{
assert_is_character(letters)
assert_is_a_string("foo bar baz")
assert_is_an_empty_string("")
assert_all_are_numeric_strings(c("1", "2.3", "-4.5", "6e7", "8E-9"))
assert_any_are_numeric_strings(c("1", "Not a number"))
assert_all_strings_are_missing_or_empty(c("", NA))
assert_any_strings_are_missing_or_empty(c("a", NA, "b"))
}
\seealso{
  \code{\link[base]{is.character}} and
  \code{\link{is_scalar}}.
}

