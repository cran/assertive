\name{assert_all_are_uk_telephone_numbers}
\alias{assert_all_are_uk_telephone_numbers}
\alias{assert_any_are_uk_telephone_numbers}
\alias{is_uk_telephone_number}
\title{Is the string a valid UK telephone number?}
\usage{
  assert_all_are_uk_telephone_numbers(x)

  assert_any_are_uk_telephone_numbers(x)

  is_uk_telephone_number(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{is_uk_telephone_number} returns \code{TRUE} if the
  input string contains a valid UK telephone number. The
  {assert_*} function returns nothing but throws an error
  when the \code{is_*} function returns \code{FALSE}.
}
\description{
  Checks that the input contains UK telephone numbers.
}
\note{
  The function doesn't guarantee that the phone number is
  in use, but checks that the format is correct, and that
  the area code exists. Spaces, hyphens and round brackets
  are allowed to appear in arbitrary places.  The
  international UK prefix of 0044 or +44 is allowed.
}
\examples{
phone_nos <- c("+44 207 219 3475", "08457 90 90 90")
is_uk_telephone_number(phone_nos)
assert_all_are_uk_telephone_numbers(phone_nos)
}
\references{
  The regex is adapted from the one at
  \url{http://www.aa-asterisk.org.uk/index.php/Regular_Expressions_for_Validating_and_Formatting_UK_Telephone_Numbers}
  with some additional consultation from
  \url{https://en.wikipedia.org/wiki/List_of_United_Kingdom_dialling_codes}
}

