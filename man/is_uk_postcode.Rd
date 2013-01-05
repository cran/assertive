\name{assert_all_are_uk_postcodes}
\alias{assert_all_are_uk_postcodes}
\alias{assert_any_are_uk_postcodes}
\alias{is_uk_postcode}
\title{Is the string a valid UK postcode?}
\usage{
  assert_all_are_uk_postcodes(x)

  assert_any_are_uk_postcodes(x)

  is_uk_postcode(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{is_uk_postcode} returns \code{TRUE} if the input
  string contains a valid UK postcode. The {assert_*}
  function returns nothing but throws an error when the
  \code{is_*} function returns \code{FALSE}.
}
\description{
  Checks that the input contains UK postcodes.
}
\note{
  The function doesn't guarantee that the postcode actually
  exists.  It should correctly return \code{TRUE} for
  genuine postcodes, and will weed out most badly formatted
  strings and non-existent areas, but some non-existent
  districts may incorrectly return \code{TRUE}.  If you
  need 100% accuracy, check against an up-to- date postcode
  database.
}
\examples{
postcodes <- c("SW1A 1AA", "SK11 9DW", "M34FP", "Le45ns", "TS25 2BZ", "gir 0aa")
is_uk_postcode(postcodes)
assert_all_are_uk_postcodes(postcodes)
}
\references{
  Regexes taken from
  \url{https://en.wikipedia.org/wiki/Postcodes_in_the_United_Kingdom#Validation}.
}

