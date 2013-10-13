\name{assert_all_are_hex_colours}
\alias{assert_all_are_hex_colours}
\alias{assert_any_are_hex_colours}
\alias{is_hex_colour}
\title{Does the character vector contain hex colours?}
\usage{
  assert_all_are_hex_colours(x)

  assert_any_are_hex_colours(x)

  is_hex_colour(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  A logical vector that is \code{TRUE} when the input
  contains hex colours.
}
\description{
  Checks that the input contains hexadecimal colours.
}
\note{
  A string is considered to represent a hexadecimal colour
  when contains a hash followed by six hex values.  That
  is, digits or the letters from a to f (case insensitive).
}
\examples{
x <- c(
  "#0123456", "#789abc", "#defDEF", #ok
  "012345",                         #no hash
  "#g12345",                        #bad letter
  "#01 23 45",                      #contains spaces
  "#12345", "#1234567"              #wrong length
)
is_hex_colour(x)
assert_any_are_hex_colours(x)
\dontrun{
#These examples should fail.
assert_all_are_hex_colours(x)
}
}

