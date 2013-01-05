\name{assert_all_are_ip_addresses}
\alias{assert_all_are_ip_addresses}
\alias{assert_any_are_ip_addresses}
\alias{is_ip_address}
\title{Does the character vector contain IP addresses?}
\usage{
  assert_all_are_ip_addresses(x)

  assert_any_are_ip_addresses(x)

  is_ip_address(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  A logical vector that is \code{TRUE} when the input
  contains valid IP addresses.
}
\description{
  Checks that the input contains IP addresses.  (It does
  not check that the address exists, merely that the string
  is in a suitable format.)
}
\note{
  Valid IP addresses are considered to be four integers in
  the range 0 to 255, separated by dots, or the string
  "localhost".
}
\examples{
x <- c(
  localhost     = "localhost",
  valid_address = "255.0.255.0",
  out_of_range  = "1.2.3.256",
  five_blocks   = "1.2.3.4.5",
  non_numeric   = "1.2.3.Z",
  missing_block = "1.2.3.NA"
)
is_ip_address(x)
assert_any_are_ip_addresses(x)
\dontrun{
#These examples should fail.
assert_all_are_ip_addresses(x)
}
}

