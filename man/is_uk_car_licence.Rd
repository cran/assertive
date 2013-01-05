\name{assert_all_are_uk_car_licences}
\alias{assert_all_are_uk_car_licences}
\alias{assert_any_are_uk_car_licences}
\alias{is_uk_car_licence}
\title{Is the string a valid UK car licence plate number?}
\usage{
  assert_all_are_uk_car_licences(x)

  assert_any_are_uk_car_licences(x)

  is_uk_car_licence(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{is_uk_national_insurance_number} returns
  \code{TRUE} if the input string contains a valid UK car
  licence plate number The {assert_*} function returns
  nothing but throw an error when the \code{is_*} function
  returns \code{FALSE}.
}
\description{
  Checks that the input contains UK car licence plate
  numbers.
}
\note{
  A single space, in the appropriate place, is allowed but
  not compulsory.
}
\examples{
licences <- c(
  #1903 to 1931
  "A 1", "AA 9999",                   #ok
  "A 01",                             #zero prefix on number
  "S0", "G0", "RG0", "LM0",           #ok, special plates
  #1931 to 1963
  "AAA 1", "AAA 999",                 #ok
  "III 1", "QQQ 1", "ZZZ 1",          #disallowed letters
  "AAA 01",                           #zero prefix on number
  #1931 to 1963 alt
  "1 AAA", "9999 AAA",                #ok
  "1 III", "1 QQQ", "1 ZZZ",          #disallowed letters
  "01 AAA",                           #zero prefix on number
  #1963 to 1982
  "AAA 1A", "AAA 999A",               #ok
  "AAA 1I", "AAA 1O", "AAA 1Q",       #disallowed letters
  "AAA 1U", "AAA 1Z",
  "AAA 01A",                          #zero prefix on number
  #1982 to 2001
  "A1 AAA", "A999 AAA",               #ok
  "I1 AAA", "O1 AAA",                 #disallowed letters
  "U1 AAA", "Z1 AAA",
  "A01 AAA",                          #zero prefix on number
  #2001 to 2051
  "AA00 AAA", "AA99 AAA",             #ok
  "II00 AAA", "QQ00 AAA", "ZZ00 AAA", #disallowed letters
  "AA00 III", "AA00 QQQ"
)
is_uk_car_licence(licences)
assert_any_are_uk_car_licences(licences)
\dontrun{
#These examples should fail.
assert_all_are_uk_car_licences(licences)
}
}
\references{
  Regex taken from
  \url{http://www.regexlib.com/REDetails.aspx?regexp_id=527}.
}

