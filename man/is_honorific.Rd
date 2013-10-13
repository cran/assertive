\name{is_honorific}
\alias{is_honorific}
\title{Is the string an honorific?}
\usage{
  is_honorific(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{is_honorific} returns \code{TRUE} if the input
  string contains a valid UK postcode. The {assert_*}
  function returns nothing but throws an error when the
  \code{is_*} function returns \code{FALSE}.
}
\description{
  Checks that the input contains honorifics (a.k.a. titles
  or salutations).
}
\note{
  Single full stops (periods) following a word boundary and
  preceding a space or the end of the string are stripped.
  Case is ignored.  There is no formal list of official
  salutations, so this should only be used as a guide,
  rather than giving a definitive result.  Especially note
  that cultural conventions differ across the world and
  this function has a UK bias.
}
\examples{
x <- c("Mr", "MR", "mr.", "Mister", "masTer", "Mr!", "M.r", ".Mr")
is_honorific(x)
}

