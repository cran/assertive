% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assertive-package.R
\docType{package}
\name{assertive}
\alias{assertive}
\alias{assertive-package}
\title{Readable check functions to ensure code integrity.}
\description{
\code{assertive} contains lots of \code{is_*} functions to check the
state of your variables, and \code{assert_*} functions to throw errors
if they aren't in the right form.
}
\details{
When the package loads, it creates a global option 
\code{"assertive.severity"} that determines what happens when an 
\code{assert_*} function's input fails the condition.  By default, an error 
is thrown but it is possible to generate warnings or messages instead (see 
the examples).
}
\examples{
is_numeric(1:10)
assert_all_are_positive(1:10)
dont_stop(assert_is_scalar(runif(10)))
}
\author{
Richard Cotton \email{richierocks@gmail.com}
}
