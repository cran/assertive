\name{assert_all_are_valid_variable_names}
\alias{assert_all_are_valid_variable_names}
\alias{assert_any_are_valid_variable_names}
\alias{is_valid_variable_name}
\title{Is the string a valid variable name?}
\usage{
  assert_all_are_valid_variable_names(x,
    allow_reserved = TRUE, allow_duplicates = TRUE)

  assert_any_are_valid_variable_names(x,
    allow_reserved = TRUE, allow_duplicates = TRUE)

  is_valid_variable_name(x, allow_reserved = TRUE,
    allow_duplicates = TRUE)
}
\arguments{
  \item{x}{Input to check.}

  \item{allow_reserved}{If \code{TRUE} then "..." and
  "..1", "..2", etc. are considered valid.}

  \item{allow_duplicates}{If \code{TRUE} then duplicated
  names are allowed.}
}
\value{
  \code{TRUE} if the input is a valid variable name. The
  \code{assert_*} functions return nothing but throw an
  error if the corresponding \code{is_*} function returns
  \code{FALSE}.
}
\description{
  Checks strings to see if they are valid variable names.
}
\examples{
assert_all_are_valid_variable_names(c("x", "y_y0.y", ".", "...", "..1"))
\dontrun{
#These examples should fail.
assert_all_are_valid_variable_names(c("...", "..1"), allow_reserved = FALSE)
assert_all_are_valid_variable_names(c("x", "x"), allow_duplicates = FALSE)
}
}
\references{
  \url{http://4dpiecharts.com/2011/07/04/testing-for-valid-variable-names/}
}
\seealso{
  \code{\link{make.names}}.
}

