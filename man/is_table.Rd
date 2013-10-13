\name{assert_is_table}
\alias{assert_is_table}
\alias{is_table}
\title{Is the input a table?}
\usage{
  assert_is_table(x)

  is_table(x, .xname = get_name_in_parent(x))
}
\arguments{
  \item{x}{Input to check.}

  \item{.xname}{Not intended to be used directly.}
}
\value{
  \code{is_table} wraps \code{is.table}, providing more
  information on failure.  \code{assert_is_table} returns
  nothing but throws an error if \code{is_table} returns
  \code{FALSE}.
}
\description{
  Checks to see if the input is a table.
}
\examples{
assert_is_table(table(sample(letters, 100, replace = TRUE)))
}
\seealso{
  \code{\link[base]{is.table}}.
}

