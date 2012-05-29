\name{is_relistable}
\alias{is_relistable}
\title{Is the input relistable?}
\usage{
  is_relistable(x, .xname = get_name_in_parent(x))
}
\arguments{
  \item{x}{Input to check.}

  \item{.xname}{Not intended to be used directly.}
}
\value{
  \code{is_relistable} wraps \code{is.relistable},
  providing more information on failure. The
  \code{assert_*} functions return nothing but throws an
  error if the corresponding \code{is_*} function returns
  \code{FALSE}.
}
\description{
  Checks to see if the input is relistable.
}
\examples{
assert_is_relistable(as.relistable(list(1,2,3)))
\dontrun{
#These examples should fail:
assert_is_relistable(list(1,2,3))
}
}
\seealso{
  \code{\link[utils]{is.relistable}} and
  \code{\link{is_scalar}}.
}

