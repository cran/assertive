\name{assert_is_mts}
\alias{assert_is_mts}
\alias{assert_is_ts}
\alias{assert_is_tskernel}
\alias{is_mts}
\alias{is_ts}
\alias{is_tskernel}
\title{Is the input a time series?}
\usage{
  assert_is_mts(x)

  assert_is_ts(x)

  assert_is_tskernel(x)

  is_mts(x, .xname = get_name_in_parent(x))

  is_ts(x, .xname = get_name_in_parent(x))

  is_tskernel(x, .xname = get_name_in_parent(x))
}
\arguments{
  \item{x}{Input to check.}

  \item{.xname}{Not intended to be used directly.}
}
\value{
  \code{is_ts} wraps \code{is.ts}, providing more
  information on failure.  \code{assert_is_ts} returns
  nothing but throws an error if \code{is_ts} returns
  \code{FALSE}.
}
\description{
  Checks to see if the input is a time series.
}
\examples{
assert_is_ts(ts(1:10))
}
\seealso{
  \code{\link[stats]{is.ts}}.
}

