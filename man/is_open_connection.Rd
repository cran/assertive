\name{assert_is_open_connection}
\alias{assert_is_incomplete_connection}
\alias{assert_is_open_connection}
\alias{is_incomplete_connection}
\alias{is_open_connection}
\title{Is the input an open/incomplete connection?}
\usage{
  assert_is_open_connection(x, rw = "")

  assert_is_incomplete_connection(x)

  is_open_connection(x, rw = "",
    .xname = get_name_in_parent(x))

  is_incomplete_connection(x,
    .xname = get_name_in_parent(x))
}
\arguments{
  \item{x}{Input to check.}

  \item{rw}{Read-write status of connection.  Passed to
  \code{isOpen}.}

  \item{.xname}{Not intended to be used directly.}
}
\value{
  \code{is_open_connection} wraps \code{isOpen}, providing
  more information on failure.
  \code{assert_is_open_connection} returns nothing but
  throws an error if \code{is_open_connection} returns
  \code{FALSE}.
}
\description{
  Checks to see if the input is an open/incomplete
  connection.
}
\seealso{
  \code{\link[base]{isOpen}}.
}

