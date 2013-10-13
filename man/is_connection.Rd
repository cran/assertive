\name{assert_is_connection}
\alias{assert_is_connection}
\alias{assert_is_incomplete_connection}
\alias{assert_is_open_connection}
\alias{is_connection}
\alias{is_incomplete_connection}
\alias{is_open_connection}
\title{Is the input a connection?}
\usage{
  assert_is_connection(x)

  assert_is_open_connection(x, rw = "")

  assert_is_incomplete_connection(x)

  is_connection(x, .xname = get_name_in_parent(x))

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
  \code{is_connection} checks for objects of class
  "connection". \code{is_open_connection} and
  \code{is_incomplete_connection} wrap \code{isOpen} and
  \code{isIncomplete} respectively, providing more
  information on failure. The \code{assert_*} functions
  return nothing but throw an error if the corresponding
  \code{is_*} function returns \code{FALSE}.
}
\description{
  Checks to see if the input is a (open/incomplete)
  connection.
}
\note{
  \code{is_incomplete_connection} will return false for
  closed connections, regardless of whether or not the
  connection ends with a newline character.
  (\code{isIncomplete} throws an error for closed
  connections.)
}
\examples{
assert_is_connection(stdin())
tcon <- textConnection("txt", "w", local = TRUE)
assert_is_open_connection(tcon)
cat("this has no final newline character", file = tcon)
assert_is_incomplete_connection(tcon)
close(tcon)
\dontrun{
#These examples should fail.
assert_is_connection("not a connection")
fcon <- file()
close(fcon)
assert_is_open_connection(fcon)
}
}
\seealso{
  \code{\link[base]{isOpen}}.
}

