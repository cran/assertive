\name{assert_all_are_existing}
\alias{assert_all_are_existing}
\alias{assert_any_are_existing}
\alias{is_existing}
\title{Does the variable exist?}
\usage{
  assert_all_are_existing(x, where = -1,
    envir = if (missing(frame)) as.environment(where) else sys.frame(frame),
    frame, mode = "any", inherits = TRUE)

  assert_any_are_existing(x, where = -1,
    envir = if (missing(frame)) as.environment(where) else sys.frame(frame),
    frame, mode = "any", inherits = TRUE)

  is_existing(x, where = -1,
    envir = if (missing(frame)) as.environment(where) else sys.frame(frame),
    frame, mode = "any", inherits = TRUE,
    .xname = get_name_in_parent(x))
}
\arguments{
  \item{x}{Input to check.}

  \item{where}{Passed to \code{exists}.}

  \item{envir}{Passed to \code{exists}.}

  \item{frame}{Passed to \code{exists}.}

  \item{mode}{Passed to \code{exists}.}

  \item{inherits}{Passed to \code{exists}.}

  \item{.xname}{Not intended to be used directly.}
}
\value{
  \code{is_existing} is a vectorised wrapper to
  \code{exists}, providing more information on failure.
  The \code{assert_*} functions return nothing but throw an
  error if \code{is_existing} returns \code{FALSE}.
}
\description{
  Checks to see if the input variables exist.
}
\examples{
e <- new.env()
e$x <- 1
e$y <- 2
assert_all_are_existing(c("x", "y"), envir = e)
\dontrun{
#These examples should fail.
assert_all_are_existing(c("x", "z"), envir = e)
}
}
\seealso{
  \code{\link[base]{exists}}.
}

