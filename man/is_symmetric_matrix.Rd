\name{assert_is_symmetric_matrix}
\alias{assert_is_symmetric_matrix}
\alias{is_symmetric_matrix}
\title{Is the input a symmetric matrix?}
\usage{
  assert_is_symmetric_matrix(x,
    tol = 100 * .Machine$double.eps, ...)

  is_symmetric_matrix(x, tol = 100 * .Machine$double.eps,
    .xname = get_name_in_parent(x), ...)
}
\arguments{
  \item{x}{Input to check.}

  \item{tol}{Differences smaller than \code{tol} are not
  considered.}

  \item{.xname}{Not intended to be used directly.}

  \item{...}{Passed to \code{all.equal}.}
}
\value{
  \code{TRUE} if the input is symmetrix (after coersion to
  be a matrix).
}
\description{
  Checks that the input is a symmetric matrix.
}
\examples{
m <- diag(3); m[3, 1] <- 1e-100
assert_is_symmetric_matrix(m)
\dontrun{
#These examples should fail.
assert_is_symmetric_matrix(m, tol = 0)
}
}

