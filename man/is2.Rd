\name{assert_is_all_of}
\alias{assert_is_all_of}
\alias{assert_is_any_of}
\alias{is2}
\title{Alternative version of is.}
\usage{
  assert_is_all_of(x, classes,
    .xname = get_name_in_parent(x))

  assert_is_any_of(x, classes,
    .xname = get_name_in_parent(x))

  is2(x, class, .xname = get_name_in_parent(x))
}
\arguments{
  \item{x}{Input to check.}

  \item{class}{Target class that \code{x} maybe belong to.}

  \item{classes}{As for \code{class}.}

  \item{.xname}{Not intended to be used directly.}
}
\value{
  \code{TRUE} if x belongs to the class and \code{FALSE}
  otherwise.  \code{assert_is} returns nothing but throws
  an error if \code{x} does not have class \code{class}.
}
\description{
  If a function named \code{is.class} exists, call
  \code{is.class(x)}. If not, call \code{is(x, class)}.
}
\examples{
assert_is_all_of(1:10, c("integer", "numeric"))
\dontrun{
#These examples should fail.
assert_is_any_of(1:10, c("list", "data.frame"))
}
}
\seealso{
  \code{\link[methods]{is}}.
}

