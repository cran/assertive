\name{assert_all_are_classes}
\alias{assert_all_are_classes}
\alias{assert_any_are_classes}
\alias{is_class}
\title{Is the input the name of a (formally defined) class?}
\usage{
  assert_all_are_classes(x)

  assert_any_are_classes(x)

  is_class(x, .xname = get_name_in_parent(x))
}
\arguments{
  \item{x}{Input to check.}

  \item{.xname}{Not intended to be used directly.}
}
\value{
  \code{is_class} is a vectorised wrapper for
  \code{isClass}. \code{assert_is_class} returns nothing
  but throws an error if \code{is_class} returns
  \code{FALSE}.
}
\description{
  Checks to see if the input is the name of a (formally
  defined) class.
}
\examples{
assert_all_are_classes(c("lm", "numeric"))
}
\seealso{
  \code{\link[methods]{isClass}}.
}

