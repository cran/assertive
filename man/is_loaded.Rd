\name{assert_is_loaded}
\alias{assert_is_loaded}
\alias{is_loaded}
\title{Is the input DLL loaded?}
\usage{
  assert_is_loaded(x)

  is_loaded(x, PACKAGE = "", type = "",
    .xname = get_name_in_parent(x))
}
\arguments{
  \item{x}{Input to check.}

  \item{PACKAGE}{Passed to \code{is.loaded}.}

  \item{type}{Passed to \code{is.loaded}.}

  \item{.xname}{Not intended to be used directly.}
}
\value{
  \code{is_loaded} wraps \code{is.loaded}, providing more
  information on failure.  \code{assert_is_loaded} returns
  nothing but throws an error if \code{is_loaded} returns
  \code{FALSE}.
}
\description{
  Checks to see if the input DLL (a.k.a. shared object) is
  loaded.
}
\seealso{
  \code{\link[base]{is.loaded}}.
}

