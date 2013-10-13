\name{assert_is_leaf}
\alias{assert_is_leaf}
\alias{is_leaf}
\title{Is the input a (dendrogram) leaf?}
\usage{
  assert_is_leaf(x)

  is_leaf(x, .xname = get_name_in_parent(x))
}
\arguments{
  \item{x}{Input to check.}

  \item{.xname}{Not intended to be used directly.}
}
\value{
  \code{is_leaf} reimplements \code{is.leaf}, providing
  more information on failure.
}
\description{
  Checks to see if the input is a (dendrogram) leaf.
}
\seealso{
  \code{\link[stats]{dendrogram}}.
}

