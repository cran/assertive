\name{coerce_to}
\alias{coerce_to}
\title{Coerce variable to a different class.}
\usage{
  coerce_to(x, target_class,
    .xname = get_name_in_parent(x))
}
\arguments{
  \item{x}{Input to coerce.}

  \item{target_class}{The desired class of x.}

  \item{.xname}{Not intended to be used directly.}
}
\value{
  The input \code{x} after attempted coersion to the target
  class.
}
\description{
  Coerce the input to a different class, with a warning.
}
\note{
  If x does not already have the target class, a warning is
  given before coersion.
}
\seealso{
  \code{\link[methods]{is}} and \code{\link[methods]{as}}.
}

