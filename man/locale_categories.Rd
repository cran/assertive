\name{locale_categories}
\alias{locale_categories}
\title{Allowed locale categories.}
\usage{
  locale_categories(include_all = TRUE,
    include_unix = is_unix())
}
\arguments{
  \item{include_all}{If \code{TRUE}, the value
  \code{LC_ALL} is included.}

  \item{include_unix}{If \code{TRUE}, extra unix-specific
  categories are included.}
}
\value{
  A character vector of locale categories.
}
\description{
  The categories of locale that can be gotten/set.
}
\seealso{
  \code{\link{sys_get_locale}}.
}

