\name{assert_all_are_on_os_path}
\alias{assert_all_are_on_os_path}
\alias{assert_any_are_on_os_path}
\alias{is_on_os_path}
\title{Is the path on the OS path?}
\usage{
  assert_all_are_on_os_path(x)

  assert_any_are_on_os_path(x)

  is_on_os_path(x)
}
\arguments{
  \item{x}{An path to check.}
}
\value{
  \code{TRUE} if the sepcified paths are on the OS search
  path.
}
\description{
  Is the specified path on the operating system search
  path?
}
\note{
  The OS search path is determined with
  \code{Sys.getenv("path")}.
}
\examples{
is_on_os_path(c(R.home("bin"), "a nonexistent path")) #probably c(TRUE, FALSE)
}

