\name{assert_all_are_dirs}
\alias{assert_all_are_dirs}
\alias{assert_any_are_dirs}
\alias{is_dir}
\title{Is the path a directory?
Checks to see if the input path is a directory.}
\usage{
  assert_all_are_dirs(x)

  assert_any_are_dirs(x)

  is_dir(x)
}
\arguments{
  \item{x}{File paths.}
}
\value{
  \code{is_dir} returns \code{TRUE} if and only if the
  input path is a directory that exists, as determined by
  \code{file.info}.
}
\description{
  Is the path a directory? Checks to see if the input path
  is a directory.
}
\examples{
assert_all_are_dirs(R.home())
}

