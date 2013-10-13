\name{assert_all_are_existing_files}
\alias{assert_all_are_existing_files}
\alias{assert_any_are_existing_files}
\alias{is_existing_file}
\title{Does the file exist?}
\usage{
  assert_all_are_existing_files(x)

  assert_any_are_existing_files(x)

  is_existing_file(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{is_existing_file} wraps \code{file.exists}, showing
  the names of the inputs in the answer.
  \code{assert_is_existing_file} returns nothing but throws
  an error if \code{is_existing_file} returns \code{FALSE}.
}
\description{
  Checks to see if the input files exist.
}
\examples{
assert_all_are_existing_files(dir())
\dontrun{
#These examples should fail.
assert_all_are_existing_files("not an existing file (probably)")
}
}
\seealso{
  \code{\link[base]{file.exists}}.
}

