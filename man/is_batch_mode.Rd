\name{assert_is_batch_mode}
\alias{assert_is_batch_mode}
\alias{assert_is_interactive}
\alias{is_batch_mode}
\alias{is_interactive}
\title{How is R running?}
\usage{
  assert_is_batch_mode()

  assert_is_interactive()

  is_batch_mode()

  is_interactive()
}
\value{
  \code{is_batch_mode} returns \code{TRUE} if R is running
  in batch mode. \code{is_interactive} returns \code{TRUE}
  if R is running interactively.
}
\description{
  Tests to see if R is running in batch mode/interactively.
}
\seealso{
  \code{\link[base]{EnvVar}} and
  \code{\link[base]{interactive}}.
}

