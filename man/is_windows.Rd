\name{assert_is_64_bit_os}
\alias{assert_is_64_bit_os}
\alias{assert_is_linux}
\alias{assert_is_mac}
\alias{assert_is_unix}
\alias{assert_is_windows}
\alias{is_64_bit_os}
\alias{is_linux}
\alias{is_mac}
\alias{is_unix}
\alias{is_windows}
\title{What OS is running?}
\usage{
  assert_is_64_bit_os()

  assert_is_linux()

  assert_is_mac()

  assert_is_unix()

  assert_is_windows()

  is_64_bit_os()

  is_linux()

  is_mac()

  is_unix()

  is_windows()
}
\value{
  \code{is_windows} returns \code{TRUE} if the OS on the
  current platform is Microsoft windows-based.
  \code{is_unix} returns \code{TRUE} if the OS is Unix
  based (pretty much anything that isn't Windows, including
  OS X). \code{is_mac} and \code{is_linux} return
  \code{TRUE} if the OS is Linux or Apple OS X
  respectively. \code{is_64_bit_os} returns \code{TRUE}
  when the operating system is 64-bit. The \code{assert_*}
  functions return nothing but throw an error if the
  corresponding \code{is_*} functions return \code{FALSE}.
}
\description{
  Is the operating system in this machine Windows/Unix/Mac
  based.
}
\examples{
is_windows()
is_unix()
is_mac()
is_linux()
}
\seealso{
  \code{\link[base]{.Platform}},
  \code{\link[base]{Sys.info}} and
  \code{\link[base]{version}}.
}

