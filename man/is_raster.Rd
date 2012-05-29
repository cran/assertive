\name{assert_is_raster}
\alias{assert_is_raster}
\alias{is_raster}
\title{Is the input a raster?}
\usage{
  assert_is_raster(x)

  is_raster(x, .xname = get_name_in_parent(x))
}
\arguments{
  \item{x}{Input to check.}

  \item{.xname}{Not intended to be used directly.}
}
\value{
  \code{is_raster} wraps \code{is.raster}, providing more
  information on failure. \code{is_a_raster} returns
  \code{TRUE} if the input is raster and scalar.  The
  \code{assert_*} functions return nothing but throw an
  error if the corresponding \code{is_*} function returns
  \code{FALSE}.
}
\description{
  Checks to see if the input is a raster.
}
\examples{
m <- matrix(hcl(0, 80, seq(50, 80, 10)), nrow=4, ncol=5)
assert_is_raster(as.raster(m))
\dontrun{
#These examples should fail:
assert_is_raster(m)
}
}
\seealso{
  \code{\link[grDevices]{is.raster}}.
}

