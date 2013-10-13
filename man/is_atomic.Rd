\name{assert_is_atomic}
\alias{assert_is_atomic}
\alias{assert_is_recursive}
\alias{assert_is_vector}
\alias{is_atomic}
\alias{is_recursive}
\alias{is_vector}
\title{Is the input atomic/recursive/vector?}
\usage{
  assert_is_atomic(x)

  assert_is_recursive(x)

  assert_is_vector(x)

  is_atomic(x, .xname = get_name_in_parent(x))

  is_recursive(x, .xname = get_name_in_parent(x))

  is_vector(x, .xname = get_name_in_parent(x))
}
\arguments{
  \item{x}{Input to check.}

  \item{.xname}{Not intended to be used directly.}
}
\value{
  \code{is_atomic}, \code{is_recursive} and
  \code{is_vector} wrap \code{is.atomic},
  \code{is.recursive} and \code{is.vector} respectively,
  providing more information on failure.  The
  \code{assert_*} functions return nothing but throw an
  error if the corresponding \code{is_*} function returns
  \code{FALSE}.
}
\description{
  Checks to see if the input is a type that is
  atomic/recursive/vector.
}
\examples{
atomic_types <- list(
  logical(),
  integer(),
  numeric(),
  complex(),
  character(),
  raw(),
  matrix(),
  array(),
  NULL
)
for(var in atomic_types) assert_is_atomic(var)
recursive_types <- list(
  list(),
  expression(),
  data.frame(),
  y ~ x,
  function(){},
  call("sin", "pi")
)
for(var in recursive_types) assert_is_recursive(var)
vector_types <- c(
  atomic_types[1:6],
  recursive_types[1:2]
)
for(var in vector_types) assert_is_vector(var)
}
\seealso{
  \code{\link[base]{is.atomic}} and
  \code{\link[base]{is.recursive}}.
}

