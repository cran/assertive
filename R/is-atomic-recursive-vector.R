#' Is the input atomic/recursive/vector?
#'
#' Checks to see if the input is a type that is atomic/recursive/vector.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_atomic}, \code{is_recursive} and \code{is_vector} wrap 
#' \code{is.atomic}, \code{is.recursive} and \code{is.vector} respectively,
#' providing more information on failure.  The \code{assert_*} functions
#' return nothing but throw an error if the corresponding \code{is_*}
#' function returns \code{FALSE}.
#' @seealso \code{\link[base]{is.atomic}} and \code{\link[base]{is.recursive}}.
#' @examples
#' atomic_types <- list(
#'   logical(),
#'   integer(),
#'   numeric(), 
#'   complex(),
#'   character(), 
#'   raw(),
#'   matrix(), 
#'   array(),
#'   NULL
#' )
#' for(var in atomic_types) assert_is_atomic(var)
#' recursive_types <- list(
#'   list(), 
#'   expression(),
#'   data.frame(), 
#'   y ~ x,
#'   function(){},
#'   call("sin", "pi")
#' )
#' for(var in recursive_types) assert_is_recursive(var)
#' vector_types <- c(
#'   atomic_types[1:6], 
#'   recursive_types[1:2]
#' )
#' for(var in vector_types) assert_is_vector(var)
#' @export
is_atomic <- function(x, .xname = get_name_in_parent(x))
{
  if(!is.atomic(x))
  {
    return(false("%s is not atomic.", .xname))
  }
  TRUE
}

#' @rdname is_atomic
#' @export
is_recursive <- function(x, .xname = get_name_in_parent(x))
{
  if(!is.recursive(x))
  {
    return(false("%s is not recursive.", .xname))
  }
  TRUE
}

#' @rdname is_atomic
#' @export
is_vector <- function(x, .xname = get_name_in_parent(x))
{
  if(!is.vector(x)) return(false("%s is not a vector.", .xname))
  TRUE
}                
