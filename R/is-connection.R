#' Is the input an open/incomplete connection?
#'
#' Checks to see if the input is an open/incomplete connection.
#'
#' @param x Input to check.
#' @param rw Read-write status of connection.  Passed to \code{isOpen}.
#' @param .xname Not intended to be used directly.
#' @return \code{is_open_connection} wraps \code{isOpen}, providing more
#' information on failure.  \code{assert_is_open_connection} returns nothing 
#' but throws an error if \code{is_open_connection} returns \code{FALSE}.
#' @seealso \code{\link[base]{isOpen}}.
#' @export
is_open_connection <- function(x, rw = "", .xname = get_name_in_parent(x))
{
  x <- use_first(x)
  rw <- use_first(rw)
  if(!isOpen(x, rw))
  {
    return(false("%s is not an open connection", .xname))
  }
  TRUE
}

#' @rdname is_open_connection
#' @export
is_incomplete_connection <- function(x, .xname = get_name_in_parent(x))
{
  x <- use_first(x)
  if(!isIncomplete(x))
  {
    return(false("%s is not an open connection", .xname))
  }
  TRUE
}
