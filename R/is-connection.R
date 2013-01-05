#' Is the input a connection?
#'
#' Checks to see if the input is a (open/incomplete) connection.
#'
#' @param x Input to check.
#' @param rw Read-write status of connection.  Passed to \code{isOpen}.
#' @param .xname Not intended to be used directly.
#' @return \code{is_connection} checks for objects of class "connection".
#' \code{is_open_connection} and \code{is_incomplete_connection} wrap \code{isOpen} and 
#' \code{isIncomplete} respectively, providing more information on failure.
#' The \code{assert_*} functions return nothing but throw an error if the corresponding
#' \code{is_*} function returns \code{FALSE}.
#' @note \code{is_incomplete_connection} will return false for closed connections, 
#' regardless of whether or not the connection ends with a newline character.
#' (\code{isIncomplete} throws an error for closed connections.)
#' @seealso \code{\link[base]{isOpen}}.
#' @examples
#' assert_is_connection(stdin())
#' tcon <- textConnection("txt", "w", local = TRUE)
#' assert_is_open_connection(tcon)
#' cat("this has no final newline character", file = tcon)
#' assert_is_incomplete_connection(tcon)
#' close(tcon)
#' \dontrun{
#' #These examples should fail.
#' assert_is_connection("not a connection")
#' fcon <- file()
#' close(fcon)
#' assert_is_open_connection(fcon)
#' }
#' @export
is_connection <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "connection", .xname)
}

#' @rdname is_connection
#' @export
is_open_connection <- function(x, rw = "", .xname = get_name_in_parent(x))
{
  rw <- use_first(rw)
  if(!(ok <- is_connection(x))) return(ok)
  if(!is_error_free(isOpen(x, rw)))
  {
    return(false("The connection %s is not open.", .xname))
  }
  TRUE
}

#' @rdname is_connection
#' @export
is_incomplete_connection <- function(x, .xname = get_name_in_parent(x))
{  
  if(!(ok <- is_open_connection(x))) return(ok)
  if(!isIncomplete(x))
  {
    return(false("The connection %s is complete.", .xname))
  }
  TRUE
}
