#' @rdname is_in_past   
#' @export
is_in_future <- function(x)
{
  x <- coerce_to(x, "POSIXct")
  ok <- rep.int(TRUE, length(x))
  ok[x <= Sys.time()] <- FALSE
  names(ok) <- x
  ok
}

#' Is the input in the past/future?
#'
#' Checks to see if the input is a time in the past/future.
#'
#' @param x Input to check.
#' @return The \code{is_*} function return \code{TRUE} if the input is 
#' a time in the future/past.  The \code{assert_*} functions return nothing but
#' throw an error if the corresponding \code{is_*} function returns 
#' \code{FALSE}.
#' @details The current time is determined by \code{Sys.time}, and the 
#' input is coerced to \code{POSIXct} format if necessary.
#' @seealso \code{\link{Sys.time}}.
#' @examples
#' assert_is_empty(NULL)
#' assert_is_empty(numeric())
#' assert_is_non_empty(1:10)
#' assert_is_non_empty(NA)
#' assert_is_scalar(1)
#' assert_is_scalar("Multiple words in a single string are scalar.")
#' assert_is_scalar(NA)
#' @export
is_in_past <- function(x)
{
  x <- coerce_to(x, "POSIXct")
  ok <- rep.int(TRUE, length(x))
  ok[x >= Sys.time()] <- FALSE
  names(ok) <- x
  ok
}
