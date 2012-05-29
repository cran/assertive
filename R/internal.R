#' Throws an error if a condition isn't met.
#'
#' The workhorse of the package.  If a condition isn't met, then an error
#' is thrown.
#'
#' @param x Input to check.  If missing, pass no args to \code{predicate}.
#' @param predicate Function that returns a logical value (possibly 
#' a vector).
#' @param msg The error message, in the event of failure.
#' @param what Either 'all' or 'any', to reduce vectorised tests to a 
#' single value.
#' @param ... Passed to the \code{predicate} function.
#' @return \code{FALSE} with the attribute \code{message}, as provided
#' in the input.
assert_engine <- function(x, predicate, msg, what = c("all", "any"), ...)
{
  handler <- match.fun(match.arg(
    getOption("assertive.severity"),
    c("stop", "warning", "message")
    ))
  what <- match.fun(match.arg(what))
  #Some functions, e.g., is.R take no args
  ok <- if(missing(x)) predicate() else predicate(x, ...)
  if(!what(ok))
  {
    if(missing(msg)) 
    {
      if(is_scalar(ok))
      {
        msg <- cause(ok)
      } else
      {
        stop("Bug in assertive; error message is missing")
      }
    }
    handler(msg, call. = FALSE)
  }
}

#' Call a function, and give the result names.
#'
#' Calls a function, and names the result with the first argument.
#'
#' @param fn A function to call.  See note below.
#' @param x The first input to \code{fn}.
#' @param ... Optional additional inputs to \code{fn}.
#' @return The result of \code{fn(x, ...)}, with names given by the
#' argument \code{x}.
#' @note The function, \code{fn}, should return an object with the 
#' same length as the input \code{x}.
#' @examples
#' \dontrun{
#' call_and_name(is.finite, c(1, Inf, Na))
#' }
#' @seealso \code{\link{cause}} and \code{\link{na}}.
call_and_name <- function(fn, x, ...)
{
  y <- fn(x, ...)
  if(!is_true(length(y) == length(x)))
  {
    warning("Vector of names is different length to results.  Trying to resize.")
    length(x) <- length(y)
  }
  names(y) <- x
  y
}

#' FALSE, with a cause of failure.
#'
#' Always returns the value \code{FALSE}, with a cause attribute.
#'
#' @param ... Passed to sprintf to create a cause of failure message.
#' @return \code{FALSE} with the attribute \code{cause}, as provided
#' in the input.
#' @seealso \code{\link{cause}} and \code{\link{na}}.
false <- function(...)
{
  msg <- if(length(list(...)) > 0L) sprintf(...) else ""
  x <- FALSE
  cause(x) <- msg
  x
}


#' NA, with a cause of failure.
#'
#' Always returns the value (logical) \code{NA}, with a cause attribute.
#'
#' @param ... Passed to sprintf to create a cause of failure message.
#' @return \code{NA} with the attribute \code{cause}, as provided
#' in the input.
#' @seealso \code{\link{cause}} and \code{\link{false}}.
na <- function(...)
{
  msg <- if(length(list(...)) > 0L) sprintf(...) else ""
  x <- NA
  cause(x) <- msg
  x
}
