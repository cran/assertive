#' Throws an error if a condition isn't met.
#'
#' The workhorse of the package.  If a condition isn't met, then an error
#' is thrown.  This function is exported for use by package developers so
#' that they can create their own assert functions.  
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
#' @note Missing values are considered as \code{FALSE} for the purposes of
#' whether or not an error is thrown.
#' @examples
#' # Extending assertive with an is function that returns a single value
#' is_identical_to_pi <- function(x, .xname = get_name_in_parent())
#' { 
#'   if(!identical(x, pi))
#'   {
#'     return(false("%s is not identical to pi", .xname))
#'   }
#'   TRUE
#' }
#' assert_is_identical_to_pi <- function(x, .xname = get_name_in_parent())
#' {
#'   assert_engine(
#'     x, 
#'     is_identical_to_pi, 
#'     .xname = get_name_in_parent(x)
#'   )
#' }
#' 
#' is_identical_to_pi(pi)
#' is_identical_to_pi(3)
#' assert_is_identical_to_pi(pi)
#' dont_stop(assert_is_identical_to_pi(3))
#' 
#' # Extending assertive with an is function that returns a vector
#' is_less_than_pi <- function(x)
#' {
#'   is_in_right_open_range(x, upper = pi)
#' }
#' 
#' assert_all_are_less_than_pi <- function(x)
#' {
#'   msg <- gettextf("%s are not all less than pi.", get_name_in_parent(x))
#'   assert_engine(
#'     x, 
#'     is_less_than_pi, 
#'     msg
#'   )
#' }
#' assert_any_are_less_than_pi <- function(x)
#' {
#'   msg <- gettextf("%s are all greater than or equal to pi.", get_name_in_parent(x))
#'   assert_engine(
#'     x, 
#'     is_less_than_pi, 
#'     msg,
#'     what = "any"
#'   )
#' }
#' 
#' x <- c(3, pi, 4, NA)
#' is_less_than_pi(x)
#' assert_any_are_less_than_pi(x)
#' dont_stop(assert_all_are_less_than_pi(x))
#' @export
assert_engine <- function(x, predicate, msg, what = c("all", "any"), ...)
{
  handlerType <- match.arg(
    getOption("assertive.severity"),
    c("stop", "warning", "message")
  )
  handler <- match.fun(
    handlerType
  )
  simple <- switch(
    handlerType,
    stop = simpleError,
    warning = simpleWarning,
    message = simpleMessage
  )
  what <- match.fun(match.arg(what))
  
  #Some functions, e.g., is.R take no args
  ok <- if(missing(x)) predicate() else predicate(x, ...)
  if(!what(ok & !is.na(ok)))
  {
    if(missing(msg)) 
    {
      if(is_scalar(ok))
      {
        msg <- cause(ok)
      } else
      {
        stop("Bug in assertive; error message is missing.")
      }
    }
    if(!is_scalar(ok))
    {
      # Append first few failure values and positions to the error message.
      fail_index <- which(!ok | is.na(ok))
      n <- length(fail_index)
      fail_index <- head(fail_index)
      failures <- data.frame(
        Position = fail_index,
        Value    = truncate(names(ok[fail_index])),
        Cause    = unclass(cause(ok)[fail_index]), # See bug 15997
        row.names = seq_along(fail_index)
      )
      msg <- paste0(
        msg, 
        "\nThere ", 
        ngettext(n, "was", "were"), 
        " ", 
        n, 
        " ", 
        ngettext(n, "failure", "failures"),
        if(nrow(failures) < n) 
        {
          paste0(" (showing the first ", nrow(failures), ")")
        },
        ":\n",
        print_and_capture(failures)
      )
    }
    # Throw error/warning/message
    caller <- sys.call(-2)
    handler(simple(msg, caller))
  }
}

#' FALSE, with a cause of failure.
#'
#' Always returns the value \code{FALSE}, with a cause attribute.
#'
#' @param ... Passed to \code{gettextf} to create a cause of failure message.
#' @return \code{FALSE} with the attribute \code{cause}, as provided
#' in the input.
#' @seealso \code{\link{cause}} and \code{\link{na}}.
#' @export
false <- function(...)
{
  msg <- if(length(list(...)) > 0L) gettextf(...) else ""
  x <- FALSE
  cause(x) <- msg
  x
}

#' NA, with a cause of failure.
#'
#' Always returns the value (logical) \code{NA}, with a cause attribute.
#'
#' @param ... Passed to \code{gettextf} to create a cause of failure message.
#' @return \code{NA} with the attribute \code{cause}, as provided
#' in the input.
#' @seealso \code{\link{cause}} and \code{\link{false}}.
#' @export
na <- function(...)
{
  msg <- if(length(list(...)) > 0L) gettextf(...) else ""
  x <- NA
  cause(x) <- msg
  x
}
