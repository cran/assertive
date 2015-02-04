#' @rdname are_identical
#' @export
assert_all_are_identical <- function(...)
{
  # Nasty reimplementation of functionality since assert_engine doesn't work
  # ... inputs right now.
  ok <- are_identical(...)
  if(!all(ok))
  {
    handler <- match.fun(
      match.arg(
        getOption("assertive.severity"), 
        c("stop", "warning", "message")
      )
    )
    handler(
      "The expressions ", 
      toString(as.list(match.call())[-1]), 
      " are not all identical.",
      call. = FALSE
    )
  }
}

#' @rdname are_identical
#' @export
assert_any_are_identical <- function(...)
{
  # Also nasty.
  ok <- are_identical(...)
  if(!any(ok))
  {
    handler <- match.fun(
      match.arg(
        getOption("assertive.severity"), 
        c("stop", "warning", "message")
      )
    )
    handler(
      "The expressions ", 
      toString(as.list(match.call())[-1]), 
      " are all not identical.",
      call. = FALSE
    )
  }
}

#' @rdname are_same_length
#' @export
assert_all_are_same_length <- function(...)
{
  # Nasty reimplementation of functionality since assert_engine doesn't work
  # ... inputs right now.
  ok <- are_same_length(...)
  if(!all(ok))
  {
    handler <- match.fun(
      match.arg(
        getOption("assertive.severity"), 
        c("stop", "warning", "message")
      )
    )
    handler(
      "The expressions ", 
      toString(as.list(match.call())[-1]), 
      " are not all the same length.",
      call. = FALSE
    )
  }
}

#' @rdname are_same_length
#' @export
assert_any_are_same_length <- function(...)
{
  # Also nasty.
  ok <- are_same_length(...)
  if(!any(ok))
  {
    handler <- match.fun(
      match.arg(
        getOption("assertive.severity"), 
        c("stop", "warning", "message")
      )
    )
    handler(
      "The expressions ", 
      toString(as.list(match.call())[-1]), 
      " are all not the same length.",
      call. = FALSE
    )
  }
}
