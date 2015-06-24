#' @rdname is_in_range
#' @export
is_in_closed_range <- function(x, lower = -Inf, upper = Inf)
{
  is_in_range(x, lower, upper, FALSE, FALSE)
}

#' @rdname is_in_range
#' @export
is_in_left_open_range <- function(x, lower = -Inf, upper = Inf)
{
  is_in_range(x, lower, upper, TRUE, FALSE)
}

#' @rdname is_in_range
#' @export
is_in_open_range <- function(x, lower = -Inf, upper = Inf)
{
  is_in_range(x, lower, upper, TRUE, TRUE)
}

#' Is the input in range?
#'
#' Checks to see if the input is within an numeric interval.
#'
#' @param x Input to check.
#' @param lower Lower bound for the interval.
#' @param upper Upper bound for the interval.
#' @param lower_is_strict If \code{TRUE}, the lower bound is open (strict) 
#' otherwise it is closed.
#' @param upper_is_strict If \code{TRUE}, the upper bound is open (strict)
#' otherwise it is closed.
#' @note \code{is_in_range} provides the most flexibility in determining
#' if values are within a numeric interval.  The other functions restrict
#' the input arguments for convience in common cases.  For example,
#' \code{is_percentage} forces the interval to be from 0 to 100.
#' @return The \code{is_*} functions return \code{TRUE} if the input is 
#' within an interval.  The \code{assert_*} functions return nothing but
#' throw an error if the corresponding \code{is_*} function returns 
#' \code{FALSE}.
#' @examples
#' assert_all_are_positive(1:10)
#' assert_all_are_non_negative(0:10)
#' assert_any_are_positive(c(-1, 1))
#' assert_all_are_percentages(c(0, 50, 100))
#' assert_all_are_proportions(c(0, 0.5, 1))
#' assert_all_are_in_left_open_range(1 + .Machine$double.eps, lower = 1)
#' @export
is_in_range <- function(x, lower = -Inf, upper = Inf, lower_is_strict = FALSE, 
  upper_is_strict = FALSE)
{
  x <- coerce_to(x, "numeric")
  ok <- rep.int(TRUE, length(x))
  ok[is.na(x)] <- NA
  too_low <- (if(lower_is_strict) `<=` else `<`)(x, lower)
  too_high <- (if(upper_is_strict) `>=` else `>`)(x, upper)
  ok[too_low] <- FALSE                     
  ok[too_high] <- FALSE
  names(ok) <- x
  set_cause(
    ok,
    ifelse(too_low, "too low", "too high")
  )
}

#' @rdname is_in_range
#' @export
is_in_right_open_range <- function(x, lower = -Inf, upper = Inf)
{
  is_in_range(x, lower, upper, FALSE, TRUE)
}

#' @rdname is_in_range
#' @export
is_negative <- function(x)
{
  is_in_range(x, upper = 0, upper_is_strict = TRUE)
}

#' @rdname is_in_range
#' @export
is_non_negative <- function(x)
{
  is_in_range(x, 0)
}

#' @rdname is_in_range
#' @export
is_non_positive <- function(x)
{
  is_in_range(x, upper = 0)
}

#' @rdname is_in_range
#' @export
is_percentage <- function(x, lower_is_strict = FALSE, upper_is_strict = FALSE)
{
  is_in_range(x, 0, 100, lower_is_strict, upper_is_strict)
}

#' @rdname is_in_range
#' @export
is_positive <- function(x)
{
  is_in_range(x, 0, lower_is_strict = TRUE)
}

#' @rdname is_in_range
#' @export
is_proportion <- function(x, lower_is_strict = FALSE, upper_is_strict = FALSE)
{
  is_in_range(x, 0, 1, lower_is_strict, upper_is_strict)
}
