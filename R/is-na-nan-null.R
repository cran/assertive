#' Are the inputs (in)finite?
#'
#' Checks to see if the inputs are (in)finite.
#'
#' @param x Input to check.
#' @return \code{is_finite} wraps \code{is.finite}, showing the 
#' names of the inputs in the answer. \code{is_infinite} works 
#' likewise for \code{is.infinite}.  The \code{assert_*} functions 
#' return nothing but throw an error if the corresponding 
#' \code{is_*} function returns \code{FALSE}.
#' @seealso \code{\link[base]{is.finite}}
#' @examples
#' x <- c(0, Inf, -Inf, NA, NaN)
#' is_finite(x)
#' is_infinite(x)
#' is_positive_infinity(x)
#' is_negative_infinity(x)
#' assert_all_are_finite(1:10)
#' assert_any_are_finite(c(1, Inf))
#' assert_all_are_infinite(c(Inf, -Inf))
#' dont_stop(assert_all_are_finite(c(0, Inf, -Inf, NA, NaN)))
#' @export
is_finite <- function(x)
{
  x <- coerce_to(x, "numeric")
  call_and_name(
    function(x)
    {
      ok <- is.finite(x)
      set_cause(
        ok, 
        ifelse(
          is.infinite(x),
          "infinite",
          ifelse(is.nan(x), "not a number", "missing")
        )
      )
    }, 
    x
  )
}

#' @rdname is_finite
#' @export
is_infinite <- function(x)
{
  x <- coerce_to(x, "numeric")
  call_and_name(
    function(x)
    {
      ok <- is.infinite(x)
      set_cause(
        ok, 
        ifelse(
          is.finite(x),
          "finite",
          ifelse(is.nan(x), "not a number", "missing")
        )
      )
    }, 
    x
  )
}


#' Is the input present/missing?
#'
#' Checks to see if the input is (not) NA.
#'
#' @param x Input to check.
#' @return \code{is_na} wraps \code{is.na}, showing the names of 
#' the inputs in the answer.  \code{is_not_na} is the negation. 
#' The \code{assert_*} functions return nothing but throw an error
#' if the corresponding \code{is_*} function returns \code{FALSE}.
#' @seealso \code{\link[base]{is.na}}
#' @examples
#' x <- c(0, NaN, NA)
#' is_na(x)
#' is_not_na(x)
#' assert_all_are_not_na(1:10)
#' assert_any_are_not_na(x)
#' dont_stop(assert_all_are_not_na(x))
#' @export
is_na <- function(x)
{
  call_and_name(
    function(x)
    {
      ok <- is.na(x)
      set_cause(ok, "not missing")
    }, 
    x
  )
}

#' Is the input (not) NaN?
#'
#' Checks to see if the input is a number that is(n't) NaN.
#'
#' @param x Input to check.
#' @return \code{is_nan} wraps \code{is.nan}, coercing the input to
#' numeric if necessary.  \code{is_not_nan} works similarly, but returns
#' the negation.  The \code{assert_*} functions return nothing but
#' throw an error if the corresponding \code{is_*} function returns
#' \code{FALSE}.
#' @seealso \code{\link[base]{is.nan}}
#' @examples
#' x <- c(0, NaN, NA)
#' is_nan(x)
#' is_not_nan(x)
#' assert_all_are_not_nan(1:10)
#' assert_any_are_not_nan(x)
#' dont_stop(assert_all_are_not_nan(x))
#' @export
is_nan <- function(x)
{
  x <- coerce_to(x, "numeric")  
  call_and_name(
    function(x)
    {
      ok <- is.nan(x)
      set_cause(ok, "a number")
    }, 
    x
  )
}

#' @rdname is_na
#' @export
is_not_na <- function(x)
{
  call_and_name(
    function(x)
    {
      ok <- !is.na(x)
      set_cause(ok, "missing")
    }, 
    x
  )
}

#' @rdname is_nan
#' @export
is_not_nan <- function(x)
{
  x <- coerce_to(x, "numeric")
  call_and_name(
    function(x)
    {
      ok <- !is.nan(x)
      set_cause(ok, "not a number")
    }, 
    x
  )
}

#' @rdname is_null
#' @export
is_not_null <- function(x, .xname = get_name_in_parent(x))
{
  if(is.null(x))
  {
    return(false("%s is NULL.", .xname))
  }
  TRUE
}

#' Is the input (not) null?
#'
#' Checks to see if the input is (not) null.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_null} wraps \code{is.null}, providing more 
#' information on failure. \code{is_not_null} returns \code{TRUE} in
#' the opposite case.  The \code{assert_*} functions return nothing but
#' throw an error if the corresponding \code{is_*} function returns 
#' \code{FALSE}.
#' @seealso \code{\link[base]{is.null}}.
#' @examples
#' assert_is_null(NULL)
#' assert_is_null(c())
#' assert_is_not_null(NA)
#' @export
is_null <- function(x, .xname = get_name_in_parent(x))
{
  if(!is.null(x))
  {
    return(false("%s is not NULL.", .xname))
  }
  TRUE
}

#' @rdname is_finite
#' @export
is_negative_infinity <- function(x)
{
  x <- coerce_to(x, "numeric")
  call_and_name(
    function(x)
    {
      ok <- is.infinite(x) & x < 0
      set_cause(
        ok, 
        ifelse(
          is.finite(x),
          "finite",
          ifelse(
            is.nan(x), 
            "not a number",
            ifelse(is.na(x), "missing", "positive inf")
          )
        )
      )
    }, 
    x
  )
}

#' @rdname is_finite
#' @export
is_positive_infinity <- function(x)
{
  x <- coerce_to(x, "numeric")
  call_and_name(
    function(x)
    {
      ok <- is.infinite(x) & x > 0
      set_cause(
        ok, 
        ifelse(
          is.finite(x),
          "finite",
          ifelse(
            is.nan(x), 
            "not a number",
            ifelse(is.na(x), "missing", "negative inf")
          )
        )
      )
    }, 
    x
  )
}
