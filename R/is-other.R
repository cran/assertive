#' Is the input function being debugged?
#'
#' Checks to see if the input DLL (a.k.a. shared object) is loaded.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_debugged} wraps \code{is.debugged}, providing more 
#' information on failure.  \code{assert_is_debugged} returns nothing but
#' throws an error if \code{is_loaded} returns \code{FALSE}.
#' @seealso \code{\link[base]{isdebugged}}.
#' @export
is_debugged <- function(x, .xname = get_name_in_parent(x))
{
  # isdebugged accepts x as either a function or a string
  if(!is.function(x))
  {
    x <- coerce_to(use_first(x), "character")
  }
  if(!isdebugged(x))
  {
    return(false("%s is not being debugged.", .xname))
  }
  TRUE
}

#' Is the input divisible by a number?
#' 
#' Checks to see if the input is divisible by some number.
#' @param x A numeric vector to divide.
#' @param n A numeric vector to divide by.
#' @param tol Differences from zero smaller than \code{tol} are not considered.
#' @param na_ignore A logical value.  If \code{FALSE}, \code{NA} values
#' cause an error; otherwise they do not.  Like \code{na.rm} in many
#' stats package functions, except that the position of the failing
#' values does not change.
#' @return \code{TRUE} if the input \code{x} is divisible by \code{n}, within 
#' the specified tolerance.
#' @note \code{is_even} and \code{is_odd} are shortcuts for divisibility by two.
#' @seealso \code{is_whole_number}
#' @examples
#' is_divisible_by(1:10, 3)
#' is_divisible_by(-5:5, -2)
#' is_divisible_by(1.5:10.5, c(1.5, 3.5))
#' assert_any_are_even(1:10)
#' dont_stop(assert_all_are_even(1:10))
#' @export
is_divisible_by <- function(x, n, tol = 100 * .Machine$double.eps)
{
  call_and_name(
    function(x) 
    {
      ok <- abs(x %% n) <= tol
      set_cause(ok, "indivisible")
    }, 
    x
  ) 
}

#' Does the code run without throwing an error?
#' 
#' Call the code inside a try block and report if an error was thrown.
#' 
#' @param x Code to check.
#' @note Note that this has the side effect of running the code contained in
#' \code{x}.
#' @return \code{TRUE} if the code runs without throwing an error.
#' @export
is_error_free <- function(x)
{
  res <- try(x, silent = TRUE)
  if(inherits(res, "try-error"))
  {
    return(false(attr(res, "condition")$message))
  }
  ok <- TRUE
  attr(ok, "result") <- res
  ok
}

#' @rdname is_divisible_by
#' @export
is_even <- function(x, tol = 100 * .Machine$double.eps)
{
  is_divisible_by(x, 2L, tol = tol)  
}

#' Does the variable exist?
#'
#' Checks to see if the input variables exist.
#'
#' @param x Input to check.
#' @param envir Passed to \code{exists}.
#' @param inherits Passed to \code{exists}.
#' @param .xname Not intended to be used directly.
#' @return \code{is_existing} is a vectorized wrapper to \code{exists}, 
#' providing more information on failure (and with a simplified interface).  
#' The \code{assert_*} functions return nothing but throw an error if 
#' \code{is_existing} returns \code{FALSE}.
#' @seealso \code{\link[base]{exists}}.
#' @examples
#' e <- new.env()
#' e$x <- 1
#' e$y <- 2
#' assert_all_are_existing(c("x", "y"), envir = e)
#' #These examples should fail.
#' dont_stop(assert_all_are_existing(c("x", "z"), envir = e))
#' @export
is_existing <- function(
  x, 
  envir = parent.frame(), 
  inherits = TRUE, 
  .xname = get_name_in_parent(x)
)
{
  x <- coerce_to(x, "character")
  if(is_empty(x)) return(logical(0))
  if(length(x) > 1L)
  {
    return(bapply(
      x, 
      is_existing,
      envir    = envir,
      inherits = inherits
    ))
  }
  if(!exists(
    x, 
    envir    = envir,
    inherits = inherits
  ))
  {
    return(false("%s does not exist.", .xname))
  }
  TRUE
}

#' Is suitable to be used as an if condition
#' 
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_if_condition} returns \code{TRUE} if the input is 
#' scalar \code{TRUE} or \code{FALSE}.
#' @note \code{if} will try to do the right thing if you pass it a number
#' or a string, but this function assumes you want to do the right thing
#' and pass either \code{TRUE} or \code{FALSE}, maybe with some attributes.
#' @examples
#' is_if_condition(TRUE)
#' is_if_condition(FALSE)
#' is_if_condition(NA)
#' is_if_condition(c(TRUE, FALSE))
#' is_if_condition("the truth")
#' # You can pass a number as a logical condition, but you shouldn't,
#' # so the next line returns FALSE.
#' is_if_condition(1)
#' dont_stop(assert_is_if_condition(raw(1)))
#' @export
is_if_condition <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_logical(x, .xname)))
  {
    return(ok)
  }
  if(!(ok <- is_scalar(x, "length", .xname)))
  {
    return(ok)
  }
  if(!is_not_na(x))
  {
    return(false("%s is NA.", .xname))
  }
  TRUE
}

#' Is the input DLL loaded?
#'
#' Checks to see if the input DLL (a.k.a. shared object) is loaded.
#'
#' @param x Input to check.
#' @param PACKAGE Passed to \code{is.loaded}.
#' @param type Passed to \code{is.loaded}.
#' @param .xname Not intended to be used directly.
#' @return \code{is_loaded} wraps \code{is.loaded}, providing more 
#' information on failure.  \code{assert_is_loaded} returns nothing but
#' throws an error if \code{is_loaded} returns \code{FALSE}.
#' @seealso \code{\link[base]{is.loaded}}.
is_loaded <- function(x, PACKAGE = "", type = "", 
  .xname = get_name_in_parent(x))
{
  if(!is.loaded(x, PACKAGE = PACKAGE, type = type))
  {
    return(false("%s is not loaded.", .xname))
  }
}

#' @rdname is_divisible_by
#' @importFrom stats setNames
#' @export
is_odd <- function(x, tol = 100 * .Machine$double.eps)
{
  setNames(is_divisible_by(x - 1, 2L, tol = tol), x)
}

#' Is the input a symmetric matrix?
#'
#' Checks that the input is a symmetric matrix.
#' 
#' @param x Input to check.
#' @param tol Differences smaller than \code{tol} are not considered.
#' @param .xname Not intended to be used directly.
#' @param ... Passed to \code{all.equal}.
#' @return \code{TRUE} if the input is symmetrix (after coersion to be a 
#' matrix).
#' @examples
#' m <- diag(3); m[3, 1] <- 1e-100
#' assert_is_symmetric_matrix(m)
#' #These examples should fail.
#' dont_stop(assert_is_symmetric_matrix(m, tol = 0))
#' @export
is_symmetric_matrix <- function(x, tol = 100 * .Machine$double.eps, 
  .xname = get_name_in_parent(x), ...)
{
  x <- coerce_to(x, "matrix")
  dimx <- dim(x)
  if(dimx[1L] != dimx[2L])
  {
    return(false("%s is not a square matrix.", .xname))
  }
  symmetry_test <- if(is.complex(x)) 
  {
    all.equal.numeric(x, Conj(t(x)), tolerance = tol, ...)
  } else 
  {
    all.equal(x, t(x), tolerance = tol, ...)
  }
  if(!is_identical_to_true(symmetry_test))
  {
    return(false("%s is not a symmetric matrix.", .xname))
  }
  TRUE
}

#' Is the input unsorted?
#' 
#' Checks to see if the input is unsorted (without the cost of sorting it).
#'
#' @param x Input to check.
#' @param na.rm If \code{TRUE}, remove \code{NA}s before checking.
#' @param strictly If \code{TRUE}, equal values count as unsorted.
#' @param .xname Not intended to be used directly.
#' @return \code{is_unsorted} reimplements \code{is.unsorted}, providing
#' more information on failure.  \code{assert_is_unsorted} returns nothing 
#' but throws an error if \code{is_unsorted} returns \code{FALSE}.
#' @note The builtin function \code{is.unsorted} usually returns \code{NA}
#' when the input is recursive and has length 2, though for some
#' classes (particularly data.frames) it returns a \code{TRUE} or
#' \code{FALSE} value.  The logic behind those is difficult to
#' interpret, and gives odd results, so \code{is_unsorted} always
#' returns \code{NA} in this case.
#' @seealso \code{\link[base]{is.unsorted}}.
#' @examples
#' assert_is_unsorted(c(1, 3, 2))
#' assert_is_unsorted(c(1, 1, 2), strictly = TRUE)
#' #These tests should fail.
#' dont_stop(assert_is_unsorted(c(1, 1, 2)))
#' dont_stop(assert_is_unsorted(c(2, 1, 0)))
#' @export
is_unsorted <- function(x, na.rm = FALSE, strictly = FALSE, 
  .xname = get_name_in_parent(x))
{
  if(!(ok <- is_not_null(x))) return(ok)
  if(!is.atomic(x) && length(x) > 1)
  {
    #See notes in Value section of ?is.unsorted.
    return(na(
      "Sortability is not tested for recursive objects of length greater than one."
    ))
  }
  nas <- is.na(x)
  if(any(nas))
  {
    if(!na.rm) 
    {
      return(na("%s contains NA values.", .xname))
    }
    x <- x[!nas]
  }
  if(!is.unsorted(x, strictly = strictly))
  {
    return(false("%s is sorted.", .xname))
  }
  TRUE
}

#' Is the input a whole number?
#'
#' Checks that the (probably floating point) input is a whole number.
#' 
#' @param x Input to check.
#' @param tol Differences smaller than \code{tol} are not considered.
#' @param na_ignore A logical value.  If \code{FALSE}, \code{NA} values
#' cause an error; otherwise they do not.  Like \code{na.rm} in many
#' stats package functions, except that the position of the failing
#' values does not change.
#' @note The term whole number is used to distinguish from integer in
#' that the input \code{x} need not have type \code{integer}.  In fact
#' it is expected that \code{x} will be \code{numeric}.
#' @return \code{TRUE} if the input is a whole number.
#' @seealso \code{is_divisible_by}
#' @examples
#' # 1, plus or minus a very small number
#' x <- 1 + c(0, .Machine$double.eps, -.Machine$double.neg.eps)
#' # By default, you get a bit of tolerance for rounding errors
#' is_whole_number(x)
#' # Set the tolerance to zero for exact matching.
#' is_whole_number(x, tol = 0)
#' @export
is_whole_number <- function(x, tol = 100 * .Machine$double.eps)
{
  x <- coerce_to(x, "numeric")
  call_and_name(
    function(x) 
    {
      ok <- abs(x - round(x)) <= tol & !is.infinite(x)
      set_cause(ok, ifelse(is.infinite(x), "infinite", "fractional"))
    }, 
    x
  )
}
