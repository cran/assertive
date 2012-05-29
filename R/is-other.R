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
is_debugged <- function(x, .xname = get_name_in_parent(x))
{
  assert_is_any_of(x, c("function", "character"), .xname)
  if(!isdebugged(x))
  {
    return(false("%s is not being debugged.", .xname))
  }
  TRUE
}

#' Does the variable exist?
#'
#' Checks to see if the input variables exist.
#'
#' @param x Input to check.
#' @param where Passed to \code{exists}.
#' @param envir Passed to \code{exists}.
#' @param frame Passed to \code{exists}.
#' @param mode Passed to \code{exists}.
#' @param inherits Passed to \code{exists}.
#' @param .xname Not intended to be used directly.
#' @return \code{is_existing} is a vectorised wrapper to \code{exists}, 
#' providing more information on failure.  The \code{assert_*} functions
#' return nothing but throw an error if \code{is_existing} returns 
#' \code{FALSE}.
#' @seealso \code{\link[base]{exists}}.
#' @examples
#' e <- new.env()
#' e$x <- 1
#' e$y <- 2
#' assert_all_are_existing(c("x", "y"), envir = e)
#' \dontrun{
#' assert_all_are_existing(c("x", "z"), envir = e)
#' }
#' @export
is_existing <- function(
  x, 
  where = -1, 
  envir = if (missing(frame)) as.environment(where) else sys.frame(frame), 
  frame, 
  mode = "any", 
  inherits = TRUE, 
  .xname = get_name_in_parent(x)
)
{
  x <- coerce_to(x, "character")
  if(is_empty(x)) return(logical(0))
  if(length(x) > 1L)
  {
    return(vapply(
      x, 
      is_existing, 
      logical(1), 
      where    = where,
      envir    = envir,
      frame    = frame,
      mode     = mode,
      inherits = inherits
    ))
  }
  if(!exists(
    x, 
    where    = where,
    envir    = envir,
    frame    = frame,
    mode     = mode,
    inherits = inherits
  ))
  {
    return(false("%s does not exist.", .xname))
  }
  TRUE
}

# ' Is the input generic?
# '
# ' Checks to see if the input is a generic function.
# '
# ' @param x Input to check.
# ' @param .xname Not intended to be used directly.
# ' @return \code{TRUE} if the input is a generic function. 
# ' \code{assert_is_generic} functions return nothing but throws an error
# ' if \code{is_generic} returns \code{FALSE}.
# ' @seealso \code{\link[methods]{GenericFunctions}}.
# ' @examples
# ' 
# ' @export
# is_generic <- function(x)
# {       
#   x <- use_first(x)  
#   if(!is.function(x)) return(false("Input is not a function"))  
#   fn_name <- get_name_in_parent(x)
#   if(fn_name %in% utils:::getKnownS3generics()) return(TRUE)      
#   where <- find(fn_name, mode = "function")
#   gen <- utils:::findGeneric(fn_name, envir = as.environment(where))
#   if(!nzchar(gen)) return(false("Input is not generic."))
#   TRUE
# }     

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
is_loaded <- function(x, PACKAGE = "", type = "", .xname = get_name_in_parent(x))
{
  if(!is.loaded(x, PACKAGE = PACKAGE, type = type))
  {
    return(false("%s is not loaded.", .xname))
  }
}

#' Are you running R?
#'
#' Checks to see you are running R.
#'
#' @return \code{is_R} wraps \code{is.R}, providing more 
#' information on failure.  \code{assert_is_R} returns nothing but
#' throws an error if \code{is_R} returns \code{FALSE}.
#' @seealso \code{\link[base]{is.R}}.
#' @examples
#' assert_is_R()
#' @export
is_R <- function()
{
  if(!is.R())
  {
    return(false("You are not running R."))
  } 
  TRUE
}

#' Is the input a symmetric matrix?
#'
#' Checks that the input is a symmetric matrix.
#' 
#' @param x Input to check.
#' @param tol Differences smaller than \code{tol} are not considered.
#' @param .xname Not intended to be used directly.
#' @param ... Passed to \code{all.equal}.
#' @return \code{TRUE} if the input is symmetrix (after coersion to be a matrix).
#' @examples
#' m <- diag(3); m[3, 1] <- 1e-100
#' assert_is_symmetric_matrix(m)
#' \dontrun{
#' assert_is_symmetric_matrix(m, tol = 0)
#'}
#' @export
is_symmetric_matrix <- function(x, tol = 100 * .Machine$double.eps, .xname = get_name_in_parent(x), ...)
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
  if(!is_true(symmetry_test))
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
#' \dontrun{
#' #These tests should fail:
#' assert_is_unsorted(c(1, 1, 2))
#' assert_is_unsorted(c(2, 1, 0))
#' }
#' @export
is_unsorted <- function(x, na.rm = FALSE, strictly = FALSE, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_not_null(x))) return(ok)
  if(!is.atomic(x) && length(x) > 1)
  {
    #See notes in Value section of ?is.unsorted.
    return(na("Sortability is not tested for recursive objects of length greater than one."))
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
#' @note The term whole number is used to distinguish from integer in
#' that the input \code{x} need not have type \code{integer}.  In fact
#' it is expected that \code{x} will be \code{numeric}.
#' @return \code{TRUE} if the input is a whole number.
is_whole_number <- function(x, tol = 100 * .Machine$double.eps)
{
  x <- coerce_to(x, "numeric")
  abs(x - floor(x)) < tol
}
