#' @rdname is_true
#' @export
is_false <- function(x)
{
  x <- coerce_to(x, "logical")
  call_and_name(
    function(x) 
    {
      ok <- !x & !is.na(x)
      set_cause(ok, ifelse(is.na(x), "missing", "true"))    
    }, 
    x
  )
}

#' @rdname is_true
#' @export
is_identical_to_false <- function(x, allow_attributes = FALSE, 
  .xname = get_name_in_parent(x))
{
  if(allow_attributes) 
  {
    x <- strip_attributes(x)
  }
  if(!identical(FALSE, x)) 
  {
    return(false("%s is not identical to FALSE.", .xname))
  }
  TRUE
}                  

#' @rdname is_true
#' @export
is_identical_to_true <- function(x, allow_attributes = FALSE, 
  .xname = get_name_in_parent(x))
{
  if(allow_attributes) 
  {
    x <- strip_attributes(x)
  }
  if(!identical(TRUE, x))
  {
    return(false("%s is not identical to TRUE.", .xname))
  }
  TRUE
}

#' Is the input TRUE?
#' 
#' Checks to see if the input is \code{TRUE}.
#'
#' @param x Input to check.
#' @param allow_attributes If \code{TRUE}, a scalar value of \code{TRUE}
#' with attributes is allowed.
#' @param .xname Not intended to be used directly.
#' @note \code{is_identical_to_true} wraps the base function \code{isTRUE}, 
#' providing more information on failure.  Likewise, 
#' \code{is_identical_to_false} checks that the input is identical to FALSE.  If
#' \code{allow_attributes} is \code{TRUE}, a scalar value of \code{TRUE} with 
#' attributes is allowed. \code{is_true} and \code{is_false} are vectorized, 
#' returning \code{TRUE} when the inputs are \code{TRUE} and \code{FALSE} 
#' respectively.
#' Note that in version 0.1-4 and prior, \code{is_identical_to_true/false} was 
#' named \code{is_true/false} and the vectorized versions were not present.
#' @return The \code{is*} functions return \code{TRUE} if the input is 
#' \code{TRUE}/\code{FALSE}. The \code{assert_*} functions return nothing but 
#' throw an error if the corresponding \code{is_*} function returns 
#' \code{FALSE}.
#' @seealso \code{\link[base]{isTRUE}}.
#' @examples
#' assert_is_identical_to_true(TRUE)
#' assert_is_identical_to_false(FALSE)
#' assert_is_identical_to_true(c(truth = TRUE), allow_attributes = TRUE)
#' assert_is_identical_to_false(matrix(FALSE), allow_attributes = TRUE)
#' x <- c(TRUE, FALSE, NA)
#' is_true(x)
#' is_false(x)
#' assert_any_are_true(x)
#' assert_any_are_false(x)
#' # These tests should fail:
#' dont_stop(assert_is_identical_to_true(c(truth = TRUE)))
#' dont_stop(assert_is_identical_to_false(matrix(FALSE)))
#' dont_stop(assert_all_are_true(x))
#' dont_stop(assert_all_are_false(x))
#' @export
is_true <- function(x)
{
  x <- coerce_to(x, "logical")
  call_and_name(
    function(x) 
    {
      ok <- x & !is.na(x)
      set_cause(ok, ifelse(is.na(x), "missing", "false"))    
    }, 
    x
  )
}
