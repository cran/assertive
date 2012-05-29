#' @rdname is_true
#' @export
is_false <- function(x, allow_attributes = FALSE, .xname = get_name_in_parent(x))
{
  if(allow_attributes) attributes(x) <- NULL
  if(!identical(FALSE, x)) 
  {
    return(false("%s is not identical to FALSE.", .xname))
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
#' @return \code{TRUE} if the input is identical to \code{TRUE}.
#' The \code{assert_*} functions return nothing but
#' throw an error if the corresponding \code{is_*} function returns 
#' \code{FALSE}.
#' @seealso \code{\link[base]{isTRUE}}.
#' @examples
#' assert_is_true(TRUE)
#' assert_is_false(FALSE)
#' assert_is_true(c(truth = TRUE), allow_attributes = TRUE)
#' assert_is_false(assertive:::false("This has an attribute"), allow_attributes = TRUE)
#' \dontrun{
#' #These tests should fail:
#' assert_is_true(c(truth = TRUE))
#' assert_is_false(assertive:::false("This has an attribute"))
#' }
#' @export
is_true <- function(x, allow_attributes = FALSE, .xname = get_name_in_parent(x))
{
  if(allow_attributes) attributes(x) <- NULL
  if(!isTRUE(x))
  {
    return(false("%s is not identical to TRUE.", .xname))
  }
  TRUE
}
