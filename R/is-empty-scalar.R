#' Is the input empty/scalar?
#'
#' Checks to see if the input has length zero/one.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_empty} returns \code{TRUE} if the input has length 
#' zero.  \code{is_scalar} returns \code{TRUE} if the input has length 
#' one.  The \code{assert_*} functions return nothing but throw an
#' error if the corresponding \code{is_*} function returns \code{FALSE}.
#' @seealso \code{\link{length}}.
#' @examples
#' assert_is_empty(NULL)
#' assert_is_empty(numeric())
#' assert_is_non_empty(1:10)
#' assert_is_non_empty(NA)
#' assert_is_scalar(1)
#' assert_is_scalar("Multiple words in a single string are scalar.")
#' assert_is_scalar(NA)
#' @export
is_empty <- function(x, .xname = get_name_in_parent(x))
{
  if(length(x) != 0L) 
  {
    return(false("%s has non-zero length.", .xname))
  }
  TRUE
}

#' Is the input the empty model?
#'
#' Checks to see if the input is the empty model.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_[non_]empty_model} returns \code{TRUE} if the input is an  
#' [non] empty model.  (\code{has_terms} is used to determine that a variable 
#' is a model object.)  The model is considered empty if there are no
#' factors and no intercept. The \code{assert_*} functions return nothing 
#' but throw an error if the corresponding \code{is_*} function returns
#' \code{FALSE}.
#' @seealso \code{\link[stats]{is.empty.model}} and \code{is_empty}.
#' @examples
#' assert_is_empty_model(lm(uptake ~ 0, CO2))
#' assert_is_non_empty_model(lm(uptake ~ conc, CO2))
#' assert_is_non_empty_model(lm(uptake ~ 1, CO2))
#' @export
is_empty_model <- function(x, .xname = get_name_in_parent(x))
{
  if(!has_terms(x)) 
  {
    return(false("%s has no terms, probably not a model.", .xname))
  }
  tt <- terms(x)
  if(length(attr(tt, "factors")) != 0L) 
  {
    return(false("%s has factors.", .xname))
  }
  if(attr(tt, "intercept") != 0L) 
  {
    return(false("%s has an intercept.", .xname))
  }
  TRUE
}

#' @rdname is_empty
#' @export
is_non_empty <- function(x, .xname = get_name_in_parent(x))
{
  if(length(x) == 0L) 
  {
    return(false("%s has length zero.", .xname))
  }
  TRUE
}

#' @rdname is_empty_model
#' @export
is_non_empty_model <- function(x, .xname = get_name_in_parent(x))
{
  if(!has_terms(x)) 
  {
    return(false("%s has no terms, is probably not a model.", .xname))
  }
  tt <- terms(x)
  if(length(attr(tt, "factors")) == 0L && attr(tt, "intercept") == 0L)  
  {
    return(false("%s is an empty model.", .xname))
  }
  TRUE
}

#' @rdname is_empty
#' @export
is_scalar <- function(x, .xname = get_name_in_parent(x))
{
  if(length(x) != 1L)
  {
    return(false("%s does not have length one.", .xname))
  }
  TRUE
}                
