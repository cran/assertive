#' @rdname is_empty
#' @export
has_elements <- function(x, n, .xname = get_name_in_parent(x))
{
  n <- use_first(n)
  assert_all_are_non_negative(n)
  assert_all_are_whole_numbers(n)
  if(n_elements(x) != n)
  {
    return(
      false(
        "%s does not have %d %s.", 
        .xname, 
        n, 
        ngettext(n, "element", "elements")
      )
    )
  }
  TRUE
}

#' Is the input empty/scalar?
#'
#' Checks to see if the input has length zero/one.
#'
#' @param x Input to check.
#' @param n Non-negative integer(s) of the expected length/number of elements/
#' lengths of dimensions.  See note.
#' @param metric A string. Should be length or the number of elements be used to
#' determine if the object is empty/non-empty/scalar?
#' @param .xname Not intended to be used directly.
#' @return \code{is_empty} returns \code{TRUE} if the input has length 
#' zero.  \code{is_scalar} returns \code{TRUE} if the input has length 
#' one.  The \code{assert_*} functions return nothing but throw an
#' error if the corresponding \code{is_*} function returns \code{FALSE}.
#' @note For \code{is_empty}, \code{is_non_empty} and \code{is_scalar}, \code{n}
#' should be an single integer representing either the expected length or the
#' expected number of elements in \code{x}.  For \code{is_of_dimension} \code{n}
#' should be a vector of integers representing the expected lengths of 
#' dimensions.
#' @seealso \code{\link{length}}.
#' @examples
#' #' # is_of_length returns TRUE if the length of an object
#' # matches a specified value.
#' is_of_length(1:5, 5)
#' assert_is_of_length(1:5, 5)
#' 
#' # has_elements returns TRUE if an object has a specified
#' # number of elements.  This is usually the same thing.
#' has_elements(1:5, 5)
#' assert_has_elements(1:5, 5)
#' 
#' # Data frames and lists behave differently for length
#' # and number of elements.
#' d <- data.frame(x = 1:5, y = letters[1:5])
#' assert_is_of_length(d, 2)
#' assert_has_elements(d, 10)
#' 
#' l <- list(a = 1:5, b = list(b.a = 1:3, b.b = 1:7))
#' assert_is_of_length(l, 2)
#' assert_has_elements(l, 15)
#' 
#' # Functions always have length one, but may have lots of 
#' # elements.
#' assert_is_of_length(var, 1)
#' assert_has_elements(var, 54)
#' 
#' # is_scalar is a shortcut for length one, or one elements.
#' assert_is_scalar(99)
#' assert_is_scalar("Multiple words in a single string are scalar.")
#' assert_is_scalar(NA)
#' 
#' # The two metrics can yield different results!
#' is_scalar(list(1:5))
#' is_scalar(list(1:5), "elements")
#' is_scalar(var)
#' is_scalar(var, "elements")
#' 
#' # Similarly, is_empty is a shortcut for length zero/zero elements.
#' assert_is_empty(NULL)
#' assert_is_empty(numeric())
#' assert_is_non_empty(1:10)
#' assert_is_non_empty(NA)
#' 
#' # is_of_dimension tests the lengths of all dimensions.
#' assert_is_of_dimension(d, c(5, 2))
#' assert_is_of_dimension(l, NULL)
#' @export
is_empty <- function(x, metric = c("length", "elements"), .xname = get_name_in_parent(x))
{  
  metric <- get_metric(metric)
  metric(x, 0L, .xname)
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
is_non_empty <- function(x, metric = c("length", "elements"), .xname = get_name_in_parent(x))
{
  metric <- get_metric(metric)
  if(metric(x, 0)) 
  {
    return(false("%s has length 0.", .xname))
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
is_of_dimension <- function(x, n, .xname = get_name_in_parent(x))
{
  # There are two cases two test: n is NULL, or n is a vector of natural 
  # numbers.
  if(is.null(n))
  {
    if(has_dims(x))
    {
      return(false("%s does not have NULL dimension.", .xname))
    }
    return(TRUE)
  }
  assert_all_are_non_negative(n)
  assert_all_are_whole_numbers(n)
  dim_x <- dim(x)
  if(!is_of_length(dim_x, length(n)))
  {
    return(false("%s does not have %d dimensions.", .xname, length(n)))
  }
  differences <- dim_x != n
  if(any(differences))
  {
    return(
      false(
        "Dimensions %s of %s are incorrect.", 
        toString(which(differences)), 
        .xname
      )
    )
  }
  TRUE
}

#' @rdname is_empty
#' @export
is_of_length <- function(x, n, .xname = get_name_in_parent(x))
{
  n <- use_first(n)
  assert_all_are_non_negative(n)
  assert_all_are_whole_numbers(n)
  if(length(x) != n)
  {
    return(false("%s does not have length %d.", .xname, n))
  }
  TRUE
}

#' @rdname is_empty
#' @export
is_scalar <- function(x, metric = c("length", "elements"), .xname = get_name_in_parent(x))
{
  metric <- get_metric(metric)
  metric(x, 1L, .xname)
}     
