#' Set comparisons
#' 
#' Checks on the contents of two vectors (ignoring the order of the elements).
#' @param x A vector.
#' @param y Another vector.
#' @param .xname Not intended to be used directly.
#' @param .yname Not intended to be used directly.
#' @return The \code{is_*} functions return \code{TRUE} or \code{FALSE}.
#' The \code{assert_*} functions throw an error in the event of failure.
#' @seealso \code{\link{is_subset}}, \code{\link[base]{sets}}, 
#' \code{\link[sets]{set_is_equal}}
#' @examples
#' # Same contents, different order, returns TRUE
#' is_set_equal(1:5, 5:1)
#' # Different lengths
#' is_set_equal(1:5, 1:6)
#' # First vector contains values not in second vector
#' is_set_equal(1:5, c(1:4, 4))
#' # Second vector contains values not in first vector
#' is_set_equal(c(1:4, 4), 1:5)
#' 
#' # Is x a subset of y?
#' is_subset(1:4, 1:5)
#' is_subset(1:5, 1:4)
#' 
#' # Is x a superset of y?
#' is_superset(1:5, 1:4)
#' is_superset(1:4, 1:5)
#' 
#' # Errors are thrown in the event of failure
#' assert_are_set_equal(1:5, 5:1)
#' dont_stop(assert_are_set_equal(1:5, 1:6))
#' 
#' assert_is_subset(1:4, 1:5)
#' dont_stop(assert_is_subset(1:5, 1:4))
#' 
#' assert_is_superset(1:5, 1:4)
#' dont_stop(assert_is_superset(1:4, 1:5))
#' @export
is_set_equal <- function(x, y, .xname = get_name_in_parent(x), .yname = get_name_in_parent(y))
{
  if(length(x) != length(y))
  {
    return(false("%s and %s have different lengths.", .xname, .yname))
  }
  if(!(ok <- is_subset(x, y, .xname, .yname)))
  {
    return(ok)
  }  
  if(!(ok <- is_subset(y, x, .yname, .xname)))
  {
    return(ok)
  }  
  TRUE
}

#' @rdname is_set_equal
#' @export
is_subset <- function(x, y, .xname = get_name_in_parent(x), .yname = get_name_in_parent(y))
{
  diffxy <- setdiff(x, y)
  if(is_non_empty(diffxy))
  {
    return(
      false(
        "The %s %s in %s %s not in %s.", 
        toString(sQuote(diffxy), 100),
        ngettext(length(diffxy), "element", "elements"),
        .xname, 
        ngettext(length(diffxy), "is", "are"),
        .yname
      )
    )
  }   
  TRUE
}

#' @rdname is_set_equal
#' @export
is_superset <- function(x, y, .xname = get_name_in_parent(x), .yname = get_name_in_parent(y))
{
  is_subset(y, x, .yname, .xname)
}
