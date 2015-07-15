#' Does the input have any attributes?
#'
#' Checks to see if the input has any attributes.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{has_terms} returns \code{TRUE} if \code{attributes(x)}
#' has length zero. \code{assert_has_terms} returns nothing but throws
#' an error if \code{has_terms} is not \code{TRUE}.
#' @examples
#' assert_has_terms(lm(uptake ~ conc, CO2))
#' @export
has_any_attributes <- function(x, .xname = get_name_in_parent(x))
{
  if(!is_empty(attributes(x)))
  {
    return(false("%s has attributes.", .xname))
  }
  TRUE
}

#' Does the current call have an argument?
#'
#' Checks to see if the current call has an argument with 
#' the name given in the input.
#'
#' @param x Argument to check. 
#' @param fn Function to find the argument in.
#' @return \code{has_arg} reimplements \code{\link[methods]{hasArg}}, 
#' letting you choose the function to search in, and providing more
#' information on failure.  
#' @note There is currently no corresponding \code{assert_has_arg}
#' function, because evaluating in the correct call is hard.
#' @seealso \code{\link[methods]{hasArg}}.
#' @examples
#' has_arg(x, mean.default)
#' has_arg(y, mean.default)   
#' f <- function(...) has_arg(z)   
#' f(z = 123)
#' f(123)
#' @importFrom methods formalArgs
#' @export
has_arg <- function(x, fn = sys.function(sys.parent()))
{
  arg_name <- get_name_in_parent(x)
  formal_args_of_fn <- formalArgs(fn)
  if(!arg_name %in% formal_args_of_fn)
  {                             
    fn_name <- get_name_in_parent(fn)
    fail <- false(
      "%s is not an argument of %s", 
      sQuote(arg_name), 
      sQuote(fn_name)
    )
    if("..." %in% formal_args_of_fn)
    {
      dots_call <- eval(quote(substitute(list(...))), sys.parent())
      if(!arg_name %in% names(dots_call))
      {
         return(fail)
      }
    } else
    {
       return(fail)
    }
  }
  TRUE
}

#' Does the input have the specified attributes?
#'
#' Checks to see if the input has the specifed attributes.
#'
#' @param x Input to check.
#' @param attrs Desired attributes.
#' @param .xname Not intended to be used directly.
#' @return \code{has_attributes} returns \code{TRUE} where \code{x} has
#' the attributes specified in \code{attrs}. \code{assert_has_terms} returns 
#' nothing but throws an error if \code{has_terms} is not \code{TRUE}.
#' @examples
#' x <- structure(c(a = 1), b = 2)
#' assert_has_all_attributes(x, c("names", "b"))
#' assert_has_any_attributes(x, c("names", "c"))
#' #These examples should fail.
#' dont_stop(assert_has_all_attributes(x, c("names", "c")))
#' @export
has_attributes <- function(x, attrs, .xname = get_name_in_parent(x))
{
  if(is_empty(attrs)) return(logical())
  bapply(
    attrs,
    function(at) is_not_null(attr(x, at))
  )
}

#' @rdname has_names
#' @export
has_colnames <- function(x, .xname = get_name_in_parent(x))
{
  colnamesx <- colnames(x)
  if(is.null(colnamesx)) 
  {
    return(false("The column names of %s are NULL.", .xname))
  }
  if(!any(nzchar(colnamesx))) 
  {
    return(false("The column names of %s are all empty.", .xname))
  }
  TRUE
} 

#' Does the input have rows/columns?
#'
#' Checks to see if the input has rows/columns.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{has_rows} and \code{has_cols} return \code{TRUE} if 
#' \code{nrow} and \code{ncol} respectively return a value that is 
#' non-null and positive.  The \code{assert_*} functions return nothing 
#' but throw an error if the corresponding \code{has_*} function returns
#' \code{FALSE}.
#' @seealso \code{\link{ncol}}.
#' @examples
#' assert_has_rows(data.frame(x = 1:10))
#' assert_has_cols(matrix())
#' @export
has_cols <- function(x, .xname = get_name_in_parent(x))
{
  ncolx <- ncol(x)
  if(is.null(ncolx)) 
  {
    return(false("The number of columns in %s is NULL.", .xname))  
  }
  if(ncolx == 0L) 
  {
    return(false("The number of columns in %s is zero.", .xname))
  }
  TRUE
} 

#' @rdname has_names
#' @export
has_dimnames <- function(x, .xname = get_name_in_parent(x))
{
  dimnamesx <- dimnames(x)
  if(is.null(dimnamesx)) 
  {
    return(false("The dimension names of %s are NULL.", .xname))
  }
  if(!any(nzchar(unlist(dimnamesx, use.names = FALSE)))) 
  {
    return(false("The dimension names of %s are all empty.", .xname))
  }
  TRUE
} 

#' Does the input have dimensions?
#'
#' Checks to see if the input has dimensions.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{has_dims} returns\code{TRUE} if \code{dim} is non-null.
#' \code{assert_has_dims} returns nothing but throws an error if
#' \code{has_dims} is not \code{TRUE}.
#' @seealso \code{\link[base]{dim}}, \code{\link{is_of_dimension}}.
#' @export
has_dims <- function(x, .xname = get_name_in_parent(x))
{
  dim_x <- dim(x)
  if(is.null(dim_x)) 
  {
    return(false("The dimensions of %s are NULL.", .xname))
  }
  TRUE
}

#' Does the input have duplicates?
#'
#' Checks to see if the input has duplicates.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{has_duplicates} returns \code{TRUE} if\code{anyDuplicated} 
#' is \code{TRUE}.  \code{assert_has_duplicates} returns nothing but 
#' throws an error if \code{has_duplicates} is not \code{TRUE}. 
#' \code{has_no_duplicates} is the negation of \code{has_duplicates}.
##' @seealso \code{\link{anyDuplicated}}.
#' @export
has_duplicates <- function(x, .xname = get_name_in_parent(x))
{
  if(!anyDuplicated(x)) 
  {
    return(false("%s has no duplicates.", .xname))
  }
  TRUE
}

#' Does the input have names?
#'
#' Checks to see if the input has (row/column/dimension) names.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{has_names} returns \code{TRUE} if \code{names} is 
#' non-null. 
#' \code{has_rownames}, \code{has_colnames} and \code{has_dimnames} work
#' in a similar fashion, checking the corresponding attributes.
#' \code{assert_has_names} returns nothing but throws an error if 
#' \code{has_names} is not \code{TRUE}.
#' @note Empty names (i.e., \code{""}) are not allowed in R, and are 
#' not checked here.
#' @seealso \code{\link[base]{names}}, \code{\link[base]{rownames}}, 
#' \code{\link[base]{colnames}}, \code{\link[base]{dimnames}}.
#' @examples
#' assert_has_names(c(a = 1, 2))
#' dfr <- data.frame(x = 1:5)
#' assert_has_rownames(dfr)
#' assert_has_colnames(dfr)
#' assert_has_dimnames(dfr)
#' @export
has_names <- function(x, .xname = get_name_in_parent(x))
{
  namesx <- names(x)
  if(is.null(namesx)) 
  {
    return(false("The names of %s are NULL.", .xname))
  }
  if(!any(nzchar(namesx))) 
  {
    return(false("The names of %s are all empty.", .xname))
  }
  TRUE
} 

#' @rdname has_duplicates
#' @export
has_no_duplicates <- function(x, .xname = get_name_in_parent(x))
{
  if(anyDuplicated(x)) 
  {
    return(false("%s has duplicates.", .xname))
  }
  TRUE
}

#' @rdname has_names
#' @export
has_rownames <- function(x, .xname = get_name_in_parent(x))
{
  rownamesx <- rownames(x)
  if(is.null(rownamesx)) 
  {
    return(false("The row names of %s are NULL.", .xname))
  }
  if(!any(nzchar(rownamesx))) 
  {
    return(false("The row names of %s are all empty.", .xname))
  }
  TRUE
} 

#' @rdname has_cols
#' @export
has_rows <- function(x, .xname = get_name_in_parent(x))
{
  nrowx <- nrow(x)
  if(is.null(nrowx)) 
  {
    return(false("The number of rows in %s is NULL.", .xname))  
  }
  if(nrowx == 0L) 
  {
    return(false("The number of rows in %s is zero.", .xname))
  }
  TRUE
} 

#' Does the input have terms?
#'
#' Checks to see if the input has a terms component or attribute.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{has_terms} returns \code{TRUE} if the input has an 
#' element or an attribute named terms. \code{assert_has_terms} returns 
#' nothing but throws an error if \code{has_terms} is not \code{TRUE}.
#' @seealso \code{\link{terms}}.
#' @examples
#' assert_has_terms(lm(uptake ~ conc, CO2))
#' @export
has_terms <- function(x, .xname = get_name_in_parent(x))
{
  if(
    is.null(attr(x, "terms")) && 
    (is.atomic(x) || is.null(x$terms))
  )
  {
    return(false("%s has no terms component nor attribute.", .xname))
  }
  TRUE
}
