#' Does the input have the specified attributes?
#'
#' Checks to see if the input has the specifed attributes.
#'
#' @param x Input to check.
#' @param attrs Desired attributes.
#' @param .xname Not intended to be used directly.
#' @return \code{has_attributes} returns \code{TRUE} where \code{x} has
#' the attributes specified in \code{attrs}. \code{assert_has_terms} returns nothing but throws
#' an error if \code{has_terms} is not \code{TRUE}.
#' @seealso \code{\link[stats]{terms.default}}.
#' @examples
#' x <- structure(c(a = 1), b = 2)
#' assert_has_all_attributes(x, c("names", "b"))
#' assert_has_any_attributes(x, c("names", "c"))
#' \dontrun{
#' assert_has_all_attributes(x, c("names", "c"))
#' }
#' @export
has_attributes <- function(x, attrs, .xname = get_name_in_parent(x))
{
  if(is_empty(attrs)) return(logical())
  vapply(
    attrs,
    function(at) is_not_null(attr(x, at)),
    logical(1)
  )
}

#' Does the input have any attributes?
#'
#' Checks to see if the input has any attributes.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{has_terms} returns \code{TRUE} if \code{attributes(x)}
#' has length zero. \code{assert_has_terms} returns nothing but throws
#' an error if \code{has_terms} is not \code{TRUE}.
#' @seealso \code{\link[stats]{terms.default}}.
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
#' @param x Input to check.
#' @return \code{has_arg} wraps \code{hasArg}, providing more
#' information on failure.  
#' @note There is currently no corresponding \code{assert_has_arg}
#' function, because evaluating in the correct call is hard.
#' @seealso \code{\link{ncol}}.
#' @examples
#' assert_has_rows(data.frame(x = 1:10))
#' assert_has_cols(matrix())
#' @export
has_arg <- function(x)
{
  ok <- eval.parent(substitute(hasArg(name), list(name = x)))
  if(!ok) return(false("The argument doesn't exist in the current call."))
  TRUE
}

#' @rdname has_names
#' @export
has_colnames <- function(x)
{
  colnamesx <- colnames(x)
  if(is.null(colnamesx)) return(false("Column names are NULL."))
  if(!any(nzchar(colnamesx))) return(false("Column names are all empty."))
  TRUE
} 

#' Does the input have rows/columns?
#'
#' Checks to see if the input has rows/columns.
#'
#' @param x Input to check.
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
has_cols <- function(x)
{
  ncolx <- ncol(x)
  if(is.null(ncolx)) return(false("Number of columns is NULL."))
  if(ncolx == 0L) return(false("Number of columns is zero."))
  TRUE
} 

#' @rdname has_names
#' @export
has_dimnames <- function(x)
{
  dimnamesx <- dimnames(x)
  if(is.null(dimnamesx)) return(false("Dimension names are NULL."))
  if(!any(nzchar(unlist(dimnamesx, use.names = FALSE)))) 
  {
    return(false("Dimension names are all empty."))
  }
  TRUE
} 

#' Does the input have dimensions?
#'
#' Checks to see if the input has dimensions.
#'
#' @param x Input to check.
#' @return \code{has_dims} returns\code{TRUE} if \code{dim} is non-null.
#' \code{assert_has_dims} returns nothing but throws an error if
#' \code{has_dims} is not \code{TRUE}.
#' @seealso \code{\link{dim}}.
#' @export
has_dims <- function(x)
{
  dimx <- dim(x)
  if(is.null(dimx)) return(false("Dimensions are NULL."))
  TRUE
}
    
#' @rdname has_duplicates
#' @export
has_no_duplicates <- function(x)
{
  if(anyDuplicated(x)) return(false("There are duplicates."))
  TRUE
} 

#' Does the input have duplicates?
#'
#' Checks to see if the input has duplicates.
#'
#' @param x Input to check.
#' @return \code{has_duplicates} returns \code{TRUE} if\code{anyDuplicated} 
#' is \code{TRUE}.  \code{assert_has_duplicates} returns nothing but 
#' throws an error if \code{has_duplicates} is not \code{TRUE}. 
#' \code{has_no_duplicates} is the negation of \code{has_duplicates}.
##' @seealso \code{\link{anyDuplicated}}.
#' @export
has_duplicates <- function(x)
{
  !has_no_duplicates(x)
}

#' Does the input have names?
#'
#' Checks to see if the input has (row/column/dimension) names.
#'
#' @param x Input to check.
#' @return \code{has_names} returns \code{TRUE} if \code{names} is 
#' non-null. 
#' \code{has_rownames}, \code{has_colnames} and \code{has_dimnames} work
#' in a similar fashion, checking the corresponding attributes.
#' \code{assert_has_names} returns nothing but throws an error if 
#' \code{has_names} is not \code{TRUE}.
#' @note Empty names (i.e., \code{""}) are not allowed in R, and are 
#' not checked here.
#' @seealso \code{\link[base]{names}}, \code{\link[base]{rownames}}, \code{\link[base]{colnames}}, \code{\link[base]{dimnames}}.
#' @examples
#' assert_has_names(c(a = 1, 2))
#' dfr <- data.frame(x = 1:5)
#' assert_has_rownames(dfr)
#' assert_has_colnames(dfr)
#' assert_has_dimnames(dfr)
#' @export
has_names <- function(x)
{
  namesx <- names(x)
  if(is.null(namesx)) return(false("Names are NULL."))
  TRUE
} 

#' @rdname has_names
#' @export
has_rownames <- function(x)
{
  rownamesx <- rownames(x)
  if(is.null(rownamesx)) return(false("Row names are NULL."))
  if(!any(nzchar(rownamesx))) return(false("Row names are all empty."))
  TRUE
} 

#' @rdname has_cols
#' @export
has_rows <- function(x)
{
  nrowx <- nrow(x)
  if(is.null(nrowx)) return(false("Number of rows is NULL."))  
  if(nrowx == 0L) return(false("Number of rows is zero."))
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
#' @seealso \code{\link[stats]{terms.default}}.
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
