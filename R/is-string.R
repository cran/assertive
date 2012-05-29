#' @rdname is_character
#' @export
is_missing_or_empty_character <- function(x)
{ 
  x <- coerce_to(x, "character")
  !nzchar(x) | is_na(x)
}

#' @rdname is_character
#' @export
is_not_missing_nor_empty_character <- function(x)
{ 
  x <- coerce_to(x, "character")
  nzchar(x) & !is_na(x)
}

#' @rdname is_character
#' @export
is_numeric_string <- function(x)
{
  x <- coerce_to(x, "character")
  numx <- suppressWarnings(as.numeric(x))
  ans <- is_not_na(numx)
  names(ans) <- x   #need to take names from x, not numx
  ans
}

#' Is the string a valid variable name?
#'
#' Checks strings to see if they are valid variable names.
#'
#' @param x Input to check.
#' @param allow_reserved If \code{TRUE} then "..." and "..1", "..2", etc. 
#' are considered valid.
#' @param allow_duplicates If \code{TRUE} then duplicated names are allowed.
#' @return \code{TRUE} if the input is a valid variable name.
#' The \code{assert_*} functions return nothing but
#' throw an error if the corresponding \code{is_*} function returns 
#' \code{FALSE}.
#' @seealso \code{\link{make.names}}.
#' @examples
#' assert_all_are_valid_variable_names(c("x", "y_y0.y", ".", "...", "..1"))
#' \dontrun{
#' #These examples should fail:
#' assert_all_are_valid_variable_names(c("...", "..1"), allow_reserved = FALSE) 
#' assert_all_are_valid_variable_names(c("x", "x"), allow_duplicates = FALSE)
#' }
#' @references
#' \url{http://4dpiecharts.com/2011/07/04/testing-for-valid-variable-names/}
#' @export
is_valid_variable_name <- function(x, allow_reserved = TRUE, allow_duplicates = TRUE)
{
  x <- coerce_to(x, "character")
  ok <- rep.int(TRUE, length(x))
  
  #is name too long?
  max_name_length <- if(getRversion() < "2.13.0") 256L else 10000L
  ok[nchar(x) > max_name_length] <- FALSE
  
  #is it a reserved variable, i.e.
  #an ellipsis or two dots then a number?
  if(!allow_reserved)
  {
    ok[x == "..."] <- FALSE
    ok[grepl("^\\.{2}[[:digit:]]+$", x)] <- FALSE
  }
  
  #are names valid (and maybe unique)
  ok[x != make.names(x, unique = !allow_duplicates)] <- FALSE
  
  names(ok) <- x
  ok
}
