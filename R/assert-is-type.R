#' Does x belong to these classes?
#' 
#' Checks to see if x belongs to any of the classes in classes.
#' @param x Input to check.
#' @param classes As for \code{class}. 
#' @return The functions return nothing but throw an error if
#' \code{x} does not have any/all of the class \code{classes}.
#' @seealso \code{\link[assertive.base]{is2}}
#' @examples 
#' assert_is_all_of(1:10, c("integer", "numeric"))
#' #These examples should fail.
#' dont_stop(assert_is_any_of(1:10, c("list", "data.frame")))
#' @export
assert_is_all_of <- function(x, classes)
{  
  msg <- gettextf(
    "%s is not in all of the classes %s.", 
    get_name_in_parent(x), 
    toString(sQuote(classes))
  )
  assert_engine(is2, x, class = classes, msg = msg)
}

#' @rdname assert_is_all_of  
#' @export
assert_is_any_of <- function(x, classes)
{  
  msg <- gettextf(
    "%s is not in any of the classes %s.", 
    get_name_in_parent(x), 
    toString(sQuote(classes))
  )
  assert_engine(is2, x, class = classes, msg = msg, what = "any")
}

#' @rdname is_array
#' @export
assert_is_array <- function(x)
{                                                         
  assert_engine(is_array, x, .xname = get_name_in_parent(x))       
}

#' @rdname is_language
#' @export
assert_is_call <- function(x)
{                                                         
  assert_engine(is_call, x, .xname = get_name_in_parent(x))       
}

#' @rdname is_character
#' @export
assert_is_character <- function(x)
{                                                         
  assert_engine(is_character, x, .xname = get_name_in_parent(x))   
}

#' @rdname is_class
#' @export
assert_all_are_classes <- function(x)
{                                                         
  assert_engine(is_class, x, .xname = get_name_in_parent(x))   
}

#' @rdname is_class
#' @export
assert_any_are_classes <- function(x)
{                                                         
  assert_engine(is_class, x, .xname = get_name_in_parent(x), what = "any")   
}

#' @rdname is_complex
#' @export
assert_is_complex <- function(x)
{                                                         
  assert_engine(is_complex, x, .xname = get_name_in_parent(x))   
}

#' @rdname is_data.frame
#' @export
assert_is_data.frame <- function(x)
{                                                         
  assert_engine(is_data.frame, x, .xname = get_name_in_parent(x))
}

#' @rdname is_environment
#' @export
assert_is_environment <- function(x)
{                                                         
  assert_engine(is_environment, x, .xname = get_name_in_parent(x))     
}

#' @rdname is_language
#' @export
assert_is_expression <- function(x)
{                                                         
  assert_engine(is_expression, x, .xname = get_name_in_parent(x))     
}

#' @rdname is_factor
#' @export
assert_is_factor <- function(x)
{                                                         
  assert_engine(is_factor, x, .xname = get_name_in_parent(x))  
}

#' @rdname is_function
#' @export
assert_is_function <- function(x)
{                                                         
  assert_engine(is_function, x, .xname = get_name_in_parent(x))     
}

#' @rdname is_integer
#' @export
assert_is_integer <- function(x)
{                                                         
  assert_engine(is_integer, x, .xname = get_name_in_parent(x))
}

#' @rdname is_language
#' @export
assert_is_language <- function(x)
{                                                         
  assert_engine(is_language, x, .xname = get_name_in_parent(x))    
}

#' @rdname is_leaf
#' @export
assert_is_leaf <- function(x)
{                                                         
  assert_engine(is_leaf, x, .xname = get_name_in_parent(x))       
}

#' @rdname is_list
#' @export
assert_is_list <- function(x)
{                                                         
  assert_engine(is_list, x, .xname = get_name_in_parent(x))       
}

#' @rdname is_logical
#' @export
assert_is_logical <- function(x)
{                                                         
  assert_engine(is_logical, x, .xname = get_name_in_parent(x))       
}

#' @rdname is_array
#' @export
assert_is_matrix <- function(x)
{                                                         
  assert_engine(is_matrix, x, .xname = get_name_in_parent(x))      
}

#' @rdname is_ts
#' @export
assert_is_mts <- function(x)
{                                                         
  assert_engine(is_mts, x, .xname = get_name_in_parent(x))       
}

#' @rdname is_language
#' @export
assert_is_name <- function(x)
{                                                         
  assert_engine(is_name, x, .xname = get_name_in_parent(x))     
}

#' @rdname is_numeric
#' @export
assert_is_numeric <- function(x)
{                                                         
  assert_engine(is_numeric, x, .xname = get_name_in_parent(x)) 
}

#' @rdname is_factor
#' @export
assert_is_ordered <- function(x)
{                                                         
  assert_engine(is_ordered, x, .xname = get_name_in_parent(x))
}

#' @rdname is_function
#' @export
assert_is_primitive <- function(x)
{                                                         
  assert_engine(is_primitive, x, .xname = get_name_in_parent(x))
}

#' @rdname is_qr
#' @export
assert_is_qr <- function(x)
{                                                         
  assert_engine(is_qr, x, .xname = get_name_in_parent(x))       
}

#' @rdname is_raster
#' @export
assert_is_raster <- function(x)
{                                                         
  assert_engine(is_raster, x, .xname = get_name_in_parent(x))
}

#' @rdname is_raw
#' @export
assert_is_raw <- function(x)
{                                                         
  assert_engine(is_raw, x, .xname = get_name_in_parent(x))
}

#' @rdname is_raw
#' @export
assert_is_relistable <- function(x)
{                                                         
  assert_engine(is_relistable, x, .xname = get_name_in_parent(x))
}

#' @rdname is_s4
#' @export
assert_is_S4 <- function(x)
{                                                         
  .Deprecated("assert_is_s4")
  assert_is_s4(x)
}

#' @rdname is_s4
#' @export
assert_is_s4 <- function(x)
{                                                         
  assert_engine(is_s4, x, .xname = get_name_in_parent(x))
}

#' @rdname is_function
#' @export
assert_is_stepfun <- function(x)
{                                                         
  assert_engine(is_stepfun, x, .xname = get_name_in_parent(x))
}

#' @rdname is_language
#' @export
assert_is_symbol <- assert_is_name

#' @rdname is_table
#' @export
assert_is_table <- function(x)
{                                                         
  assert_engine(is_table, x, .xname = get_name_in_parent(x))       
}

#' @rdname is_ts
#' @export
assert_is_ts <- function(x)
{                                                         
  assert_engine(is_ts, x, .xname = get_name_in_parent(x))       
}

#' @rdname is_ts
#' @export
assert_is_tskernel <- function(x)
{                                                         
  assert_engine(is_tskernel, x, .xname = get_name_in_parent(x))       
}
