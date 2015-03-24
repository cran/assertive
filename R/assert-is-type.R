#' @rdname is2  
#' @export
assert_is_all_of <- function(x, classes, .xname = get_name_in_parent(x))
{  
  msg <- gettextf(
    "%s is not in all of the classes %s.", 
    .xname, 
    toString(sQuote(classes))
  )
  assert_engine(x, is2, class = classes, msg = msg)
}

#' @rdname is2  
#' @export
assert_is_any_of <- function(x, classes, .xname = get_name_in_parent(x))
{  
  msg <- gettextf(
    "%s is not in any of the classes %s.", 
    .xname, 
    toString(sQuote(classes))
  )
  assert_engine(x, is2, class = classes, msg = msg, what = "any")
}

#' @rdname is_array
#' @export
assert_is_array <- function(x)
{                                                         
  assert_engine(x, is_array, .xname = get_name_in_parent(x))       
}

#' @rdname is_language
#' @export
assert_is_call <- function(x)
{                                                         
  assert_engine(x, is_call, .xname = get_name_in_parent(x))       
}

#' @rdname is_character
#' @export
assert_is_character <- function(x)
{                                                         
  assert_engine(x, is_character, .xname = get_name_in_parent(x))   
}

#' @rdname is_class
#' @export
assert_all_are_classes <- function(x)
{                                                         
  assert_engine(x, is_class, .xname = get_name_in_parent(x))   
}

#' @rdname is_class
#' @export
assert_any_are_classes <- function(x)
{                                                         
  assert_engine(x, is_class, .xname = get_name_in_parent(x), what = "any")   
}

#' @rdname is_complex
#' @export
assert_is_complex <- function(x)
{                                                         
  assert_engine(x, is_complex, .xname = get_name_in_parent(x))   
}

#' @rdname is_data.frame
#' @export
assert_is_data.frame <- function(x)
{                                                         
  assert_engine(x, is_data.frame, .xname = get_name_in_parent(x))
}

#' @rdname is_environment
#' @export
assert_is_environment <- function(x)
{                                                         
  assert_engine(x, is_environment, .xname = get_name_in_parent(x))     
}

#' @rdname is_language
#' @export
assert_is_expression <- function(x)
{                                                         
  assert_engine(x, is_expression, .xname = get_name_in_parent(x))     
}

#' @rdname is_factor
#' @export
assert_is_factor <- function(x)
{                                                         
  assert_engine(x, is_factor, .xname = get_name_in_parent(x))  
}

#' @rdname is_function
#' @export
assert_is_function <- function(x)
{                                                         
  assert_engine(x, is_function, .xname = get_name_in_parent(x))     
}

#' @rdname is_integer
#' @export
assert_is_integer <- function(x)
{                                                         
  assert_engine(x, is_integer, .xname = get_name_in_parent(x))
}

#' @rdname is_language
#' @export
assert_is_language <- function(x)
{                                                         
  assert_engine(x, is_language, .xname = get_name_in_parent(x))    
}

#' @rdname is_leaf
#' @export
assert_is_leaf <- function(x)
{                                                         
  assert_engine(x, is_leaf, .xname = get_name_in_parent(x))       
}

#' @rdname is_list
#' @export
assert_is_list <- function(x)
{                                                         
  assert_engine(x, is_list, .xname = get_name_in_parent(x))       
}

#' @rdname is_logical
#' @export
assert_is_logical <- function(x)
{                                                         
  assert_engine(x, is_logical, .xname = get_name_in_parent(x))       
}

#' @rdname is_array
#' @export
assert_is_matrix <- function(x)
{                                                         
  assert_engine(x, is_matrix, .xname = get_name_in_parent(x))      
}

#' @rdname is_ts
#' @export
assert_is_mts <- function(x)
{                                                         
  assert_engine(x, is_mts, .xname = get_name_in_parent(x))       
}

#' @rdname is_language
#' @export
assert_is_name <- function(x)
{                                                         
  assert_engine(x, is_name, .xname = get_name_in_parent(x))     
}

#' @rdname is_numeric
#' @export
assert_is_numeric <- function(x)
{                                                         
  assert_engine(x, is_numeric, .xname = get_name_in_parent(x)) 
}

#' @rdname is_factor
#' @export
assert_is_ordered <- function(x)
{                                                         
  assert_engine(x, is_ordered, .xname = get_name_in_parent(x))
}

#' @rdname is_function
#' @export
assert_is_primitive <- function(x)
{                                                         
  assert_engine(x, is_primitive, .xname = get_name_in_parent(x))
}

#' @rdname is_qr
#' @export
assert_is_qr <- function(x)
{                                                         
  assert_engine(x, is_qr, .xname = get_name_in_parent(x))       
}

#' @rdname is_raster
#' @export
assert_is_raster <- function(x)
{                                                         
  assert_engine(x, is_raster, .xname = get_name_in_parent(x))
}

#' @rdname is_raw
#' @export
assert_is_raw <- function(x)
{                                                         
  assert_engine(x, is_raw, .xname = get_name_in_parent(x))
}

#' @rdname is_raw
#' @export
assert_is_relistable <- function(x)
{                                                         
  assert_engine(x, is_relistable, .xname = get_name_in_parent(x))
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
  assert_engine(x, is_s4, .xname = get_name_in_parent(x))
}

#' @rdname is_function
#' @export
assert_is_stepfun <- function(x)
{                                                         
  assert_engine(x, is_stepfun, .xname = get_name_in_parent(x))
}

#' @rdname is_language
#' @export
assert_is_symbol <- assert_is_name

#' @rdname is_table
#' @export
assert_is_table <- function(x)
{                                                         
  assert_engine(x, is_table, .xname = get_name_in_parent(x))       
}

#' @rdname is_ts
#' @export
assert_is_ts <- function(x)
{                                                         
  assert_engine(x, is_ts, .xname = get_name_in_parent(x))       
}

#' @rdname is_ts
#' @export
assert_is_tskernel <- function(x)
{                                                         
  assert_engine(x, is_tskernel, .xname = get_name_in_parent(x))       
}
