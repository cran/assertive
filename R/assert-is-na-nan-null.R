#' @rdname is_finite
#' @export
assert_all_are_finite <- function(x)
{                                                     
  msg <- sprintf("%s are not all finite.", get_name_in_parent(x))
  assert_engine(x, is.finite, msg)        
}

#' @rdname is_finite
#' @export
assert_any_are_finite <- function(x)
{                                                     
  msg <- sprintf("%s are all not finite.", get_name_in_parent(x))
  assert_engine(x, is.finite, msg, what = "any")        
}

#' @rdname is_finite
#' @export
assert_all_are_infinite <- function(x)
{                                                     
  msg <- sprintf("%s are not all infinite.", get_name_in_parent(x))
  assert_engine(x, is.infinite, msg)        
}

#' @rdname is_finite
#' @export
assert_any_are_infinite <- function(x)
{                                                     
  msg <- sprintf("%s are all not infinite.", get_name_in_parent(x))
  assert_engine(x, is.infinite, msg, what = "any")        
}

#' @rdname is_nan
#' @export
assert_all_are_nan <- function(x)
{                                                                
  msg <- sprintf("%s are not all NaN.", get_name_in_parent(x))
  assert_engine(x, is_nan, msg)
}

#' @rdname is_nan
#' @export
assert_any_are_nan <- function(x)
{                                                                
  msg <- sprintf("%s are all not NaN.", get_name_in_parent(x))
  assert_engine(x, is_nan, msg, what = "any")
}

#' @rdname is_na
#' @export
assert_all_are_not_na <- function(x)
{                                                      
  msg <- sprintf("%s contains NAs.", get_name_in_parent(x))
  assert_engine(x, is_not_na, msg)
}

#' @rdname is_na
#' @export
assert_any_are_not_na <- function(x)
{                                                      
  msg <- sprintf("%s are all NA.", get_name_in_parent(x))
  assert_engine(x, is_not_na, msg, what = "any")
}

#' @rdname is_nan
#' @export
assert_all_are_not_nan <- function(x)
{                                                      
  msg <- sprintf("%s contains NaNs.", get_name_in_parent(x))
  assert_engine(x, is_not_nan, msg)
}

#' @rdname is_nan
#' @export
assert_any_are_not_nan <- function(x)
{                                                      
  msg <- sprintf("%s are all NaN.", get_name_in_parent(x))
  assert_engine(x, is_not_nan, msg, what = "any")
}

#' @rdname is_null
#' @export
assert_is_not_null <- function(x)
{                                                      
  assert_engine(x, is_not_null, .xname = get_name_in_parent(x))   
}

#' @rdname is_null
#' @export
assert_is_null <- function(x)
{                                                         
  
  assert_engine(x, is_null, .xname = get_name_in_parent(x))       
}
