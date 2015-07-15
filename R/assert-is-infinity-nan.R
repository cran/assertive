#' @rdname is_finite
#' @export
assert_all_are_finite <- function(x)
{                                                     
  msg <- gettextf("%s are not all finite.", get_name_in_parent(x))
  assert_engine(is_finite, x, msg = msg)        
}

#' @rdname is_finite
#' @export
assert_any_are_finite <- function(x)
{                                                     
  msg <- gettextf("%s are all not finite.", get_name_in_parent(x))
  assert_engine(is_finite, x, msg = msg, what = "any")        
}

#' @rdname is_finite
#' @export
assert_all_are_infinite <- function(x)
{                                                     
  msg <- gettextf("%s are not all infinite.", get_name_in_parent(x))
  assert_engine(is_infinite, x, msg = msg)        
}

#' @rdname is_finite
#' @export
assert_any_are_infinite <- function(x)
{                                                     
  msg <- gettextf("%s are all not infinite.", get_name_in_parent(x))
  assert_engine(is_infinite, x, msg = msg, what = "any")        
}

#' @rdname is_nan
#' @export
assert_all_are_nan <- function(x)
{                                                                
  msg <- gettextf("%s are not all NaN.", get_name_in_parent(x))
  assert_engine(is_nan, x, msg = msg)
}

#' @rdname is_nan
#' @export
assert_any_are_nan <- function(x)
{                                                                
  msg <- gettextf("%s are all not NaN.", get_name_in_parent(x))
  assert_engine(is_nan, x, msg = msg, what = "any")
}

#' @rdname is_finite
#' @export
assert_all_are_negative_infinity <- function(x)
{                                                     
  msg <- gettextf("%s are not all negative infinity.", get_name_in_parent(x))
  assert_engine(is_negative_infinity, x, msg = msg)        
}

#' @rdname is_finite
#' @export
assert_any_are_negative_infinity <- function(x)
{                                                     
  msg <- gettextf("%s are all not negative infinity.", get_name_in_parent(x))
  assert_engine(is_negative_infinity, x, msg = msg, what = "any")        
}

#' @rdname is_nan
#' @export
assert_all_are_not_nan <- function(x)
{                                                      
  msg <- gettextf("%s contains NaNs.", get_name_in_parent(x))
  assert_engine(is_not_nan, x, msg = msg)
}

#' @rdname is_nan
#' @export
assert_any_are_not_nan <- function(x)
{                                                      
  msg <- gettextf("%s are all NaN.", get_name_in_parent(x))
  assert_engine(is_not_nan, x, msg = msg, what = "any")
}

#' @rdname is_finite
#' @export
assert_all_are_positive_infinity <- function(x)
{                                                     
  msg <- gettextf("%s are not all positive infinity.", get_name_in_parent(x))
  assert_engine(is_positive_infinity, x, msg = msg)        
}

#' @rdname is_finite
#' @export
assert_any_are_positive_infinity <- function(x)
{                                                     
  msg <- gettextf("%s are all not positive infinity.", get_name_in_parent(x))
  assert_engine(is_positive_infinity, x, msg = msg, what = "any")        
}
