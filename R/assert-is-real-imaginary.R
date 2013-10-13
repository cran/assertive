#' @rdname is_real
#' @export
assert_all_are_imaginary <- function(x)
{                                                     
  msg <- sprintf("%s are not all imaginary.", get_name_in_parent(x))
  assert_engine(x, is_imaginary, msg)        
}

#' @rdname is_real
#' @export
assert_any_are_imaginary <- function(x)
{                                                     
  msg <- sprintf("%s are all not imaginary.", get_name_in_parent(x))
  assert_engine(x, is_imaginary, msg, what = "any")        
}

#' @rdname is_real
#' @export
assert_all_are_real <- function(x)
{                                                     
  msg <- sprintf("%s are not all real.", get_name_in_parent(x))
  assert_engine(x, is_real, msg)        
}

#' @rdname is_real
#' @export
assert_any_are_real <- function(x)
{                                                     
  msg <- sprintf("%s are all not real.", get_name_in_parent(x))
  assert_engine(x, is_real, msg, what = "any")        
}
