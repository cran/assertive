#' @rdname is_real
#' @export
assert_all_are_imaginary <- function(x, tol = 100 * .Machine$double.eps)
{                                                     
  msg <- gettextf("%s are not all imaginary.", get_name_in_parent(x))
  assert_engine(x, is_imaginary, msg, tol = tol)        
}

#' @rdname is_real
#' @export
assert_any_are_imaginary <- function(x, tol = 100 * .Machine$double.eps)
{                                                     
  msg <- gettextf("%s are all not imaginary.", get_name_in_parent(x))
  assert_engine(x, is_imaginary, msg, what = "any", tol = tol)        
}

#' @rdname is_real
#' @export
assert_all_are_real <- function(x, tol = 100 * .Machine$double.eps)
{                                                     
  msg <- gettextf("%s are not all real.", get_name_in_parent(x))
  assert_engine(x, is_real, msg, tol = tol)        
}

#' @rdname is_real
#' @export
assert_any_are_real <- function(x, tol = 100 * .Machine$double.eps)
{                                                     
  msg <- gettextf("%s are all not real.", get_name_in_parent(x))
  assert_engine(x, is_real, msg, what = "any", tol = tol)        
}
