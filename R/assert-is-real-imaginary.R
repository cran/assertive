#' @rdname is_real
#' @export
assert_all_are_imaginary <- function(x, tol = 100 * .Machine$double.eps, 
  na_ignore = FALSE)
{                                                     
  msg <- gettextf(
    "%s are not all imaginary (tol = %g).", 
    get_name_in_parent(x), 
    tol
  )
  assert_engine(
    is_imaginary, 
    x, 
    tol = tol, 
    msg = msg, 
    na_ignore = na_ignore
  )        
}

#' @rdname is_real
#' @export
assert_any_are_imaginary <- function(x, tol = 100 * .Machine$double.eps, 
  na_ignore = FALSE)
{                                                     
  msg <- gettextf(
    "%s are all not imaginary (tol = %g).", 
    get_name_in_parent(x), 
    tol
  )
  assert_engine(
    is_imaginary, 
    x, 
    tol = tol, 
    msg = msg, 
    what = "any", 
    na_ignore = na_ignore
  )        
}

#' @rdname is_real
#' @export
assert_all_are_real <- function(x, tol = 100 * .Machine$double.eps, 
  na_ignore = FALSE)
{                                                     
  msg <- gettextf(
    "%s are not all real (tol = %g).", 
    get_name_in_parent(x), 
    tol
  )
  assert_engine(
    is_real, 
    x, 
    tol = tol, 
    msg = msg, 
    na_ignore = na_ignore
  )
}

#' @rdname is_real
#' @export
assert_any_are_real <- function(x, tol = 100 * .Machine$double.eps, 
  na_ignore = FALSE)
{                                                     
  msg <- gettextf("%s are all not real (tol = %g).", get_name_in_parent(x), tol)
  assert_engine(
    is_real, 
    x, 
    tol = tol, 
    msg = msg, 
    what = "any", 
    na_ignore = na_ignore
  )
}
