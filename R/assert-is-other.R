#' @rdname is_divisible_by
#' @export
assert_all_are_divisible_by <- function(x, n, tol = 100 * .Machine$double.eps, 
  na_ignore = FALSE)
{  
  msg <- gettextf(
    "%s are not all divisible by %s (tol = %g).", 
    get_name_in_parent(x), 
    toString(n, width = 20),
    tol
  )
  assert_engine(
    is_divisible_by, 
    x, 
    n = n, 
    tol = tol, 
    msg = msg, 
    na_ignore = na_ignore
  )  
}

#' @rdname is_divisible_by
#' @export
assert_any_are_divisible_by <- function(x, n, tol = 100 * .Machine$double.eps, 
  na_ignore = FALSE)
{  
  msg <- gettextf(
    "%s are all not divisible by %s (tol = %g).", 
    get_name_in_parent(x), 
    toString(n, width = 20),
    tol
  )
  assert_engine(
    is_divisible_by, 
    x, 
    n = n, 
    tol = tol, 
    msg = msg,
    what = "any",
    na_ignore = na_ignore
  )   
}

#' @rdname is_divisible_by
#' @export
assert_all_are_even <- function(x, tol = 100 * .Machine$double.eps, 
  na_ignore = FALSE)
{  
  msg <- gettextf(
    "%s are not all even (tol = %g).", 
    get_name_in_parent(x),
    tol
  )
  assert_engine(
    is_even, 
    x, 
    tol = tol,
    msg = msg, 
    na_ignore = na_ignore
  )  
}

#' @rdname is_divisible_by
#' @export
assert_any_are_even <- function(x, tol = 100 * .Machine$double.eps, 
  na_ignore = FALSE)
{  
  msg <- gettextf(
    "%s are all not even (tol = %g).", 
    get_name_in_parent(x),
    tol
  )
  assert_engine(
    is_even, 
    x, 
    tol = tol,
    msg = msg, 
    what = "any",
    na_ignore = na_ignore
  ) 
}

#' @rdname is_existing
#' @export
assert_all_are_existing <- function(
  x, 
  envir = parent.frame(),
  inherits = TRUE
)
{    
  msg <- gettextf("%s do not all exist.", get_name_in_parent(x))
  assert_engine(
    is_existing, 
    x,
    envir = envir,
    inherits = inherits, 
    msg = msg
  )       
}

#' @rdname is_existing
#' @export
assert_any_are_existing <- function(
  x, 
  envir = parent.frame(), 
  inherits = TRUE
)
{    
  msg <- gettextf("%s all do not exist.", get_name_in_parent(x))
  assert_engine(
    is_existing, 
    x, 
    envir = envir,
    inherits = inherits,
    msg = msg,
    what = "any"
  )       
}

#' @rdname is_debugged
#' @export
assert_is_debugged <- function(x)
{                                                         
  assert_engine(is_debugged, x, .xname = get_name_in_parent(x))       
}

# ' @rdname is_generic
# ' @export
# assert_is_generic <- function(x)
# {                                                     
#   msg <- gettextf("%s is not a generic function.", get_name_in_parent(x))
#   assert_engine(is_generic, x, msg = msg)        
# }

#' @rdname is_if_condition
#' @export
assert_is_if_condition <- function(x)
{
  assert_engine(is_if_condition, x, .xname = get_name_in_parent(x))
}

#' @rdname is_loaded
#' @export
assert_is_loaded <- function(x)
{                                                         
  assert_engine(is_loaded, x, .xname = get_name_in_parent(x))       
}

#' @rdname is_divisible_by
#' @export
assert_all_are_odd <- function(x, tol = 100 * .Machine$double.eps, 
  na_ignore = FALSE)
{  
  msg <- gettextf(
    "%s are not all odd (tol = %g).",
    get_name_in_parent(x),
    tol
  )
  assert_engine(
    is_odd, 
    x, 
    tol = tol,
    msg = msg, 
    na_ignore = na_ignore
  )  
}

#' @rdname is_divisible_by
#' @export
assert_any_are_odd <- function(x, tol = 100 * .Machine$double.eps, 
  na_ignore = FALSE)
{  
  msg <- gettextf(
    "%s are all not odd (tol = %g).", 
    get_name_in_parent(x),
    tol
  )  
  assert_engine(
    is_odd, 
    x, 
    tol = tol,
    msg = msg, 
    what = "any",
    na_ignore = na_ignore
  )
}

#' @rdname is_symmetric_matrix
#' @export
assert_is_symmetric_matrix <- function(x, tol = 100 * .Machine$double.eps, ...)
{                                                         
  assert_engine(
    is_symmetric_matrix, 
    x, 
    tol = tol, 
    .xname = get_name_in_parent(x),
    ...
  )       
}

#' @rdname is_unsorted
#' @export
assert_is_unsorted <- function(x, na.rm = FALSE, strictly = FALSE)
{                                                         
  assert_engine(
    is_unsorted, 
    x,
    na.rm = na.rm,
    strictly = strictly, 
    .xname = get_name_in_parent(x)
  )       
}

#' @rdname is_whole_number
#' @export
assert_all_numbers_are_whole_numbers <- function(x,
  tol = 100 * .Machine$double.eps, na_ignore = FALSE)
{                                                       
  .Deprecated("assert_all_are_whole_numbers")
  assert_all_are_whole_numbers(x, tol, na_ignore = na_ignore)
}

#' @rdname is_whole_number
#' @export
assert_any_numbers_are_whole_numbers <- function(x, 
  tol = 100 * .Machine$double.eps, na_ignore = FALSE)
{                                                      
  .Deprecated("assert_any_are_whole_numbers")
  assert_any_are_whole_numbers(x, tol, na_ignore = na_ignore)  
}

#' @rdname is_whole_number
#' @export
assert_all_are_whole_numbers <- function(x,
  tol = 100 * .Machine$double.eps, na_ignore = FALSE)
{                                                
  msg <- gettextf(
    "%s are not all whole numbers (tol = %g).", 
    get_name_in_parent(x),
    tol
  )
  assert_engine(
    is_whole_number, 
    x, 
    tol = tol,
    msg = msg, 
    na_ignore = na_ignore
  ) 
}

#' @rdname is_whole_number
#' @export
assert_any_are_whole_numbers <- function(x, 
  tol = 100 * .Machine$double.eps, na_ignore = FALSE)
{                                             
  msg <- gettextf(
    "%s are not all whole numbers (tol = %g).", 
    get_name_in_parent(x),
    tol
  )
  assert_engine(
    is_whole_number, 
    x, 
    tol = tol,
    msg = msg,
    what = "any",
    na_ignore = na_ignore
  )
}
