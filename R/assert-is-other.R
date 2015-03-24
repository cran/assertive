#' @rdname is_divisible_by
#' @export
assert_all_are_divisible_by <- function(x, n, tol = 100 * .Machine$double.eps)
{  
  msg <- gettextf("%s are not all divisible by %s.", get_name_in_parent(x), toString(n, width = 20))
  assert_engine(x, is_divisible_by, msg, n = n, tol = tol)  
}

#' @rdname is_divisible_by
#' @export
assert_any_are_divisible_by <- function(x, n, tol = 100 * .Machine$double.eps)
{  
  msg <- gettextf("%s are all not divisible by %s.", get_name_in_parent(x), toString(n, width = 20))
  assert_engine(
    x, is_divisible_by, msg, what = "any", n = n, tol = tol
  )  
}

#' @rdname is_divisible_by
#' @export
assert_all_are_even <- function(x, tol = 100 * .Machine$double.eps)
{  
  msg <- gettextf("%s are not all even.", get_name_in_parent(x))
  assert_engine(x, is_even, msg, tol = tol)  
}

#' @rdname is_divisible_by
#' @export
assert_any_are_even <- function(x, tol = 100 * .Machine$double.eps)
{  
  msg <- gettextf("%s are all not even.", get_name_in_parent(x))
  assert_engine(x, is_even, msg, what = "any", tol = tol)  
}

#' @rdname is_existing
#' @export
assert_all_are_existing <- function(
  x, 
  where = -1, 
  envir = if (missing(frame)) as.environment(where) else sys.frame(frame), 
  frame, 
  mode = "any", 
  inherits = TRUE
)
{    
  msg <- gettextf("%s do not all exist.", get_name_in_parent(x))
  assert_engine(
    x, 
    is_existing, 
    msg = msg,
    where = where,
    envir = envir,
    frame = frame,
    mode = mode,
    inherits = inherits
  )       
}

#' @rdname is_existing
#' @export
assert_any_are_existing <- function(
  x, 
  where = -1, 
  envir = if (missing(frame)) as.environment(where) else sys.frame(frame), 
  frame, 
  mode = "any", 
  inherits = TRUE
)
{    
  msg <- gettextf("%s all do not exist.", get_name_in_parent(x))
  assert_engine(
    x, 
    is_existing, 
    msg = msg,
    what = "any",
    where = where,
    envir = envir,
    frame = frame,
    mode = mode,
    inherits = inherits
  )       
}

#' @rdname is_divisible_by
#' @export
assert_all_are_odd <- function(x, tol = 100 * .Machine$double.eps)
{  
  msg <- gettextf("%s are not all odd.", get_name_in_parent(x))
  assert_engine(x, is_odd, msg, tol = tol)  
}

#' @rdname is_divisible_by
#' @export
assert_any_are_odd <- function(x, tol = 100 * .Machine$double.eps)
{  
  msg <- gettextf("%s are all not odd.", get_name_in_parent(x))
  assert_engine(x, is_odd, msg, what = "any", tol = tol)  
}

#' @rdname is_debugged
#' @export
assert_is_debugged <- function(x)
{                                                         
  assert_engine(x, is_debugged, .xname = get_name_in_parent(x))       
}

# ' @rdname is_generic
# ' @export
# assert_is_generic <- function(x)
# {                                                     
#   msg <- gettextf("%s is not a generic function.", get_name_in_parent(x))
#   assert_engine(x, is_generic, msg)        
# }

#' @rdname is_loaded
#' @export
assert_is_loaded <- function(x)
{                                                         
  assert_engine(x, is_loaded, .xname = get_name_in_parent(x))       
}

#' @rdname is_divisible_by
#' @export
assert_all_are_odd <- function(x, n, tol = 100 * .Machine$double.eps)
{  
  msg <- gettextf("%s are not all odd.", get_name_in_parent(x), toString(n, width = 20))
  assert_engine(x, is_odd, msg, tol = tol)  
}

#' @rdname is_divisible_by
#' @export
assert_any_are_odd <- function(x, n, tol = 100 * .Machine$double.eps)
{  
  msg <- gettextf("%s are all not odd.", get_name_in_parent(x), toString(n, width = 20))
  assert_engine(
    x, is_odd, msg, what = "any", tol = tol
  )  
}

#' @rdname is_symmetric_matrix
#' @export
assert_is_symmetric_matrix <- function(x, tol = 100 * .Machine$double.eps, ...)
{                                                         
  assert_engine(
    x, 
    is_symmetric_matrix, 
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
    x, 
    is_unsorted, 
    .xname = get_name_in_parent(x),
    na.rm = na.rm,
    strictly = strictly
  )       
}

#' @rdname is_whole_number
#' @export
assert_all_numbers_are_whole_numbers <- function(x,
  tol = 100 * .Machine$double.eps)
{                                                       
  .Deprecated("assert_all_are_whole_numbers")
  assert_all_are_whole_numbers(x, tol)
}

#' @rdname is_whole_number
#' @export
assert_any_numbers_are_whole_numbers <- function(x, 
  tol = 100 * .Machine$double.eps)
{                                                      
  .Deprecated("assert_any_are_whole_numbers")
  assert_any_are_whole_numbers(x, tol)  
}

#' @rdname is_whole_number
#' @export
assert_all_are_whole_numbers <- function(x, 
  tol = 100 * .Machine$double.eps)
{                                                       
  msg <- gettextf("%s are not all whole numbers.", get_name_in_parent(x))
  assert_engine(x, is_whole_number, msg, tol = tol)
}

#' @rdname is_whole_number
#' @export
assert_any_are_whole_numbers <- function(x, 
  tol = 100 * .Machine$double.eps)
{                                                      
  msg <- gettextf("%s are all not whole numbers.", get_name_in_parent(x))
  assert_engine(x, is_whole_number, msg, what = "any", tol = tol)
}
