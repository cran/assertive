#' @rdname is_in_past
#' @export
assert_all_are_in_future <- function(x, na_ignore = FALSE)
{                                                     
  msg <- gettextf("%s are not all in the future.", get_name_in_parent(x))
  assert_engine(
    x, 
    msg = msg, 
    what = "any",
    na_ignore = na_ignore
  ) 
}

#' @rdname is_in_past
#' @export
assert_any_are_in_future <- function(x, na_ignore = FALSE)
{                                                     
  msg <- gettextf("%s are all in the past.", get_name_in_parent(x))
  assert_engine(
    is_in_future, 
    x, 
    msg = msg, 
    what = "any",
    na_ignore = na_ignore
  )   
}
 
#' @rdname is_in_past
#' @export
assert_all_are_in_past <- function(x, na_ignore = FALSE)
{                                                     
  msg <- gettextf("%s are not all in the past.", get_name_in_parent(x))
  assert_engine(
    is_in_past, 
    x, 
    msg = msg, 
    na_ignore = na_ignore
  )    
}

#' @rdname is_in_past
#' @export
assert_any_are_in_past <- function(x, na_ignore = FALSE)
{                                                     
  msg <- gettextf("%s are all in the future.", get_name_in_parent(x))
  assert_engine(
    is_in_past, 
    x, 
    msg = msg, 
    what = "any",
    na_ignore = na_ignore
  )  
}
