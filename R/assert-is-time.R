#' @rdname is_in_past
#' @export
assert_all_are_in_future <- function(x)
{                                                     
  msg <- gettextf("%s are not all in the future.", get_name_in_parent(x))
  assert_engine(x, is_in_future, msg)  
}

#' @rdname is_in_past
#' @export
assert_any_are_in_future <- function(x)
{                                                     
  msg <- gettextf("%s are all in the past.", get_name_in_parent(x))
  assert_engine(x, is_in_future, msg, what = "any")  
}
 
#' @rdname is_in_past
#' @export
assert_all_are_in_past <- function(x)
{                                                     
  msg <- gettextf("%s are not all in the past.", get_name_in_parent(x))
  assert_engine(x, is_in_past, msg)  
}

#' @rdname is_in_past
#' @export
assert_any_are_in_past <- function(x)
{                                                     
  msg <- gettextf("%s are all in the future.", get_name_in_parent(x))
  assert_engine(x, is_in_past, msg, what = "any")  
}
