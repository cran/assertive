#' @rdname is_open_connection
#' @export
assert_is_open_connection <- function(x, rw = "")
{                                                         
  assert_engine(x, is_open_connection, .xname = get_name_in_parent(x), rw = rw)   
}

#' @rdname is_open_connection
#' @export
assert_is_incomplete_connection <- function(x)
{                                                         
  assert_engine(x, is_incomplete_connection, .xname = get_name_in_parent(x))   
}
