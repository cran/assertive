#' @rdname is_null
#' @export
assert_is_not_null <- function(x)
{                                                      
  assert_engine(is_not_null, x, .xname = get_name_in_parent(x))   
}

#' @rdname is_null
#' @export
assert_is_null <- function(x)
{                                                         
  assert_engine(is_null, x, .xname = get_name_in_parent(x))       
}
