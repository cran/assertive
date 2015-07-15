#' @rdname is_atomic
#' @export
assert_is_atomic <- function(x)
{                                                    
  assert_engine(is_atomic, x, .xname = get_name_in_parent(x))
}

#' @rdname is_atomic
#' @export
assert_is_recursive <- function(x)
{                                                         
  assert_engine(is_recursive, x, .xname = get_name_in_parent(x)) 
}

#' @rdname is_atomic
#' @export
assert_is_vector <- function(x)
{                                                    
  assert_engine(is_vector, x, .xname = get_name_in_parent(x))
}
