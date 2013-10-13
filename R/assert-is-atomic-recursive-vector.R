#' @rdname is_atomic
#' @export
assert_is_atomic <- function(x)
{                                                    
  assert_engine(x, is_atomic, .xname = get_name_in_parent(x))
}

#' @rdname is_atomic
#' @export
assert_is_recursive <- function(x)
{                                                         
  assert_engine(x, is_recursive, .xname = get_name_in_parent(x)) 
}

#' @rdname is_atomic
#' @export
assert_is_vector <- function(x)
{                                                    
  assert_engine(x, is_vector, .xname = get_name_in_parent(x))
}
