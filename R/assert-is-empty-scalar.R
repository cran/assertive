#' @rdname is_empty
#' @export
assert_has_elements <- function(x, n)
{                                                  
  assert_engine(x, has_elements, n = n, .xname = get_name_in_parent(x))      
}

#' @rdname is_empty
#' @export
assert_is_empty <- function(x)
{                                                  
  assert_engine(x, is_empty, .xname = get_name_in_parent(x))      
}

#' @rdname is_empty_model
#' @export
assert_is_empty_model <- function(x)
{                                                     
  assert_engine(x, is_empty_model, .xname = get_name_in_parent(x))     
}

#' @rdname is_empty
#' @export
assert_is_non_empty <- function(x)
{                                                     
  assert_engine(x, is_non_empty, .xname = get_name_in_parent(x))    
}

#' @rdname is_empty_model
#' @export
assert_is_non_empty_model <- function(x)
{                                                     
  assert_engine(x, is_non_empty_model, .xname = get_name_in_parent(x))    
}

#' @rdname is_empty
#' @export
assert_is_of_dimension <- function(x, n)
{                                                  
  assert_engine(x, is_of_dimension, n = n, .xname = get_name_in_parent(x))      
}

#' @rdname is_empty
#' @export
assert_is_of_length <- function(x, n)
{                                                  
  assert_engine(x, is_of_length, n = n, .xname = get_name_in_parent(x))      
}

#' @rdname is_empty
#' @export
assert_is_scalar <- function(x)
{                                                     
  assert_engine(x, is_scalar, .xname = get_name_in_parent(x))    
}
