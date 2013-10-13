#' @rdname is_true
#' @export
assert_all_are_false <- function(x)
{                                                     
  msg <- sprintf("%s are not all false.", get_name_in_parent(x))
  assert_engine(x, is_false, msg)        
}

#' @rdname is_true
#' @export
assert_any_are_false <- function(x)
{                                                     
  msg <- sprintf("%s are all not false.", get_name_in_parent(x))
  assert_engine(x, is_false, msg, what = "any")        
}

#' @rdname is_true
#' @export
assert_is_identical_to_false <- function(x, allow_attributes = FALSE)
{                                                  
  assert_engine(
    x, 
    assertive::is_identical_to_false,
    allow_attributes = allow_attributes, 
    .xname = get_name_in_parent(x)
  )      
}

#' @rdname is_true
#' @export
assert_is_identical_to_true <- function(x, allow_attributes = FALSE)
{                                                  
  assert_engine(
    x, 
    is_identical_to_true,
    allow_attributes = allow_attributes, 
    .xname = get_name_in_parent(x)
  )    
}

#' @rdname is_true
#' @export
assert_all_are_true <- function(x)
{                                                     
  msg <- sprintf("%s are not all true.", get_name_in_parent(x))
  assert_engine(x, is_true, msg)        
}

#' @rdname is_true
#' @export
assert_any_are_true <- function(x)
{                                                     
  msg <- sprintf("%s are all not true.", get_name_in_parent(x))
  assert_engine(x, is_true, msg, what = "any")        
}
