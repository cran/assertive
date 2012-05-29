#' @rdname is_true
#' @export
assert_is_false <- function(x, allow_attributes = FALSE)
{                                                  
  assert_engine(
    x, 
    assertive::is_false,  #avoid conflict with testthat::is_false
    allow_attributes = allow_attributes, 
    .xname = get_name_in_parent(x)
  )      
}

#' @rdname is_true
#' @export
assert_is_true <- function(x, allow_attributes = FALSE)
{                                                  
  assert_engine(
    x, 
    assertive::is_true,  #avoid conflict with testthat::is_true
    allow_attributes = allow_attributes, 
    .xname = get_name_in_parent(x)
  )    
}
