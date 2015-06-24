#' @rdname is_logical
#' @export
assert_is_a_bool <- function(x)
{      
  assert_engine(x, is_a_bool, .xname = get_name_in_parent(x))    
}

#' @rdname is_complex
#' @export
assert_is_a_complex <- function(x)
{                                                          
  assert_engine(x, is_a_complex, .xname = get_name_in_parent(x))   
}

#' @rdname is_numeric
#' @export
assert_is_a_number <- function(x)
{                                                          
  assert_engine(x, is_a_number, .xname = get_name_in_parent(x))   
}

#' @rdname is_character
#' @export
assert_is_a_non_empty_string <- function(x)
{                                                     
  assert_engine(x, is_a_non_empty_string, .xname = get_name_in_parent(x))    
}  

#' @rdname is_raw
#' @export
assert_is_a_raw <- function(x)
{                                                          
  assert_engine(x, is_a_raw, .xname = get_name_in_parent(x))      
}

#' @rdname is_character
#' @export
assert_is_a_string <- function(x)
{                                                         
  assert_engine(x, is_a_string, .xname = get_name_in_parent(x))   
}

#' @rdname is_character
#' @export
assert_is_an_empty_string <- function(x)
{                                                  
  assert_engine(x, is_an_empty_string, .xname = get_name_in_parent(x))     
}

#' @rdname is_integer
#' @export
assert_is_an_integer <- function(x)
{
  assert_engine(x, is_an_integer, .xname = get_name_in_parent(x)) 
}

#' @rdname is_inherited_from
#' @export
assert_is_inherited_from <- function(x, classes)
{
  assert_engine(
    x, 
    is_inherited_from, 
    classes = classes, 
    .xname = get_name_in_parent(x)
  )
}
