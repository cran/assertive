#' @rdname is_logical
#' @export
assert_is_a_bool <- function(x)
{      
  assert_engine(is_a_bool, x, .xname = get_name_in_parent(x))    
}

#' @rdname is_complex
#' @export
assert_is_a_complex <- function(x)
{                                                          
  assert_engine(is_a_complex, x, .xname = get_name_in_parent(x))   
}

#' @rdname is_numeric
#' @export
assert_is_a_number <- function(x)
{                                                          
  assert_engine(is_a_number, x, .xname = get_name_in_parent(x))   
}

#' @rdname is_character
#' @export
assert_is_a_non_empty_string <- function(x)
{                                                     
  assert_engine(is_a_non_empty_string, x, .xname = get_name_in_parent(x))    
}  

#' @rdname is_raw
#' @export
assert_is_a_raw <- function(x)
{                                                          
  assert_engine(is_a_raw, x, .xname = get_name_in_parent(x))      
}

#' @rdname is_character
#' @export
assert_is_a_string <- function(x)
{                                                         
  assert_engine(is_a_string, x, .xname = get_name_in_parent(x))   
}

#' @rdname is_character
#' @export
assert_is_an_empty_string <- function(x)
{                                                  
  assert_engine(is_an_empty_string, x, .xname = get_name_in_parent(x))     
}

#' @rdname is_integer
#' @export
assert_is_an_integer <- function(x)
{
  assert_engine(is_an_integer, x, .xname = get_name_in_parent(x)) 
}

#' @rdname is_inherited_from
#' @export
assert_is_inherited_from <- function(x, classes)
{
  assert_engine(
    is_inherited_from, 
    x, 
    classes = classes, 
    .xname = get_name_in_parent(x)
  )
}
