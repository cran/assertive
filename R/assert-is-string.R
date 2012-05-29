#' @rdname is_character
#' @export
assert_all_are_numeric_strings <- function(x)
{                                                     
  msg <- sprintf("%s is not a character of numbers.", get_name_in_parent(x))
  assert_engine(x, is_numeric_string, msg)        
}

#' @rdname is_character
#' @export
assert_any_are_numeric_strings <- function(x)
{                                                     
  msg <- sprintf("%s is not a character of numbers.", get_name_in_parent(x))
  assert_engine(x, is_numeric_string, msg, what = "any")        
}

#' @rdname is_character
#' @export
assert_all_strings_are_missing_or_empty <- function(x)
{                                                       
  msg <- sprintf("%s are not all missing or empty strings.", get_name_in_parent(x))
  assert_engine(x, is_missing_or_empty_character, msg)
}

#' @rdname is_character
#' @export
assert_any_strings_are_missing_or_empty <- function(x)
{                                                      
  msg <- sprintf("%s are all not missing or empty strings.", get_name_in_parent(x))
  assert_engine(x, is_missing_or_empty_character, msg, what = "any")
}

#' @rdname is_character
#' @export
assert_all_strings_are_not_missing_nor_empty <- function(x)
{                                                       
  msg <- sprintf("%s are not all missing or empty strings.", get_name_in_parent(x))
  assert_engine(x, is_not_missing_nor_empty_character, msg)
}

#' @rdname is_character
#' @export
assert_any_strings_are_not_missing_nor_empty <- function(x)
{                                                      
  msg <- sprintf("%s are all not missing or empty strings.", get_name_in_parent(x))
  assert_engine(x, is_not_missing_nor_empty_character, msg, what = "any")
}

#' @rdname is_valid_variable_name
#' @export
assert_all_are_valid_variable_names <- function(x, allow_reserved = TRUE, allow_duplicates = TRUE)
{                                                       
  msg <- sprintf("%s are not all valid variable names.", get_name_in_parent(x))
  assert_engine(
    x, 
    is_valid_variable_name, 
    msg,
    allow_reserved = allow_reserved,
    allow_duplicates = allow_duplicates
    )
}

#' @rdname is_valid_variable_name
#' @export
assert_any_are_valid_variable_names <- function(x, allow_reserved = TRUE, allow_duplicates = TRUE)
{                                                      
  msg <- sprintf("%s are all not valid variable names.", get_name_in_parent(x))
  assert_engine(
    x, 
    is_valid_variable_name, 
    msg,
    what = "any",
    allow_reserved = allow_reserved,
    allow_duplicates = allow_duplicates
    )
}
