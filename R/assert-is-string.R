#' @rdname is_cas_number
#' @export
assert_all_are_cas_numbers <- function(x)
{                                                     
  msg <- gettextf("%s are not all CAS numbers.", get_name_in_parent(x))
  assert_engine(x, is_cas_number, msg)        
}

#' @rdname is_cas_number
#' @export
assert_any_are_cas_numbers <- function(x)
{                                                     
  msg <- gettextf("%s are all not CAS numbers.", get_name_in_parent(x))
  assert_engine(x, is_cas_number, msg, what = "any")        
}

#' @rdname is_credit_card_number
#' @export
assert_all_are_credit_card_numbers <- function(x)
{                                                     
  msg <- gettextf("%s are not all credit card numbers.", get_name_in_parent(x))
  assert_engine(x, is_credit_card_number, msg)        
}

#' @rdname is_credit_card_number
#' @export
assert_any_are_credit_card_numbers <- function(x)
{                                                     
  msg <- gettextf("%s are all not credit card numbers.", get_name_in_parent(x))
  assert_engine(x, is_credit_card_number, msg, what = "any")        
}

#' @rdname is_date_string
#' @export
assert_all_are_date_strings <- function(x, format = "%F %T")
{                                                     
  msg <- gettextf(
    "%s is not a character vector of dates.", 
    get_name_in_parent(x)
  )
  assert_engine(x, is_date_string, msg, format = format)        
}

#' @rdname is_date_string
#' @export
assert_any_are_date_strings <- function(x, format = "%F %T")
{                                                     
  msg <- gettextf(
    "%s is not a character vector of dates.", 
    get_name_in_parent(x)
  )
  assert_engine(x, is_date_string, msg, what = "any", format = format)        
}

#' @rdname is_email_address
#' @export
assert_all_are_email_addresses <- function(x, method = c("simple", "rfc5322"))
{                    
  method <- match.arg(method)
  msg <- gettextf("%s are not all email addresses.", get_name_in_parent(x))
  assert_engine(x, is_email_address, msg, method = method)        
}

#' @rdname is_email_address
#' @export
assert_any_are_email_addresses <- function(x, method = c("simple", "rfc5322"))
{                                 
  method <- match.arg(method)                    
  msg <- gettextf("%s are all not email addresses.", get_name_in_parent(x))
  assert_engine(x, is_email_address, msg, method = method, what = "any")        
}

#' @rdname is_hex_color
#' @export
assert_all_are_hex_colors <- function(x)
{                    
  msg <- gettextf("%s are not all hex colors.", get_name_in_parent(x))
  assert_engine(x, is_hex_color, msg)        
}

#' @rdname is_hex_color
#' @export
assert_any_are_hex_colors <- function(x)
{                                                
  msg <- gettextf("%s are all not hex colors.", get_name_in_parent(x))
  assert_engine(x, is_hex_color, msg, what = "any")        
}

#' @rdname is_hex_color
#' @export
assert_all_are_hex_colours <- assert_all_are_hex_colors

#' @rdname is_hex_color
#' @export
assert_any_are_hex_colours <- assert_any_are_hex_colors

#' @rdname is_ip_address
#' @export
assert_all_are_ip_addresses <- function(x)
{                                                     
  msg <- gettextf("%s are not all IP addresses.", get_name_in_parent(x))
  assert_engine(x, is_ip_address, msg)        
}

#' @rdname is_ip_address
#' @export
assert_any_are_ip_addresses <- function(x)
{                                                     
  msg <- gettextf("%s are all not IP addresses.", get_name_in_parent(x))
  assert_engine(x, is_ip_address, msg, what = "any")        
}

#' @rdname is_isbn_code
#' @export
assert_all_are_isbn_codes <- function(x, type = c("10", "13"))
{                                                     
  msg <- gettextf("%s are not all ISBN codes.", get_name_in_parent(x))
  assert_engine(x, is_isbn_code, msg, type = type)        
}

#' @rdname is_isbn_code
#' @export
assert_any_are_isbn_codes <- function(x, type = c("10", "13"))
{                                                     
  msg <- gettextf("%s are all not ISBN codes.", get_name_in_parent(x))
  assert_engine(x, is_isbn_code, msg, what = "any", type = type)        
}

#' @rdname is_character
#' @export
assert_all_are_numeric_strings <- function(x)
{                                                     
  msg <- gettextf(
    "%s is not a character vector of numbers.", 
    get_name_in_parent(x)
  )
  assert_engine(x, is_numeric_string, msg)        
}

#' @rdname is_character
#' @export
assert_any_are_numeric_strings <- function(x)
{                                                     
  msg <- gettextf(
    "%s is not a character vector of numbers.", 
    get_name_in_parent(x)
  )
  assert_engine(x, is_numeric_string, msg, what = "any")        
}

#' @rdname is_single_character
#' @export
assert_all_are_single_characters <- function(x)
{                                                     
  msg <- gettextf("%s are not all single characters.", get_name_in_parent(x))
  assert_engine(x, is_single_character, msg)        
}

#' @rdname is_single_character
#' @export
assert_any_are_single_characters <- function(x)
{                                                     
  msg <- gettextf("%s are all not single characters.", get_name_in_parent(x))
  assert_engine(x, is_single_character, msg, what = "any")        
}

#' @rdname is_character
#' @export
assert_all_strings_are_missing_or_empty <- function(x)
{                                                       
  msg <- gettextf(
    "%s are not all missing or empty strings.", 
    get_name_in_parent(x)
  )
  assert_engine(x, is_missing_or_empty_character, msg)
}

#' @rdname is_character
#' @export
assert_any_strings_are_missing_or_empty <- function(x)
{                                                      
  msg <- gettextf(
    "%s are all not missing or empty strings.", 
    get_name_in_parent(x)
  )
  assert_engine(x, is_missing_or_empty_character, msg, what = "any")
}

#' @rdname is_character
#' @export
assert_all_strings_are_not_missing_nor_empty <- function(x)
{                                                       
  .Deprecated("assert_all_are_not_missing_nor_empty_characters")
  assert_all_are_not_missing_nor_empty_characters(x)
}

#' @rdname is_character
#' @export
assert_any_strings_are_not_missing_nor_empty <- function(x)
{                                                      
  .Deprecated("assert_any_are_not_missing_nor_empty_characters")
  assert_any_are_not_missing_nor_empty_characters(x)
}

#' @rdname is_character
#' @export
assert_all_are_not_missing_nor_empty_characters <- function(x)
{                                                       
  msg <- gettextf("%s contains missing or empty strings.", get_name_in_parent(x))
  assert_engine(x, is_not_missing_nor_empty_character, msg)
}

#' @rdname is_character
#' @export
assert_any_are_not_missing_nor_empty_characters <- function(x)
{                                                      
  msg <- gettextf("%s are all missing or empty strings.", get_name_in_parent(x))
  assert_engine(x, is_not_missing_nor_empty_character, msg, what = "any")
}


#' @rdname is_valid_variable_name
#' @export
assert_all_are_valid_variable_names <- function(x, allow_reserved = TRUE, 
  allow_duplicates)
{   
  if(!missing(allow_duplicates))
  {
    .Deprecated(
      msg = "The 'allow_duplicates' argument is deprecated and will be ignored."
    )
  }
  msg <- gettextf("%s are not all valid variable names.", get_name_in_parent(x))
  assert_engine(
    x, 
    is_valid_variable_name, 
    msg,
    allow_reserved = allow_reserved
  )
}

#' @rdname is_valid_variable_name
#' @export
assert_any_are_valid_variable_names <- function(x, allow_reserved = TRUE, 
  allow_duplicates)
{          
  if(!missing(allow_duplicates))
  {
    .Deprecated(
      msg = "The 'allow_duplicates' argument is deprecated and will be ignored."
    )
  }
  msg <- gettextf("%s are all not valid variable names.", get_name_in_parent(x))
  assert_engine(
    x, 
    is_valid_variable_name, 
    msg,
    what = "any",
    allow_reserved = allow_reserved
  )
}
