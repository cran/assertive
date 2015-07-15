#' @rdname is_us_telephone_number
#' @export
assert_all_are_us_telephone_numbers <- function(x, na_ignore = FALSE)
{                                                     
  msg <- gettextf("%s are not all US telephone numbers.", get_name_in_parent(x))
  assert_engine(
    is_us_telephone_number, 
    x, 
    msg = msg, 
    na_ignore = na_ignore
  )  
}

#' @rdname is_us_telephone_number
#' @export
assert_any_are_us_telephone_numbers <- function(x, na_ignore = FALSE)
{                                                     
  msg <- gettextf("%s are all not US telephone numbers.", get_name_in_parent(x))
  assert_engine(
    is_us_telephone_number, 
    x, 
    msg = msg, 
    what = "any",
    na_ignore = na_ignore
  )       
}


#' @rdname is_us_zip_code
#' @export
assert_all_are_us_zip_codes <- function(x, na_ignore = FALSE)
{                                                     
  msg <- gettextf("%s are not all US zip codes.", get_name_in_parent(x))
  assert_engine(
    is_us_zip_code, 
    x, 
    msg = msg, 
    na_ignore = na_ignore
  )         
}

#' @rdname is_us_zip_code
#' @export
assert_any_are_us_zip_codes <- function(x, na_ignore = FALSE)
{                                                     
  msg <- gettextf("%s are all not US zip codes.", get_name_in_parent(x))
  assert_engine(
    is_us_zip_code, 
    x, 
    msg = msg, 
    what = "any",
    na_ignore = na_ignore
  )        
}
