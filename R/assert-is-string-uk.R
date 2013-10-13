#' @rdname is_uk_car_licence
#' @export
assert_all_are_uk_car_licences <- function(x)
{                                                     
  msg <- sprintf("%s are not all UK car licence plates.", get_name_in_parent(x))
  assert_engine(x, is_uk_car_licence, msg)        
}

#' @rdname is_uk_car_licence
#' @export
assert_any_are_uk_car_licences <- function(x)
{                                                     
  msg <- sprintf("%s are all not UK car licence plates.", get_name_in_parent(x))
  assert_engine(x, is_uk_car_licence, msg, what = "any")        
}

#' @rdname is_uk_national_insurance_number
#' @export
assert_all_are_uk_national_insurance_numbers <- function(x)
{                                                     
  msg <- sprintf("%s are not all UK national insurance numbers.", get_name_in_parent(x))
  assert_engine(x, is_uk_national_insurance_number, msg)        
}

#' @rdname is_uk_national_insurance_number
#' @export
assert_any_are_uk_national_insurance_numbers <- function(x)
{                                                     
  msg <- sprintf("%s are all not UK national insurance numbers.", get_name_in_parent(x))
  assert_engine(x, is_uk_national_insurance_number, msg, what = "any")        
}

#' @rdname is_uk_postcode
#' @export
assert_all_are_uk_postcodes <- function(x)
{                                                     
  msg <- sprintf("%s are not all UK postcodes.", get_name_in_parent(x))
  assert_engine(x, is_uk_postcode, msg)        
}

#' @rdname is_uk_postcode
#' @export
assert_any_are_uk_postcodes <- function(x)
{                                                     
  msg <- sprintf("%s are all not UK postcodes.", get_name_in_parent(x))
  assert_engine(x, is_uk_postcode, msg, what = "any")        
}

#' @rdname is_uk_telephone_number
#' @export
assert_all_are_uk_telephone_numbers <- function(x)
{                                                     
  msg <- sprintf("%s are not all UK telephone numbers.", get_name_in_parent(x))
  assert_engine(x, is_uk_telephone_number, msg)        
}

#' @rdname is_uk_telephone_number
#' @export
assert_any_are_uk_telephone_numbers <- function(x)
{                                                     
  msg <- sprintf("%s are all not UK telephone numbers.", get_name_in_parent(x))
  assert_engine(x, is_uk_telephone_number, msg, what = "any")        
}
