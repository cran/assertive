#' @rdname is_in_range
#' @export
assert_all_are_in_closed_range <- function(x, lower = -Inf, upper = Inf)
{                                                     
  msg <- sprintf("%s are not all in range.", get_name_in_parent(x))
  assert_engine(x, is_in_closed_range, msg, lower = lower, upper = upper)  
}

#' @rdname is_in_range
#' @export
assert_any_are_in_closed_range <- function(x, lower = -Inf, upper = Inf)
{                                                     
  msg <- sprintf("%s are all out of range.", get_name_in_parent(x))
  assert_engine(
    x, 
    is_in_closed_range, 
    msg, 
    what = "any", 
    lower = lower, 
    upper = upper
  )  
}

#' @rdname is_in_range
#' @export
assert_all_are_in_left_open_range <- function(x, lower = -Inf, upper = Inf)
{                                                     
  msg <- sprintf("%s are not all in range.", get_name_in_parent(x))
  assert_engine(x, is_in_left_open_range, msg, lower = lower, upper = upper)  
}

#' @rdname is_in_range
#' @export
assert_any_are_in_left_open_range <- function(x, lower = -Inf, upper = Inf)
{                                                     
  msg <- sprintf("%s are all out of range.", get_name_in_parent(x))
  assert_engine(
    x, 
    is_in_left_open_range, 
    msg, 
    what = "any", 
    lower = lower, 
    upper = upper
  )  
}

#' @rdname is_in_range
#' @export
assert_all_are_in_open_range <- function(x, lower = -Inf, upper = Inf)
{                                                     
  msg <- sprintf("%s are not all in range.", get_name_in_parent(x))
  assert_engine(x, is_in_open_range, msg, lower = lower, upper = upper)  
}

#' @rdname is_in_range
#' @export
assert_any_are_in_open_range <- function(x, lower = -Inf, upper = Inf)
{                                                     
  msg <- sprintf("%s are all out of range.", get_name_in_parent(x))
  assert_engine(
    x, 
    is_in_open_range, 
    msg, 
    what = "any", 
    lower = lower, 
    upper = upper
  )  
}

#' @rdname is_in_range
#' @export
assert_all_are_in_range <- function(x, lower = -Inf, upper = Inf, 
  lower_is_strict = FALSE, upper_is_strict = FALSE)
{                                                     
  msg <- sprintf("%s are not all in range.", get_name_in_parent(x))
  assert_engine(
    x, 
    is_in_range, 
    msg, 
    lower = lower, 
    upper = upper, 
    lower_is_strict = lower_is_strict, 
    upper_is_strict = upper_is_strict
    ) 
}

#' @rdname is_in_range
#' @export
assert_any_are_in_range <- function(x, lower = -Inf, upper = Inf, 
  lower_is_strict = FALSE, upper_is_strict = FALSE)
{                                                     
  msg <- sprintf("%s are all out of range.", get_name_in_parent(x))
  assert_engine(
    x, 
    is_in_range, 
    msg, 
    what = "any",
    lower = lower, 
    upper = upper, 
    lower_is_strict = lower_is_strict, 
    upper_is_strict = upper_is_strict
    )
}

#' @rdname is_in_range
#' @export
assert_all_are_in_right_open_range <- function(x, lower = -Inf, upper = Inf)
{                                                     
  msg <- sprintf("%s are not all in range.", get_name_in_parent(x))
  assert_engine(x, is_in_right_open_range, msg, lower = lower, upper = upper)  
}

#' @rdname is_in_range
#' @export
assert_any_are_in_right_open_range <- function(x, lower = -Inf, upper = Inf)
{                                                     
  msg <- sprintf("%s are all out of range.", get_name_in_parent(x))
  assert_engine(
    x, 
    is_in_right_open_range, 
    msg, 
    what = "any", 
    lower = lower, 
    upper = upper
  )  
}

#' @rdname is_in_range
#' @export
assert_all_are_negative <- function(x)
{                                                                
  msg <- sprintf("%s are not all negative.", get_name_in_parent(x))
  assert_engine(x, is_negative, msg)
}

#' @rdname is_in_range
#' @export
assert_any_are_negative <- function(x)
{                                                        
  msg <- sprintf("%s are all not negative.", get_name_in_parent(x))
  assert_engine(x, is_negative, msg, what = "any")
}

#' @rdname is_in_range
#' @export
assert_all_are_non_negative <- function(x)
{                                                       
  msg <- sprintf("%s are not all non-negative.", get_name_in_parent(x))
  assert_engine(x, is_non_negative, msg)
}

#' @rdname is_in_range
#' @export
assert_any_are_non_negative <- function(x)
{                                                      
  msg <- sprintf("%s are all not non-negative.", get_name_in_parent(x))
  assert_engine(x, is_non_negative, msg, what = "any")
}

#' @rdname is_in_range
#' @export
assert_all_are_non_positive <- function(x)
{                                                       
  msg <- sprintf("%s contains positive values.", get_name_in_parent(x))
  assert_engine(x, is_non_positive, msg)
}

#' @rdname is_in_range
#' @export
assert_any_are_non_positive <- function(x)
{                                                      
  msg <- sprintf("%s are all positive.", get_name_in_parent(x))
  assert_engine(x, is_non_positive, msg, what = "any")
}

#' @rdname is_in_range
#' @export
assert_all_are_percentages <- function(x, lower_is_strict = FALSE, 
  upper_is_strict = FALSE)
{                                                       
  msg <- sprintf("%s are not all percentages.", get_name_in_parent(x))
  assert_engine(
    x, 
    is_percentage, 
    msg, 
    lower_is_strict = lower_is_strict, 
    upper_is_strict = upper_is_strict
    )
}

#' @rdname is_in_range
#' @export
assert_any_are_percentages <- function(x, lower_is_strict = FALSE, 
  upper_is_strict = FALSE)
{                                                       
  msg <- sprintf("%s are all not percentages.", get_name_in_parent(x))
  assert_engine(
    x, 
    is_percentage, 
    msg, 
    what = "any",
    lower_is_strict = lower_is_strict, 
    upper_is_strict = upper_is_strict
    )
}  

#' @rdname is_in_range
#' @export
assert_all_are_positive <- function(x)
{                                                       
  msg <- sprintf("%s contains non-positive values.", get_name_in_parent(x))
  assert_engine(x, is_positive, msg)
}

#' @rdname is_in_range
#' @export
assert_any_are_positive <- function(x)
{                                                      
  msg <- sprintf("%s are all non-positive.", get_name_in_parent(x))
  assert_engine(x, is_positive, msg, what = "any")
}

#' @rdname is_in_range
#' @export
assert_all_are_proportions <- function(x, lower_is_strict = FALSE, 
  upper_is_strict = FALSE)
{                                                       
  msg <- sprintf("%s are not all proportions.", get_name_in_parent(x))
  assert_engine(
    x, 
    is_proportion, 
    msg, 
    lower_is_strict = lower_is_strict, 
    upper_is_strict = upper_is_strict
    )
}

#' @rdname is_in_range
#' @export
assert_any_are_proportions <- function(x, lower_is_strict = FALSE, 
  upper_is_strict = FALSE)
{                                                       
  msg <- sprintf("%s are all not proportions.", get_name_in_parent(x))
  assert_engine(
    x, 
    is_proportion, 
    msg, 
    what = "any",
    lower_is_strict = lower_is_strict, 
    upper_is_strict = upper_is_strict
    )
}  
