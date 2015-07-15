#' @rdname is_set_equal
#' @export
assert_are_set_equal <- function(x, y)
{
  assert_engine(
    is_set_equal, 
    x, 
    y = y, 
    .xname = get_name_in_parent(x), 
    .yname = get_name_in_parent(y)
  )
}

#' @rdname is_set_equal
#' @export
assert_is_set_equal <- function(x, y)
{
  .Deprecated("assert_are_set_equal")
  assert_are_set_equal(x, y)
}

#' @rdname is_set_equal
#' @export
assert_is_subset <- function(x, y)
{
  assert_engine(
    is_subset, 
    x, 
    y = y, 
    .xname = get_name_in_parent(x), 
    .yname = get_name_in_parent(y)
  ) 
}

#' @rdname is_set_equal
#' @export
assert_is_superset <- function(x, y)
{
  assert_engine(
    is_superset, 
    x, 
    y = y, 
    .xname = get_name_in_parent(x), 
    .yname = get_name_in_parent(y)
  ) 
}
