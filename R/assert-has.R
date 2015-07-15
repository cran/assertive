#' @rdname has_attributes
#' @export
assert_has_all_attributes <- function(x, attrs)
{                                       
  msg <- gettextf(
    "%s does not have all the attributes %s.", 
    get_name_in_parent(x), 
    toString(sQuote(attrs))
  )
  assert_engine(has_attributes, x, attrs = attrs, msg = msg)
}

#' @rdname has_attributes
#' @export
assert_has_any_attributes <- function(x, attrs)
{                                       
  msg <- gettextf(
    "%s does not have any of the attributes %s.", 
    get_name_in_parent(x), 
    toString(sQuote(attrs))
    )
  assert_engine(has_attributes, x, attrs = attrs, msg = msg, what = "any")
}

#' @rdname has_names
#' @export
assert_has_colnames <- function(x)
{                                       
  msg <- gettextf("%s has no column names.", get_name_in_parent(x))
  assert_engine(has_colnames, x, msg = msg)
}

#' @rdname has_cols
#' @export
assert_has_cols <- function(x)
{                                       
  msg <- gettextf("%s has no columns.", get_name_in_parent(x))
  assert_engine(has_cols, x, msg = msg)
}

#' @rdname has_names
#' @export
assert_has_dimnames <- function(x)
{                                       
  msg <- gettextf("%s has no dimension names.", get_name_in_parent(x))
  assert_engine(has_dimnames, x, msg = msg)
}

#' @rdname has_dims
#' @export
assert_has_dims <- function(x)
{                                                                
  msg <- gettextf("%s has no dimensions attribute.", get_name_in_parent(x))
  assert_engine(has_dims, x, msg = msg)
}

#' @rdname has_duplicates
#' @export
assert_has_duplicates <- function(x)
{                                                                
  msg <- gettextf("%s has no duplicates.", get_name_in_parent(x))
  assert_engine(has_no_duplicates, x, msg = msg)
}

#' @rdname has_duplicates
#' @export
assert_has_no_duplicates <- function(x)
{                                                                
  msg <- gettextf("%s has duplicates.", get_name_in_parent(x))
  assert_engine(has_no_duplicates, x, msg = msg)
}

#' @rdname has_names
#' @export
assert_has_names <- function(x)
{                                                             
  msg <- gettextf("%s has no names.", get_name_in_parent(x))
  assert_engine(has_names, x, msg = msg)
}

#' @rdname has_names
#' @export
assert_has_rownames <- function(x)
{                                                             
  msg <- gettextf("%s has no row names.", get_name_in_parent(x))
  assert_engine(has_rownames, x, msg = msg)
}

#' @rdname has_cols
#' @export
assert_has_rows <- function(x)
{                                                             
  msg <- gettextf("%s has no rows.", get_name_in_parent(x))
  assert_engine(has_rows, x, msg = msg)
}

#' @rdname has_terms
#' @export
assert_has_terms <- function(x)
{                                                             
  msg <- gettextf("%s has no terms.", get_name_in_parent(x))
  assert_engine(has_terms, x, msg = msg)
}
