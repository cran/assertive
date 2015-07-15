#' @rdname is_dir
#' @export
assert_all_are_dirs <- function(x)
{
  msg <- gettextf(
    "The paths %s are not all directories.", 
    get_name_in_parent(x)
  )
  assert_engine(is_dir, x, msg = msg)
}

#' @rdname is_dir
#' @export
assert_any_are_dirs <- function(x)
{
  msg <- gettextf(
    "The paths %s are all not directories.", 
    get_name_in_parent(x)
  )
  assert_engine(is_dir, x, msg = msg, what = "any")
}

#' @rdname is_empty_file
#' @export
assert_all_are_empty_files <- function(x)
{
  msg <- gettextf(
    "The files %s are not all empty", 
    get_name_in_parent(x)
  )
  assert_engine(is_empty_file, x, msg = msg)
}

#' @rdname is_empty_file
#' @export
assert_any_are_empty_files <- function(x)
{
  msg <- gettextf(
    "The files %s are all not empty.", 
    get_name_in_parent(x)
  )
  assert_engine(is_empty_file, x, msg = msg, what = "any")
}

#' @rdname is_existing_file
#' @export
assert_all_are_existing_files <- function(x)
{
  msg <- gettextf(
    "The files %s do not all exist.", 
    get_name_in_parent(x)
  )
  assert_engine(is_existing_file, x, msg = msg)
}

#' @rdname is_existing_file
#' @export
assert_any_are_existing_files <- function(x)
{
  msg <- gettextf(
    "The files %s all do not exist.", 
    get_name_in_parent(x)
    )
  assert_engine(is_existing_file, x, msg = msg, what = "any")
}

#' @rdname is_executable_file
#' @export
assert_all_are_executable_files <- function(x)
{
  msg <- gettextf(
    "The files %s are not all executable.", 
    get_name_in_parent(x)
    )
  assert_engine(is_executable_file, x, msg = msg)
}

#' @rdname is_executable_file
#' @export
assert_any_are_executable_files <- function(x)
{
  msg <- gettextf(
    "The files %s all are not executable.", 
    get_name_in_parent(x)
    )
  assert_engine(is_executable_file, x, msg = msg, what = "any")
}

#' @rdname is_library
#' @export
assert_all_are_libraries <- function(x)
{
  msg <- gettextf(
    "The paths %s are not all libraries.", 
    get_name_in_parent(x)
  )
  assert_engine(is_library, x, msg = msg)
}

#' @rdname is_library
#' @export
assert_any_are_libraries <- function(x)
{
  msg <- gettextf(
    "The paths %s are all not libraries.", 
    get_name_in_parent(x)
  )
  assert_engine(is_library, x, msg = msg, what = "any")
}
#' @rdname is_executable_file
#' @export
assert_all_are_readable_files <- function(x)
{
  msg <- gettextf(
    "The files %s are not all readable.", 
    get_name_in_parent(x)
    )
  assert_engine(is_readable_file, x, msg = msg)
}

#' @rdname is_executable_file
#' @export
assert_any_are_readable_files <- function(x)
{
  msg <- gettextf(
    "The files %s all are not readable.", 
    get_name_in_parent(x)
    )
  assert_engine(is_readable_file, x, msg = msg, what = "any")
}

#' @rdname is_executable_file
#' @export
assert_all_are_writable_files <- function(x)
{
  msg <- gettextf(
    "The files %s are not all writable.", 
    get_name_in_parent(x)
    )
  assert_engine(is_writable_file, x, msg = msg)
}

#' @rdname is_executable_file
#' @export
assert_any_are_writable_files <- function(x)
{
  msg <- gettextf(
    "The files %s all are not writable.", 
    get_name_in_parent(x)
    )
  assert_engine(is_writable_file, x, msg = msg, what = "any")
}
