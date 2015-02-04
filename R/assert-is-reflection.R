#' @rdname is_windows
#' @export
assert_is_64_bit_os <- function()
{
  assert_engine(predicate = is_64_bit_os)
}

#' @rdname is_r
#' @export
assert_is_architect <- function()
{                                                         
  assert_engine(predicate = is_architect)        
}

#' @rdname is_batch_mode
#' @export
assert_is_batch_mode <- function()
{
  assert_engine(predicate = is_batch_mode)
}

#' @rdname is_windows
#' @export
assert_is_bsd <- function()
{
  assert_engine(predicate = is_bsd)
}

#' @rdname is_xxx_for_decimal_point
#' @export
assert_is_comma_for_decimal_point <- function()
{
  assert_engine(predicate = is_comma_for_decimal_point)
}

#' @rdname is_batch_mode
#' @export
assert_is_interactive <- function()
{
  assert_engine(predicate = is_interactive)
}

#' @rdname is_windows
#' @export
assert_is_linux <- function()
{
  assert_engine(predicate = is_linux)
}

#' @rdname is_windows
#' @export
assert_is_mac <- function()
{
  assert_engine(predicate = is_mac)
}

#' @rdname is_on_os_path
#' @export
assert_all_are_on_os_path <- function(x)
{                                                     
  msg <- sprintf("%s are not all on the operating system path.", get_name_in_parent(x))
  assert_engine(x, is_on_os_path, msg)        
}

#' @rdname is_on_os_path
#' @export
assert_any_are_on_os_path <- function(x)
{                                                     
  msg <- sprintf("%s are all not on the operating system path.", get_name_in_parent(x))
  assert_engine(x, is_on_os_path, msg, what = "any")        
}

#' @rdname is_xxx_for_decimal_point
#' @export
assert_is_period_for_decimal_point <- function()
{
  assert_engine(predicate = is_period_for_decimal_point)
}

#' @rdname is_r
#' @export
assert_is_r <- function()
{                                                         
  assert_engine(predicate = is_r)        
}

#' @rdname is_r
#' @export
assert_is_r_devel <- function()
{                                                         
  assert_engine(predicate = is_r_devel)        
}

#' @rdname is_r
#' @export
assert_is_r_patched <- function()
{                                                         
  assert_engine(predicate = is_r_patched)        
}

#' @rdname is_r
#' @export
assert_is_r_stable <- function()
{                                                         
  assert_engine(predicate = is_r_stable)        
}

#' @rdname is_r
#' @export
assert_is_revo_r <- function()
{                                                         
  assert_engine(predicate = is_revo_r)        
}

#' @rdname is_r
#' @export
assert_is_rstudio <- function()
{                                                         
  assert_engine(predicate = is_rstudio)        
}

#' @rdname is_r
#' @export
assert_is_slave_r <- function()
{                                                         
  assert_engine(predicate = is_slave_r)        
}

#' @rdname is_windows
#' @export
assert_is_solaris <- function()
{
  assert_engine(predicate = is_solaris)
}

#' @rdname is_windows
#' @export
assert_is_unix <- function()
{
  assert_engine(predicate = is_unix)
}

#' @rdname is_windows
#' @export
assert_is_windows <- function()
{
  assert_engine(predicate = is_windows)
}
