#' @rdname is_binding_locked
#' @export
assert_is_binding_locked <- function(x)
{      
  assert_engine(is_binding_locked, x, .xname = get_name_in_parent(x))    
}
