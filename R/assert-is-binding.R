#' @rdname is_binding_locked
#' @export
assert_is_binding_locked <- function(x)
{      
  assert_engine(x, is_binding_locked, .xname = get_name_in_parent(x))    
}
