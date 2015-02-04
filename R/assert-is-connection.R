#' @rdname is_connection
#' @export
assert_is_bzfile_connection <- function(x)
{                                                         
  assert_engine(x, is_bzfile_connection, .xname = get_name_in_parent(x))   
}

#' @rdname is_connection
#' @export
assert_is_connection <- function(x)
{                                                         
  assert_engine(x, is_connection, .xname = get_name_in_parent(x))   
}

#' @rdname is_connection
#' @export
assert_is_fifo_connection <- function(x)
{                                                         
  assert_engine(x, is_fifo_connection, .xname = get_name_in_parent(x))   
}

#' @rdname is_connection
#' @export
assert_is_file_connection <- function(x)
{                                                         
  assert_engine(x, is_file_connection, .xname = get_name_in_parent(x))   
}

#' @rdname is_connection
#' @export
assert_is_gzfile_connection <- function(x)
{                                                         
  assert_engine(x, is_gzfile_connection, .xname = get_name_in_parent(x))   
}

#' @rdname is_connection
#' @export
assert_is_incomplete_connection <- function(x)
{                                                         
  assert_engine(x, is_incomplete_connection, .xname = get_name_in_parent(x))   
}

#' @rdname is_connection
#' @export
assert_is_open_connection <- function(x, rw = "")
{                                                         
  assert_engine(x, is_open_connection, .xname = get_name_in_parent(x), rw = rw)   
}

#' @rdname is_connection
#' @export
assert_is_pipe_connection <- function(x)
{                                                         
  assert_engine(x, is_pipe_connection, .xname = get_name_in_parent(x))   
}

#' @rdname is_connection
#' @export
assert_is_readable_connection <- function(x)
{                                                         
  assert_engine(x, is_readable_connection, .xname = get_name_in_parent(x))   
}

#' @rdname is_connection
#' @export
assert_is_socket_connection <- function(x)
{                                                         
  assert_engine(x, is_socket_connection, .xname = get_name_in_parent(x))   
}

#' @rdname is_connection
#' @export
assert_is_stderr <- function(x)
{                                                         
  assert_engine(x, is_stderr, .xname = get_name_in_parent(x))   
}

#' @rdname is_connection
#' @export
assert_is_stdin <- function(x)
{                                                         
  assert_engine(x, is_stdin, .xname = get_name_in_parent(x))   
}

#' @rdname is_connection
#' @export
assert_is_stdout <- function(x)
{                                                         
  assert_engine(x, is_stdout, .xname = get_name_in_parent(x))   
}

#' @rdname is_connection
#' @export
assert_is_terminal_connection <- function(x)
{                                                         
  assert_engine(x, is_terminal_connection, .xname = get_name_in_parent(x))   
}

#' @rdname is_connection
#' @export
assert_is_text_connection <- function(x)
{                                                         
  assert_engine(x, is_text_connection, .xname = get_name_in_parent(x))   
}

#' @rdname is_connection
#' @export
assert_is_unz_connection <- function(x)
{                                                         
  assert_engine(x, is_unz_connection, .xname = get_name_in_parent(x))   
}

#' @rdname is_connection
#' @export
assert_is_url_connection <- function(x)
{                                                         
  assert_engine(x, is_url_connection, .xname = get_name_in_parent(x))   
}

#' @rdname is_connection
#' @export
assert_is_writable_connection <- function(x)
{                                                         
  assert_engine(x, is_writable_connection, .xname = get_name_in_parent(x))   
}

#' @rdname is_connection
#' @export
assert_is_xzfile_connection <- function(x)
{                                                         
  assert_engine(x, is_xzfile_connection, .xname = get_name_in_parent(x))   
}
