#' Does the file exist?
#'
#' Checks to see if the input files exist.
#'
#' @param x Input to check.
#' @return \code{is_existing_file} wraps \code{file.exists}, showing
#' the names of the inputs in the answer.   \code{assert_is_existing_file} 
#' returns nothing but throws an error if \code{is_existing_file} returns
#' \code{FALSE}.
#' @seealso \code{\link[base]{file.exists}}.
#' @examples
#' assert_all_are_existing_files(c(dir(), "~", getwd()))
#' \dontrun{
#' assert_all_are_existing_files("not an existing file (probably)")
#' }
#' @export
is_existing_file <- function(x)
{
  x <- coerce_to(x, "character")
  call_and_name(file.exists, x)
}

#' Is the file accessible?
#'
#' Checks to see if the input files can be executed/read/written to.
#'
#' @param x Input to check.
#' @return \code{is_ex_file} wraps \code{file.access}, showing
#' the names of the inputs in the answer.   \code{assert_is_ex_file} 
#' returns nothing but throws an error if \code{is_ex_file} returns
#' \code{FALSE}.
#' @seealso \code{\link[base]{file.access}}.
#' @examples
#' \dontrun{
#' assert_all_are_readable_files(dir())
#' }
#' @export
is_ex_file <- function(x)
{
  x <- coerce_to(x, "character")
  call_and_name(file.access, x, mode = 1) == 0L
}

#' @rdname is_ex_file
#' @export
is_readable_file <- function(x)
{
  x <- coerce_to(x, "character")
  call_and_name(file.access, x, mode = 4) == 0L
}

#' @rdname is_ex_file
#' @export
is_writable_file <- function(x)
{
  x <- coerce_to(x, "character")
  call_and_name(file.access, x, mode = 2) == 0L
}