#' @rdname is_real
#' @export
is_imaginary <- function(x)
{
  call_and_name(
    function(x)
    {
      ok <- Re(x) == 0
      set_cause(ok, "real")
    },
    x
  )
}

#' Is the input real/imaginary?
#'
#' Checks to see if the input is real or imaginary.
#'
#' @param x Input to check.
#' @return \code{TRUE} if the input has imaginary component equal to zero.
#' The \code{assert_*} functions return nothing but
#' throw an error if the corresponding \code{is_*} function returns 
#' \code{FALSE}.
#' @seealso \code{\link[base]{complex}}
#' @examples
#' (x <- with(expand.grid(re = -1:1, im = -1:1), re + im * 1i))
#' is_real(x)
#' is_imaginary(x)
#' assert_all_are_real(1:10)
#' assert_all_are_real(1:10 + 0i)
#' assert_any_are_real(c(1i, 0))
#' assert_all_are_imaginary(1:10 * 1i)
#' assert_any_are_imaginary(c(1i, 0))
#' dont_stop(assert_all_are_real(x))
#' dont_stop(assert_all_are_imaginary(x))
#' @export
is_real <- function(x)
{
  call_and_name(
    function(x)
    {
      ok <- Im(x) == 0
      set_cause(ok, "imaginary")
    },
    x
  )
}
