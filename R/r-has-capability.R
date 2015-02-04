#' Can R find tools for compiling code?
#' 
#' Checks to see if R can see the \code{gcc} and \code{make} tools in order
#' to compile code.
#' @return \code{r_can_compile_code} returns \code{TRUE} if R can see \code{gcc} 
#' and \code{make} tools, and \code{FALSE} (with a cause) otherwise.
#' \code{assert_r_can_compile_code} returns nothing but throws an error if 
#' \code{r_can_compile_code} function returns \code{FALSE}.
#' @examples
#' r_can_compile_code()
#' dont_stop(assert_r_can_compile_code())
#' @export
r_can_compile_code <- function()
{
  tools <- c("gcc", "make")
  paths <- Sys.which(tools)
  not_found <- !nzchar(paths)
  if(any(not_found))
  {
    return(
      false(
        "R cannot find the %s %s.", 
        toString(tools[not_found]),
        ngettext(sum(not_found), "tool", "tools")
      )
    )
  }
  TRUE
}

#' Does R have a capability?
#' 
#' Check to see if R has a specific capability.
#' @return The \code{is_*} functions return \code{TRUE} if R has the capability 
#' and \code{FALSE} (with a cause) otherwise.
#' The \code{assert_*} functions return nothing but throw an error if the 
#' corresponding \code{is_*} function returns \code{FALSE}.
#' @seealso \code{\link[base]{capabilities}}
#' @aliases r_has_capability
#' @examples
#' \dontrun{
#' if(r_has_png_capability())
#' {
#'   png("test.png")
#'   with(cars, plot(speed, dist))
#'   dev.off()
#' } else 
#' {
#'   pdf("test.pdf")
#'   with(cars, plot(speed, dist))
#'   dev.off()
#' }
#' }
#' @export
r_has_jpeg_capability <- function()
{
  if(!capabilities("jpeg"))
  {
    return(false("R does not have jpeg capability."))
  }
  TRUE
}

#' @rdname r_has_jpeg_capability
#' @export
r_has_png_capability <- function()
{
  if(!capabilities("png"))
  {
    return(false("R does not have png capability."))
  }
  TRUE
}

#' @rdname r_has_jpeg_capability
#' @export
r_has_tiff_capability <- function()
{
  if(!capabilities("tiff"))
  {
    return(false("R does not have tiff capability."))
  }
  TRUE
}

#' @rdname r_has_jpeg_capability
#' @export
r_has_tcltk_capability <- function()
{
  if(!capabilities("tcltk"))
  {
    return(false("R does not have tcltk capability."))
  }
  TRUE
}

#' @rdname r_has_jpeg_capability
#' @export
r_has_x11_capability <- function()
{
  if(!capabilities("X11"))
  {
    return(false("R does not have X11 capability."))
  }
  TRUE
}

#' @rdname r_has_jpeg_capability
#' @export
r_has_aqua_capability <- function()
{
  if(!capabilities("aqua"))
  {
    return(false("R does not have aqua capability."))
  }
  TRUE
}

#' @rdname r_has_jpeg_capability
#' @export
r_has_http_ftp_capability <- function()
{
  if(!capabilities("http/ftp"))
  {
    return(false("R does not have http/ftp capability."))
  }
  TRUE
}

#' @rdname r_has_jpeg_capability
#' @export
r_has_sockets_capability <- function()
{
  if(!capabilities("sockets"))
  {
    return(false("R does not have sockets capability."))
  }
  TRUE
}

#' @rdname r_has_jpeg_capability
#' @export
r_has_libxml_capability <- function()
{
  if(!capabilities("libxml"))
  {
    return(false("R does not have libxml capability."))
  }
  TRUE
}

#' @rdname r_has_jpeg_capability
#' @export
r_has_fifo_capability <- function()
{
  if(!capabilities("fifo"))
  {
    return(false("R does not have fifo capability."))
  }
  TRUE
}

#' @rdname r_has_jpeg_capability
#' @export
r_has_cledit_capability <- function()
{
  if(!capabilities("cledit"))
  {
    return(false("R does not have cledit capability."))
  }
  TRUE
}

#' @rdname r_has_jpeg_capability
#' @export
r_has_iconv_capability <- function()
{
  if(!capabilities("iconv"))
  {
    return(false("R does not have iconv capability."))
  }
  TRUE
}

#' @rdname r_has_jpeg_capability
#' @export
r_has_nls_capability <- function()
{
  if(!capabilities("NLS"))
  {
    return(false("R does not have NLS capability."))
  }
  TRUE
}

#' @rdname r_has_jpeg_capability
#' @export
r_has_profmem_capability <- function()
{
  if(!capabilities("profmem"))
  {
    return(false("R does not have profmem capability."))
  }
  TRUE
}

#' @rdname r_has_jpeg_capability
#' @export
r_has_cairo_capability <- function()
{
  if(!capabilities("cairo"))
  {
    return(false("R does not have cairo capability."))
  }
  TRUE
}

#' @rdname r_has_jpeg_capability
#' @export
r_has_icu_capability <- function()
{
  if(as.package_version(version) < "3.1.2")
  {
    return(
      false("ICU capability is not declared for versions of R before 3.1.2.")
    )
  }
  if(!capabilities("ICU"))
  {
    return(false("R does not have ICU capability."))
  }
  TRUE
}

#' @rdname r_has_jpeg_capability
#' @export
r_has_long_double_capability <- function()
{
  if(as.package_version(version) < "3.2.0")
  {
    return(
      false("long.double capability is not declared for versions of R before 3.2.0.")
    )
  }
  if(!capabilities("long.double"))
  {
    return(false("R does not have long.double capability."))
  }
  TRUE
}

#' @rdname r_has_jpeg_capability
#' @export
r_has_libcurl_capability <- function()
{
  if(as.package_version(version) < "3.2.0")
  {
    return(
      false("libcurl capability is not declared for versions of R before 3.2.0.")
    )
  }
  if(!capabilities("libcurl"))
  {
    return(false("R does not have libcurl capability."))
  }
  TRUE
}
