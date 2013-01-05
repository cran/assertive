#' @rdname is_windows
#' @export
is_64_bit_os <- function()
{
  if(.Machine$sizeof.pointer != 8)
  {
    return(false("The operating system is not 64 bit."))
  }
  TRUE
}

#' How is R running?
#' 
#' Tests to see if R is running in batch mode/interactively.
#' 
#' @return \code{is_batch_mode} returns \code{TRUE} if R is running in batch mode.
#' \code{is_interactive} returns \code{TRUE} if R is running interactively.
#' @seealso \code{\link[base]{EnvVar}} and \code{\link[base]{interactive}}.
#' @export
is_batch_mode <- function()
{
  if(is.na(Sys.getenv("R_BATCH", NA)))
  {
    return(false("R is not running in batch mode."))
  }
  TRUE
}

#' @rdname is_xxx_for_decimal_point
#' @export
is_comma_for_decimal_point <- function()
{
  is_xxx_for_decimal_point(",")
}

#' @rdname is_batch_mode
#' @export
is_interactive <- function()
{
  if(!interactive())
  {
    return(false("R is not running interactively."))
  }
  TRUE
}

#' @rdname is_windows
#' @export
is_linux <- function()
{
  if(Sys.info()["sysname"] != "Linux")
  {
    return(false("The operating system is not Linux."))
  }
  TRUE
}

#' @rdname is_windows
#' @export
is_mac <- function()
{
  if(Sys.info()["sysname"] != "Darwin")
  {
    return(false("The operating system is not OS X (or another Darwin-based OS)."))
  }
  TRUE
}

#' Is the path on the OS path?
#'
#' Is the specified path on the operating system search path?
#' 
#' @param x An path to check.
#' @note The OS search path is determined with \code{Sys.getenv("path")}.
#' @return \code{TRUE} if the sepcified paths are on the OS search path.
#' @examples
#' is_on_os_path(c(R.home("bin"), "a nonexistent path")) #probably c(TRUE, FALSE)
#' @export
is_on_os_path <- function(x)
{
  paths <- normalizePath(strsplit(Sys.getenv("path"), ";")[[1]], mustWork = FALSE)
  x <- normalizePath(coerce_to(x, "character"), mustWork = FALSE)
  call_and_name(function(x) x %in% paths, x)  
}

#' @rdname is_xxx_for_decimal_point
#' @export
is_period_for_decimal_point <- function()
{
  is_xxx_for_decimal_point(".")
}

#' Are you running R?
#'
#' Checks to see you are running R.
#'
#' @return \code{is_r} wraps \code{is.R}, providing more 
#' information on failure.  \code{assert_is_r} returns nothing but
#' throws an error if \code{is_R} returns \code{FALSE}.
#' @seealso \code{\link[base]{is.R}}.
#' @examples
#' \dontrun{
#' assert_is_r()
#' }
#' @export
is_r <- function()
{
  if(!exists("is.R") || !is.function(is.R) || !is.R())
  {
    return(false("You are not running R."))
  } 
  TRUE
}

#' @rdname is_windows
#' @export
is_unix <- function()
{
  if(.Platform$OS.type != "unix")
  {
    return(false("The operating system is not Unix-based."))
  }
  TRUE
}

#' What OS is running?
#' 
#' Is the operating system in this machine Windows/Unix/Mac based.
#' 
#' @return \code{is_windows} returns \code{TRUE} if the OS on the current platform
#' is Microsoft windows-based.  \code{is_unix} returns \code{TRUE} if the OS is
#' Unix based (pretty much anything that isn't Windows, including OS X). 
#' \code{is_mac} and \code{is_linux} return \code{TRUE} if the OS is Linux or 
#' Apple OS X respectively.
#' \code{is_64_bit_os} returns \code{TRUE} when the operating system is 64-bit.
#' The \code{assert_*} functions return nothing but throw an error if the 
#' corresponding \code{is_*} functions return \code{FALSE}.
#' @examples
#' is_windows()
#' is_unix()
#' is_mac()
#' is_linux()
#' @seealso \code{\link[base]{.Platform}}, \code{\link[base]{Sys.info}} and 
#' \code{\link[base]{version}}.
#' @export
is_windows <- function()
{
  if(.Platform$OS.type != "windows")
  {
    return(false("The operating system is not Windows."))
  }
  TRUE
}

#' What does the current locale specify for the decimal point?
#' 
#' Does the current locale specify a comma or a period for the decimal point?
#' 
#' @param dp Character to be used as a decimal point.
#' @return \code{is_comma_for_decimal_point} returns \code{TRUE} when the current 
#' locale uses a comma for a decimal place, as determined by \code{Sys.localeconv}.  
#' Similarly, \code{is_period_for_decimal_point} returns \code{TRUE} when the current 
#' locale uses a period (a.k.a. full stop) for a decimal place.  If R has been 
#' compiled without support for locales, then the value will always be \code{NA}.
#' @examples
#' #A useful guess for reading in files:
#' read_csv <- if(is_comma_for_decimal_point()) read.csv else read.csv2 
#' #Force locale and test (may require admin rights)
#' \dontrun{
#' current_locale <- sys_get_locale()
#' a_period_locale <- if(is_windows()) "English_United Kingdom.1252" else if(is_mac()) "en_GB" else if(is_linux()) "en_GB.utf8" else "en"
#' sys_set_locale(LC_ALL = a_period_locale)
#' assert_is_period_for_decimal_point()
#' a_comma_locale <- if(is_windows()) "French_France.1252" else if(is_mac()) "fr_FR" else if(is_linux()) "fr_FR.utf8" else "fr"
#' sys_set_locale(LC_ALL = a_comma_locale)
#' assert_is_comma_for_decimal_point()
#' suppressWarnings(sys_set_locale(l = current_locale))
#' }
is_xxx_for_decimal_point <- function(dp)
{
  locale_conventions <- Sys.localeconv()
  if(is.null(locale_conventions))
  {
    return(na("R has been compiled without support for locales."))
  }
  if(locale_conventions["mon_decimal_point"] != dp)
  {
    return(false(
      "The locale convention is to use a '%s' for a decimal point.", 
      locale_conventions["mon_decimal_point"]
    ))
  }
  TRUE
}