#' @rdname is_windows
#' @export
is_64_bit_os <- function()
{
  if(.Machine$sizeof.pointer != 8)
  {
    return(false("R is not 64 bit."))
  }
  TRUE
}

#' @rdname is_r
#' @export
is_architect <- function()
{
  if(!"package:rj" %in% search() ||
    is.null(device_name <- formals(getOption("device"))$name) ||
    device_name != "rj.gd")
  {
    return(false("You are not running Architect/StatET."))
  }
  TRUE
}

#' How is R running?
#' 
#' Tests to see if R is running in batch mode/interactively.
#' 
#' @return \code{is_batch_mode} returns \code{TRUE} if R is running in batch 
#' mode.
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

#' @rdname is_windows
#' @export
is_bsd <- function()
{
  if(!grepl("BSD", Sys.info()[["sysname"]]))
  {
    return(false("The operating system is not BSD-based."))
  }
  TRUE
}

#' @rdname is_xxx_for_decimal_point
#' @export
is_comma_for_decimal_point <- function(type = c("numbers", "money"))
{
  is_xxx_for_decimal_point(",", type)
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
    return(false(
      "The operating system is not OS X."
    ))
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
#' is_on_os_path(
#'   c(R.home("bin"), R.home("etc"), "a nonexistent path")
#' ) # probably c(TRUE, FALSE, FALSE)
#' @export
is_on_os_path <- function(x)
{
  call_and_name(
    function(x) 
    {
      x <- normalizePath(path.expand(coerce_to(x, "character")), mustWork = FALSE)
      paths <- normalizePath(
        strsplit(Sys.getenv("path"), ";")[[1]], 
        mustWork = FALSE
      )
      ok <- x %in% paths
      set_cause(ok, ifelse(file.exists(x), "not on path", "nonexistent"))
    }, 
    x
  )  
}

#' @rdname is_xxx_for_decimal_point
#' @export
is_period_for_decimal_point <- function(type = c("numbers", "money"))
{
  is_xxx_for_decimal_point(".", type)
}

#' Are you running R?
#'
#' Checks to see what type of R you are running.
#'
#' @return \code{is_r} wraps \code{is.R}, providing more 
#' information on failure.  \code{is_r_stable}, \code{is_r_patched} and
#' \code{is_r_devel} tell you what type of R build you are 
#' running.  \code{is_architect}, \code{is_rstudio} and \code{is_revo_r} tell
#' you if you are running Architect/StatET, RStudio, or Revolution Analytics'
#' Revolution R build.  \code{is_slave_r} tells you if you are running a slave
#' instance of R (e.g. when building a package with \code{devtools} or using a
#' cluster).
#' The \code{assert_*} functions return nothing but throw an error if 
#' the corresponding \code{is_*} function returns \code{FALSE}.
#' @seealso \code{\link[base]{is.R}}, \code{\link[base]{version}}.
#' @references \url{http://www.revolutionanalytics.com/revolution-r-open}
#' @examples
#' is_r()
#' is_r_stable()
#' is_r_patched()
#' is_r_devel()
#' is_architect()
#' is_revo_r()
#' is_rstudio()
#' is_slave_r()
#' switch(
#'   version$status,
#'   "Patched"                      = assert_is_r_patched(),
#'   "Under development (unstable)" = assert_is_r_devel(),
#'   assert_is_r_stable()
#' )
#' dont_stop(assert_is_r())
#' @export
is_r <- function()
{
  if(!exists("is.R") || !is.function(is.R) || !is.R())
  {
    return(false("You are not running R."))
  } 
  TRUE
}

#' @rdname is_r
#' @export
is_r_devel <- function()
{
  if(version$status != "Under development (unstable)")
  {
    return(false("You are not running a development build of R."))
  }
  TRUE
}

#' @rdname is_r
#' @export
is_r_patched <- function()
{
  if(version$status != "Patched")
  {
    return(false("You are not running a patched build of R."))
  }
  TRUE
}

#' @rdname is_r
#' @export
is_r_stable <- function()
{
  if(nzchar(version$status))
  {
    return(false("You are not running a stable build of R."))
  }
  TRUE
}

# This is to avoid a NOTE by R CMD check.  See 
# http://stackoverflow.com/q/9439256/134830 and
# http://stackoverflow.com/q/8096313/134830
if(getRversion() >= "2.15.1")  utils::globalVariables("Revo.version")

#' @rdname is_r
#' @export
is_revo_r <- function()
{
  if(!exists("Revo.version", "package:base", inherits = FALSE) || 
    !is.list(Revo.version))
  {
    return(false("You are not running Revolution R."))
  }
  TRUE
}

#' @rdname is_r
#' @export
is_rstudio <- function()
{
  gui <- .Platform$GUI
  if(is.null(gui) || gui != "RStudio")
  {
    return(false("You are not running RStudio."))
  }
  TRUE
}

#' @rdname is_r
#' @export
is_slave_r <- function()
{
  if(!("--slave" %in% commandArgs()))
  {
    return(false("You are not running a slave instance of R."))
  }
  TRUE
}

#' @rdname is_windows
#' @export
is_solaris <- function()
{
  if(Sys.info()["sysname"] != "SunOS")
  {
    return(false(
      "The operating system is not Solaris."
    ))
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
#' @return \code{is_windows} returns \code{TRUE} if the OS on the current 
#' platform is Microsoft windows-based.  \code{is_unix} returns \code{TRUE} if 
#' the OS is Unix based (pretty much anything that isn't Windows, including OS 
#' X). 
#' \code{is_mac}, \code{is_linux}, \code{is_bsd}, \code{is_solaris} return 
#' \code{TRUE} if the OS is Apple OS X, Linux, FreeBSD/NetBSD, or Solaris 
#' respectively.
#' \code{is_64_bit_os} returns \code{TRUE} when the operating system is 64-bit.
#' The \code{assert_*} functions return nothing but throw an error if the 
#' corresponding \code{is_*} functions return \code{FALSE}.
#' @references With the exception of \code{is_windows} and \code{is_unix} that 
#' use \code{.Platform$OS.type}, the OS is determined from 
#' \code{Sys.info()[["sysname"]]}, which (not on Windows) is calculated via the 
#' OS \code{uname} progam.  GNU has more information on the return value: 
#' \url{https://www.gnu.org/software/libc/manual/html_node/Platform-Type.html}
#' and Wikipedia has a nice list of possible values: 
#' \url{https://en.wikipedia.org/wiki/Uname#Examples}
#' @seealso \code{\link[base]{.Platform}}, \code{\link[base]{Sys.info}} and 
#' \code{\link[base]{version}}.
#' @examples
#' is_windows()
#' is_unix()
#' is_mac()
#' is_linux()
#' is_bsd()
#' is_solaris()
#' is_64_bit_os()
#' dont_stop(assert_is_windows())
#' dont_stop(assert_is_unix())
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
#' @param type Decimal point for numbers of money?
#' @return \code{is_comma_for_decimal_point} returns \code{TRUE} when the 
#' current locale uses a comma for a decimal place, as determined by 
#' \code{Sys.localeconv}.  Similarly, \code{is_period_for_decimal_point} returns 
#' \code{TRUE} when the current locale uses a period (a.k.a. full stop) for a 
#' decimal place.  If R has been compiled without support for locales, then the 
#' value will always be \code{NA}.
#' @references \url{http://www.cplusplus.com/reference/clocale/lconv/}
#' @seealso \code{\link[base]{Sys.localeconv}}
#' @examples
#' # Current settings:
#' is_comma_for_decimal_point()
#' is_comma_for_decimal_point("money")
#' # Or equivalently:
#' is_period_for_decimal_point()
#' is_period_for_decimal_point("money")
#' # A useful guess for reading in files:
#' read_csv <- if(is_comma_for_decimal_point()) read.csv else read.csv2 
#' \dontrun{
#' # Force locale and test (may require admin rights)
#' current_locale <- sys_get_locale()
#' a_period_locale <- if(is_windows()) 
#' {
#'   "English_United Kingdom.1252"
#' } else if(is_mac()) 
#' {
#'   "en_GB"
#' } else if(is_linux()) 
#' {
#'   "en_GB.utf8"
#' } else 
#' {
#'   "en"
#' }
#' sys_set_locale(LC_ALL = a_period_locale)
#' assert_is_period_for_decimal_point()
#' a_comma_locale <- if(is_windows())
#' {
#'   "French_France.1252"
#' } else if(is_mac()) 
#' {
#'   "fr_FR"
#' } else if(is_linux()) 
#' {
#'   "fr_FR.utf8" 
#' } else 
#' {
#'   "fr"
#' }
#' sys_set_locale(LC_ALL = a_comma_locale)
#' assert_is_comma_for_decimal_point()
#' suppressWarnings(sys_set_locale(l = current_locale))
#' }
is_xxx_for_decimal_point <- function(dp, type = c("numbers", "money"))
{
  locale_conventions <- Sys.localeconv()
  if(is.null(locale_conventions))
  {
    return(na("R has been compiled without support for locales."))
  }
  type <- match.arg(type)
  element <- switch(type, numbers = "decimal_point", money = "mon_decimal_point")
  if(locale_conventions[element] != dp)
  {
    if(!nzchar(locale_conventions[element]))
    {
      return(
        na(
          "The locale convention for a (%s) decimal point has not been defined.",
          switch(type, numbers = "numeric", money = "monetary")  
        )
      )
    }
    return(
      false(
        "The locale convention is to use a '%s' for a (%s) decimal point.", 
        locale_conventions[element],
        switch(type, numbers = "numeric", money = "monetary")  
      )
    )
  }
  TRUE
}
