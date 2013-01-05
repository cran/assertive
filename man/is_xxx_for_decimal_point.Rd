\name{assert_is_comma_for_decimal_point}
\alias{assert_is_comma_for_decimal_point}
\alias{assert_is_period_for_decimal_point}
\alias{is_comma_for_decimal_point}
\alias{is_period_for_decimal_point}
\alias{is_xxx_for_decimal_point}
\title{What does the current locale specify for the decimal point?}
\usage{
  assert_is_comma_for_decimal_point()

  assert_is_period_for_decimal_point()

  is_comma_for_decimal_point()

  is_period_for_decimal_point()

  is_xxx_for_decimal_point(dp)
}
\arguments{
  \item{dp}{Character to be used as a decimal point.}
}
\value{
  \code{is_comma_for_decimal_point} returns \code{TRUE}
  when the current locale uses a comma for a decimal place,
  as determined by \code{Sys.localeconv}. Similarly,
  \code{is_period_for_decimal_point} returns \code{TRUE}
  when the current locale uses a period (a.k.a. full stop)
  for a decimal place.  If R has been compiled without
  support for locales, then the value will always be
  \code{NA}.
}
\description{
  Does the current locale specify a comma or a period for
  the decimal point?
}
\examples{
#A useful guess for reading in files:
read_csv <- if(is_comma_for_decimal_point()) read.csv else read.csv2
#Force locale and test (may require admin rights)
\dontrun{
current_locale <- sys_get_locale()
a_period_locale <- if(is_windows()) "English_United Kingdom.1252" else if(is_mac()) "en_GB" else if(is_linux()) "en_GB.utf8" else "en"
sys_set_locale(LC_ALL = a_period_locale)
assert_is_period_for_decimal_point()
a_comma_locale <- if(is_windows()) "French_France.1252" else if(is_mac()) "fr_FR" else if(is_linux()) "fr_FR.utf8" else "fr"
sys_set_locale(LC_ALL = a_comma_locale)
assert_is_comma_for_decimal_point()
suppressWarnings(sys_set_locale(l = current_locale))
}
}

