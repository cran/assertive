test_that("test.is_32_bit.any_version_of_r.returns_pointer_size_equals_4", {
  expected <- .Machine$sizeof.pointer == 4
  actual <- is_32_bit()
  expect_equal(strip_attributes(actual), expected)
  if (!actual) {
    expect_equal(cause(actual), noquote("R is not 32 bit."))
  }
})

test_that("test.is_64_bit.any_version_of_r.returns_pointer_size_equals_8", {
  expected <- .Machine$sizeof.pointer == 8
  actual <- is_64_bit()
  expect_equal(strip_attributes(actual), expected)
  if (!actual) {
    expect_equal(cause(actual), noquote("R is not 64 bit."))
  }
})
test_that("test.is_architect.some_ide.returns_true_if_ide_is_architect",
{
  rj_is_loaded <- "package:rj" %in% search()
  device_name <- formals(getOption("device"))$name
  expected <- rj_is_loaded && !is.null(device_name) && device_name == "rj.gd"
  actual <- is_architect()
  expect_equal(strip_attributes(actual), expected)
  if (!actual) {
    expect_equal(cause(actual), noquote("You are not running Architect/StatET."))
  }  
})

test_that("test.is_batch_mode.any_mode.returns_true_if_called_from_batch_mode", 
  {
    expected <- !is.na(Sys.getenv("R_BATCH", NA))
    actual <- is_batch_mode()
    expect_equal(strip_attributes(actual), expected)
    if (!actual) {
      expect_equal(cause(actual), noquote("R is not running in batch mode."))
    }
  })

test_that("test.is_bsd.any_os.returns_true_if_os_is_osx", {
  expected <- grepl("BSD", unname(Sys.info()["sysname"]))
  actual <- is_bsd()
  expect_equal(strip_attributes(actual), expected)
  if (!actual) {
    expect_equal(cause(actual), cause(not_this_os("BSD-based")))
  }
})

test_that("test.is_comma_for_decimal_point.any_locale.returns_true_if_locale_uses_comma", 
{
  dp <- unname(Sys.localeconv()["decimal_point"])
  expected <- if(is.null(dp) || !nzchar(dp)) NA else dp == ","
  actual <- is_comma_for_decimal_point()
  expect_equal(strip_attributes(actual), expected)
  if (!isTRUE(actual)) 
  {
    msg <- if(is.null(dp))
    {
      "R has been compiled without support for locales."
    } else if(!nzchar(dp))
    {
      "The locale convention for a (numeric) decimal point has not been defined."
    } else
    {
      "The locale convention is to use a '.' for a (numeric) decimal point."
    }
    expect_equal(cause(actual), noquote(msg))
  }
})

test_that("test.is_comma_for_decimal_point.any_locale_money_type.returns_true_if_locale_uses_comma", 
{
  dp <- unname(Sys.localeconv()["mon_decimal_point"])
  expected <- if(is.null(dp) || !nzchar(dp)) NA else dp == ","
  actual <- is_comma_for_decimal_point("money")
  expect_equal(strip_attributes(actual), expected)
  if (!isTRUE(actual)) 
  {
    msg <- if(is.null(dp))
    {
      "R has been compiled without support for locales."
    } else if(!nzchar(dp))
    {
      "The locale convention for a (monetary) decimal point has not been defined."
    } else
    {
      "The locale convention is to use a '.' for a (monetary) decimal point."
    }
    expect_equal(cause(actual), noquote(msg))
  }
})

test_that("test.is_interactive.any_mode.returns_true_if_r_runs_interactively", 
{
  expected <- interactive()
  actual <- is_interactive()
  expect_equal(strip_attributes(actual), expected)
  if (!actual) {
    expect_equal(cause(actual), noquote("R is not running interactively."))
  }
})

test_that("test.is_linux.any_os.returns_true_if_os_is_linux", {
  expected <- unname(Sys.info()["sysname"] == "Linux")
  actual <- is_linux()
  expect_equal(strip_attributes(actual), expected)
  if (!actual) {
    expect_equal(cause(actual), cause(not_this_os("Linux")))
  }
})

test_that("test.is_mac.any_os.returns_true_if_os_is_osx", {
  expected <- unname(Sys.info()["sysname"] == "Darwin")
  actual <- is_mac()
  expect_equal(strip_attributes(actual), expected)
  if (!actual) {
    expect_equal(cause(actual), cause(not_this_os("OS X")))
  }
})

test_that("test.is_on_os_path.made_up_paths.returns_false_for_all", {
  paths <- c("a made up path", "path with bad chars !@#$%^&*(){}[]<>;:/?'")
  expect_false(any(is_on_os_path(paths)))
})

test_that("test.is_on_os_path.os_paths.returns_true_for_all", {
  paths <- strsplit(Sys.getenv("path"), ";")[[1]]
  expect_true(all(is_on_os_path(paths)))
})

test_that("test.is_period_for_decimal_point.any_locale.returns_true_if_locale_uses_period", 
{
  dp <- unname(Sys.localeconv()["decimal_point"])
  expected <- if(is.null(dp) || !nzchar(dp)) NA else dp == "."
  actual <- is_period_for_decimal_point()
  expect_equal(strip_attributes(actual), expected)
  if (!isTRUE(actual)) 
  {
    msg <- if(is.null(dp))
    {
      "R has been compiled without support for locales."
    } else if(!nzchar(dp))
    {
      "The locale convention for a (numeric) decimal point has not been defined."
    } else
    {
      "The locale convention is to use a ',' for a (numeric) decimal point."
    }
    expect_equal(cause(actual), noquote(msg))
  }
})

test_that("test.is_period_for_decimal_point.any_locale_money_type.returns_true_if_locale_uses_period", 
{
  dp <- unname(Sys.localeconv()["mon_decimal_point"])
  expected <- if(is.null(dp) || !nzchar(dp)) NA else dp == "."
  actual <- is_period_for_decimal_point("money")
  expect_equal(strip_attributes(actual), expected)
  if (!isTRUE(actual)) 
  {
    msg <- if(is.null(dp))
    {
      "R has been compiled without support for locales."
    } else if(!nzchar(dp))
    {
      "The locale convention for a (monetary) decimal point has not been defined."
    } else
    {
      "The locale convention is to use a ',' for a (monetary) decimal point."
    }
    expect_equal(cause(actual), noquote(msg))
  }
})

test_that("test.is_r.r_or_s.returns_true_if_is_r", {
  expected <- exists("is.R") && is.function(is.R) && is.R()
  actual <- is_r()
  expect_equal(strip_attributes(actual), expected)
  if (!actual) {
    expect_equal(cause(actual), noquote("You are not running R."))
  }
})

test_that("test.is_r_alpha.any_r.returns_true_if_is_r_alpha", 
{
  expected <- version$status == "alpha"
  actual <- is_r_alpha()
  expect_equal(strip_attributes(actual), expected)
  if (!actual) {
    expect_equal(
      cause(actual), 
      noquote(
        sprintf(
          "You are running a %s build of R, not an alpha build.",
          clean_status_string()
        )
      )
    )
  }
})

test_that("test.is_r_beta.any_r.returns_true_if_is_r_beta", 
{
  expected <- version$status == "beta"
  actual <- is_r_beta()
  expect_equal(strip_attributes(actual), expected)
  if (!actual) {
    expect_equal(
      cause(actual), 
      noquote(
        sprintf(
          "You are running a %s build of R, not a beta build.",
          clean_status_string()
        )
      )
    )
  }
})

test_that("test.is_r_devel.any_r.returns_true_if_is_r_devel", {
  expected <- version$status == "Under development (unstable)"
  actual <- is_r_devel()
  expect_equal(strip_attributes(actual), expected)
  if (!actual) {
    expect_equal(
      cause(actual), 
      noquote(
        sprintf(
          "You are running a %s build of R, not a development build.",
          clean_status_string()
        )
      )
    )
  }
})

test_that("test.is_r_patched.any_r.returns_true_if_is_r_patched", {
  expected <- version$status == "Patched"
  actual <- is_r_patched()
  expect_equal(strip_attributes(actual), expected)
  if (!actual) {
    expect_equal(
      cause(actual), 
      noquote(
        sprintf(
          "You are running a %s build of R, not a patched build.",
          clean_status_string()
        )
      )
    )
  }
})

test_that("test.is_r_stable.any_r.returns_true_if_is_r_stable", 
{
  expected <- version$status == ""
  actual <- is_r_stable()
  expect_equal(strip_attributes(actual), expected)
  if (!actual) {
    expect_equal(
      cause(actual), 
      noquote(
        sprintf(
          "You are running a %s build of R, not a stable build.",
          clean_status_string()
        )
      )
    )
  }
})

test_that("test.is_revo_r.any_os.returns_true_if_ide_is_revo_r", {
  expected <- exists("Revo.version", "package:base", inherits = FALSE) &&
    is.list(get("Revo.version", "package:base", inherits = FALSE))
  actual <- is_revo_r()
  expect_equal(strip_attributes(actual), expected)
  if (!actual) {
    expect_equal(cause(actual), noquote("You are not running Revolution R."))
  }
})

test_that("test.is_rstudio.any_os.returns_true_if_ide_is_rstudio", 
{
  gui <- .Platform$GUI
  expected <- !is.null(gui) && gui == "RStudio"
  actual <- is_rstudio()
  expect_equal(strip_attributes(actual), expected)
  if (!actual) {
    expect_equal(cause(actual), noquote("You are not running RStudio."))
  }
})

test_that("test.is_revo_r.any_os.returns_true_if_ide_is_revo_r", {
  expected <- exists("Revo.version", "package:base", inherits = FALSE) &&
    is.list(get("Revo.version", "package:base", inherits = FALSE))
  actual <- is_revo_r()
  expect_equal(strip_attributes(actual), expected)
  if (!actual) {
    expect_equal(cause(actual), noquote("You are not running Revolution R."))
  }
})

test_that("test.is_slave_r.any_os.returns_true_if_os_is_osx", {
  expected <- "--slave" %in% commandArgs()
  actual <- is_slave_r()
  expect_equal(strip_attributes(actual), expected)
  if (!actual) {
    expect_equal(
      cause(actual), 
      noquote("You are not running a slave instance of R.")
    )
  }
})

test_that("test.is_unix.any_os.returns_true_if_os_is_unix_based", {
  expected <- .Platform$OS.type == "unix"
  actual <- is_unix()
  expect_equal(strip_attributes(actual), expected)
  if (!actual) {
    expect_equal(cause(actual), cause(not_this_os("Unix-based")))
  }
})

test_that("test.is_windows.any_os.returns_true_if_os_is_windows", {
  expected <- .Platform$OS.type == "windows"
  actual <- is_windows()
  expect_equal(strip_attributes(actual), expected)
  if (!actual) {
    expect_equal(cause(actual), cause(not_this_os("Windows")))
  }
}) 

test_that(
  "test.not_this_os_returns_os_message",
  {
    os <-"foo"
    expected_cause <- noquote(
      paste0(
        "The operating system is not ",
        os,
        ". R reports it as: Sys.info()['sysname'] = ",
        Sys.info()['sysname'],
        ", .Platform$OS = ", 
        .Platform$OS,
        "."
      )
    )
    actual <- not_this_os(os)
    expect_equal(strip_attributes(actual), FALSE)
    expect_equal(cause(actual), expected_cause)
  }
)
