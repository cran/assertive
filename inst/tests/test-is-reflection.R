test.is_64_bit_os.any_os.returns_pointer_size_equals_8 <- function()
{
  expected <- .Machine$sizeof.pointer == 8
  checkEquals(expected, is_64_bit_os(), check.attributes = FALSE)
}

test.is_batch_mode.any_mode.returns_true_if_called_from_batch_mode <- function()
{
  expected <- !is.na(Sys.getenv("R_BATCH", NA))
  checkEquals(expected, is_batch_mode(), check.attributes = FALSE)
}

test.is_comma_for_decimal_point.any_locale.returns_true_if_locale_uses_comma <- function()
{
  expected <- Sys.localeconv()["mon_decimal_point"] == ","
  checkEquals(expected, is_comma_for_decimal_point(), check.attributes = FALSE)
}

test.is_interactive.any_mode.returns_true_if_r_runs_interactively <- function()
{
  expected <- interactive()
  checkEquals(expected, is_interactive(), check.attributes = FALSE)
}

test.is_linux.any_mode.returns_true_if_os_is_linux <- function()
{
  expected <- Sys.info()["sysname"] == "Linux"
  checkEquals(expected, is_linux(), check.attributes = FALSE)
}

test.is_mac.any_mode.returns_true_if_os_is_osx <- function()
{
  expected <- Sys.info()["sysname"] == "Darwin"
  checkEquals(expected, is_mac(), check.attributes = FALSE)
}

test.is_on_os_path.os_paths.returns_true_for_all <- function()
{
  paths <- strsplit(Sys.getenv("path"), ";")[[1]]
  checkTrue(all(is_on_os_path(paths)))
}

test.is_on_os_path.made_up_paths.returns_false_for_all <- function()
{
  paths <- c("a made up path", "path with bad chars !@#$%^&*(){}[]<>;:/?'")
  checkTrue(!any(is_on_os_path(paths)))
}

test.is_period_for_decimal_point.any_locale.returns_true_if_locale_uses_period <- function()
{
  expected <- Sys.localeconv()["mon_decimal_point"] == "."
  checkEquals(expected, is_period_for_decimal_point(), check.attributes = FALSE)
}

test.is_r.r_or_s.returns_true_if_is_r <- function()
{
  expected <- exists("is.R") && is.function(is.R) && is.R()
  checkEquals(expected, is_r(), check.attributes = FALSE)
}

test.is_unix.any_os.returns_true_if_os_is_unix_based <- function()
{
  expected <- .Platform$OS.type == "unix"
  checkEquals(expected, is_unix(), check.attributes = FALSE)
}

test.is_windows.any_os.returns_true_if_os_is_windows <- function()
{
  expected <- .Platform$OS.type == "windows"
  checkEquals(expected, is_windows(), check.attributes = FALSE)
}
