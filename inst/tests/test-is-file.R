test.is_dir.some_paths.returns_true_when_path_is_dir <- function()
{
  x <- c(R.home(), dir(R.home("bin"), full.names = TRUE))
  expected <- c(TRUE, logical(length(x) - 1))  
  names(expected) <- x
  checkEquals(
    expected,
    is_dir(x)
  )
}

test.is_existing_file.some_paths.returns_true_when_file_exists <- function()
{
  tf <- tempfile()
  file.create(tf)
  x <- c("~", getwd(), tf, "~not an existing file~")
  expected <- c(TRUE, TRUE, TRUE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_existing_file(x)
  )  
}

test.is_ex_file.r_exes.returns_true <- function()
{  
  x <- dir(R.home("bin"), "\\.exe$", full.names = TRUE)
  expected <- rep.int(TRUE, length(x))
  names(expected) <- x
  checkEquals(
    expected,
    is_ex_file(x)
  )  
}

test.is_library.some_paths.returns_true_when_path_is_library <- function()
{
  x <- c(.libPaths(), "a made up directory")
  expected <- c(rep.int(TRUE, length(x) - 1), FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_library(x)
  )
}

test.is_readable_file.r_bin_files.returns_true <- function()
{  
  #R should have access to read its own bin directory.
  x <- dir(R.home("bin"), full.names = TRUE)
  expected <- rep.int(TRUE, length(x))
  names(expected) <- x
  checkEquals(
    expected,
    is_readable_file(x)
  )  
}

test.is_writable_file.tempfile.returns_true <- function()
{  
  file.create(x <- tempfile()) #should be able to write to temp dir
  expected <- TRUE
  names(expected) <- x
  checkEquals(
    expected,
    is_writable_file(x)
  )  
  unlink(x)
}
