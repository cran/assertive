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

test.is_executable_file.r_exes.returns_true <- function()
{  
  x <- dir(R.home("bin"), "\\.exe$", full.names = TRUE)
  expected <- rep.int(TRUE, length(x))
  names(expected) <- x
  checkEquals(
    expected,
    is_executable_file(x)
  )  
}

test.is_readable_file.r_bin_files.returns_true <- function()
{  
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
  unlink(tf)
}
