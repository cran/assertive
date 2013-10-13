test.is_debugged.a_function.returns_true_when_debugged <- function()
{
  x <- function() {}
  checkTrue(!is_debugged(x))
  debug(x)
  checkTrue(is_debugged(x))
  undebug(x)
  checkTrue(!is_debugged(x))  
}

test.is_existing.some_variables.returns_true_when_they_exist <- function()
{
  a_variable <- 1
  x <- c("a_variable", "not_a_variable")
  expected <- c(TRUE, FALSE)
  names(expected) <- x
  this_env <- sys.frame(sys.nframe())
  checkEquals(
    expected,
    is_existing(x, envir = this_env, inherits = FALSE)
  )
}

test.is_symmetric_matrix.a_symmetric_matrix.returns_true <- function()
{
  x <- diag(3)
  checkTrue(is_symmetric_matrix(x))
}

test.is_symmetric_matrix.a_symmetric_matrix.returns_logical <- function()
{
  x <- diag(3); x[3, 1] <- 1e-100
  checkTrue(is_symmetric_matrix(x))
  checkTrue(!is_symmetric_matrix(x, tol = 0))
}

test.is_symmetric_matrix.an_assymmetric_matrix.returns_false <- function()
{
  x <- matrix(rnorm(9), nrow = 3)
  checkTrue(!is_symmetric_matrix(x))
}

test.is_symmetric_matrix.not_coercible_to_matrix.throws_error <- function()
{
  suppressWarnings(checkException(is_symmetric_matrix(sqrt), silent = TRUE))
}


test.is_unsorted.an_unsorted_vector.returns_true <- function()
{
  checkTrue(is_unsorted(c(1, 3, 2)))
} 

test.is_unsorted.an_weakly_unsorted_vector.returns_strictly <- function()
{
  checkTrue(!is_unsorted(c(1, 1, 2)))
  checkTrue(is_unsorted(c(1, 1, 2), strictly = TRUE))
} 

test.is_unsorted.a_sorted_vector.returns_false <- function()
{
  checkTrue(!is_unsorted(1:3))
} 


test.is_whole_number.NA.returns_false <- function()
{
  x <- c(1, -1.5, 1 + .Machine$double.eps, 1 + 100 *.Machine$double.eps, Inf, NA)
  checkEquals(
    c(TRUE, FALSE, TRUE, FALSE, NA, NA),
    is_whole_number(x)
  )
}
