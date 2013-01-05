test.is_false.logical_vector.returns_true_when_false <- function()
{
  x <- c(TRUE, FALSE, NA)
  expected <- c(FALSE, TRUE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_false(x)
  )
}


test.is_identical_to_false.false.returns_true <- function()
{
  checkTrue(is_identical_to_false(FALSE))
} 

test.is_identical_to_false.false_vector.returns_false <- function()
{
  checkTrue(!is_identical_to_false(logical(2)))
} 

test.is_identical_to_false.NA.returns_false <- function()
{
  checkTrue(!is_identical_to_false(NA))
} 

test.is_identical_to_false.true_with_attr.returns_allow_attributes <- function()
{
  x <- false("This has an attribute.")
  checkTrue(!is_identical_to_false(x))
  checkTrue(is_identical_to_false(x, allow_attributes = TRUE))
} 


test.is_identical_to_true.true.returns_true <- function()
{
  checkTrue(is_identical_to_true(TRUE))
} 

test.is_identical_to_true.true_vector.returns_false <- function()
{
  checkTrue(!is_identical_to_true(rep.int(TRUE, 2)))
} 

test.is_identical_to_true.NA.returns_false <- function()
{
  checkTrue(!is_identical_to_true(NA))
}

test.is_identical_to_true.true_with_attr.returns_allow_attributes <- function()
{
  x <- c(truth = TRUE)
  checkTrue(!is_identical_to_true(x))
  checkTrue(is_identical_to_true(x, allow_attributes = TRUE))
} 


test.is_true.logical_vector.returns_true_when_true <- function()
{
  x <- c(TRUE, FALSE, NA)
  expected <- c(TRUE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_true(x)
  )
}
