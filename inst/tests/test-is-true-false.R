test.is_false.false.returns_true <- function()
{
  checkTrue(is_false(FALSE))
} 

test.is_false.false_vector.returns_false <- function()
{
  checkTrue(!is_false(logical(2)))
} 

test.is_false.NA.returns_false <- function()
{
  checkTrue(!is_false(NA))
} 

test.is_false.true_with_attr.returns_allow_attributes <- function()
{
  x <- false("This has an attribute.")
  checkTrue(!is_false(x))
  checkTrue(is_false(x, allow_attributes = TRUE))
} 


test.is_true.true.returns_true <- function()
{
  checkTrue(is_true(TRUE))
} 

test.is_true.true_vector.returns_false <- function()
{
  checkTrue(!is_true(rep.int(TRUE, 2)))
} 

test.is_true.NA.returns_false <- function()
{
  checkTrue(!is_true(NA))
}

test.is_true.true_with_attr.returns_allow_attributes <- function()
{
  x <- c(truth = TRUE)
  checkTrue(!is_true(x))
  checkTrue(is_true(x, allow_attributes = TRUE))
} 
