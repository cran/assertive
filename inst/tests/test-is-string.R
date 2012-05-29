test.is_numeric_string.a_character_vector.returns_true_when_string_contains_a_number <- function()
{
  x <- c("1", "-2.3e4", "Inf", "one", "NA")
  expected <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_numeric_string(x)
  )
} 


test.is_missing_or_empty_character.a_scalar.returns_logical <- function()
{
  x <- c("foo", "", NA_character_, " ")
  expected <- c(FALSE, TRUE, TRUE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_missing_or_empty_character(x)
  )
} 


test.is_not_missing_nor_empty_character.a_scalar.returns_logical <- function()
{
  x <- c("foo", "", NA_character_, " ")
  expected <- c(TRUE, FALSE, FALSE, TRUE)
  names(expected) <- x
  checkEquals(
    expected,
    is_not_missing_nor_empty_character(x)
    )
} 
