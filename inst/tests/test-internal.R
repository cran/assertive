test.character_to_list_of_integer_vectors.strings.returns_list_of_integer_vectors <- function()
{
  x <- c("12345", "1b3d5", "abcde", NA, "", " ", " 2 4 ") 
  expected <- list(
    1:5,
    c(1, NA, 3, NA, 5),
    rep.int(NA_integer_, 5),
    NA_integer_,
    numeric(),
    NA_integer_,
    c(NA, 2, NA, 4, NA)
  )
  names(expected) <- x
  checkEquals(
    expected,
    character_to_list_of_integer_vectors(x)
  )
}


test.matches_regex.strings.returns_true_when_string_matches_regex <- function()
{
  rx <- "foo"
  x <- c("foo", "fooo", "fo", "", "FOO", NA)  
  expected <- c(TRUE, TRUE, FALSE, FALSE, TRUE, NA)
  names(expected) <- x
  checkEquals(
    expected,
    matches_regex(x, rx)
  )
}
