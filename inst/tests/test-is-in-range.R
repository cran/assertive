test.is_in_closed_range.0_to_4_in_1_to_3.returns_true_inside_bounds <- function()
{
  x <- 0:4
  expected <- c(FALSE, TRUE, TRUE, TRUE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_in_closed_range(x, 1, 3)
  )
}


test.is_in_left_open_range.0_to_4_in_1_to_3.returns_true_inside_bounds <- function()
{
  x <- 0:4
  expected <- c(FALSE, FALSE, TRUE, TRUE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_in_left_open_range(x, 1, 3)
  )
}


test.is_in_open_range.0_to_4_in_1_to_3.returns_true_inside_bounds <- function()
{
  x <- 0:4
  expected <- c(FALSE, FALSE, TRUE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_in_open_range(x, 1, 3)
  )
}


test.is_in_range.0_to_4_in_1_to_3.returns_true_inside_bounds <- function()
{
  x <- 0:4
  expected <- c(FALSE, TRUE, TRUE, TRUE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_in_range(x, 1, 3)
  )
}


test.is_in_right_open_range.0_to_4_in_1_to_3.returns_true_inside_bounds <- function()
{
  x <- 0:4
  expected <- c(FALSE, TRUE, TRUE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_in_right_open_range(x, 1, 3)
  )
}


test.is_negative.minus_2_to_2.returns_true_when_negative <- function()
{
  x <- -2:2
  expected <- c(TRUE, TRUE, FALSE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_negative(x)
  )
}


test.is_non_negative.minus_2_to_2.returns_true_when_non_negative <- function()
{
  x <- -2:2
  expected <- c(FALSE, FALSE, TRUE, TRUE, TRUE)
  names(expected) <- x
  checkEquals(
    expected,
    is_non_negative(x)
  )
}


test.is_non_positive.minus_2_to_2.returns_true_when_non_positive <- function()
{
  x <- -2:2
  expected <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_non_positive(x)
  )
}


test.is_percentage.minus_minus_1_to_101.returns_true_when_percentage <- function()
{
  x <- -1:101
  expected <- c(FALSE, rep.int(TRUE, 101), FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_percentage(x)
  )
}


test.is_positive.minus_2_to_2.returns_true_when_positive <- function()
{
  x <- -2:2
  expected <- c(FALSE, FALSE, FALSE, TRUE, TRUE)
  names(expected) <- x
  checkEquals(
    expected,
    is_positive(x)
  )
}


test.is_proportion.minus_minus_point_01_to_1_point_01.returns_true_when_percentage <- function()
{
  x <- seq.int(-0.01, 1.01, 0.01)
  expected <- c(FALSE, rep.int(TRUE, 101), FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_proportion(x)
  )
}
