test_that("test.is_false.logical_vector.returns_true_when_false", {
  x <- c(TRUE, FALSE, NA)
  expected <- c(FALSE, TRUE, FALSE)
  expect_equal(strip_attributes(actual <- assertive::is_false(x)), expected)
  expect_equal(names(actual), as.character(x))
  expect_equal(cause(actual), noquote(c("true", "", "missing")))
})

test_that("test.is_identical_to_false.false.returns_true", {
  expect_true(is_identical_to_false(FALSE))
})

test_that("test.is_identical_to_false.false_vector.returns_false", {
  expect_false(is_identical_to_false(logical(2)))
})

test_that("test.is_identical_to_false.NA.returns_false", {
  expect_false(is_identical_to_false(NA))
})

test_that("test.is_identical_to_false.false_with_attr.returns_allow_attributes", 
{
  x <- c(truth = FALSE)
  expect_false(is_identical_to_false(x))
  expect_true(is_identical_to_false(x, allow_attributes = TRUE))
})

test_that("test.is_identical_to_true.NA.returns_false", {
  expect_false(is_identical_to_true(NA))
})

test_that("test.is_identical_to_true.true.returns_true", {
  expect_true(is_identical_to_true(TRUE))
})

test_that("test.is_identical_to_true.true_vector.returns_false", {
  expect_false(is_identical_to_true(rep.int(TRUE, 2)))
})

test_that("test.is_identical_to_true.true_with_attr.returns_allow_attributes", 
  {
    x <- c(truth = TRUE)
    expect_false(is_identical_to_true(x))
    expect_true(is_identical_to_true(x, allow_attributes = TRUE))
  })

test_that("test.is_true.logical_vector.returns_true_when_true", {
  x <- c(TRUE, FALSE, NA)
  expected <- c(TRUE, FALSE, FALSE)
  expect_equal(strip_attributes(actual <- assertive::is_true(x)), expected)
  expect_equal(names(actual), as.character(x))
  expect_equal(cause(actual), noquote(c("", "false", "missing")))
}) 
