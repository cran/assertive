test_that("test.is_not_na.na.returns_false", {
  expect_false(is_not_na(NA))
})

test_that("test.is_not_na.not_na.returns_true", {
  expect_true(is_not_na(1))
})

test_that("test.is_not_nan.nan.returns_false", {
  expect_false(is_not_nan(NaN))
})

test_that("test.is_not_nan.not_na.returns_true", {
  expect_true(is_not_nan(1))
})

test_that("test.is_null.na.returns_false", {
  expect_false(assertive::is_null(NA))
})

test_that("test.is_null.nan.returns_false", {
  expect_false(assertive::is_null(NaN))
})

test_that("test.is_null.null.returns_true", {
  expect_true(assertive::is_null(NULL))
}) 
