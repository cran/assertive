test_that("test.is_valid_variable_name.1_x.returns_false", {
  expect_false(is_valid_variable_name("1x"))
})

test_that("test.is_valid_variable_name.2_dots.returns_true", {
  expect_true(is_valid_variable_name(".."))
})

test_that("test.is_valid_variable_name.3_dots.returns_allow_reserved", {
  expect_true(is_valid_variable_name("..."))
  expect_false(is_valid_variable_name("...", allow_reserved = FALSE))
})

test_that("test.is_valid_variable_name.4_dots.returns_true", {
  expect_true(is_valid_variable_name("...."))
})

test_that("test.is_valid_variable_name.5_dots.returns_true", {
  expect_true(is_valid_variable_name("....."))
})

test_that("test.is_valid_variable_name.dash.returns_false", {
  expect_false(is_valid_variable_name("_"))
})

test_that("test.is_valid_variable_name.dot.returns_true", {
  expect_true(is_valid_variable_name("."))
})

test_that("test.is_valid_variable_name.dot_1.returns_false", {
  expect_false(is_valid_variable_name(".1"))
})

test_that("test.is_valid_variable_name.dot_dash.returns_true", {
  expect_true(is_valid_variable_name("._"))
})

test_that("test.is_valid_variable_name.dot_dash_1.returns_true", {
  expect_true(is_valid_variable_name("._1"))
})

test_that("test.is_valid_variable_name.dot_dot_1.returns_allow_reserved", {
  expect_true(is_valid_variable_name("..1"))
  expect_false(is_valid_variable_name("..1", allow_reserved = FALSE))
})

test_that("test.is_valid_variable_name.dot_dot_2.returns_allow_reserved", {
  expect_true(is_valid_variable_name("..2"))
  expect_false(is_valid_variable_name("..2", allow_reserved = FALSE))
})

test_that("test.is_valid_variable_name.dot_dot_dot_1.returns_true", {
  expect_true(is_valid_variable_name("...1"))
})

test_that("test.is_valid_variable_name.dot_dot_dot_dot_1.returns_true", {
  expect_true(is_valid_variable_name("....1"))
})

test_that("test.is_valid_variable_name.dot_dot_x.returns_true", {
  expect_true(is_valid_variable_name("..x"))
})

test_that("test.is_valid_variable_name.dot_x.returns_true", {
  expect_true(is_valid_variable_name(".x"))
})

test_that("test.is_valid_variable_name.long_name.returns_false", {
  vn <- paste(rep.int("a", 10001L), collapse = "")
  expect_false(is_valid_variable_name(vn))
})

test_that("test.is_valid_variable_name.x.returns_true", {
  expect_true(is_valid_variable_name("x"))
}) 
