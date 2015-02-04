test_that("test.coerce_to.numeric_vector_to_data_frame.returns_data_frame", 
  {
    x <- 1:5
    expected <- data.frame(x = x)
    expect_equal(suppressWarnings(coerce_to(x, "data.frame")), expected)
    expect_warning(coerce_to(x, "data.frame"))
  })

test_that(
  "test.parenthesise.character_input.returns_parenthesised_input",  
  {
    x <- "foo"
    types <- eval(formals(parenthesise)$type)
    actual <- vapply(
      types,
      function(type) parenthesise(x, type),
      character(1),
      USE.NAMES = FALSE
    )
    expected <- c(
      "(foo)", "[foo]", "{foo}", "<foo>", "\u3008foo\u3009", 
      "- foo -", "\u2013 foo \u2013", "\u2014foo\u2014", ", foo, "
    )
    expect_identical(actual, expected)
  }
)

test_that("test.use_first.a_list_double_indexing.returns_contents_of_first_element", 
  {
    x <- as.list(letters)
    expected <- "a"
    expect_identical(expected, suppressWarnings(use_first(x)))
    expect_warning(use_first(x))
  })

test_that("test.use_first.a_list_single_indexing.returns_first_element", {
  x <- as.list(letters)
  expected <- list("a")
  expect_identical(expected, suppressWarnings(use_first(x, "[")))
  expect_warning(use_first(x, "["))
})

test_that("test.use_first.a_scalar.returns_x", {
  x <- "a"
  expected <- x
  expect_identical(expected, use_first(x))
})

test_that("test.use_first.a_vector_double_indexing.returns_first_element", 
  {
    x <- letters
    expected <- "a"
    expect_identical(expected, suppressWarnings(use_first(x)))
    expect_warning(use_first(x))
  })

test_that("test.use_first.a_vector_single_indexing.returns_first_element", 
  {
    x <- letters
    expected <- "a"
    expect_identical(expected, suppressWarnings(use_first(x, "[")))
    expect_warning(use_first(x, "["))
  })

test_that("test.use_first.empty.throws_error", {
  x <- NULL
  expect_error(use_first(x))
}) 
