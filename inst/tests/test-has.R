test_that(
  "test.has_attributes.struct_with_attrs.returns_true_if_attr_exists",
  {
    x <- structure(list(a = 1), foo = 1, bar = 2)
    attrs <- c("names", "foo", "bar", "baz", NA)
    expected <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
    names(expected) <- attrs
    expect_equal(
      has_attributes(x, c("names", "foo", "bar", "baz", NA)),
      expected
    )
  }
)

test_that(
  "test.has_colnames.with_colnames.returns_true",
  {
    x <- matrix(1:12, nrow = 3, dimnames = list(letters[1:3], LETTERS[1:4]))
    expect_true(has_colnames(x))
  }
)

test_that(
  "test.has_colnames.without_colnames.returns_false",
  {
    x <- matrix(1:12, nrow = 3)
    actual <- has_colnames(x)
    expect_false(actual)
    expect_equal(cause(actual), noquote("The column names of x are NULL."))
  }
)

test_that(
  "test.has_colnames.with_empty_colnames.returns_false",
  {
    x <- matrix(
      1:12, 
      nrow = 3, 
      dimnames = list(character(3), character(4))
    )
    actual <- has_colnames(x)
    expect_false(actual)
    expect_equal(cause(actual), noquote("The column names of x are all empty."))
  }
)


test_that(
  "test.has_cols.with_columns.returns_true",
  {
    x <- matrix(1:12, nrow = 3)
    expect_true(has_cols(x))
  }
)

test_that(
  "test.has_cols.without_columns.returns_true",
  {
    x <- 1:10
    actual <- has_cols(x)
    expect_false(actual)
    expect_equal(
      cause(actual), 
      noquote("The number of columns in x is NULL.")
    )
  }
)

test_that(
  "test.has_rows.zero_cols.returns_false",
  {
    x <- matrix(numeric(), ncol = 0)
    actual <- has_cols(x)
    expect_false(actual)
    expect_equal(
      cause(actual), 
      noquote("The number of columns in x is zero.")
    )
  }
)


test_that(
  "test.has_dimnames.with_dimnames.returns_true",
  {
    x <- matrix(1:12, nrow = 3, dimnames = list(letters[1:3], LETTERS[1:4]))
    expect_true(has_dimnames(x))
  }
)

test_that(
  "test.has_dimnames.without_dimnames.returns_false",
  {
    x <- matrix(1:12, nrow = 3)
    actual <- has_dimnames(x)
    expect_false(actual)
    expect_equal(cause(actual), noquote("The dimension names of x are NULL."))
  }
)

test_that(
  "test.has_dimnames.with_empty_dimnames.returns_false",
  {
    x <- matrix(
      1:12, 
      nrow = 3, 
      dimnames = list(character(3), character(4))
    )
    actual <- has_dimnames(x)
    expect_false(actual)
    expect_equal(cause(actual), noquote("The dimension names of x are all empty."))
  }
)


test_that(
  "test.has_dims.a_matrix.returns_true",
  {
    mat <- matrix(1:12, nrow = 3)
    expect_true(has_dims(mat))
  }
)

test_that(
  "test.has_dims.a_data_frame.returns_true",
  {
    dfr <- data.frame(x = 1:5, y = runif(5))
    expect_true(has_dims(dfr))
  }
)

test_that(
  "test.has_dims.a_vector.returns_false",
  {
    x <- 1:3
    actual <- has_dims(x)
    expect_false(actual)
    expect_equal(cause(actual), noquote("The dimensions of x are NULL."))
  }
)


test_that(
  "test.has_no_duplicates.without_duplicates.returns_false",
  {
    x <- 1:10
    expect_true(has_no_duplicates(x))
  }
)

test_that(
  "test.has_no_duplicates.with_duplicates.returns_false",
  {
    x <- rep.int(1, 2)
    actual <- has_no_duplicates(x)
    expect_false(actual)
    expect_equal(cause(actual), noquote("x has duplicates."))
  }
)


test_that(
  "test.has_duplicates.without_duplicates.returns_false",
  {
    x <- 1:10
    actual <- has_duplicates(x)
    expect_false(actual)
    expect_equal(cause(actual), noquote("x has no duplicates."))
  }
)

test_that(
  "test.has_duplicates.with_duplicates.returns_false",
  {
    x <- rep.int(1, 2)
    expect_true(has_duplicates(x))
  }
)


test_that(
  "test.has_names.named_vector.returns_true",
  {
    x <- c(foo = 1, 2, 3)
    expect_true(has_names(x))
  }
)

test_that(
  "test.has_names.data_frame.returns_true",
  {
    dfr <- data.frame(x = 1:5, y = runif(5))
    expect_true(has_names(dfr))
  }
)

test_that(
  "test.has_names.unnamed_vector.returns_false",
  {
    x <- 1:3
    actual <- has_names(x)
    expect_false(actual)
    expect_equal(cause(actual), noquote("The names of x are NULL."))
  }
)


test_that(
  "test.has_rownames.with_rownames.returns_true",
  {
    x <- matrix(
      1:12, 
      nrow = 3, 
      dimnames = list(letters[1:3], LETTERS[1:4])
    )
    expect_true(has_rownames(x))
  }
)

test_that(
  "test.has_rownames.without_rownames.returns_false",
  {
    x <- matrix(1:12, nrow = 3)
    actual <- has_rownames(x)
    expect_false(actual)
    expect_equal(cause(actual), noquote("The row names of x are NULL."))
  }
)

test_that(
  "test.has_rownames.with_empty_rownames.returns_false",
  {
    x <- matrix(
      1:12, 
      nrow = 3, 
      dimnames = list(character(3), character(4))
    )
    actual <- has_rownames(x)
    expect_false(actual)
    expect_equal(cause(actual), noquote("The row names of x are all empty."))
  }
)


test_that(
  "test.has_rows.with_rows.returns_true",
  {
    x <- matrix(1:12, nrow = 3)
    expect_true(has_rows(x))
  }
)

test_that(
  "test.has_rows.without_rows.returns_false",
  {
    x <- 1:10
    actual <- has_rows(x)
    expect_false(actual)
    expect_equal(
      cause(actual), 
      noquote("The number of rows in x is NULL.")
    )
  }
)

test_that(
  "test.has_rows.zero_rows.returns_false",
  {
    x <- matrix(numeric(), nrow = 0)
    actual <- has_rows(x)
    expect_false(actual)
    expect_equal(
      cause(actual), 
      noquote("The number of rows in x is zero.")
    )
  }
)


test_that(
  "test.has_terms.without_terms.returns_false",
  {
    x <- 1:10
    actual <- has_terms(x)
    expect_false(actual)
    expect_equal(
      cause(actual), 
      noquote("x has no terms component nor attribute.")
    )
  }
)

test_that(
  "test.has_terms.with_terms_component.returns_false",
  {
    x <- list(terms = 1:10)
    expect_true(has_terms(x))
  }
)

test_that(
  "test.has_terms.with_terms_attribute.returns_false",
  {
    x <- 1:10
    attr(x, "terms") <- 1:10
    expect_true(has_terms(x))
  }
)

test_that(
  "test.has_terms.lm_model.returns_false",
  {
    x <- lm(uptake ~ conc, CO2)
    expect_true(has_terms(x))
  }
)
