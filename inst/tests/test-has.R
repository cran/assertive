test.has_attributes.struct_with_attrs.returns_true_if_attr_exists <- function()
{
  x <- structure(list(a = 1), foo = 1, bar = 2)
  attrs <- c("names", "foo", "bar", "baz", NA)
  expected <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
  names(expected) <- attrs
  checkEquals(
    expected,
    has_attributes(x, c("names", "foo", "bar", "baz", NA))
  )
}


test.has_colnames.with_colnames.returns_true <- function()
{
  x <- matrix(1:12, nrow = 3, dimnames = list(letters[1:3], LETTERS[1:4]))
  checkTrue(has_colnames(x))
}

test.has_colnames.without_colnames.returns_false <- function()
{
  x <- matrix(1:12, nrow = 3)
  checkTrue(!has_colnames(x))
}

test.has_colnames.with_empty_colnames.returns_false <- function()
{
  x <- matrix(
    1:12, 
    nrow = 3, 
    dimnames = list(character(3), character(4))
  )
  checkTrue(!has_colnames(x))
}


test.has_cols.with_columns.returns_true <- function()
{
  x <- matrix(1:12, nrow = 3)
  checkTrue(has_cols(x))
}

test.has_cols.without_columns.returns_true <- function()
{
  x <- 1:10
  checkTrue(!has_cols(x))
}


test.has_dimnames.with_dimnames.returns_true <- function()
{
  x <- matrix(1:12, nrow = 3, dimnames = list(letters[1:3], LETTERS[1:4]))
  checkTrue(has_dimnames(x))
}

test.has_dimnames.without_dimnames.returns_false <- function()
{
  x <- matrix(1:12, nrow = 3)
  checkTrue(!has_dimnames(x))
}

test.has_dimnames.with_empty_dimnames.returns_false <- function()
{
  x <- matrix(
    1:12, 
    nrow = 3, 
    dimnames = list(character(3), character(4))
  )
  checkTrue(!has_dimnames(x))
}


test.has_dims.a_matrix.returns_true <- function()
{
  mat <- matrix(1:12, nrow = 3)
  checkTrue(has_dims(mat))
}
              
test.has_dims.a_data_frame.returns_true <- function()
{
  dfr <- data.frame(x = 1:5, y = runif(5))
  checkTrue(has_dims(dfr))
}
                                          
test.has_dims.a_vector.returns_false <- function()
{
  x <- 1:3
  checkTrue(!has_dims(x))
}


test.has_no_duplicates.without_duplicates.returns_false <- function()
{
  x <- 1:10
  checkTrue(has_no_duplicates(x))
}

test.has_no_duplicates.with_duplicates.returns_false <- function()
{
  x <- rep.int(1, 2)
  checkTrue(!has_no_duplicates(x))
}


test.has_duplicates.without_duplicates.returns_false <- function()
{
  x <- 1:10
  checkTrue(!has_duplicates(x))
}

test.has_duplicates.with_duplicates.returns_false <- function()
{
  x <- rep.int(1, 2)
  checkTrue(has_duplicates(x))
}


test.has_names.named_vector.returns_true <- function()
{
  x <- c(foo = 1, 2, 3)
  checkTrue(has_names(x))
}
              
test.has_names.data_frame.returns_true <- function()
{
  dfr <- data.frame(x = 1:5, y = runif(5))
  checkTrue(has_names(dfr))
}
              
test.has_names.unnamed_vector.returns_false <- function()
{
  x <- 1:3
  checkTrue(!has_names(x))
}




test.has_rows.with_rows.returns_true <- function()
{
  x <- matrix(1:12, nrow = 3)
  checkTrue(has_rows(x))
}

test.has_rows.without_rows.returns_true <- function()
{
  x <- 1:10
  checkTrue(!has_rows(x))
}

test.has_rownames.with_rownames.returns_true <- function()
{
  x <- matrix(1:12, nrow = 3, dimnames = list(letters[1:3], LETTERS[1:4]))
  checkTrue(has_rownames(x))
}

test.has_rownames.without_rownames.returns_false <- function()
{
  x <- matrix(1:12, nrow = 3)
  checkTrue(!has_rownames(x))
}

test.has_rownames.with_empty_rownames.returns_false <- function()
{
  x <- matrix(
    1:12, 
    nrow = 3, 
    dimnames = list(character(3), character(4))
  )
  checkTrue(!has_rownames(x))
}


test.has_terms.without_terms.returns_false <- function()
{
  x <- 1:10
  checkTrue(!has_terms(x))
}

test.has_terms.with_terms_component.returns_false <- function()
{
  x <- list(terms = 1:10)
  checkTrue(has_terms(x))
}

test.has_terms.with_terms_attribute.returns_false <- function()
{
  x <- 1:10
  attr(x, "terms") <- 1:10
  checkTrue(has_terms(x))
}

test.has_terms.lm_model.returns_false <- function()
{
  x <- lm(uptake ~ conc, CO2)
  checkTrue(has_terms(x))
}
