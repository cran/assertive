test.is2.1_to_5_is_numeric.returns_true <- function()
{
  checkTrue(is2(1:5, "numeric"))
}

test.is2.1_to_5_is_list.returns_false <- function()
{
  checkTrue(!is2(1:5, "list"))
}

test.is2.1_to_5_is_nonsense.returns_false <- function()
{
  checkTrue(!is2(1:5, "a b c"))
}


test.is_array.an_array.returns_true <- function()
{
  checkTrue(is_array(array()))
}

test.is_array.a_matrix.returns_true <- function()
{
  checkTrue(is_array(matrix()))
}

test.is_array.a_data.frame.returns_false <- function()
{
  checkTrue(!is_array(data.frame(x = 1:5)))
}

test.is_array.a_vector.returns_false <- function()
{
  checkTrue(!is_array(1:10))
}


test.is_call.a_call.returns_true <- function()
{
  checkTrue(is_call(call("sin", "pi")))
}

test.is_atomic.not_a_call.returns_false <- function()
{
  checkTrue(!is_call(expression(sin(pi))))
}


test.is_character.character_vector.returns_true <- function()
{
  checkTrue(is_character(letters))
}

test.is_character.NA_character_.returns_true <- function()
{
  checkTrue(is_character(NA_character_))
}

test.is_character.not_a_character_vector.returns_false <- function()
{
  checkTrue(!is_character(1:10))
}


test.is_class.lm_numeric_raster.returns_true <- function()
{
  x <- c("lm", "numeric", "raster")
  expected <- c(TRUE, TRUE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_class(x)
  )
}


test.is_complex.1i.returns_true <- function()
{
  checkTrue(is_complex(1i))
}

test.is_complex.1.returns_false <- function()
{
  checkTrue(!is_complex(1L))
}

test.is_complex.1_plus_0i.returns_true <- function()
{
  checkTrue(is_complex(1 + 0i))
}

test.is_complex.na_complex_.returns_true <- function()
{
  checkTrue(is_complex(NA_complex_))
}


test.is_data.frame.a_data.frame.returns_true <- function()
{
  checkTrue(is_data.frame(data.frame(x = 1:5)))
}

test.is_data.frame.not_a_data.frame.returns_false <- function()
{
  checkTrue(!is_data.frame(list(x = 1:5)))
}


test.is_environment.an_environment.returns_true <- function()
{
  checkTrue(is_environment(new.env()))
}

test.is_environment.global_environment.returns_true <- function()
{
  checkTrue(is_environment(globalenv()))
}

test.is_environment.base_environment.returns_true <- function()
{
  checkTrue(is_environment(baseenv()))
}

test.is_environment.not_an_environment.returns_false <- function()
{
  checkTrue(!is_environment(list()))
}


test.is_expression.an_environment.returns_true <- function()
{
  checkTrue(is_expression(expression(sin(pi))))
}

test.is_expression.not_an_expression.returns_false <- function()
{
  checkTrue(!is_expression(call("sin", "pi")))
}


test.is_factor.a_factor.returns_true <- function()
{
  checkTrue(is_factor(factor(letters)))
}

test.is_factor.an_ordered_factor.returns_true <- function()
{
  checkTrue(is_factor(ordered(letters, levels = letters)))
}

test.is_factor.not_a_factor.returns_false <- function()
{
  checkTrue(!is_factor(letters))
}


test.is_function.a_function.returns_true <- function()
{
  checkTrue(is_function(function(){}))
}

test.is_function.a_primitive_function.returns_true <- function()
{
  checkTrue(is_function(sqrt))
}

test.is_function.not_a_function.returns_false <- function()
{
  checkTrue(!is_function(call("sin", "pi")))
}


test.is_integer.an_integer_vector.returns_true <- function()
{
  checkTrue(is_integer(1L:10L))
}

test.is_integer.na_integer_.returns_true <- function()
{
  checkTrue(is_integer(NA_integer_))
}

test.is_integer.not_an_integer.returns_false <- function()
{
  checkTrue(!is_integer(pi:10))
}


test.is_language.a_call.returns_true <- function()
{
  checkTrue(is_language(call("sin", "pi")))
}

test.is_language.an_expression.returns_true <- function()
{
  checkTrue(is_language(expression(sin(pi))))
}

test.is_language.a_name.returns_true <- function()
{
  checkTrue(is_language(as.name("foo")))
}

test.is_language.not_a_language_object.returns_false <- function()
{
  checkTrue(!is_language(sin))
}


test.is_leaf.a_leaf.returns_true <- function()
{
  x <- structure(list(), leaf = TRUE)
  checkTrue(is_leaf(x))
}

test.is_leaf.a_null_leaf.returns_false <- function()
{
  x <- list()
  checkTrue(!is_leaf(x))
}

test.is_leaf.a_non_logical_leaf.returns_false <- function()
{
  x <- structure(list(), leaf = 1:10)
  checkTrue(!is_leaf(x))
}

test.is_leaf.a_false_leaf.returns_false <- function()
{
  x <- structure(list(), leaf = FALSE)
  checkTrue(!is_leaf(x))
}


test.is_list.a_list.returns_true <- function()
{
  checkTrue(is_list(list(1,2,3)))
}

test.is_list.an_atomic_vector.returns_false <- function()
{
  checkTrue(!is_list(1:10))
}

test.is_list.null.returns_false <- function()
{
  checkTrue(!is_list(NULL))
}


test.is_logical.a_logical_vector.returns_true <- function()
{
  checkTrue(is_logical(c(TRUE, FALSE)))
}

test.is_logical.na.returns_true <- function()
{
  checkTrue(is_logical(NA))
}

test.is_logical.not_a_logical.returns_false <- function()
{
  checkTrue(!is_logical(1:10))
}


test.is_matrix.a_matrix.returns_true <- function()
{
  checkTrue(is_matrix(matrix()))
}

test.is_matrix.an_array.returns_false <- function()
{
  checkTrue(!is_matrix(array()))
}

test.is_matrix.a_data.frame.returns_false <- function()
{
  checkTrue(!is_matrix(data.frame(x = 1:5)))
}

test.is_matrix.a_vector.returns_false <- function()
{
  checkTrue(!is_matrix(1:10))
}


test.is_name.a_name.returns_true <- function()
{
  checkTrue(is_name(as.name("foo")))
}

test.is_name.not_a_name.returns_false <- function()
{
  checkTrue(!is_name(call("sin", "pi")))
}


test.is_numeric.a_numeric_vector.returns_true <- function()
{
  checkTrue(is_numeric(1:10))
}

test.is_numeric.an_integer_vector.returns_true <- function()
{
  checkTrue(is_numeric(1L:10L))
}

test.is_numeric.not_numeric.returns_false <- function()
{
  checkTrue(!is_numeric(c(TRUE, FALSE)))
}


test.is_ordered.an_ordered_factor.returns_true <- function()
{
  checkTrue(is_ordered(ordered(letters, levels = letters)))
}

test.is_ordered.an_unordered_factor.returns_false <- function()
{
  checkTrue(!is_ordered(factor(letters)))
}

test.is_ordered.not_a_factor.returns_false <- function()
{
  checkTrue(!is_ordered(letters))
}


test.is_primitive.a_primitive_function.returns_true <- function()
{
  checkTrue(is_primitive(sqrt))
}

test.is_primitive.a_regular_function.returns_false <- function()
{
  checkTrue(!is_primitive(function(){}))
}

test.is_primitive.not_a_function.returns_false <- function()
{
  checkTrue(!is_primitive(call("sin", "pi")))
}


test.is_raster.a_raster.returns_true <- function()
{
  m <- matrix(hcl(0, 80, seq(50, 80, 10)), nrow=4, ncol=5)
  checkTrue(is_raster(as.raster(m)))
}

test.is_raster.a_matrix.returns_false <- function()
{
  m <- matrix(hcl(0, 80, seq(50, 80, 10)), nrow=4, ncol=5)
  checkTrue(!is_raster(m))
}


test.is_raw.a_raw_vector.returns_true <- function()
{
  checkTrue(is_raw(as.raw(1:10)))
}

test.is_raw.not_raw.returns_false <- function()
{
  checkTrue(!is_raw(c(TRUE, FALSE)))
}


test.is_relistable.a_relistable_object.returns_true <- function()
{
  checkTrue(is_relistable(as.relistable(list(1,2,3))))
}

test.is_relistable.not_relistable.returns_false <- function()
{
  checkTrue(!is_relistable(list(1,2,3)))
}


test.is_S4.an_S4_instance.returns_true <- function()
{
  x <- getClass("MethodDefinition")
  checkTrue(is_S4(x))
}

test.is_S4.not_an_S4_instance.returns_true <- function()
{
  checkTrue(!is_S4(1:10))
}


test.is_stepfun.a_step_function.returns_true <- function()
{
  x <- stepfun(1:3, c(1, 2, 4, 3), f = 0)
  checkTrue(is_stepfun(x))
}

test.is_stepfun.a_regular_function.returns_false <- function()
{
  checkTrue(!is_stepfun(function(){}))
}

test.is_stepfun.not_a_function.returns_false <- function()
{
  checkTrue(!is_stepfun(call("sin", "pi")))
}


test.is_table.a_table.returns_true <- function()
{
  x <- table(sample(letters, 100, replace = TRUE))
  checkTrue(is_table(x))
}

test.is_table.not_a_table.returns_false <- function()
{
  checkTrue(!is_table(1:10))
}


test.is_ts.a_time_series.returns_true <- function()
{
  checkTrue(is_ts(ts(1:10)))
}

test.is_ts.not_a_time_series.returns_false <- function()
{
  checkTrue(!is_ts(1:10))
}


test.is_tskernel.a_time_series_kernel.returns_true <- function()
{
  checkTrue(is_tskernel(kernel("daniell", 10)))
}

test.is_tskernel.not_a_time_series_kernel.returns_false <- function()
{
  checkTrue(!is_tskernel(1:10))
}
