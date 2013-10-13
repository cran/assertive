test.is_atomic.logical.returns_true <- function()
{
  checkTrue(is_atomic(logical()))
}

test.is_atomic.integer.returns_true <- function()
{
  checkTrue(is_atomic(integer()))
}

test.is_atomic.numeric.returns_true <- function()
{
  checkTrue(is_atomic(numeric()))
}

test.is_atomic.complex.returns_true <- function()
{
  checkTrue(is_atomic(complex()))
}

test.is_atomic.raw.returns_true <- function()
{
  checkTrue(is_atomic(raw()))
}

test.is_atomic.matrix.returns_true <- function()
{
  checkTrue(is_atomic(matrix()))
}

test.is_atomic.array.returns_true <- function()
{
  checkTrue(is_atomic(array()))
}

test.is_atomic.null.returns_true <- function()
{
  checkTrue(is_atomic(NULL))
}

test.is_atomic.something_recursive.returns_false <- function()
{
  checkTrue(!is_atomic(list()))
}


test.is_recursive.a_list.returns_true <- function()
{
  checkTrue(is_recursive(list()))
}

test.is_recursive.an_expression.returns_true <- function()
{
  checkTrue(is_recursive(expression()))
}

test.is_recursive.a_data.frame.returns_true <- function()
{
  checkTrue(is_recursive(data.frame()))
}

test.is_recursive.a_formula.returns_true <- function()
{
  checkTrue(is_recursive(y ~ x))
}

test.is_recursive.a_function.returns_true <- function()
{
  checkTrue(is_recursive(function(){}))
}

test.is_recursive.a_call.returns_true <- function()
{
  checkTrue(is_recursive(call("sin", "pi")))
}

test.is_recursive.something_atomic.returns_false <- function()
{
  checkTrue(!is_recursive(1:10))
}


test.is_vector.logical.returns_true <- function()
{
  checkTrue(is_vector(logical()))
}

test.is_vector.integer.returns_true <- function()
{
  checkTrue(is_vector(integer()))
}

test.is_vector.numeric.returns_true <- function()
{
  checkTrue(is_vector(numeric()))
}

test.is_vector.complex.returns_true <- function()
{
  checkTrue(is_vector(complex()))
}

test.is_vector.character.returns_true <- function()
{
  checkTrue(is_vector(character()))
}

test.is_vector.raw.returns_true <- function()
{
  checkTrue(is_vector(raw()))
}

test.is_vector.list.returns_true <- function()
{
  checkTrue(is_vector(list()))
}

test.is_vector.expression.returns_true <- function()
{
  checkTrue(is_vector(expression()))
}

test.is_vector.not_a_vector.returns_false <- function()
{
  checkTrue(!is_vector(matrix()))
}
