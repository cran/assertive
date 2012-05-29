test.is_empty.empty_vector.returns_true <- function()
{
  checkTrue(is_empty(numeric()))
}

test.is_empty.empty_list.returns_true <- function()
{
  checkTrue(is_empty(list()))
}

test.is_empty.null.returns_true <- function()
{
  checkTrue(is_empty(NULL))
}

test.is_empty.non_empty_vector.returns_false <- function()
{
  checkTrue(!is_empty(1))
}


test.is_empty_model.an_empty_model.returns_true <- function()
{
  checkTrue(is_empty_model(lm(y ~ 0, data.frame(y = 1:5))))
}

test.is_empty_model.a_model_with_intercept.returns_false <- function()
{
  checkTrue(!is_empty_model(lm(y ~ 1, data.frame(y = 1:5))))
}

test.is_empty_model.a_model_with_factors.returns_false <- function()
{
  checkTrue(!is_empty_model(lm(y ~ x + 0, data.frame(y = 1:5, x = 1:5))))
}

test.is_empty_model.not_a_model.returns_false <- function()
{
  checkTrue(!is_empty_model(1:10))
}


test.is_non_empty.non_empty_vector.returns_true <- function()
{
  checkTrue(is_non_empty(1))
}

test.is_non_empty.empty_vector.returns_false <- function()
{
  checkTrue(!is_non_empty(numeric()))
}

test.is_non_empty.empty_list.returns_false <- function()
{
  checkTrue(!is_non_empty(list()))
}

test.is_non_empty.null.returns_false <- function()
{
  checkTrue(!is_non_empty(NULL))
}


test.is_non_empty_model.a_model_with_intercept.returns_true <- function()
{
  checkTrue(is_non_empty_model(lm(y ~ 1, data.frame(y = 1:5))))
}

test.is_non_empty_model.a_model_with_factors.returns_true <- function()
{
  checkTrue(is_non_empty_model(lm(y ~ x + 0, data.frame(y = 1:5, x = 1:5))))
}

test.is_non_empty_model.an_empty_model.returns_false <- function()
{
  checkTrue(!is_non_empty_model(lm(y ~ 0, data.frame(y = 1:5))))
}

test.is_non_empty_model.not_a_model.returns_false <- function()
{
  checkTrue(!is_non_empty_model(1:10))
}


test.is_scalar.a_scalar.returns_true <- function()
{
  checkTrue(is_scalar(1))
} 

test.is_scalar.a_vector.returns_false <- function()
{
  checkTrue(!is_scalar(1:2))
} 

test.is_scalar.empty.returns_false <- function()
{
  checkTrue(!is_scalar(numeric()))
} 
