test.is_not_na.not_na.returns_true <- function()
{
  checkTrue(is_not_na(1))
} 

test.is_not_na.na.returns_false <- function()
{
  checkTrue(!is_not_na(NA))
} 


test.is_not_nan.not_na.returns_true <- function()
{
  checkTrue(is_not_nan(1))
} 

test.is_not_nan.nan.returns_false <- function()
{
  checkTrue(!is_not_nan(NaN))
} 


test.is_null.null.returns_true <- function()
{
  checkTrue(is_null(NULL))
} 

test.is_null.na.returns_false <- function()
{
  checkTrue(!is_null(NA))
} 

test.is_null.nan.returns_false <- function()
{
  checkTrue(!is_null(NaN))
} 

