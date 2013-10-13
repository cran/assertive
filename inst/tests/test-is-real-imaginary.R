test.is_imaginary.imaginary_numbers.returns_true_when_purely_imaginary <- function()
{
  x <- c(1i, 1 + 0i, 0i, 1 + 1i, Inf, NA_complex_)
  checkEquals(
    c(TRUE, FALSE, TRUE, FALSE, FALSE, NA),
    is_imaginary(x)
    )
} 

test.is_imaginary.real_numbers.returns_true_when_0 <- function()
{
  x <- c(1, 0, -1, Inf, NA_real_)
  checkEquals(
    c(FALSE, TRUE, FALSE, FALSE, NA),
    is_imaginary(x)
    )
} 


test.is_real.imaginary_numbers.returns_true_when_purely_real <- function()
{
  x <- c(1i, 1 + 0i, 0i, 1 + 1i, Inf, NA_complex_)
  checkEquals(
    c(FALSE, TRUE, TRUE, FALSE, TRUE, NA),
    is_real(x)
    )
} 

test.is_real.real_numbers.returns_true_always <- function()
{
  x <- c(1, 0, -1, Inf, NA_real_)
  checkEquals(
    rep.int(TRUE, 5),
    is_real(x)
    )
} 
