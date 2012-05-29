test.is_a_bool.true.returns_true <- function()
{
  checkTrue(is_a_bool(TRUE))
}

test.is_a_bool.false.returns_true <- function()
{
  checkTrue(is_a_bool(FALSE))
}

test.is_a_bool.na.returns_true <- function()
{
  checkTrue(is_a_bool(NA))
}
           
test.is_a_bool.a_vector.returns_false <- function()
{
  checkTrue(!is_a_bool(c(TRUE, FALSE)))
}

test.is_a_bool.empty_logical.returns_false <- function()
{
  checkTrue(!is_a_bool(logical()))
}


test.is_a_complex.1i.returns_true <- function()
{
  checkTrue(is_a_complex(1i))
}

test.is_a_complex.1.returns_false <- function()
{
  checkTrue(!is_a_complex(1L))
}

test.is_a_complex.1_plus_0i.returns_true <- function()
{
  checkTrue(is_a_complex(1 + 0i))
}

test.is_a_complex.na_complex_.returns_true <- function()
{
  checkTrue(is_a_complex(NA_complex_))
}

test.is_a_complex.a_vector.returns_false <- function()
{
  checkTrue(!is_a_complex(c(1i, 2i)))
}

test.is_a_complex.empty_complex.returns_false <- function()
{
  checkTrue(!is_a_number(complex()))
}


test.is_a_non_empty_string.non_empty_string.returns_true <- function()
{
  checkTrue(is_a_non_empty_string("foo"))
}

test.is_a_non_empty_string.na_character_.returns_true <- function()
{
  checkTrue(is_a_non_empty_string(NA_character_))
}

test.is_a_non_empty_string.empty_string.returns_false <- function()
{
  checkTrue(!is_a_non_empty_string(""))
}

test.is_a_non_empty_string.empty_character.returns_false <- function()
{
  checkTrue(!is_a_non_empty_string(character()))
}


test.is_a_number.1.returns_true <- function()
{
  checkTrue(is_a_number(1))
}

test.is_a_number.1L.returns_true <- function()
{
  checkTrue(is_a_number(1L))
}

test.is_a_number.Inf.returns_true <- function()
{
  checkTrue(is_a_number(Inf))
}

test.is_a_number.na_real_.returns_true <- function()
{
  checkTrue(is_a_number(NA_real_))
}

test.is_a_number.a_vector.returns_false <- function()
{
  checkTrue(!is_a_number(1:10))
}

test.is_a_number.empty_numeric.returns_false <- function()
{
  checkTrue(!is_a_number(numeric()))
}


test.is_a_raw.a_raw.returns_true <- function()
{
  checkTrue(is_a_raw(as.raw(1)))
}

test.is_a_raw.a_vector.returns_false <- function()
{
  checkTrue(!is_a_raw(as.raw(1:10)))
}

test.is_a_raw.empty_raw.returns_false <- function()
{
  checkTrue(!is_a_raw(raw()))
}


test.is_a_string.foo.returns_true <- function()
{
  checkTrue(is_a_string("foo"))
}

test.is_a_string.na.returns_true <- function()
{
  checkTrue(is_a_string(NA_character_))
}
                     
test.is_a_string.empty_string.returns_true <- function()
{
  checkTrue(is_a_string(""))
}               
                  
test.is_a_string.a_vector.returns_false <- function()
{
  checkTrue(!is_a_string(c("foo", "bar")))
}

test.is_a_string.empty_character.returns_false <- function()
{
  checkTrue(!is_a_string(character()))
}


test.is_an_empty_string.empty_string.returns_true <- function()
{
  checkTrue(is_an_empty_string(""))
}

test.is_an_empty_string.non_empty_string.returns_false <- function()
{
  checkTrue(!is_an_empty_string("foo"))
}

test.is_an_empty_string.empty_character.returns_false <- function()
{
  checkTrue(!is_an_empty_string(character()))
}

test.is_an_empty_string.na_character_.returns_false <- function()
{
  checkTrue(!is_an_empty_string(NA_character_))
}



test.is_an_integer.1L.returns_true <- function()
{
  checkTrue(is_an_integer(1L))
}

test.is_an_integer.minus_1L.returns_true <- function()
{
  checkTrue(is_an_integer(-1L))
}

test.is_an_integer.na.returns_true <- function()
{
  checkTrue(is_an_integer(NA_integer_))
}

test.is_an_integer.floating_point.returns_false <- function()
{
  checkTrue(!is_an_integer(1))
}

test.is_an_integer.a_vector.returns_false <- function()
{
  checkTrue(!is_an_integer(1L:2L))
}

test.is_an_integer.empty_integer.returns_false <- function()
{
  checkTrue(!is_an_integer(integer()))
}
