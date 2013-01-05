test.is_cas_number.a_character_vector.returns_true_when_string_contains_a_cas_number <- function()
{
  x <- c(
    water = "7732-18-5", 
    d_glucose = "50-99-7",
    l_glucose = "921-60-8",
    no_hyphens = "7732185", 
    two_check_digits = "7732-18-55",
    bad_check_digit = "7732-18-4"
  )
  expected <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_cas_number(x)
  )
} 


test.is_credit_card_number.valid_card_numbers.returns_true_for_all <- function()
{
  x <- c(
    #visa
    "4111 1111 1111 1111",
    "4012888888881881",
    #mastercard
    "5555 5555 5555 4444",
    "5105 1051 0510 5100",
    #amex
    "3782 822463 10005",
    "3714 496353 98431",
    "3787 344936 71000", 
    #diners
    "3056 930902 5904",
    "3852 000002 3237",
    #discover
    "6011 1111 1111 1117",
    "6011 0009 9013 9424",
    #jcb
    "3530 1113 3330 0000",
    "3566 0020 2036 0505"
  )  
  expected <- rep.int(TRUE, length(x))
  names(expected) <- x
  checkEquals(
    expected,
    is_credit_card_number(x)
  )
}

test.is_credit_card_number.invalid_card_numbers.returns_false_for_all <- function()
{
  x <- c(
    #visa
    "4111 1111 1111 11111",  #too many digits
    "4012888888881882",      #bad check digit
    #mastercard
    "5655 5555 5555 4443",   #starts 56
    "51051 051 0510 5100",   #bad spacing
    #amex
    "3782 822463 1005"       #not enough digits
  )  
  expected <- rep.int(FALSE, length(x))
  names(expected) <- x
  checkEquals(
    expected,
    is_credit_card_number(x)
  )
}


test.is_date_string.a_character_vector.returns_true_when_string_contains_a_date <- function()
{
  x <- c("1999-12-31 23:59:59", "1979-08-01 01:00:00", "31 Dec 1999 11:59:59PM", "not a date", "NA")
  expected <- c(TRUE, TRUE, FALSE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_date_string(x)
  )
} 


test.is_email_address.a_character_vector_simple_match.returns_true_when_string_contains_an_email_address <- function()
{
  x <- c("foo@bar.com", "foo@@bar.com", "@bar.com", "foo@bar", "foo@bar.comma", "foo!@bar.com", "NA")
  expected <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_email_address(x)
  )
} 

test.is_email_address.a_character_vector_rfc2822_match.returns_true_when_string_contains_an_email_address <- function()
{
  x <- c("foo@bar.com", "foo@@bar.com", "@bar.com", "foo@bar", "foo@bar.comma", "foo!@bar.com", "NA")
  expected <- c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_email_address(x, method = "rfc2822")
  )
} 


test.is_hex_colour.a_character_vector.returns_true_when_string_contains_a_hex_colour <- function()
{
  x <- c(
    "#123456", "#789aBc", "#dEF000",  #ok
    "123456",                         #no hash
    "g12345",                         #bad letter
    "#12 34 56",                      #contains spaces
    "#12345", "#1234567"              #wrong length
  )
  expected <- rep(c(TRUE, FALSE), times = c(3, 5))
  names(expected) <- x
  checkEquals(
    expected,
    is_hex_colour(x)
  )
}


test.is_ip_address.a_character_vector.returns_true_when_string_contains_an_ip_address <- function()
{
  x <- c(   
    localhost     = "localhost", 
    valid_address = "255.0.255.0", 
    out_of_range  = "1.2.3.256",
    five_blocks   = "1.2.3.4.5",
    non_numeric   = "1.2.3.Z",
    missing_block = "1.2.3.NA"
  )
  expected <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_ip_address(x)
  )
} 


test.is_isbn_code.a_character_vector_type_10.returns_true_when_string_contains_an_isbn10_code <- function()
{
  x <- c(
    hyphens             = "0-387-98503-4",
    spaces              = "0 387 98503 4",
    just_numbers        = "0387985034",
    too_long            = "00-387-98503-4",
    too_short           = "0-387-9850-4",
    non_numeric         = "Z-387-98503-4",
    invalid_check_digit = "0-387-98503-5"
  )
  expected <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_isbn_code(x, type = "10")
  )
} 

test.is_isbn_code.a_character_vector_type_13.returns_true_when_string_contains_an_isbn13_code <- function()
{
  x <- c(
    hyphens             = "978-0-387-98503-9",
    spaces              = "978 0 387 98503 9",
    just_numbers        = "9780387985039",
    too_long            = "9978-0-387-98503-9",
    too_short           = "978-0-387-9850-9",
    non_numeric         = "Z78-0-387-9850-9",
    invalid_check_digit = "978-0-387-98503-8"
  )
  expected <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_isbn_code(x, type = "13")
  )
} 


test.is_missing_or_empty_character.a_character_vector.returns_true_when_string_is_missing_or_empty <- function()
{
  x <- c(
    missing      = NA_character_,
    empty        = "",
    non_empty    = "a",
    space        = " ",
    not_missing1 = "NA",
    not_missing2 = "<NA>"
  )
  expected <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_missing_or_empty_character(x)
  )
  checkEquals(
    !expected,
    is_not_missing_nor_empty_character(x)
  )
} 


test.is_numeric_string.a_character_vector.returns_true_when_string_contains_a_number <- function()
{
  x <- c("1", "-2.3e4", "Inf", "one", "NA")
  expected <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_numeric_string(x)
  )
} 


test.is_valid_r_code.valid_r_code.returns_true <- function()
{
  x <- "x <- 1 + sqrt(pi); y <- sin(x)"
  checkTrue(is_valid_r_code(x))
}

test.is_valid_r_code.invalid_r_code.returns_false <- function()
{
  x <- "x <- 1 + sqrt(pi) y <- sin(x)"
  checkTrue(!is_valid_r_code(x))  
}


test.is_valid_variable_name.a_character_vector.returns_true_when_string_contains_a_valid_variable_name <- function()
{
  x <- c(
    "x", "Y1", "zZ._..1",                      #ok
    ".", "..", "....",
    "1x", ".1x",                               #starts with number
    "_", "_x",                                 #starts with underscore
    paste0(rep.int("x", 10001), collapse = "") #too long
  )
  expected <- rep(c(TRUE, FALSE), times = c(6, 5))
  names(expected) <- x
  checkEquals(
    expected,
    is_valid_variable_name(x)
  )
} 

test.is_valid_variable_name.reserved_names.returns_true_when_allow_reserved_is_true <- function()
{
  x <- c(
    "...", "..1", "..0", "..999999999"     
  )
  expected <- rep.int(TRUE, length(x))
  names(expected) <- x
  checkEquals(
    expected,
    is_valid_variable_name(x)
  )
  checkEquals(
    !expected,
    is_valid_variable_name(x, allow_reserved = FALSE)
  )
} 

test.is_valid_variable_name.repeated_names.returns_true_when_allow_duplicates_is_true <- function()
{
  x <- rep.int("x", 2)
  expected <- rep.int(TRUE, length(x))
  names(expected) <- x
  checkEquals(
    expected,
    is_valid_variable_name(x)
  )
  expected[2] <- FALSE
  checkEquals(
    expected,
    is_valid_variable_name(x, allow_duplicates = FALSE)
  )
} 
