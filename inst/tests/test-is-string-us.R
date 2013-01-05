test.is_us_telephone_number.a_character_vector.returns_true_when_string_contains_a_valid_us_telephone_number <- function()
{
  x <- c(
    "12345678901",   #country code as 1 
    "+12345678901",  #country code as +1
    "0012345678901", #country code as 001
    "2345678901",    #no country code
    "10345678901",   #NPA can't begin 0 
    "11345678901",   #...or 1
    "12335678901",   #2nd, 3rd digits of NPA can't match
    "12340678901",   #NXX can't begin 0        
    "12341678901",   #...or 1
    "12345118901",   #2nd, 3rd digits of NXX can't be 11
    "1234567",       #NPA must be included               
    "12345678"       #ditto
  )
    
  expected <- rep.int(c(TRUE, FALSE), c(4, 8))
  names(expected) <- x
  checkEquals(
    expected,
    is_us_telephone_number(x)
  )
}

test.is_us_zip_code.a_character_vector.returns_true_when_string_contains_a_valid_us_zip_code <- function()
{
  x <- c(
    "22313",         #ok
    "22313-1450",    #ok
    "223130",        #six digits, too long
    "2231",          #four digits, too short
    "223131450",     #no hyphen
    "2231E",         #contains letters
    " 22313"         #contains spaces
  ) 
  expected <- rep.int(c(TRUE, FALSE), c(2, 5))
  names(expected) <- x
  checkEquals(
    expected,
    is_us_zip_code(x)
  )
}  

test.is_us_zip_code.5_digit_strings.returns_true_when_zip_code_has_valid_prefix <- function()
{
  unused <- c(
    0, 2:4, 99, 213, 269, 343, 345, 348, 353, 419,
    428, 429, 517:519, 529, 533, 536, 552, 568, 578,
    579, 589, 621, 632, 642, 643, 659, 663, 682, 
    694:699, 702, 709, 715, 732, 742, 771, 817, 818, 
    819, 839, 848, 849, 854, 858, 861, 862, 866:869, 
    876, 886:888, 892, 896, 899, 909, 929, 987
  )
  three_digits <- formatC(0:999, width = 3, flag = "0")
  expected <- !(0:999 %in% unused)
  x <- paste0(three_digits, "01")
  names(expected) <- x
  checkEquals(
    expected,
    is_us_zip_code(x)
  )
}
