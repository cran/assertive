#' Does the character vector contain CAS registry numbers? 
#' 
#' Checks that the input contains Chemical Abstract Service registry numbers.
#' 
#' @param x Input to check.
#' @param .xname Not intended to be called directly.
#' @note CAS numbers take the form of 1 to 7 digits followed by a hyphen,  
#' followed by 2 digits, another hyphen and a final check digit.
#' @return A logical vector that is \code{TRUE} when the input contains valid  
#' CAS registry numbers.
#' @examples
#' x <- c(
#'   water = "7732-18-5", 
#'   d_glucose = "50-99-7",
#'   l_glucose = "921-60-8",
#'   no_hyphens = "7732185", 
#'   two_check_digits = "7732-18-55",
#'   bad_check_digit = "7732-18-4"
#' )
#' is_cas_number(x)
#' assert_any_are_cas_numbers(x)
#' \dontrun{
#' #These examples should fail.
#' assert_all_are_cas_numbers(x)
#' }
#' @references Chemspider is a good service for looking up CAS numbers.
#' @export
is_cas_number <- function(x, .xname = get_name_in_parent(x))
{
  #Check format
  rx <- c(d(1, 7), d(2), d(1))
  rx <- create_regex(rx, sep = "\\-")
  
  ok <- matches_regex(x, rx)
  
  #Check checkdigit
  x[ok] <- suppressWarnings(strip_non_numeric(x[ok]))
  ok[ok] <- bapply(
    character_to_list_of_numeric_vectors(x[ok]), 
    function(x)
    {
      lenx <- length(x)
      actual_check_digit <- x[lenx]
      x <- x[-lenx]
      expected_check_digit <- sum(rev(x) * seq_along(x)) %% 10L
      expected_check_digit == actual_check_digit
    }
  )
  ok
}

#' Does the character vector contain credit card numbers? 
#' 
#' Checks that the input contains credit card numbers.
#' 
#' @param x Input to check.
#' @param type Type of credit card.  Multiple types can be selected.
#' @param .xname Not intended to be called directly.
#' @note Legacy card numbers, for example 13 digit Visa numbers and 15 digits JCB 
#' numbers are not supported.
#' @return A logical vector that is \code{TRUE} when the input contains valid credit 
#' card numbers.
#' @examples
#' x <- c(
#'   #visa
#'   "4111 1111 1111 1111",    #spaces are allowed where they 
#'                             #would occur on the card
#'   "4012888888881881",       #though they can be omitted
#'   "4111 1111 1111 11111",   #too many digits
#'   "4012888888881882",       #bad check digit
#'   #mastercard
#'   "5555 5555 5555 4444",
#'   "5105 1051 0510 5100",
#'   "5655 5555 5555 4443",    #starts 56
#'   "51051 051 0510 5100",    #bad spacing
#'   #amex
#'   "3782 822463 10005",
#'   "3714 496353 98431",
#'   "3787 344936 71000", 
#'   "3782 822463 1005",       #not enough digits
#'   #diners
#'   "3056 930902 5904",
#'   "3852 000002 3237",
#'   #discover
#'   "6011 1111 1111 1117",
#'   "6011 0009 9013 9424",
#'   #jcb
#'   "3530 1113 3330 0000",
#'   "3566 0020 2036 0505"
#' )
#' is_credit_card_number(x)
#' assert_any_are_credit_card_numbers(x)
#' \dontrun{
#' assert_all_are_credit_card_numbers(x)
#' }
#' @references \url{http://www.regular-expressions.info/creditcard.html} contains the regexes
#' used by this function.
#' The example valid card numbers are from
#' \url{http://www.paypalobjects.com/en_US/vhelp/paypalmanager_help/credit_card_numbers.htm}
#' @export
is_credit_card_number <- function(x, type = c("visa", "mastercard", "amex", "diners", "discover", "jcb"), .xname = get_name_in_parent(x))
{
  #Check format
  type <- match.arg(type, several.ok = TRUE)
  
  rx <- list(
    visa       = c(paste0("4", d(3)), rep.int(d(4), 3)),
    mastercard = c(paste0("5[1-5]", d(2)), rep.int(d(4), 3)),
    amex       = c(paste0("3[47]", d(2)), d(6), d(5)),
    diners     = c("3(0[0-5]|[68][[:digit:]])[[:digit:]]", d(6), d(4)),
    discover   = c(paste("6011", paste0("65", d(2)), sep = "|"), rep.int(d(4), 3)),
    jcb        = c(paste0("35", d(2)), rep.int(d(4), 3))
  )
  rx <- create_regex(l = rx[type], sep = " ?")
  ok <- matches_regex(x, rx)
  
  x[ok] <- suppressWarnings(strip_non_numeric(x[ok]))
  
  #Check check digit with Luhn algorithm
  ok[ok] <- bapply(
    character_to_list_of_numeric_vectors(x[ok]),
    function(x)
    {
      lenx <- length(x)
      actual_check_digit <- x[lenx]
      x <- rev(x[-lenx])
      doubled <- suppressWarnings(x * 2:1L)
      total <- sum(doubled) - 9 * sum(doubled > 9)
      expected_check_digit <- (9 * total) %% 10L
      expected_check_digit == actual_check_digit
    }   
  )  
  ok
}

#' Does the character vector contain dates? 
#' 
#' Checks that the input contains dates or times.
#' 
#' @param x Input to check.
#' @param format Expected format of the dates.  See \code{\link[base]{strptime}}.
#' @param .xname Not intended to be called directly.
#' @return A logical vector that is \code{TRUE} when the input contains valid dates or times.
#' @examples
#' assert_all_are_date_strings("01Aug1979", format = "%d%b%Y") #My DOB!
#' @seealso \code{\link[base]{strptime}} for specifying formats, and the \code{lubridate}
#' package for automatic guessing of date formats (and other date manipulation functions).
#' @export
is_date_string <- function(x, format = "%F %T", .xname = get_name_in_parent(x))
{
  x <- coerce_to(x, "character")
  format <- use_first(format)
  f <- function(x) !is.na(strptime(x, format))
  call_and_name(f, x)  
}

#' Does the character vector contain email addresses?
#' 
#' Checks that the input contains email addresses.  (It does not check the the address exists, 
#' merely that the string is in a suitable format.)
#' 
#' @param x Input to check.
#' @param method Name of method to check for validity.  See notes below.
#' @param .xname Not intended to be called directly.
#' @note Each method specifies a regular expression (see \code{\link[base]{regex}}) to match 
#' against. The \code{simple} method matches most email addresses in use, and is quite good at
#' filtering out typos and nonsense.  It won't match \emph{every} email address however.  For 
#' example, emails from a top level domain longer than 4 characters won't pass.  The \code{rfc822}
#' method implements the offical standard for emails.  Thus all genuine emails will pass, but since
#' the spec is very broad, it isn't as good at filtering out nonsense.
#' @return A logical vector that is \code{TRUE} when the input contains valid email addresses.
#' @examples
#' addresses <- c("a@@b.com", "a_at_b.com", "a@@bcom", "a@@b.comma", "a!$&@@b.com")
#' is_email_address(addresses)
#' is_email_address(addresses, method = "rfc2822")
#' @references \url{http://www.regular-expressions.info/email.html} contains the regexes used
#' by this function and a good discussion of the pros and cons of each.
#' @export
is_email_address <- function(x, method = c("simple", "rfc2822"), .xname = get_name_in_parent(x))
{
  method <- match.arg(method)
  x <- coerce_to(x, "character")
  rx <- switch(
    method,
    simple = "^[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,4}$",
    rfc2822 = "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])"
  )
  matches_regex(x, rx, perl = TRUE)
}

#' Does the character vector contain hex colours?
#'
#' Checks that the input contains hexadecimal colours.
#' 
#' @param x Input to check.
#' @note A string is considered to represent a hexadecimal colour when contains a 
#' hash followed by six hex values.  That is, digits or the letters from a to f 
#' (case insensitive).
#' @return A logical vector that is \code{TRUE} when the input contains hex colours.
#' @examples
#' x <- c(
#'   "#0123456", "#789abc", "#defDEF", #ok
#'   "012345",                         #no hash
#'   "#g12345",                        #bad letter
#'   "#01 23 45",                      #contains spaces
#'   "#12345", "#1234567"              #wrong length
#' )
#' is_hex_colour(x)
#' assert_any_are_hex_colours(x)
#' \dontrun{
#' #These examples should fail.
#' assert_all_are_hex_colours(x)
#' }
#' @export
is_hex_colour <- function(x)
{
  rx <- create_regex("#[0-9a-f]{6}")
  matches_regex(x, rx)
}
       #' Is the string an honorific?
#' 
#' Checks that the input contains honorifics (a.k.a. titles 
#' or salutations).
#' @param x Input to check.
#' @return \code{is_honorific} returns \code{TRUE} if the input string contains
#' a valid UK postcode. The {assert_*} function returns nothing but throws an error 
#' when the \code{is_*} function returns \code{FALSE}. 
#' @note Single full stops (periods) following a word boundary 
#' and preceding a space or the end of the string are stripped.  
#' Case is ignored.  There is no formal list of official salutations,
#' so this should only be used as a guide, rather than giving a 
#' definitive result.  Especially note that cultural conventions
#' differ across the world and this function has a UK bias.
#' @examples
#' x <- c("Mr", "MR", "mr.", "Mister", "masTer", "Mr!", "M.r", ".Mr")
#' is_honorific(x)
#' @export
is_honorific <- function(x)
{
  #Strip single dots after words
  x <- gsub("(?<=\\b)\\.(?=\\s|$)", "", x, perl = TRUE)  
  rx <- create_regex(
    #standard
    "m([ia]ste)?r", "mrs", "miss", "d(octo)?r", 
    #academic
    "((assoc)?iate)prof(essor)?", "dean", 
    #religious
    "rev(erend)?", "ft", "father", "bro(ther)?", "s(iste)?r", "(arch)?bishop", 
    #politics and nobility
    "r(igh)?t hon(ourable)", "sir", "lord", "lady", "dame", "prince(ss)?", "king", "queen"#,
    #military 
    #TODO "", "", "", "", "", "", ""
  )
  matches_regex(x, rx)
}

#' Does the character vector contain IP addresses?
#' 
#' Checks that the input contains IP addresses.  (It does not check that the address exists, 
#' merely that the string is in a suitable format.)
#' 
#' @param x Input to check.
#' @note Valid IP addresses are considered to be four integers in the range 0 to 255, separated
#' by dots, or the string "localhost".
#' @return A logical vector that is \code{TRUE} when the input contains valid IP addresses.
#' @examples
#' x <- c(
#'   localhost     = "localhost", 
#'   valid_address = "255.0.255.0", 
#'   out_of_range  = "1.2.3.256",
#'   five_blocks   = "1.2.3.4.5",
#'   non_numeric   = "1.2.3.Z",
#'   missing_block = "1.2.3.NA"
#' )
#' is_ip_address(x)
#' assert_any_are_ip_addresses(x)
#' \dontrun{
#' #These examples should fail.
#' assert_all_are_ip_addresses(x)
#' }
#' @export
is_ip_address <- function(x)
{
  x <- coerce_to(x, "character")  
  rx <- create_regex(rep.int(d(1, 3), 4), sep = "\\.")
  ok <- matches_regex(x, rx)
  
  blocks <- strsplit(x[ok], ".", fixed = TRUE)
  ok[ok] <- bapply(
    blocks,
    function(b) all(suppressWarnings(as.integer(b) %in% 0:255))
  )
  
  ok | (x == "localhost")
}

#' @rdname is_isbn_code
is_isbn10_code <- function(x, .xname = get_name_in_parent(x))
{
  #Check basic format
  digits <- "[[:digit:]]+"
  sep <- "[- ]?"
  rx <- create_regex(c(rep.int(digits, 3), "[[:digit:]X]"))
  ok <- matches_regex(x, rx)
  
  #Check correct amount of numbers
  x[ok] <- suppressWarnings(strip_non_numeric(x[ok], allow_x = TRUE))
  ok[ok] <- nchar(x[ok]) == 10L
  
  #Check checkdigit
  ok[ok] <- bapply(
    character_to_list_of_numeric_vectors(x[ok]),
    function(x)
    {
      actual_check_digit <- x[10L]
      x <- x[1:9L]
      expected_check_digit <- (sum(x * 1:9L) %% 11L)
      if(expected_check_digit == 10L) return(is.na(actual_check_digit))
      expected_check_digit == actual_check_digit
    }
  )  
  ok
}

#' @rdname is_isbn_code
is_isbn13_code <- function(x, .xname = get_name_in_parent(x))
{
  #Check basic format
  digits <- "[[:digit:]]+"
  rx <- create_regex(c(rep.int(digits, 4), "[[:digit:]X]"))
  ok <- matches_regex(x, rx)
  
  #Check correct amount of numbers
  x[ok] <- suppressWarnings(strip_non_numeric(x[ok]))
  ok[ok] <- nchar(x[ok]) == 13L
  
  #Check checkdigit
  ok[ok] <- bapply(
    character_to_list_of_numeric_vectors(x[ok]),
    function(x)
    {
      (sum(suppressWarnings(x * c(1, 3))) %% 10L) == 0L
    }
  )  
  ok
}

#' Does the character vector contain ISBN book codes?
#' 
#' Checks that the input contains ISBN-10 or ISBN-13 book codes.
#' 
#' @param x Input to check.
#' @param type Either "isbn10", "isbn13" or both (for matching either type).
#' @param .xname Not intended to be called directly.
#' @return  A logical vector that is \code{TRUE} when the input contains valid ISBN book codes.
#' @examples
#' x10 <- c(
#'   hyphens             = "0-387-98503-4",
#'   spaces              = "0 387 98503 4",
#'   just_numbers        = "0387985034",
#'   too_long            = "00-387-98503-4",
#'   too_short           = "0-387-9850-4",
#'   non_numeric         = "Z-387-98503-4",
#'   invalid_check_digit = "0-387-98503-5"
#' )
#' x13 <- c(
#'   hyphens             = "978-0-387-98503-9",
#'   spaces              = "978 0 387 98503 9",
#'   just_numbers        = "9780387985039",
#'   too_long            = "9978-0-387-98503-9",
#'   too_short           = "978-0-387-9850-9",
#'   non_numeric         = "Z78-0-387-9850-9",
#'   invalid_check_digit = "978-0-387-98503-8"
#' )
#' is_isbn_code(x10, type = "10")
#' assert_any_are_isbn_codes(x10, type = "10")
#' is_isbn_code(x13, type = "13")
#' assert_any_are_isbn_codes(x13, type = "13")
#' \dontrun{
#' #These tests should fail.
#' assert_all_are_isbn_codes(x10, type = "10")
#' assert_all_are_isbn_codes(x13, type = "13")
#' }
#' @export
is_isbn_code <- function(x, type = c("10", "13"), .xname = get_name_in_parent(x))
{
  type <- match.arg(type, several.ok = TRUE)
  ok <- lapply(
    type, 
    function(isbn) 
    {
      fn <- switch(
        isbn,
        "10" = is_isbn10_code,
        "13" = is_isbn13_code
      )
      fn(x, .xname)
    }
  )
  Reduce(`|`, ok)
}

#' @rdname is_character
#' @export
is_missing_or_empty_character <- function(x)
{ 
  x <- coerce_to(x, "character")
  !nzchar(x) | is_na(x)
}

#' @rdname is_character
#' @export
is_not_missing_nor_empty_character <- function(x)
{ 
  x <- coerce_to(x, "character")
  nzchar(x) & !is_na(x)
}

#' @rdname is_character
#' @export
is_numeric_string <- function(x)
{
  x <- coerce_to(x, "character")
  numx <- suppressWarnings(as.numeric(x))
  ans <- is_not_na(numx)
  names(ans) <- x   #need to take names from x, not numx
  ans
}

#' Is the input valid R code?
#'
#' Check to see if the input is a valid (parseable) R code.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{TRUE} if the input string is valid R code.
#' @examples
#' is_valid_r_code("x <- 1 + sqrt(pi)")
#' is_valid_r_code("x <- ")
#' is_valid_r_code("<- 1 + sqrt(pi)")
#' @seealso \code{\link[base]{parse}}
#' @export
is_valid_r_code <- function(x, .xname = get_name_in_parent(x))
{
  x <- coerce_to(x, "character")
  x <- use_first(x)
  ok <- is_error_free(parse(text = x))
  if(!ok)
  {
    return(false(
      "%s is not valid R code. %s.", 
      .xname, 
      cause(ok)
    ))
  }
  TRUE
}

#' Is the string a valid variable name?
#'
#' Checks strings to see if they are valid variable names.
#'
#' @param x Input to check.
#' @param allow_reserved If \code{TRUE} then "..." and "..1", "..2", etc. 
#' are considered valid.
#' @param allow_duplicates If \code{TRUE} then duplicated names are allowed.
#' @return \code{TRUE} if the input is a valid variable name.
#' The \code{assert_*} functions return nothing but
#' throw an error if the corresponding \code{is_*} function returns 
#' \code{FALSE}.
#' @seealso \code{\link{make.names}}.
#' @examples
#' assert_all_are_valid_variable_names(c("x", "y_y0.y", ".", "...", "..1"))
#' \dontrun{
#' #These examples should fail.
#' assert_all_are_valid_variable_names(c("...", "..1"), allow_reserved = FALSE) 
#' assert_all_are_valid_variable_names(c("x", "x"), allow_duplicates = FALSE)
#' }
#' @references
#' \url{http://4dpiecharts.com/2011/07/04/testing-for-valid-variable-names/}
#' @export
is_valid_variable_name <- function(x, allow_reserved = TRUE, allow_duplicates = TRUE)
{
  x <- coerce_to(x, "character")
  ok <- rep.int(TRUE, length(x))
  
  #is name too long?
  max_name_length <- if(getRversion() < "2.13.0") 256L else 10000L
  ok[ok] <- nchar(x[ok]) <= max_name_length
  
  #is it a reserved variable, i.e.
  #an ellipsis or two dots then a number?
  if(!allow_reserved)
  {
    ok[ok] <- x[ok] != "..."
    rx <- create_regex("\\.{2}[[:digit:]]+")
    ok[ok] <- !matches_regex(x[ok], rx)
  }
  
  #are names valid (and maybe unique)
  ok[ok] <- x[ok] == make.names(x[ok], unique = !allow_duplicates)
  
  names(ok) <- x
  ok
}
