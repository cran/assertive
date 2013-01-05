#' Is the string a valid US telephone number?
#' 
#' Checks that the input contains US/Canadian (NANPA) telephone numbers.
#' 
#' @param x Input to check.
#' @return \code{is_us_telephone_number} returns \code{TRUE} if the input string contains
#' a valid US telephone number. The {assert_*} functions return nothing but throw an error 
#' when the \code{is_*} function returns \code{FALSE}. 
#' @note A valid US phone number consists of an optional country 
#' code (either +1, 001 or just 1), followed by a 3 digit NPA area
#' code, where the first digit is between 2 and 9, and the second 
#' and third digits don't match.  Next is a 3 digit exchange (NXX)
#' code, where the first digit is between 2 and 9 and the second
#' and third digits aren't 11.  Finally there is a four digit 
#' subscriber number (with no restrictions).  7 digit numbers 
#' (without the NPA code) are not supported here.
#' Canada, parts of the Caribbean, and some Atlantic and Pacific 
#' islands also use the same numbering system.
#' @examples
#' phone_numbers <- c(
#'   "12345678901",   #country code as 1 
#'   "+12345678901",  #country code as +1
#'   "0012345678901", #country code as 001
#'   "2345678901",    #no country code
#'   "10345678901",   #NPA can't begin 0 
#'   "11345678901",   #...or 1
#'   "12335678901",   #2nd, 3rd digits of NPA can't match
#'   "12340678901",   #NXX can't begin 0        
#'   "12341678901",   #...or 1
#'   "12345118901",   #2nd, 3rd digits of NXX can't be 11
#'   "1234567",       #NPA must be included               
#'   "12345678"      #ditto
#' )
#' is_us_telephone_number(phone_numbers)
#' @export
is_us_telephone_number <- function(x)
{ 
  enbracket <- function(x) paste("(", x, ")", sep = "")
  
  #Spaces and round brackets appear in arbitrary places; ignore them.
  x <- suppressWarnings(strip_invalid_chars(x, invalid_chars="[ -()]"))
  
  #All numbers should begin with 1 or the country code, 001. Check and remove.
  start <- "((00|\\+)?1)?"
  
  first_rx <- create_regex(
    c(start, d(10)), #country prefix + 10 digits
    sep = ""
  )
  ok <- matches_regex(x, first_rx) 
  x[ok] <- sub(paste0("^", start), "", x[ok]) #remove country code prefix
    
  npa1 <- "[2-9]"
  npa23 <- enbracket(     
    paste(c("0[1-9]", "1[02-9]", "2[013-9]", "3[0-24-9]", "4[0-35-9]", "5[0-46-9]", "6[0-57-9]", "7[0-689]", "8[0-79]"), collapse = "|")    
  )
  nxx1 <- "[2-9]"
  nxx23 <- enbracket(paste(c("1[02-9]", "[02-9][0-9]"), collapse = "|"))
  xxxx <- d(4)
  
  second_rx <- create_regex(
    c(npa1, npa23, nxx1, nxx23, xxxx), sep = ""
  )

  ok[ok] <- matches_regex(x[ok], second_rx)
  ok  
}

#' Is the string a valid US zip code?
#' 
#' Checks that the input contains US zip codes.
#' 
#' @param x Input to check.
#' @return \code{is_us_zip_code} returns \code{TRUE} if the input string contains
#' a valid US zip code. The {assert_*} functions return nothing but throw an error 
#' when the \code{is_*} function returns \code{FALSE}.
#' @note A valid zip code is considered to be 5 digits, or 5 digits then a hyphen 
#' then 4 digits.  Unused area prefixes return FALSE, but the function doesn't 
#' guarantee that the zip code actually exists.  It should correctly return 
#' \code{TRUE} for genuine zip codes, and will weed out most badly formatted strings 
#' non-existent areas, but some non-existent codes may incorrectly return 
#' \code{TRUE}.  If you need 100% accuracy, check against an up-to-date zip code 
#' base.
#' @examples
#' zip_codes <- c(
#'   "90210", 
#'   "20500", 
#'   "22313-1450",  #5+4 style ok
#'   "223131450",   #fails, no hyphen
#'   "09901"        #fails, invalid area prefix
#'  )
#' is_us_zip_code(zip_codes)
#' assert_any_are_us_zip_codes(zip_codes)
#' \dontrun{
#' #The following code should throw an error.
#' assert_all_are_us_zip_codes(zip_codes)
#' }
#' @references Regexes inferred from 
#' \url{https://en.wikipedia.org/wiki/ZIP_code} and 
#' \url{https://en.wikipedia.org/wiki/List_of_ZIP_code_prefixes}.
#' @export
is_us_zip_code <- function(x)
{
  prefix <- setdiff(
    0:999,
    c(
      0, 2:4, 99, 213, 269, 343, 345, 348, 353, 419,
      428, 429, 517:519, 529, 533, 536, 552, 568, 578,
      579, 589, 621, 632, 642, 643, 659, 663, 682, 
      694:699, 702, 709, 715, 732, 742, 771, 817, 818, 
      819, 839, 848, 849, 854, 858, 861, 862, 866:869, 
      876, 886:888, 892, 896, 899, 909, 929, 987
    )
  )
  prefix <- paste0(
    "(",
    paste(
      formatC(
        prefix,
        width = 3,
        flag = "0"
      ),
      collapse = "|"
    ),
    ")" 
  )  
  rx <- create_regex(c(prefix, d(2), "(-", d(4), ")?"), sep = "")  
  matches_regex(x, rx)
}
