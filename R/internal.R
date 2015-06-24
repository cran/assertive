#' Wrapper to vapply that returns booleans.
#' 
#' Wrapper to \code{\link{vapply}} for functions that return a boolean (logical 
#' scalar) value.
#' 
#' @param x A vector (atomic or list).
#' @param predicate A predicate (function that returns a bool) to apply.
#' elementwise to \code{x}.
#' @param ... Passed to \code{vapply}.
#' @return A logical vector.
#' @note \code{USE.NAMES} is set to \code{TRUE}
#' @seealso \code{\link{vapply}}.
bapply <- function(x, predicate, ...)
{
  vapply(x, predicate, logical(1L), ..., USE.NAMES = TRUE)
}

#' Call a function, and give the result names.
#'
#' Calls a function, and names the result with the first argument.
#'
#' @param fn A function to call.  See note below.
#' @param x The first input to \code{fn}.
#' @param ... Optional additional inputs to \code{fn}.
#' @return The result of \code{fn(x, ...)}, with names given by the
#' argument \code{x}.
#' @note The function, \code{fn}, should return an object with the 
#' same length as the input \code{x}.
#' @examples
#' \dontrun{
#' call_and_name(is.finite, c(1, Inf, NA))
#' }
#' @seealso \code{\link{cause}} and \code{\link{na}}.
call_and_name <- function(fn, x, ...)
{
  y <- fn(x, ...)
  if(!is_identical_to_true(length(y) == length(x)))
  {
    warning(
      "Vector of names is different length to results.  Trying to resize."
    )
    length(x) <- length(y)
  }
  dim(y) <- dim(x)
  names(y) <- x
  y
}

#' Convert a character vector to a list of integer vectors.
#'
#' Split strings by character, then convert to numbers
#' @param x Input to convert.
#' @return A list of numeric vectors.
#' @examples
#' \dontrun{
#' character_to_list_of_integer_vectors(c("123", "4567a"))
#' }
#' @seealso \code{\link[base]{strsplit}} and \code{\link[base]{as.integer}}.
character_to_list_of_integer_vectors <- function(x)
{
  x <- coerce_to(x, "character")
  names(x) <- x
  lapply(strsplit(x, ""), as.integer)
}

#' Create a regex from components.
#'
#' Creates a regex from regex components.
#' @param ... Character vectors of regex components.
#' @param l A list of character vectors for alternate specification.
#' @param sep Regex for separating components of complete regex.
#' Defaults to "an optional space or hyphen".
#' @return A string containing a regex.
#' Each element in the vectors are pasted together, separated by the
#' \code{sep} value.  Those character vectors are then preceded by "^"
#' (regex for 'start of string'() and followed by "$" (regex for end
#' of string).  Finally, the regexes are collapsed with "|" (regex for
#' 'or').
#' @examples
#' \dontrun{
#' cas_number_components <- c(
#'   "[[:digit:]]{1,7}", "[[:digit:]]{2}", "[[:digit:]]"
#' )
#' cas_number_rx <- create_regex(rx_components, sep = "-")
#' }
create_regex <- function (..., l = list(), sep = "[- ]?")
{
  x <- merge_dots_with_list(..., l = l)
  rx <- vapply(
    x,
    function(x)
    {
      parenthesise(paste0(parenthesise(x), collapse = sep))
    },
    character(1)
  )
  paste0("^", rx, "$", collapse = "|")
}

#' Create regex for repeated digits
#' 
#' Creates a regex string for repeated digits.
#' 
#' @param lo Minimum number of digits to match.
#' @param hi Optional maximum number of digits to match.
#' @param optional If \code{TRUE}, the digits are optional.
#' @note If \code{hi} is omitted, the returned regex will only match the exact 
#' number of digits given by \code{lo}.
#' @return A character vector of regexes.
#' @examples
#' \dontrun{
#' d(1:5)
#' d(1:5, 6:8)
#' d(0:2, Inf)
#' }
d <- function(lo, hi = NA_integer_, optional = FALSE)
{
  lo <- as.integer(lo)
  assert_all_are_non_negative(lo)
  l <- recycle(lo = lo, hi = hi)
  lo <- l$lo
  hi <- l$hi
  rx <- ifelse(
    is.na(hi),
    {    
      sub("{1}", "", paste0("[[:digit:]]{", lo, "}"), fixed = TRUE)
    },
    {
      ifelse(
        is_positive_infinity(hi),
        {
          ifelse(
            lo == 0,
            {
              "[[:digit:]]*"
            },
            ifelse(
              lo == 1,
              {
                "[[:digit:]]+"
              },
              {
                paste0("[[:digit:]]{", lo, ",}")
              }
            )
          )
        },
        {
          hi <- as.integer(hi)
          assert_all_are_true(hi > lo)
          rx <- paste0("[[:digit:]]{", lo, ",", hi, "}")
        }
      )
    } 
  )
  if(optional)
  {
    rx <- paste0("(", rx, ")?")
  }
  rx
}

get_metric <- function(metric = c("length", "elements"))
{
  switch(
    match.arg(force(metric)[1], eval(formals(sys.function())$metric)),
    length   = is_of_length,
    elements = has_elements
  )
}

#' Allowed locale categories.
#'
#' The categories of locale that can be gotten/set.
#'
#' @param include_all If \code{TRUE}, the value \code{LC_ALL} is included.
#' @param include_unix If \code{TRUE}, the extra unix-only values are included.
#' @return A character vector of locale categories.
#' @seealso \code{\link{sys_get_locale}}.
locale_categories <- function(include_all = TRUE, include_unix = is_unix())
{
  allowed_categories <- c(
    if(include_all) "ALL",
    "COLLATE", "CTYPE", "MONETARY", "NUMERIC", "TIME",
    if(include_unix) c("MESSAGES", "PAPER", "MEASUREMENT")
  )
  paste0("LC_", allowed_categories)
}

#' Does the input match the regular expression?
#' 
#' Checks that the input matches the regular expression.
#'
#' @param x Input to check.
#' @param rx A regular expression.
#' @param ignore.case Should the case of alphabetic characters be ignored?
#' @param ... Passed to \code{\link{grepl}}.
#' @note The default for \code{ignore.case} is different to the default in 
#' \code{grepl}.
#' @return A logical vector that is \code{TRUE} when the input matches the 
#' regular expression.
#' @seealso \code{\link{regex}} and \code{\link{regexpr}}.
matches_regex <- function(x, rx, ignore.case = TRUE, ...)
{
  call_and_name(
    function(x) 
    {
      if(!nzchar(rx[1]))
      {
        warning(
          "Regular expression is the empty string, and matches everything."
        )
        return(rep.int(TRUE, length(x)))
      }
      # call to ifelse needed because grepl always returns TRUE or FALSE
      # need to unname, because ifelse preserves x's names, when we want to
      # name result with values of x, and merge.list throws a warning about
      # duplicate names attr.
      ifelse(   
        is.na(unname(x)),
        NA,
        grepl(rx, x, ignore.case = ignore.case, ...)
      )
    }, 
    x
  )
}

#' The most common value in a vector.
#'
#' The modal value of a vector.
#' @param x vector to find the modal value of.
#' @note Probably very inefficient; not suitable for general use.
#' @return The modal value of \code{x}.
modal_value <- function(x)
{
  names(sort(table(x), descending = TRUE))[1]
}

#' Print a variable and capture the output
#' 
#' Prints a variable and captures the output, collapsing the value to a single 
#' string.
#' @param x A variable.
#' @return A string.
#' @seealso \code{\link[base]{print}}, \code{\link[utils]{capture.output}}
#' @examples
#' \dontrun{
#' # This is useful for including data frames in warnings or errors
#' message("This is the CO2 dataset:\n", print_and_capture(CO2))
#' }
print_and_capture <- function(x)
{
  paste(capture.output(print(x)), collapse = "\n")
}

#' Recycle arguments
#' 
#' Explicit recycling of arguments to make them all have the same length.
#' @param ... Arguments, usually vectors.
#' @return A \code{list} of vectors, all with the same length.
#' @note The function is based on \code{rep_len}, which drops attributes (hence
#' this being most appropriate for vector inputs).
#' @seealso \code{\link[base]{rep_len}}.
#' @examples
#' \dontrun{
#' # z is the longest argument, with 6 elements
#' recycle(x = 1:4, y = list(a = month.abb, b = pi), z = matrix(1:6, nrow = 3))
#' }
recycle <- function(...)
{
  dots <- list(...)
  n <- max(vapply(dots, length, integer(1)))
  lapply(dots, rep_len, length.out = n)
}

#' Removes invalid characters from a string.
#'
#' Removes invalid characters from a string, leaving only digits.
#' @param x Input to strip.
#' @param invalid_chars A regular expression detailing characters to remove.
#' @param char_desc A string describing the characters to remove.
#' @param allow_x If \code{TRUE}, the letter "X" is allowed - useful for check 
#' digits.
#' @param allow_plus If \code{TRUE}, the symbol "+" is allowed - useful for 
#' phone numbers.
#' @return A character vector of the same length as \code{x}, consisting of 
#' strings without the characters detailed in the \code{invalid_chars}.
#' @examples
#' \dontrun{
#' strip_invalid_chars(
#'   "  We're floating\tin    space\n\n\n", "[[:space:]]", "whitespace"
#' )
#' strip_non_numeric(" +44 800-123-456 ", allow_plus = TRUE)
#' #Inputs such as factors as coerced to character.
#' strip_non_alphanumeric(factor(c(" A1\t1AA.", "*(B2^2BB)%")))
#' }
strip_invalid_chars <- function(x, invalid_chars, char_desc = gettext("invalid"))
{
  x <- coerce_to(x, "character")
  if(any(grepl(invalid_chars, x)))
  {
    warning(gettextf("Removing %s characters from input.", char_desc))
    x <- gsub(invalid_chars, "", x)
  }
  x
}

#' @rdname strip_invalid_chars
strip_non_alphanumeric <- function(x)
{
  strip_invalid_chars(x, "[^[:alnum:]]+", "non-alphanumeric")
}

#' @rdname strip_invalid_chars
strip_non_numeric <- function(x, allow_x = FALSE, allow_plus = FALSE)
{
  invalid_chars <- paste0(
    "[^[:digit:]", 
    if(allow_x) "X", 
    if(allow_plus) "\\+", 
    "]+", 
    collapse = ""
  )
  strip_invalid_chars(x, invalid_chars, "non-numeric")
}

#' Truncate a string
#' 
#' Truncates a character vector to have a maximum length.
#' @param x A character vector, or something coercible to one.
#' @param width A positive integer.
#' @return A character vector
#' @examples
#' \dontrun{
#' truncate(c("abcd", "efghi", "jklmno", "pqrstuv"), 5)
#' }
truncate <- function(x, width = getOption("width"))
{
  x <- as.character(x)
  ifelse(
    nchar(x) > width,
    # paste0(substring(x, 1, width - 1), "\u2026") would be better, but some
    # setups don't display unicode properly.
    paste0(substring(x, 1, width - 3), "..."),
    x
  )
} 

