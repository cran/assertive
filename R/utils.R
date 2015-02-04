#' Convert file connections to strings
#' 
#' \code{as.character} method for file connections.
#' @param x A file connection.
#' @param ... Not currently used.
#' @return A string containing the target location of the file connection.
#' @seealso \code{\link[base]{file}}, \code{\link[base]{summary.connection}},
#' \code{\link[base]{as.character}}
#' @examples
#' rprofile <- file.path(R.home("etc"), "Rprofile.site")
#' fcon <- file(rprofile)
#' assert_all_are_true(identical(as.character(fcon), rprofile))
#' close(fcon)
#' @method as.character file
#' @export
as.character.file <- function(x, ...)
{
  # Assertion is to double check that no other package has overwritten the 
  # file class.
  assert_is_file_connection(x)
  summary(x)$description
}

#' Get or set the \code{"cause"} attribute.
#'
#' Gets or sets the \code{"cause"} (of failure) attribute of a variable.
#'
#' @param x Any variable.
#' @param value Passed to \code{sprintf} and stored in the \code{"cause"}
#' attribute.
#' @return The get method returns the \code{"cause"} attribute.
#' @examples
#' yn <- is_a_bool(123)
#' cause(yn)
#' @export
cause <- function(x)
{
  attr(x, "cause")
}

#' @rdname cause
#' @export
`cause<-` <- function(x, value)
{
  if(!is_scalar(value) && length(value) != length(x))
  {
    stop(
      "The length of value should be 1 or the length of x (", 
      length(x),
      ") but is ", 
      length(value),
      "."
    )
  }
  attr(x, "cause") <- noquote(as.character(value))
  x
}

#' Coerce variable to a different class.
#'
#' Coerce the input to a different class, with a warning.  More reliable then 
#' \code{\link[methods]{as}}, and supports coercion to multiple classes.
#'
#' @param x Input to coerce.
#' @param target_class The desired class of x.  Multiple values allowed (see 
#' note).
#' @param .xname Not intended to be used directly.
#' @return The input \code{x} after attempted coercion to the target class.
#' @note If x does not already have the target class, a warning is given
#' before coercion.  
#' The function will try and convert the \code{x} to each of the classes given
#' in \code{target_class}, in order, until it succeeds or runs out of classes
#' to try.  It will first try and convert \code{x} using a dedicated 
#' \code{as.target_class} function if that exists.  If it does not exist, or 
#' throws an error then \code{coerce_to} will try to use 
#' \code{as(x, target_class)}.
#' @seealso \code{\link[methods]{is}} and \code{\link[methods]{as}}.
#' @examples
#' # Numbers can be coerced to characters but not to calls.
#' dont_stop(coerce_to(1:5, c("call", "character")))
#' @export
coerce_to <- function(x, target_class, .xname = get_name_in_parent(x))
{
  # Can't use is_empty in next line because that function calls this one.
  if(length(target_class) == 0L) 
  {
    stop("You must provide a class.")
  }
  for(this_class in target_class)
  {
    if(!is2(x, this_class))
    {
      warning(
        "Coercing ", .xname, " to class ", sQuote(this_class), ".",
        call. = FALSE
      )
    }
    as.class_function_exists <- is_error_free(
      match.fun(paste0("as.", this_class))
    )
    if(as.class_function_exists) 
    {
      can_be_coerced <- is_error_free(
        attr(as.class_function_exists, "result")(x)
      ) 
      if(can_be_coerced)
      {
        return(attr(can_be_coerced, "result"))
      }
    } 
    can_be_coerced <- is_error_free(as(x, this_class))
    if(can_be_coerced)
    {
      return(attr(can_be_coerced, "result"))
    }
    warning(
      .xname, 
      " cannot be coerced to type ", 
      sQuote(this_class), 
      ".", 
      call. = FALSE
    )
  }
  stop(
    .xname, 
    " cannot be coerced to type ",
    ".",
    toString(sQuote(target_class))
  )
}

#' Get the dimensions of an object
#' 
#' Get the dimensions of an object, retuning the length if that object has no
#' \code{dim} attribute.
#' @param x Any object.
#' @return A integer vector of non-negative values.
#' @seealso \code{\link[base]{NROW}}, \code{\link[base]{dim}}
#' @examples
#' # For data frames and matrices, DIM is the same as dim.
#' DIM(sleep) 
#' # For vectors (and other objects without a dim attribute), DIM is the 
#' # same as length.
#' DIM(1:10)
#' DIM(list(x = 1:10))
#' @export
DIM <- function(x)
{
  dim_x <- dim(x)
  if(is.null(dim_x)) length(x) else dim_x
}

#' Get the name of a variable in the parent frame.
#'
#' Gets the name of the input in the parent frame.
#'
#' @param x Variable to get the name of.
#' @return A string giving the name of the input in the parent frame.
#' @export
get_name_in_parent <- function(x)
{  
  paste0(
    deparse(
      do.call(
        substitute, 
        list(substitute(x), parent.frame())
      )
    ),
    collapse = ""
  )
}

#' Merge two lists
#' 
#' Merges two lists, taking duplicated elements from the first list.
#' @param x A list.
#' @param y A list.
#' @param ... Ignored.
#' @return A list, combining elements from \code{x} and \code{y}.
#' @seealso \code{\link{merge_dots_with_list}}, \code{\link[base]{merge}}
#' @examples
#' merge(
#'   list(foo = 1, bar = 2, baz = 3), 
#'   list(foo = 4, baz = 5, quux = 6)
#' )
#' @export
merge.list <- function(x, y, ...)
{
  if(is.null(y)) return(x)
  y <- coerce_to(y, "list")
  all_names <- c(names(x), names(y))
  all_values <- c(x, y)
  if(has_duplicates(all_names))
  {
    warning(
      "Duplicated arguments: ", 
      toString(all_names[duplicated(all_names)])
    )
    all_values <- all_values[!duplicated(all_names)]
  }
  all_values
}

merge.NULL <- function(x, y, ...)
{
  return(y)
}

#' Merge ellipsis args with a list.
#'
#' Merges variable length ellipsis arguments to a function with a list argument.
#'
#' @param ... Some inputs.
#' @param l A list.
#' @note If any arguments are present in both the \code{...} and \code{l} 
#' arguments, the \code{...} version takes preference, and a warning is thrown.
#' @return A list containing the merged inputs.
#' @seealso \code{\link{merge.list}}, \code{\link[base]{merge}}
#' @examples
#' merge_dots_with_list(
#'   foo = 1, 
#'   bar = 2, 
#'   baz = 3, 
#'   l = list(foo = 4, baz = 5, quux = 6)
#' )
#' @export
merge_dots_with_list <- function(..., l = list())
{
  dots <- list(...)
  l <- coerce_to(l, "list")
  merge(dots, l)
}

#' Get the number of elements
#' 
#' Gets the number of elements in an object.
#' @param x Any object.
#' @return A non-negative integer of the number of elements.
#' @note For atomic objects, the number of elements is the product of the
#' dimensions, as calculated by \code{\link{DIM}}.  For recursive objects,
#' the number of elements is the sum of the number of elements of each of
#' their atomic components.
#' @seealso \code{\link{DIM}}
#' @examples
#' n_elements(1:10)
#' n_elements(NULL)
#' n_elements(data.frame(x = 1:5, y = rnorm(5)))
#' n_elements(list(1:5, list(1:3, list(1:7))))
#' n_elements(var) # depends upon the length of the body
#' @export
n_elements <- function(x)
{
  if(is.recursive(x))
  {
    sum(vapply(x, n_elements, integer(1)))
  } else
  {
    as.integer(prod(DIM(x)))
  }  
}

#' Wrap a string in brackets
#'
#' Parenthesise a character vector by wrapping elements in brackets, 
#' dashes or commas.
#' @param x Character vector to wrap in parenthenses.
#' @param type String naming the type of parenthesis.
#' @return A character vector of the input wrapped in parentheses.
#' @note English grammar terminology is awfully confusing.  The verb 'to 
#' parenthesise' means to wrap a phrase in brackets or dashes or commas,
#' thus denoting it as supplementary material that could be left out.
#' A 'parenthesis' as a noun is often used as a synonym for a round bracket.
#' @seealso \code{\link[base]{sQuote}}
#' @examples
#' paste("There were three", parenthesise(3), "mice in the experiment.")
#' paste(
#'   "I love parmos", 
#'   parenthesise("Teesside's finest culinary invention", "en_dashes"), 
#'   "but they are sure to give me heart disease."
#' )
#' parenthesise(letters[1:5], "curly")
#' paste0(
#'   "The R language", 
#'   parenthesise("an offshoot of S and Scheme", "commas"), 
#'   "is quite good for data analysis."
#' )
#' @export
parenthesise <- function(x, 
  type = c("round_brackets", "square_brackets", "curly_brackets", "angle_brackets", "chevrons", "hyphens", "en_dashes", "em_dashes", "commas")) 
{
  type <- match.arg(type)
  x <- assertive::coerce_to(x, "character")
  before <- switch(
    type,
    round_brackets  = "(",
    square_brackets = "[",
    curly_brackets  = "{",
    angle_brackets  = "<",
    chevrons        = "\u3008",
    hyphens         = "- ",
    en_dashes       = "\u2013 ",
    em_dashes       = "\u2014",
    commas          = ", "
  )
  after <- switch(
    type,
    round_brackets  = ")",
    square_brackets = "]",
    curly_brackets  = "}",
    angle_brackets  = ">",
    chevrons        = "\u3009",
    hyphens         = " -",
    en_dashes       = " \u2013",
    em_dashes       = "\u2014",
    commas          = ", "
  )
  paste0(before, x, after)
}

#' Run code without stopping
#' 
#' Runs code without stopping, warnings and errors are only printed.
#' @param ... Passed to \code{tryCatch}.
#' @return The expression that was passed in is run.
#' @note This function is dangerous, since it overrides warnings and errors.
#' Its intended use is for documenting examples of errors.
#' @examples
#' dont_stop(warning("!!!"))
#' dont_stop(stop("!!!"))
#' f <- function() g()
#' g <- function() stop("!!!")
#' dont_stop(f())
#' @export
dont_stop <- function(...)
{
  # The expression, without dont_stop().
  cl <- sys.call()[[2]]
  p <- function(e) 
  {
    # If the error call claims to be to doTryCatch, then nothing interesting
    # was captured, so use the parent call that we captured earlier.
    if(identical(e$call[[1]], as.name("doTryCatch")))
    {
      e$call <- cl
    }
    print(e)
  }
  tryCatch(..., warning = p, error = p)
}

#' Set a cause and return the input
#' 
#' Sets the cause attribute of an object and returns that object.
#' @param x A variable.
#' @param value A character vector to set the cause to, where \code{x} is
#' not \code{TRUE}.
#' @details If \code{x} is \code{TRUE} everywhere, this returns the input 
#' without setting a cause.  Otherwise, the cause is an empty string where 
#' \code{x} is \code{TRUE}, and \code{value} elsewhere.
#' @return \code{x}, with a new cause attribute.
#' @seealso \code{\link{cause}} , \code{\link[stats]{setNames}}
set_cause <- function(x, value)
{
  if(all(!is.na(x) & x)) return(x)
  cause(x) <- ifelse(
    is.na(x), 
    "missing", 
    ifelse(x, "", value)
  )
  x
}

#' Strip all attributes from a variable.
#'
#' Strips all the attributes from a variable.
#'
#' @param x Input to strip.
#' @return \code{x}, without attributes.
#' @examples
#' x <- structure(c(foo = 1, bar = 2), some_attr = 3)
#' x2 <- strip_attributes(x)
#' assert_is_identical_to_false(has_names(x2), TRUE)
#' assert_is_null(attr(x2, "some_attr"))
#' @export
strip_attributes <- function(x)
{
  attributes(x) <- NULL
  x
}
 #' Get or set the system locale
#'
#' Wrappers to \code{Sys.getlocale} and \code{Sys.setlocale} for getting and
#' setting the system locale.
#'
#' @param simplify If \code{TRUE}, the locale settings are returned as a vector,
#' otherwise, a list.
#' @param ... Name-value pairs of locale categories to set.
#' @param l A list, as an alternative method of passing local categories to set.
#' @return A named list or vector giving the system locale names.
#' @examples
#' (current_locale <- sys_get_locale())
#' \dontrun{
#' english <- if(is_windows()) "English" 
#'   else if(is_mac()) "en_GB" 
#'   else if(is_linux()) "en_GB.utf8" 
#'   else "en"
#' sys_set_locale(LC_MONETARY = english)
#' sys_get_locale()
#' sys_set_locale(l = current_locale)  #restore everything
#' }
#' @seealso \code{\link[base]{Sys.getlocale}}.
#' @export
sys_get_locale <- function(simplify = FALSE)
{
  locale <- Sys.getlocale()
  if(locale == "C")
  {
    categories <- locale_categories(FALSE)
    values <- lapply(categories, function(x) "C")
  } else
  {
    splitter <- if(is_windows() || is_linux()) ";" else "/"
    locale <- strsplit(locale, splitter)[[1]]
    locale <- strsplit(locale, "=")
    categories <- vapply(
      locale,
      function(x) x[1],
      character(1)
    )
    values <- lapply(
      locale,
      function(x) x[2]
    )
  }

  names(values) <- categories
  if(simplify) unlist(values) else values
}

#' @rdname sys_get_locale
#' @export
sys_set_locale <- function(..., l = list())
{
  values <- merge_dots_with_list(..., l = l)
  categories <- names(values)
  categories <- match.arg(
    categories,
    locale_categories(),
    several.ok = TRUE
  )

  for(i in seq_along(values))
  {
    Sys.setlocale(categories[i], values[[i]])
  }
}

#' Only use the first element of a vector.
#'
#' If the input is not scalar, then only the first element is returned, 
#' with a warning.
#'
#' @param x Input that should be scalar.
#' @param indexer Either double indexing, \code{"[["} (the default) or
#' single indexing \code{"["}.
#' @return If \code{x} is scalar, it is returned unchanged, otherwise
#' only the first element is returned, with a warning.
#' @export
use_first <- function(x, indexer = c("[[", "["))
{
  # Can't use assert_is_non_empty, is_scalar in next lines because those 
  # functions calls this one.
  if(length(x) == 0L)
  {
    stop(get_name_in_parent(x), " is has length 0.")
  }
  if(length(x) == 1L)
  {
    return(x)
  }
  indexer <- match.fun(match.arg(indexer))
  warning(
    "Only the first value of ", sQuote(get_name_in_parent(x)), " will be used.",
    call. = FALSE
  )
  indexer(x, 1L)
}
