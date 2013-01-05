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
  attr(x, "cause") <- noquote(as.character(value))
  x
}

#' Coerce variable to a different class.
#'
#' Coerce the input to a different class, with a warning.
#'
#' @param x Input to coerce.
#' @param target_class The desired class of x.
#' @param .xname Not intended to be used directly.
#' @return The input \code{x} after attempted coersion to the target class.
#' @note If x does not already have the target class, a warning is given
#' before coersion.
#' @seealso \code{\link[methods]{is}} and \code{\link[methods]{as}}.
#' @export
coerce_to <- function(x, target_class, .xname = get_name_in_parent(x))
{
  if(!is2(x, target_class))
  {
    warning(
      "Coercing ", .xname, " to class ", sQuote(target_class), ".",
      call. = FALSE
    )
    x <- as(x, target_class)
  }
  x
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
  deparse(do.call(
    substitute, 
    list(substitute(x), parent.frame())
  ))
}

#' Merge ellipsis args with a list.
#'
#' Merges variable length ellipsis arguments to a function with a list argument.
#'
#' @param ... Some inputs.
#' @param l A list.
#' @note If any arguments are present in both the \code{...} and \code{l} arguments,
#' the \code{...} version takes preference, and a warning is thrown.
#' @return A list containing the merged inputs.
#' @examples
#' merge_dots_with_list(foo = 1, bar = 2, baz = 3, l = list(foo = 4, baz = 5, quux = 6))
#' @export
merge_dots_with_list <- function(..., l = list())
{
  dots <- list(...)
  l <- coerce_to(l, "list")
  all_names <- c(names(dots), names(l))
  all_values <- c(dots, l)
  if(has_duplicates(all_names))
  {
    warning("Duplicated arguments: ", toString(all_names[duplicated(all_names)]))
    all_values <- all_values[!duplicated(all_names)]
  }
  all_values
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
#' @return If \code{x} is scalar, it is returned unchanged, otherwise
#' only the first element is returned, with a warning.
#' @export
use_first <- function(x)
{
  assert_is_vector(x)
  assert_is_non_empty(x)
  if(!is_scalar(x))
  {
    warning(
      "Only the first value of ", sQuote(deparse(substitute(x))), " will be used.",
      call. = FALSE
      )
    x <- x[[1]]
  }
  x
}
