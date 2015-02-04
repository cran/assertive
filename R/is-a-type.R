#' @rdname is_logical
#' @export
is_a_bool <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_logical(x, .xname))) return(ok)
  if(!(ok <- is_scalar(x, .xname = .xname))) return(ok)
  TRUE
}

#' @rdname is_complex
#' @export
is_a_complex <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_complex(x, .xname))) return(ok)
  if(!(ok <- is_scalar(x, .xname = .xname))) return(ok)
  TRUE
}

#' @rdname is_character
#' @export
is_a_non_empty_string <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_a_string(x))) return(ok)
  if(!nzchar(x))
  {
    return(false("%s has no characters.", .xname))
  }
  TRUE
}

#' @rdname is_numeric
#' @export
is_a_number <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_numeric(x, .xname))) return(ok)
  if(!(ok <- is_scalar(x, .xname = .xname))) return(ok)
  TRUE
} 

#' @rdname is_raw
#' @export
is_a_raw <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_raw(x, .xname))) return(ok)
  if(!(ok <- is_scalar(x, .xname = .xname))) return(ok)
  TRUE
} 

#' @rdname is_character
#' @export
is_a_string <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_character(x, .xname))) return(ok)
  if(!(ok <- is_scalar(x, .xname = .xname))) return(ok)
  TRUE
}

#' @rdname is_character
#' @export
is_an_empty_string <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_a_string(x, .xname))) return(ok)
  if(nzchar(x)) 
  {
    return(false("%s contains characters.", .xname))
  }
  TRUE
}

#' @rdname is_integer
#' @export
is_an_integer <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_integer(x, .xname))) return(ok)
  if(!(ok <- is_scalar(x, .xname = .xname))) return(ok)
  TRUE
} 

#' Does the object inherit from some class?
#' 
#' Checks to see if an object is inherited from any of the specifed classes.
#' @param x Any R variable.
#' @param classes A character vector of classes.
#' @param .xname Not intended to be used directly.
#' @return \code{TRUE} if \code{x} inherits from at least one of the classes,
#' as determined by \code{\link[base]{inherits}}.
#' @seealso \code{\link[base]{inherits}}, \code{\link{is2}}
#' @examples
#' x <- structure(1:5, class = c("foo", "bar"))
#' assert_is_inherited_from(x, c("foo", "baz"))
#' dont_stop(assert_is_inherited_from(x, c("Foo", "baz")))
#' @export
is_inherited_from <- function(x, classes, .xname = get_name_in_parent(x))
{
  ok <- bapply(classes, function(class) inherits(x, class))
  if(!any(ok)) 
  {
    return(
      false(
        "%s does not inherit from any of the classes %s.", 
        .xname, 
        toString(classes)
      )
    )
  }
  TRUE
}
