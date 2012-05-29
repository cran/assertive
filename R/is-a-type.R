
#' @rdname is_logical
#' @export
is_a_bool <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_logical(x, .xname))) return(ok)
  if(!(ok <- is_scalar(x, .xname))) return(ok)
  TRUE
}

#' @rdname is_complex
#' @export
is_a_complex <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_complex(x, .xname))) return(ok)
  if(!(ok <- is_scalar(x, .xname))) return(ok)
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
  if(!(ok <- is_scalar(x, .xname))) return(ok)
  TRUE
} 

#' @rdname is_raw
#' @export
is_a_raw <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_raw(x, .xname))) return(ok)
  if(!(ok <- is_scalar(x, .xname))) return(ok)
  TRUE
} 

#' @rdname is_character
#' @export
is_a_string <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_character(x, .xname))) return(ok)
  if(!(ok <- is_scalar(x, .xname))) return(ok)
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
  if(!(ok <- is_scalar(x, .xname))) return(ok)
  TRUE
} 
