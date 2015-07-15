#' Is the binding of a variable locked?
#' 
#' Check to see if the binding of a variable is locked (that is, it has been 
#' made read-only).
#' @param x Input to check. (Unlike \code{bindingIsLocked}, you can pass the
#' variable itself, rather than a string naming that variable.)
#' @param env Environment to check where binding had been locked.
#' @param .xname Not intended to be used directly.
#' @return \code{TRUE} or \code{FALSE}, depending upon whether or not the 
#' binding is locked in the specified environment.
#' \code{assert_is_binding_locked} returns nothing but throws an error if 
#' the corresponding \code{is_*} function returns \code{FALSE}.
#' @note The environment is guessed as follows: The name of \code{x} is 
#' determined via \code{get_name_in_parent}.  Then find is called, 
#' @seealso \code{\link[base]{bindingIsLocked}}, which this wraps, 
#' \code{\link[utils]{find}} for how the environment is guessed.  If this returns
#' a single environment, that is used.  Otherwise the parent environment is 
#' used (as determined with \code{\link[base]{parent.frame}}).
#' @examples
#' is_binding_locked(a_non_existent_variable)
#' x <- 1:10
#' is_binding_locked(x)
#' lockBinding("x", parent.frame())
#' is_binding_locked(x)
#' unlockBinding("x", parent.frame())
#' is_binding_locked(x)
#' @importFrom utils find
#' @export
is_binding_locked <- function(x, env = if(is_scalar(e <- find(.xname))) as.environment(e) else parent.frame(), .xname = get_name_in_parent(x))
{
  assert_is_environment(env)
  .xname <- force(.xname)
  env <- force(env)
  if(!exists(.xname, env, inherits = FALSE))
  {
    return(false("%s does not exist in %s.", .xname, format(env)))
  }
  if(!bindingIsLocked(.xname, env))
  {
    return(false("%s is not locked (read-only) in %s.", .xname, format(env)))
  }
  TRUE
}
