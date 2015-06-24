#' Are the inputs identical
#' 
#' Generalisation of \code{identical} to an arbitrary number of inputs.
#' @param ... Some R expressions
#' @return A symmetric square logical matrix which is \code{TRUE} where pairs
#' of inputs are identical.
#' @seealso \code{\link[base]{identical}}, \code{\link{are_same_length}}
#' @examples
#' x <- 1:5
#' are_identical(cos(pi), -1, x, (2:6) - 1L)
#' assert_any_are_identical(cos(pi), -1, x, (2:6) - 1L)
#' dont_stop(assert_all_are_identical(cos(pi), -1, x, (2:6) - 1L))
#' @export
are_identical <- function(...)
{
  envir <- parent.frame()
  inputs <- as.list(match.call())[-1]
  input_pairs <- expand.grid(expr1 = inputs, expr2 = inputs)
  identicality <- apply(
    input_pairs, 
    1, 
    function(row)
    {       
      with(
        row, 
        identical(
          eval(expr1, envir = envir),
          eval(expr2, envir = envir)
        )
      )
    }
  )
  matrix(
    identicality,
    nrow     = nargs(),
    dimnames = list(inputs, inputs) 
  )
}

#' Are the inputs the same length
#' 
#' Checks if the inputs are the same length.
#' @param ... Some R expressions.
#' @param l A list of R expressions.
#' @return A symmetric square logical matrix which is \code{TRUE} where pairs
#' of inputs are the same length.
#' @seealso \code{\link[base]{length}}, \code{\link{are_identical}}
#' @examples
#' x <- 1:5
#' are_same_length(
#'   runif(5), x, list(1, 2:3, 4:6, 7:10, 11:15), 1:6, 
#'   l = list(seq.int(0, 1, 0.2), rnorm(5))
#' )
#' assert_any_are_same_length(runif(5), x, list(1, 2:3, 4:6, 7:10, 11:15), 1:6)
#' dont_stop(assert_all_are_same_length(runif(5), x, list(1, 2:3, 4:6, 7:10, 11:15), 1:6))
#' @export
are_same_length <- function(..., l = list())
{
  #merge_dots_with_list(..., l)
  envir <- parent.frame()
  inputs <- as.list(match.call())[-1]
  inputs_in_list <- as.list(inputs$l)[-1]
  inputs <- c(inputs[names(inputs) != "l"], inputs_in_list)
  input_pairs <- expand.grid(expr1 = inputs, expr2 = inputs)
  equality <- apply(
    input_pairs, 
    1, 
    function(row)
    {       
      with(
        row,         
        length(eval(expr1, envir = envir)) == length(eval(expr2, envir = envir))
      )
    }
  )
  matrix(
    equality,
    nrow     = length(inputs),
    dimnames = list(inputs, inputs) 
  )
}
