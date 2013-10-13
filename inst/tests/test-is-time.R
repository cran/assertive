test.is_in_past.then_now_soon.returns_true_in_past <- function()
{  
  #Can't test _now_ (x == Sys.time()) easily because on slow 
  #machines time has moved on, but this isn't the case for 
  #fast machines.  5secs should be plenty of leaway.
  x <- Sys.time() + c(-1, 5)
  expected <- c(TRUE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_in_past(x)
  )
}

test.is_in_future.then_now_soon.returns_true_in_past <- function()
{
  #See problem in test.is_in_past
  x <- Sys.time() + c(-1, 5)
  expected <- c(FALSE, TRUE)
  names(expected) <- x
  checkEquals(
    expected,
    is_in_future(x)
  )
}
