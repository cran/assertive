test.is_connection.not_a_connection.returns_false <- function()
{
  checkTrue(!is_connection("not a connection"))
}

test.is_connection.a_connection.returns_true <- function()
{
  fcon <- file()
  on.exit(close(fcon))
  checkTrue(is_connection(fcon))
}

test.is_connection.std_connections.returns_true <- function()
{  
  for(con in c(stdin, stdout, stderr))
  {
    checkTrue(is_connection(con())) 
  }
}


test.is_incomplete_connection.not_a_connection.returns_false <- function()
{
  checkTrue(!is_incomplete_connection("not a connection"))  
}

test.is_incomplete_connection.a_closed_connection.returns_false <- function()
{  
  tcon <- textConnection("txt", "w", local = TRUE)
  close(tcon)
  checkTrue(!is_incomplete_connection(tcon))
}

test.is_incomplete_connection.an_incomplete_connection.returns_true <- function()
{  
  tcon <- textConnection("txt", "w", local = TRUE)
  on.exit(close(tcon))
  cat("this has no final newline character", file = tcon)
  checkTrue(is_incomplete_connection(tcon))
}

test.is_incomplete_connection.a_complete_connection.returns_false <- function()
{  
  tcon <- textConnection("txt", "w", local = TRUE)
  on.exit(close(tcon))
  cat("this has a final newline character\n", file = tcon)
  checkTrue(!is_incomplete_connection(tcon))
}


test.is_open_connection.not_a_connection.returns_false <- function()
{
  checkTrue(!is_open_connection("not a connection"))  
}

test.is_open_connection.a_closed_connection.returns_false <- function()
{
  fcon <- file()
  close(fcon)
  checkTrue(!is_open_connection(fcon))  
}

test.is_open_connection.an_open_readable_connection.returns_true <- function()
{
  readable <- "r"
  file.create(tmp <- tempfile())
  fcon <- file(tmp, open = readable)
  on.exit({close(fcon); unlink(tmp)})
  checkTrue(is_open_connection(fcon, readable))  
}

test.is_open_connection.an_open_writable_connection.returns_true <- function()
{
  writable <- "w"
  file.create(tmp <- tempfile())
  fcon <- file(tmp, open = writable)
  on.exit({close(fcon); unlink(tmp)})
  checkTrue(is_open_connection(fcon, writable))  
}
