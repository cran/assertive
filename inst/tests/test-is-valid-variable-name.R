                                        
test_is_valid_variable_name_x_returns_true <- function()
{
  checkTrue(is_valid_variable_name("x"))
}
                                         
test_is_valid_variable_name_dot_returns_true <- function()
{
  checkTrue(is_valid_variable_name("."))
}
                                           
test_is_valid_variable_name_2_dots_returns_true <- function()
{
  checkTrue(is_valid_variable_name(".."))
}
                                             
test_is_valid_variable_name_3_dots_returns_allow_reserved <- function()
{
  checkTrue(is_valid_variable_name("..."))             
  checkTrue(!is_valid_variable_name("...", allow_reserved = FALSE))
}
                       
test_is_valid_variable_name_4_dots_returns_true <- function()
{
  checkTrue(is_valid_variable_name("...."))
}
                       
test_is_valid_variable_name_5_dots_returns_true <- function()
{
  checkTrue(is_valid_variable_name("....."))
}

test_is_valid_variable_name_dash_returns_false <- function()
{
  checkTrue(!is_valid_variable_name("_"))
}

test_is_valid_variable_name_1_x_returns_false <- function()
{
  checkTrue(!is_valid_variable_name("1x"))
}
                                   
test_is_valid_variable_name_dot_dash_returns_true <- function()
{
  checkTrue(is_valid_variable_name("._"))
}
                                   
test_is_valid_variable_name_dot_1_returns_false <- function()
{
  checkTrue(!is_valid_variable_name(".1"))
}
                    
test_is_valid_variable_name_dot_x_returns_true <- function()
{
  checkTrue(is_valid_variable_name(".x"))
}
                    
test_is_valid_variable_name_dot_dash_1_returns_true <- function()
{
  checkTrue(is_valid_variable_name("._1"))
}
                    
test_is_valid_variable_name_dot_dot_1_returns_allow_reserved <- function()
{
  checkTrue(is_valid_variable_name("..1")) 
  checkTrue(!is_valid_variable_name("..1", allow_reserved = FALSE))
}
                                          
test_is_valid_variable_name_dot_dot_2_returns_allow_reserved <- function()
{
  checkTrue(is_valid_variable_name("..2")) 
  checkTrue(!is_valid_variable_name("..2", allow_reserved = FALSE))
}
                    
test_is_valid_variable_name_dot_dot_dot_1_returns_true <- function()
{
  checkTrue(is_valid_variable_name("...1"))
}
                    
test_is_valid_variable_name_dot_dot_dot_dot_1_returns_true <- function()
{
  checkTrue(is_valid_variable_name("....1"))
}
                    
test_is_valid_variable_name_dot_dot_x_returns_true <- function()
{
  checkTrue(is_valid_variable_name("..x"))
}
                  
test_is_valid_variable_name_long_name_returns_false <- function()
{
  vn <- paste(rep.int("a", 10001L), collapse = "")
  checkTrue(!is_valid_variable_name(vn))
}      
                 
test_is_valid_variable_name_same_names_returns_allow_duplicates <- function()
{
  vn <- rep.int("foo", 2)
  checkTrue(all(is_valid_variable_name(vn)))
  checkTrue(!all(is_valid_variable_name(vn, allow_duplicates = FALSE)))
}
