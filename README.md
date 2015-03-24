# assertive

Readable check functions to ensure code integrity.


### Installation

To install the stable version, type:

```{r}
install.packages("assertive")
```

To install the development version, you first need the devtools package.

```{r}
install.packages("devtools")
```

Then you can install the assertive package using

```{r}
library(devtools)
install_bitbucket("richierocks/assertive")
```


### How to use the package

assertive contains lots of *assert* functions that throw errors if conditions 
aren't met.  They are very useful for checking user input to your functions.

For example,

```{r}
f <- function(x)
{
  assert_is_not_null(x)
  x + 1
}
f(1)
## [1] 2
f(NULL)
## Error: x is NULL.
```

(You can think of the *assert* functions as more specific versions of 
`base::stopifnot` that make your code easier to read and give more informative 
error messages.)

Each *assert* function has a corresponding *is* function.  In this case, 
`is_not_null` is a wrapper to base-R's `!is.null`, that gives a more informative 
error message on failure (in an attribute named `cause`).

```{r}
is_not_null(1)
## [1] TRUE

is_not_null(NULL)
## [1] FALSE
## attr(,"cause")
## [1] NULL is NULL.
```

Many of the *is* functions are wrappers to base functions.  They all return 
causes of failure, and they have consistent naming, beginning `is_` or `has_` 
(so `base::interactive` becomes `is_interactive`, for example.)


### Vectorised *is* functions

Some *is* functions return a logical vector rather than a single value.  For 
example,

```{r}
is_positive(c(1, 0, -1, NA))
##     1     0    -1  <NA> 
##  TRUE FALSE FALSE    NA 
## attr(,"cause")
## [1]         too low too low missing
```

In this case the input values are returned in the names to make it easier to see 
which values succeeded/failed, and the cause attribute is also vectorised.

There are two corresponding *assert* functions for these vectorised *is* 
functions.

```{r}
assert_any_are_positive(c(1, 0, -1, NA)) # test passed since 1 is positive
assert_all_are_positive(c(1, 0, -1, NA))
## Error: c(1, 0, -1, NA) contains non-positive values.
## There were 3 failures:
##   Position Value   Cause
## 1        2     0 too low
## 2        3    -1 too low
## 3        4  <NA> missing
```

### Testing types

You can test for a particular type of object using `is_numeric`, `is_character`, 
`is_matrix`, `is_data.frame`, through to more obscure types like `is_qr`, 
`is_name`, and `is_relistable`.

`is_s4`, `is_atomic` and `is_recursive` test properties of variables.

`is_a_number`, `is_a_string`, `is_a_bool`, etc. combine tests for types with 
`is_scalar` (see below) to check for a single numeric/character/logical value 
respectively.


### Testing sizes

`is_scalar` tests for objects of length one, or with one element (this can be 
different for lists; you choose the metric).

Similarly `is_empty` and `is_non_empty` test for objects of zero 
length/containing zero elements.

More generally `is_of_length` and `has_elements` test for a particular length/
number of elements.


### Testing missing values

You can test for NAs, NaNs, and NULLs using `is_na`, `is_nan` and `is_null`, or 
their opposites `is_not_na`, `is_not_nan` and `is_not_null`.


### Testing numbers

`is_in_range` tests if a number is in a numeric range, along with the more 
specialised wrappers: `is_in_open_range`, `is_in_closed_range`, 
`is_in_left_open_range`, `is_in_right_open_range`, `is_positive`, `is_negative`, 
`is_non_positive`, `is_non_negative`, `is_proportion` and `is_percentage`.

Finiteness can be tested with `is_finite`, `is_infinite`, `is_positive_infinity` 
and `is_negative_infinity`.

`is_odd` and `is_even` test for those qualities, and are generalized by 
`is_divisible_by`.

`is_whole_number` tests whether a number is an integer, give or take some 
tolerance.

`is_real` and `is_imaginary` test for real/imaginary numbers.


### Testing attributes

Rows, columns and dimensions can be tested for using, `has_rows`, `has_cols` and 
`has_dims`.

Similarly their names can be tested for using `has_rownames`, `has_colnames`, 
`has_dimnames` and `has_names`.

Duplicates can be tested for using `has_duplicates` or its opposite `has_no_duplicates`.

Attributes can be tested for using `has_attributes` and `has_any_attributes`.

Functions arguments can be tested using `has_arg`.

Model terms can be tested using `has_terms`.


### Testing files and connections

`is_dir` tests if a path refers to an existing directory.

`is_existing_file` tests whether a file exists.

`is_executable_file`, `is_readable_file`, and `is_writable_file` test your 
permissions to access a file (though they are based on `file.access`, which is 
slightly unreliable on Windows).

`is_connection` tests whether an object is a connection, and there are many 
specialized types including `is_file_connection`, `is_fifo_connection`, 
`is_pipe_connection`, `is_readable_connection`, `is_open_connection`, 
`is_writable_connection`, `is_stdin`, `is_stdout` and `is_stderr`.


### Testing complex data types

`is_email_address`, `is_credit_card_number`, `is_date_string`, `is_honorific`, 
`is_ip_address`, `is_hex_color`, `is_cas_number` and `is_isbn_code` check for 
these more complex data types. 

`is_uk_car_licence`, `is_uk_national_insurance_number`, `is_uk_postcode` and 
`is_uk_telephone_number` test the United Kingdom-specific data types.

`is_us_telephone_number`, `is_us_zip_code` test the United States-specific data 
types.

Adding to this section is a priority for *assertive* development.  Request 
additional data types on the 
[issues](https://bitbucket.org/richierocks/assertive/issues?status=new&status=open) 
page.


### Reflection

You can test the operating system with `is_windows`, `is_linux`, `is_mac`, 
`is_solaris` and the more general `is_unix`.

`is_r`, `is_r_devel`, `is_r_patched`, `is_r_release_candidate` and `is_r_stable`
test if you are running R, and if so what type.

`is_rstudio`, `is_architect` and `is_revo_r` test for specific IDEs.

You can test R's capabilities using `r_has_png_capability`, 
`r_has_tcltk_capability`, etc.

You can test the currentlocale's preference for decimal points using 
`is_comma_for_decimal_point` or `is_period_for_decimal_point`.

`is_slave_r`, `is_interactive`, `is_batch_mode`, `is_32_bit` and `is_64_bit` 
test how R is being run.

`r_can_compile_code` tests whether the OS has the necessary tools for compiling
code, and that R can see them.  Useful for working with `Rcpp`.


### Testing code

`is_debugged` tests whether a function is being debugged (by `debug` or 
`debugonce`).

`is_valid_variable_name` tests whether a string is a valid variable name.

`is_error_free` runs code and returns an indicator of whether or not an error 
was thrown.

`is_valid_r_code` parses code and returns an indicator of whether or not an 
error was thrown.


### Utilities

`use_first` takes the first value of a vector, warning you if it one longer than 
length one.

`coerce_to` is a wrapper to `as`, changing an object's type with a warning.

`get_name_in_parent` gets the name of a variable in the parent environment 
(stopping you have to remember `deparse(substitute())` arcana).

`strip_attributes` strips the attributes from an object.

`merge_dots_with_list` merges the contents of `...` with a list argument, to 
allow users to pass arguments to your function in either form.

`dont_stop` runs code without stopping at errors, which is useful for 
demonstrating errors in examples.

`parenthesise` wraps a string in parentheses.
