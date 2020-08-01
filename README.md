[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active)
[![Is the package on CRAN?](http://www.r-pkg.org/badges/version/assertive)](http://www.r-pkg.org/pkg/assertive)
[![SemaphoreCI Build Status](https://semaphoreci.com/api/v1/projects/a9fbd8d5-fa9c-4b63-9457-23321ece7c8b/635061/badge.svg)](https://semaphoreci.com/richierocks/assertive)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/a7lqf6vwr3mbemi5?svg=true)](https://ci.appveyor.com/project/richierocks/assertive)
[![Research software impact](http://depsy.org/api/package/cran/assertive/badge.svg)](http://depsy.org/package/r/assertive)

![assertive logo](http://s25.postimg.org/uk9jho2db/assertive_logo.png)

# assertive

*An R package that provides readable check functions to ensure code integrity.*

There are times when it is a good idea to check the state of your variables, to
ensure that they have the properties that you think they have. For example,
if you have a count variable, you might want to check that it is numeric, that
all the values are non-negative, and that all the values are whole numbers.

*assertive* provides lots of functions ("predicates" and "assertions") to provide 
such checks.  It is designed to make your code very easy to read, and to provide 
highly informative error messages.


### Installation

To install the stable version, type:

```{r}
install.packages("assertive")
```

To install the development version, you first need the *devtools* package.

```{r}
install.packages("devtools")
```

Then you can install the *assertive* package using

```{r}
library(devtools)
install_bitbucket("richierocks/assertive")
```

### How to use the package

*assertive* contains lots of *assert* functions ("assertions") that throw errors 
if conditions aren't met.  They are very useful for checking user input to your 
functions.

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
## Error in f(NULL) : x is NULL.
```

(You can think of the *assert* functions as more specific versions of 
`base::stopifnot` that make your code easier to read and give more informative 
error messages.)

Each *assert* function has a corresponding *is* function (a "predicate").  In 
this case,  `is_not_null` is a wrapper to base-R's `!is.null`, that gives a more 
informative error message on failure (in an attribute named `cause`).

```{r}
is_not_null(1)
## [1] TRUE

is_not_null(NULL)
## [1] FALSE
## Cause of failure:  NULL is NULL.
```

Many of the *is* functions are wrappers to base functions.  They all return 
causes of failure, and they have consistent naming, beginning `is_` or `has_` 
(so `base::interactive` becomes `is_interactive`, for example.)


### Vectorised *is* functions

Some *is* functions return a logical vector rather than a single value.  In 
this case the input values are returned in the names to make it easier to see 
which values succeeded/failed, and the cause attribute is also vectorised.

For example,

```{r}
is_positive(c(1, 0, -1, NA))
## There were 3 failures:
##   Position Value   Cause
## 1        2     0 too low
## 2        3    -1 too low
## 3        4  <NA> missing
```

Using `unclass`, you can see that this is just a logical vector, with a `cause`
attribute.

```{r}
unclass(is_positive(c(1, 0, -1, NA)))
##     1     0    -1  <NA> 
##  TRUE FALSE FALSE    NA 
## attr(,"cause")
## [1]         too low too low missing
```

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


### Can't I just use *testthat*?

[*testthat*](https://github.com/hadley/testthat) is an excellent package for 
writing unit tests, and I recommend that you use it.  Unit tests are a form of
development-time testing.  That is, you write the tests while you develop your
code in order to check that you haven't made any mistakes.

*assertive*, and assertions in general, are for run-time testing.  That is,
you include them in your code to check that the user hasn't made a mistake
while running your code.


### The virtual package system

*assertive* is a virtual package; it does not contain any functions, but merely 
reexports them from lower-level packages.  For a complete reference, see the 
individual package pages.

*[assertive.base](https://bitbucket.org/richierocks/assertive.base)* contains 
the core functionality.  For example, `is_true` checks when inputs return 
`TRUE`.  It also contains some utility functions, such as `use_first`, which 
returns the first value of a vector, warning you if it was longer than length 
one.

*[assertive.properties](https://bitbucket.org/richierocks/assertive.properties)* 
contains checks on properties of variables.  For example, `is_scalar` checks
for values that have length one, and `has_duplicates` checks for the presence of 
duplicate values.

*[assertive.types](https://bitbucket.org/richierocks/assertive.types)* contains 
checks for types of variables. For example, `is_character` wraps the base 
`is.character`, while `is_a_string` combines that check with `is_scalar` to 
check for single strings.

*[assertive.numbers](https://bitbucket.org/richierocks/assertive.numbers)* 
contains checks for numbers.  For example, `is_in_range` checks if a number is 
in a numeric range.

*[assertive.strings](https://bitbucket.org/richierocks/assertive.strings)* 
contains checks for strings.  For example, `is_an_empty_string` checks if
a character vector contains a single empty string.

*[assertive.datetimes](https://bitbucket.org/richierocks/assertive.datetimes)* 
contains checks for dates and times.  For example, `is_in_past` checks if a
`Date` or `POSIXt` obejct is in the past.

*[assertive.files](https://bitbucket.org/richierocks/assertive.files)* contains 
checks for files and connections.  For example, `is_readable_file` checks if
a path points to a file that R has permission to read, and `is_file_connection`
check if an object is a file connection.

*[assertive.sets](https://bitbucket.org/richierocks/assertive.sets)* contains 
checks for sets.  For example, `is_subset` checks if a vector is a subset of 
another vector.

*[assertive.matrices](https://bitbucket.org/richierocks/assertive.matrices)* 
contains checks for matrices.  For example, `is_symmetric_matrix` checks if
a matrix is symmetric.

*[assertive.models](https://bitbucket.org/richierocks/assertive.models)* 
contains checks for models.  For example, `is_empty_model` checks if a model
is the empty model (no factors).

*[assertive.data](https://bitbucket.org/richierocks/assertive.data)* contains 
checks for complex data types.  For example, `is_credit_card_number` checks a
character vector for valid credit card numbers.

*[assertive.data.uk](https://bitbucket.org/richierocks/assertive.data.uk)* 
contains checks for UK-specific complex data types.  For example, 
`is_uk_postcode` checks a character vector for valid UK postcodes.

*[assertive.data.us](https://bitbucket.org/richierocks/assertive.data.us)* 
contains checks for US-specific complex data types.   For example, 
`is_us_telephone_number` checks a character vector for valid US telephone 
numbers.

*[assertive.reflection](https://bitbucket.org/richierocks/assertive.reflection)* 
contains checks on the state of R.  For example, `is_solaris` tests for that
operating system, and `is_rstudio` tests for that IDE.

*[assertive.code](https://bitbucket.org/richierocks/assertive.code)* contains 
checks for code.  For example, `is_valid_variable_name` checks whether a 
character vector contains valid variable names.


### I hate this; what's the alternative?

There are at least five other R packages for doing assertions.  In alphabetical 
order of package:

- Tony Fishettti's [assertr](https://github.com/tonyfischetti/assertr)
- Hadley Wickham's [assertthat](https://github.com/hadley/assertthat)
- Michel Lang's [checkmate](https://github.com/mllg/checkmate)
- Stefan Bache's [ensurer](https://github.com/smbache/ensurer)
- Gaston Sanchez's [tester](https://github.com/gastonstat/tester)

### I want to know more

There are several vignettes with more details on how to use the package, 
including case studies and exercises.  Find them using

```{r}
browseVignettes()
```

### I want to help

The *assertive* packages are in the process of being translated into many languages.
If you speak a language other than English, and have an hour or two spare, your
translation skills would be appreciated.

*assertive* is also currently lacking assertions for time series, spatial data,
personal data for countries other than the UK and US, and industry-sector-specific
data.  If you want to contribute a package for these data types, let me know and 
I can talk you through how to do it.