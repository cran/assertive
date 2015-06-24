library(testthat)
library(devtools)
library(assertive)

with_envvar(
  c(LANG = "en_US"),
  test_check("assertive")
)
