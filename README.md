
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ATE

<!-- badges: start -->
<!-- badges: end -->

The package provide a variety of methods to estimate site, path, and
event terms from ground motion residuals.

## Installation

1.  Make sure “tiydverse”, “cmdstanr”, “stringr”,“dplyr”, “lme4”, and
    “magrittr” are installed as they are dependencies. Otherwise, you
    can run the following command to install them. For error and
    warnings when installing “cmdstanr”, please refer to the
    \[page\]{<https://mc-stan.org/cmdstanr/>}.

``` r
install.packages(c("tiydverse", "cmdstanr", "stringr","dplyr", "lme4", "magrittr"))
```

2.  Install utility package “devtools”:

``` r
install.packages("devtools")
```

3.  Lastly, install “ATE” package:

``` r
library(devtools)
install_github('gui33627/ATE')
```

## Example

This is the example code that reproduces the results of our simulation
study:

``` r
library(ATE)
```