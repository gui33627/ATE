
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ATE

<!-- badges: start -->
<!-- badges: end -->

The ATE (additive term extraction) package provides a variety of methods
to estimate site, path, and event terms from ground motion residuals.

## Installation

1.  Make sure “tiydverse”, “cmdstanr”, “stringr”,“dplyr”, “lme4”, and
    “magrittr” are installed as they are dependencies. Otherwise, you
    can run the following command to install them. For error and
    warnings when installing “cmdstanr”, please refer to the
    [page](https://mc-stan.org/cmdstanr/).

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
library(tidyverse)
library(lme4)
N <- 10
results_list <- list()

for (i in 1:N) {
  gm_data <- dgp(num_eq = 10, num_site = 20,
            eta_eq_sd = 1, eta_site_sd = 1, eta_noise_sd = 1,
            path_term = TRUE, eta_path_sd = 1,
            num_eq_cluster = 4, num_site_cluster = 5,
            missing_percent = 0.1)
  
  eta_site_fixed <- ATE::stepwise_extraction(gm_data, random = FALSE, path = FALSE)
  eta_site_fixed_path <- ATE::stepwise_extraction(gm_data, random = FALSE, path = TRUE)
  eta_site_random <- ATE::stepwise_extraction(gm_data, random = TRUE, path = FALSE)
  eta_site_random_path <- ATE::stepwise_extraction(gm_data, random = TRUE, path = TRUE)
  eta_site_fixed_random <- ATE::combined_extraction(gm_data, path = FALSE)
  eta_site_fixed_random_path <- ATE::combined_extraction(gm_data, path = TRUE, path_method = "stepwise")
  eta_site_fixed_random2 <- ATE::combined_extraction(gm_data, path = TRUE, path_method = "random")
  eta_site_fixed2_random <- ATE::combined_extraction(gm_data, path = TRUE, path_method = "fixed")
  eta_site_bayesian <- ATE::bayesian_extraction(gm_data, path = TRUE)
  
  results <- eta_site_fixed %>% left_join(eta_site_fixed_path, by = c("eta_site" = "eta_site", "site_id" = "site_id")) %>% 
    left_join(eta_site_random, by = c("eta_site" = "eta_site", "site_id" = "site_id")) %>% 
    left_join(eta_site_random_path, by = c("eta_site" = "eta_site", "site_id" = "site_id")) %>% 
    left_join(eta_site_fixed_random, by = c("eta_site" = "eta_site", "site_id" = "site_id")) %>% 
    left_join(eta_site_fixed_random_path, by = c("eta_site" = "eta_site", "site_id" = "site_id")) %>% 
    left_join(eta_site_fixed_random2, by = c("eta_site" = "eta_site", "site_id" = "site_id")) %>% 
    left_join(eta_site_fixed2_random, by = c("eta_site" = "eta_site", "site_id" = "site_id")) %>% 
    left_join(eta_site_bayesian, by = c("eta_site" = "eta_site")) 
  
  
  results_list <- c(results_list, list(results))
}
```
