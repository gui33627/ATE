
#' Estimate site terms by Bayesian model.
#'
#' @param data A data frame used for estimating site effect.
#' @param path A logical variable. If path == TRUE, path effect will be
#' controlled in estimation of site effect.
#'
#' @return A data frame that includes site ID, and the corresponding
#' true site terms and estimated site terms.
#' @import tidyverse dplyr stringr cmdstanr
#' @importFrom stats coefficients fitted lm rnorm sd setNames
#' @export
#'
#' @examples
#' data(gm_data)
#' bayesian_extraction(data = gm_data, path = FALSE)
bayesian_extraction <- function(data, path = FALSE){

  if(path == TRUE){

    stan_file <- system.file("stan", "eta_terms_extraction.stan", package = "ATE")
    mod <- cmdstan_model(stan_file)

    fit_eq_fixed_path <- lm(r ~ factor(earthquake_id) - 1, data = data)
    data_fixed_path <- data %>% mutate(r_eq_rm = r - fitted(fit_eq_fixed_path))
    fit_path_fixed <- lm(r_eq_rm ~ factor(path_id) - 1, data = data_fixed_path)
    data_fixed_path <- data_fixed_path %>% mutate(r_eq_path_rm = r_eq_rm - fitted(fit_path_fixed))
    fit_eq_site_fixed_path <- lm(r_eq_path_rm ~ factor(site_id) - 1, data = data_fixed_path)

    data_relevel <- data
    data_relevel$earthquake_id <- remap_elements_and_levels(data$earthquake_id)
    data_relevel$site_id <- remap_elements_and_levels(data$site_id)
    data_relevel$path_id <- remap_elements_and_levels(data$path_id)

    eta_e_sd <- sd(fit_eq_fixed_path$fitted.values)
    eta_p_sd <- sd(fit_path_fixed$fitted.values)
    eta_s_sd <- sd(fit_eq_site_fixed_path$fitted.values)

    unique_site_len <- length(unique(data_relevel$site_id))
    unique_eq_len <- length(unique(data_relevel$earthquake_id))
    unique_path_len <- length(unique(data_relevel$path_id))

    matrix_id <- cbind(as.numeric(data_relevel$earthquake_id),
                       as.numeric(data_relevel$site_id),
                       as.numeric(data_relevel$path_id))

    data_list <- list(N = nrow(data_relevel), M = 3, N_e = unique_eq_len, N_s = unique_site_len,
                      N_p = unique_path_len,
                      eta_e_sd = eta_e_sd, eta_s_sd = eta_s_sd, eta_p_sd = eta_p_sd,
                      tot_res = data_relevel$r,
                      mat_ids = matrix_id)

  }else{
    stan_file <- system.file("stan", "eta_terms_extraction_nopath.stan", package = "ATE")
    mod <- cmdstan_model(stan_file)

    fit_eq_fixed <- lm(r ~ factor(earthquake_id) - 1, data = data)
    data_fixed <- data %>% mutate(r_eq_rm = r - fitted(fit_eq_fixed))
    fit_eq_site_fixed <- lm(r_eq_rm ~ factor(site_id) - 1, data = data_fixed)

    data_relevel <- data
    data_relevel$earthquake_id <- remap_elements_and_levels(data$earthquake_id)
    data_relevel$site_id <- remap_elements_and_levels(data$site_id)

    eta_e_sd <- sd(fit_eq_fixed$fitted.values)
    eta_s_sd <- sd(fit_eq_site_fixed$fitted.values)

    unique_site_len <- length(unique(data_relevel$site_id))
    unique_eq_len <- length(unique(data_relevel$earthquake_id))

    matrix_id <- cbind(as.numeric(data_relevel$earthquake_id),
                       as.numeric(data_relevel$site_id))

    data_list <- list(N = nrow(data_relevel), M = 2, N_e = unique_eq_len, N_s = unique_site_len,
                      eta_e_sd = eta_e_sd, eta_s_sd = eta_s_sd,
                      tot_res = data_relevel$r,
                      mat_ids = matrix_id)
  }
  fit <- mod$sample(data = data_list, seed = 123, chains = 4, parallel_chains = 4)
  eta_site_fitted <- data_relevel %>% select(eta_site, site_id) %>%
    unique() %>% arrange(site_id) %>%
    mutate(eta_site_fitted =
             fit$summary()$mean[str_which(fit$summary()$variable, "eta_s")])

  return(eta_site_fitted)
}


