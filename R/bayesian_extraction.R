
#' Bayesian Estimate of Site Terms
#'
#' The function estimates site terms in ground motion models using a Bayesian framework.
#'
#' @param data A data frame containing the input data for estimating site terms.
#' The data frame must include `event_id`, `site_id`, `r` (ground motion residuals), and optionally `path_id` if path effects are considered.
#' @param path A logical variable indicating whether to control for path effects.
#' If `FALSE` (default), the estimation is performed without path effects. If `path = TRUE`, path effects are included in the model.
#'
#' @return A data frame containing the site IDs (`site_id`) and their corresponding estimated site terms (`delta_S2S`).
#' @import tidyverse dplyr stringr cmdstanr
#' @importFrom stats coefficients fitted lm rnorm sd setNames
#' @export
#'
#' @examples
#' data(gm_data)
#' bayesian_extraction(data = gm_data)
bayesian_extraction <- function(data, path = FALSE){

  if(path == TRUE){

    stan_file <- system.file("stan", "terms_extraction.stan", package = "ATE")
    mod <- cmdstan_model(stan_file)

    fit_event_fixed_path <- lm(r ~ factor(event_id) - 1, data = data)
    data_fixed_path <- data %>% mutate(r_event_rm = r - fitted(fit_event_fixed_path))
    fit_path_fixed <- lm(r_event_rm ~ factor(path_id) - 1, data = data_fixed_path)
    data_fixed_path <- data_fixed_path %>% mutate(r_event_path_rm = r_event_rm - fitted(fit_path_fixed))
    fit_event_site_fixed_path <- lm(r_event_path_rm ~ factor(site_id) - 1, data = data_fixed_path)

    data_relevel <- data
    data_relevel$event_id <- remap_elements_and_levels(data$event_id)
    data_relevel$site_id <- remap_elements_and_levels(data$site_id)
    data_relevel$path_id <- remap_elements_and_levels(data$path_id)

    tau <- sd(fit_event_fixed_path$fitted.values)
    phi_P2P <- sd(fit_path_fixed$fitted.values)
    phi_S2S <- sd(fit_event_site_fixed_path$fitted.values)

    unique_site_len <- length(unique(data_relevel$site_id))
    unique_event_len <- length(unique(data_relevel$event_id))
    unique_path_len <- length(unique(data_relevel$path_id))

    matrix_id <- cbind(as.numeric(data_relevel$event_id),
                       as.numeric(data_relevel$site_id),
                       as.numeric(data_relevel$path_id))

    data_list <- list(N = nrow(data_relevel), M = 3, N_e = unique_event_len, N_s = unique_site_len,
                      N_p = unique_path_len,
                      tau = tau, phi_S2S = phi_S2S, phi_P2P = phi_P2P,
                      r = data_relevel$r,
                      mat_ids = matrix_id)

  }else{
    stan_file <- system.file("stan", "terms_extraction_nopath.stan", package = "ATE")
    mod <- cmdstan_model(stan_file)

    fit_event_fixed <- lm(r ~ factor(event_id) - 1, data = data)
    data_fixed <- data %>% mutate(r_event_rm = r - fitted(fit_event_fixed))
    fit_event_site_fixed <- lm(r_event_rm ~ factor(site_id) - 1, data = data_fixed)

    data_relevel <- data
    data_relevel$event_id <- remap_elements_and_levels(data$event_id)
    data_relevel$site_id <- remap_elements_and_levels(data$site_id)

    tau <- sd(fit_event_fixed$fitted.values)
    phi_S2S <- sd(fit_event_site_fixed$fitted.values)

    unique_site_len <- length(unique(data_relevel$site_id))
    unique_event_len <- length(unique(data_relevel$event_id))

    matrix_id <- cbind(as.numeric(data_relevel$event_id),
                       as.numeric(data_relevel$site_id))

    data_list <- list(N = nrow(data_relevel), M = 2, N_e = unique_event_len, N_s = unique_site_len,
                      tau = tau, phi_S2S = phi_S2S,
                      r = data_relevel$r,
                      mat_ids = matrix_id)
  }
  fit <- mod$sample(data = data_list, seed = 123, chains = 4, parallel_chains = 4)
  delta_S2S <- data_relevel %>% select(site_id) %>%
    unique() %>% arrange(site_id) %>%
    mutate(delta_S2S =
             fit$summary()$mean[str_which(fit$summary()$variable, "delta_S2S")])

  return(delta_S2S)
}


