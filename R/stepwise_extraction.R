
#' Estimate site terms by removing the earthquake effects, and path effects
#' if path is provided, using either fixed effect or random effect models.
#'
#' @param data A data frame used for estimating site effect.
#' @param random A logical variable. If random == TRUE, fixed effect models are used;
#' if random == FALSE, random effect models are used.
#' @param path A logical variable. If path == TRUE, path effect will be removed
#' in estimation of site effect.
#'
#' @return A data frame that includes site ID, and the corresponding
#' true site terms and estimated site terms.
#' @import tidyverse dplyr lme4
#' @export
#'
#' @examples
#' data(gm_data)
#' eta_site_fitted <- stepwise_extraction(gm_data, random = FALSE, path = FALSE)
stepwise_extraction <- function(data, random = FALSE, path = FALSE){
  # TODO: convert all tidyverse syntax to base R syntax
  # TODO: return earthquake and path terms
  if(random == FALSE & path == FALSE){

    # 1. fixed effect, remove stepwise
    fit_eq_fixed <- lm(r ~ factor(earthquake_id) - 1, data = data)
    data_fixed <- data %>% mutate(r_eq_rm = r - fitted(fit_eq_fixed))
    fit_eq_site_fixed <- lm(r_eq_rm ~ factor(site_id) - 1, data = data_fixed)
    eta_site_fitted <- data_fixed %>% select(eta_site, site_id) %>% unique() %>%
      arrange(site_id) %>%
      mutate(eta_site_fitted = coefficients(fit_eq_site_fixed))


  }else if(random == FALSE & path == TRUE){

    # 2. fixed effect, remove stepwise including path effect
    fit_eq_fixed_path <- lm(r ~ factor(earthquake_id) - 1, data = data)
    data_fixed_path <- data %>% mutate(r_eq_rm = r - fitted(fit_eq_fixed_path))
    fit_path_fixed <- lm(r_eq_rm ~ factor(path_id) - 1, data = data_fixed_path)
    data_fixed_path <- data_fixed_path %>%
      mutate(r_eq_path_rm = r_eq_rm - fitted(fit_path_fixed))
    fit_eq_site_fixed_path <- lm(r_eq_path_rm ~ factor(site_id) - 1,
                                 data = data_fixed_path)
    eta_site_fitted <- data_fixed_path %>% select(eta_site, site_id) %>%
      unique() %>% arrange(site_id) %>%
      mutate(eta_site_fitted = coefficients(fit_eq_site_fixed_path))

  }else if(random == TRUE & path == FALSE){

    # 3. random effect, remove stepwise
    fit_eq_random <- lmer(r ~ 0 + (1|earthquake_id), data = data)
    data_random <- data %>% mutate(r_eq_rm = as.numeric(r - fitted(fit_eq_random)))
    fit_eq_site_random <- lmer(r_eq_rm ~ 0 + (1|site_id), data = data_random)
    fitted_eta_site_random <- coefficients(fit_eq_site_random)$site_id$`(Intercept)`
    eta_site_fitted <- data_random %>% select(eta_site, site_id) %>% unique() %>%
      arrange(site_id) %>%
      mutate(eta_site_fitted =  fitted_eta_site_random)

  }else{

    # 4. random effect, remove stepwise including path effect
    fit_eq_random_path <- lmer(r ~ 0 + (1|earthquake_id), data = data)
    data_random_path <- data %>% mutate(r_eq_rm = as.numeric(r - fitted(fit_eq_random_path)))
    fit_path_random <- lmer(r_eq_rm ~ 0 + (1|path_id), data = data_random_path)
    data_random_path <- data_random_path %>%
      mutate(r_eq_path_rm = as.numeric(r_eq_rm - fitted(fit_path_random)))
    fit_eq_site_random_path <- lmer(r_eq_path_rm ~ 0 + (1|site_id),
                                    data = data_random_path)
    fitted_eta_site_random_path <- coefficients(fit_eq_site_random_path)$site_id$`(Intercept)`
    eta_site_fitted <- data_random_path %>% select(eta_site, site_id) %>%
      unique() %>% arrange(site_id) %>%
      mutate(eta_site_fitted = fitted_eta_site_random_path)

  }
  return(eta_site_fitted)
}
