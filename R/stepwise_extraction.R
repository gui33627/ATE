
#' Stepwise Extraction of Site Terms
#'
#' The function estimate site terms by removing the event terms (and path terms) from total ground motion residuals in a stepwise manner.
#'
#' @param data A data frame containing the input data for estimating site terms.
#' The data frame must include columns `event_id`, `site_id`, `r` (ground motion residuals), and optionally `path_id` if path effects are included.
#' @param random A logical variable indicating whether to use random effect models.
#' - `random = FALSE`(default) uses fixed effect models.
#' - `random = TRUE` uses random effect models (based on `lme4::lmer()`).
#' @param path A logical variable indicating whether to account for path effects.
#' - `path = FALSE`(default) estimates site terms without considering path effects.
#' - `path = TRUE` removes path terms before estimating site terms.
#'
#' @return  A data frame containing site IDs (`site_id`) and their corresponding estimated site terms (`delta_S2S`).
#' @import tidyverse dplyr lme4
#' @export
#'
#' @examples
#' data(gm_data)
#' delta_S2S <- stepwise_extraction(gm_data, random = FALSE)
stepwise_extraction <- function(data, random = FALSE, path = FALSE){
  # TODO: convert all tidyverse syntax to base R syntax
  # TODO: return earthquake and path terms
  if(random == FALSE & path == FALSE){

    # 1. fixed effect, remove stepwise
    fit_event_fixed <- lm(r ~ factor(event_id) - 1, data = data)
    data_fixed <- data %>% mutate(r_event_rm = r - fitted(fit_event_fixed))
    fit_event_site_fixed <- lm(r_event_rm ~ factor(site_id) - 1, data = data_fixed)
    delta_S2S <- data_fixed %>% select(site_id) %>% unique() %>%
      arrange(site_id) %>%
      mutate(delta_S2S = coefficients(fit_event_site_fixed))


  }else if(random == FALSE & path == TRUE){

    # 2. fixed effect, remove stepwise including path effect
    fit_event_fixed_path <- lm(r ~ factor(event_id) - 1, data = data)
    data_fixed_path <- data %>% mutate(r_event_rm = r - fitted(fit_event_fixed_path))
    fit_path_fixed <- lm(r_event_rm ~ factor(path_id) - 1, data = data_fixed_path)
    data_fixed_path <- data_fixed_path %>%
      mutate(r_event_path_rm = r_event_rm - fitted(fit_path_fixed))
    fit_event_site_fixed_path <- lm(r_event_path_rm ~ factor(site_id) - 1,
                                 data = data_fixed_path)
    delta_S2S <- data_fixed_path %>% select(site_id) %>%
      unique() %>% arrange(site_id) %>%
      mutate(delta_S2S = coefficients(fit_event_site_fixed_path))

  }else if(random == TRUE & path == FALSE){

    # 3. random effect, remove stepwise
    fit_event_random <- lmer(r ~ 0 + (1|event_id), data = data)
    data_random <- data %>% mutate(r_event_rm = as.numeric(r - fitted(fit_event_random)))
    fit_event_site_random <- lmer(r_event_rm ~ 0 + (1|site_id), data = data_random)
    fitted_delta_S2S_random <- coefficients(fit_event_site_random)$site_id$`(Intercept)`
    delta_S2S <- data_random %>% select(site_id) %>% unique() %>%
      arrange(site_id) %>%
      mutate(delta_S2S =  fitted_delta_S2S_random)

  }else{

    # 4. random effect, remove stepwise including path effect
    fit_event_random_path <- lmer(r ~ 0 + (1|event_id), data = data)
    data_random_path <- data %>% mutate(r_event_rm = as.numeric(r - fitted(fit_event_random_path)))
    fit_path_random <- lmer(r_event_rm ~ 0 + (1|path_id), data = data_random_path)
    data_random_path <- data_random_path %>%
      mutate(r_event_path_rm = as.numeric(r_event_rm - fitted(fit_path_random)))
    fit_event_site_random_path <- lmer(r_event_path_rm ~ 0 + (1|site_id),
                                    data = data_random_path)
    fitted_delta_S2S_random_path <- coefficients(fit_event_site_random_path)$site_id$`(Intercept)`
    delta_S2S <- data_random_path %>% select(site_id) %>%
      unique() %>% arrange(site_id) %>%
      mutate(delta_S2S = fitted_delta_S2S_random_path)

  }
  return(delta_S2S)
}
