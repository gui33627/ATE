

#' Combined Extraction of Site Terms Using Fixed and Random Effect Models
#'
#' The function estimate site terms by combining fixed effect and random effect models.
#'
#' @param data A data frame containing the input data for estimating site terms.
#' The data frame must include `site_id`, `event_id`, `r` (ground motion residuals), and optionally `path_id` if path effects are included.
#' @param path A logical variable indicating whether to control for path effects in the estimation.
#' - `path = FALSE` (default): Path effects are not considered.
#' - `path = TRUE`: Path effects are included, and the modeling strategy is determined by the `path_method` parameter.
#' @param path_method A character string specifying the method to control for path effects.
#' Options are `"stepwise"`, `"random"`, or `"fixed"`. This parameter is only required if `path = TRUE`.
#' - **"stepwise"**: Removes path effects stepwise.
#' - **"random"**: Treats path effects as random effects.
#' - **"fixed"**: Treats path effects as fixed effects.
#'
#' @return A data frame containing the site IDs (`site_id`) and their corresponding estimated site terms (`delta_S2S`).
#'
#' @import tidyverse dplyr lme4 stringr
#' @export
#'
#' @examples
#' data(gm_data)
#' mixed_extraction(data = gm_data)
mixed_extraction <- function(data, path = FALSE,
                                path_method = c("stepwise", "random", "fixed")){
  data$site_id <- as.factor(data$site_id)
  data$event_id <- as.factor(data$event_id)
  if(path == FALSE){

    # delta_S2S is fixed effect and delta_B is random effect
    fit_re <- lmer(r ~ 0 + site_id + (1|event_id), data = data)
    delta_S2S <- data %>% select(site_id) %>% unique() %>%
      arrange(site_id) %>% mutate(delta_S2S = fixef(fit_re))

  }else{
    if(path_method == "stepwise"){

      # delta_S2S is fixed effect and delta_B is random effect, plus delta_P2P removed stepwise
      fit_re_path <- lmer(r ~ 0 + path_id + (1|event_id) + (1|site_id),
                          data = data)
      data_re_path <- data %>% mutate(r_path = r - fitted(fit_re_path))
      fit_re_path_site <- lmer(r_path ~ 0 + site_id + (1|event_id),
                               data = data_re_path)
      delta_S2S <- data_re_path %>%
        select(site_id) %>% unique() %>% arrange(site_id) %>%
        mutate(delta_S2S = fixef(fit_re_path_site))

    }else if(path_method == "random"){

      # delta_S2S is fixed effect and delta_B is random effect, plus delta_P2P as a random effect
      fit_re_path <- lmer(r ~ 0 + site_id + (1|path_id) + (1|event_id),
                           data = data)
      delta_S2S <- data %>% select(site_id) %>%
        unique() %>% arrange(site_id) %>%
        mutate(delta_S2S = fixef(fit_re_path))

    }else if(path_method == "fixed"){

      # delta_S2S is fixed effect and delta_B is random effect, plus delta_P2P as a fixed effect
      fit_re_path <- lmer(r ~ 0 + site_id + path_id + (1|event_id),
                           data = data)
      delta_S2S <- data %>% select(site_id) %>%
        unique() %>% arrange(site_id) %>%
        mutate(delta_S2S =
                 fixef(fit_re_path)[str_which(names(fixef(fit_re_path)),
                                               "site")])
    }
  }
  return(delta_S2S)
}
