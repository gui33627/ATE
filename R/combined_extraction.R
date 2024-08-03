

#' Estimate site terms by a combination of fixed effect and random effect models.
#'
#' @param data A data frame used for estimating site effect.
#' @param path A logical variable. If path == TRUE, path effect will be
#' controlled in estimation of site effect.
#' @param path_method A character string specifying the method to be used.
#' Options are "stepwise", "random", "fixed".
#'
#' @return A data frame that includes site ID, and the corresponding
#' true site terms and estimated site terms.
#' @import tidyverse dplyr lme4 stringr
#' @export
#'
#' @examples
#' data(gm_data)
#' combined_extraction(data = gm_data, path = TRUE, path_method = "random")
combined_extraction <- function(data, path = FALSE,
                                path_method = c("stepwise", "random", "fixed")){

  if(path == FALSE){
    # 5. eta_s fixed effect and eta_eq is random effect
    fit_re <- lmer(r ~ 0 + site_id + (1|earthquake_id), data = data)
    eta_site_fitted <- data %>% select(eta_site, site_id) %>% unique() %>%
      arrange(site_id) %>% mutate(eta_site_fitted = fixef(fit_re))
  }else{
    if(path_method == "stepwise"){
      # 6. eta_s fixed effect and eta_eq is random effect, plus path_id removed stepwise
      fit_re_path <- lmer(r ~ 0 + path_id + (1|earthquake_id) + (1|site_id),
                          data = data)
      data_re_path <- data %>% mutate(r_path = r - fitted(fit_re_path))
      fit_re_path_site <- lmer(r_path ~ 0 + site_id + (1|earthquake_id),
                               data = data_re_path)
      eta_site_fitted <- data_re_path %>%
        select(eta_site, site_id) %>% unique() %>% arrange(site_id) %>%
        mutate(eta_site_fitted = fixef(fit_re_path_site))
    }else if(path_method == "random"){
      # 7. eta_s fixed effect and eta_eq is random effect, plus path_id as a random effect
      fit_re_path <- lmer(r ~ 0 + site_id + (1|path_id) + (1|earthquake_id),
                           data = data)
      eta_site_fitted <- data %>% select(eta_site, site_id) %>%
        unique() %>% arrange(site_id) %>%
        mutate(eta_site_fitted = fixef(fit_re_path))
    }else if(path_method == "fixed"){
      # 8. eta_s fixed effect and eta_eq is random effect, plus path_id as a fixed effect
      fit_re_path <- lmer(r ~ 0 + site_id + path_id + (1|earthquake_id),
                           data = data)
      eta_site_fitted <- data %>% select(eta_site, site_id) %>%
        unique() %>% arrange(site_id) %>%
        mutate(eta_site_fitted =
                 fixef(fit_re_path)[str_which(names(fixef(fit_re_path)),
                                               "site")])
    }
  }
  return(eta_site_fitted)
}
