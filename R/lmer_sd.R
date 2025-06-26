#' Estimate the SD for Effect Size for a MLM
#'
#' @param fit REQUIRED. Bare name of a model fit via lme4::lmer()
#' @param type REQUIRED. Text. Default = "pool", alternative is "resid"
#'
#' @returns list of 2 estimates of standard deviation
#' @import tidyverse
#' @import lme4
#' @export
#'
#' @examples
#'
#' library(tidyverse)
#' library(lme4)
#'
#' fit <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
#'
#' lmer_sd(fit)
#' lmer_sd(fit, type = "resid")
#'
lmer_sd <- function(fit, type = "pool"){

  sd_pool <- fit  %>%
    lme4::VarCorr() %>%
    data.frame() %>%
    dplyr::pull(vcov) %>%
    sum() %>%
    sqrt()

  sd_resid <- summary(fit)$sigma

  sd <- case_when(type == "pool"  ~ sd_pool,
                  type == "resid" ~ sd_resid)

  return(sd)
}
