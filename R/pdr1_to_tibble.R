#' APA: calculate significance for single term deletion
#'
#' @param x REQUIRED: a models (lm or glm), bare name
#' @param test Optional: Character.  Default = "Chisq", which is an exact test (`lm` models with known scale) or a likelihood-ratio test or a test of the reduction in scaled deviance `glm` models, see help for `drop1()`
#' @param d REQUIRED: number of digits after the decimal for rounding
#' @param k REQUIRED: the penalty constant in AIC/C_p.  see help for `drop1()`
#'
#' @returns a data.frame for use with apaSupp's model tables
#' @import tidyverse
#' @export
#'
#' @examples
#'
#' library(tidyverse)
#'
#' mtcars <- mtcars %>% dplyr::mutate(am = factor(am))
#'
#' fit_lm1 <- lm(mpg ~ wt + hp + am, data = mtcars)
#' fit_glm2 <- glm(vs ~ wt + mpg + am, data = mtcars, family = "binomial")
#'
#' pdr1_to_tibble(fit_lm1)
#' pdr1_to_tibble(fit_glm2)
#'
pdr1_to_tibble <- function(x,
                           test = "Chisq",
                           d = 2,
                           k = 2) {

  result <- drop1(x, test = test, k = k)  %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "variable") %>%
    tibble::as_tibble() %>%
    dplyr::select(variable, p_dr1 = `Pr(>Chi)`) %>%
    dplyr::filter(!is.na(p_dr1)) %>%
    dplyr::mutate(row_type = "label")

  return(result)
}
