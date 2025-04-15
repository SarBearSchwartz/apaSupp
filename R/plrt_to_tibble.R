#' APA: calculate significance for single term deletion
#'
#' @param x REQUIRED: a models (lm or glm), bare name
#' @param test Optional: Character.  Default = "Chisq", which is an exact test (`lm` models with known scale) or a likelihood-ratio test or a test of the reduction in scaled deviance `glm` models, see help for `drop1()`
#' @param d Optional: Number. Digits after the decimal place
#'
#' @returns a data.frame for use with apaSupp's model tables
#' @import tidyverse
#' @export
#'
#' @examples
#'
#' fit_lm1 <- lm(mpg ~ wt + hp + I(factor(cyl)), data = mtcars)
#' fit_glm1 <-
#'
#' plrt_to_tibble(fit_lm1)
#'
plrt_to_tibble <- function(x,
                           test = "Chisq",
                           d = 2,
                           k = 2) {



  result <- drop1(x, test = test, k = k)  %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "variable") %>%
    tibble::as_tibble() %>%
    dplyr::select(variable, p_lrt = `Pr(>Chi)`) %>%
    dplyr::filter(!is.na(p_lrt)) %>%
    dplyr::mutate(row_type = "label")%>%
    dplyr::mutate(p_lrt = apaSupp::p_num(value   = p_lrt,
                                         d       = d + 1,
                                         leading = TRUE))

  return(result)
}
