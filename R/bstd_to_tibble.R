#' APA: calculate standardized beta estimates for a model
#'
#' @param x REQUIRED: a models (lm or glm), bare name
#' @param d Optional: Number. Digits after the decimal place
#'
#' @returns a data.frame for use with apaSupp's model tables
#' @import parameters
#' @import tidyverse
#' @export
#'
#' @examples
#'
#' fit_lm1 <- lm(mpg ~ wt + hp + I(factor(cyl)), data = mtcars)
#'
#' bstd_to_tibble(fit_lm1)
#'
bstd_to_tibble <- function(x, d = 2) {

  result <- x %>%
    parameters::standardise_parameters() %>%
    as.data.frame() %>%
    dplyr::filter(Parameter != "(Intercept)") %>%
    dplyr::select("variable" = "Parameter",
                  "bs" = "Std_Coefficient") %>%
    dplyr::mutate(row_type = "label")%>%
    dplyr::mutate(across(c(bs),
                         ~ MOTE::apa(value = .,
                                     decimals = d,
                                     leading = TRUE)))

  return(result)
}
