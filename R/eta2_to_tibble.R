#' APA: calculate eta-squared effect sizes
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
#' mtcars <- mtcars %>% dplyr::mutate(cyl = factor(cyl))
#'
#' fit_lm1 <- lm(mpg ~ wt + hp + cyl, data = mtcars)
#'
#' eta2_to_tibble(fit_lm1)
#'
eta2_to_tibble <- function(x, d = 2) {

  eta2 <- DescTools::EtaSq(x)

  if (!is.matrix(eta2)){
    result <- eta2 %>%
      enframe("variable", "eta.sq", "eta.sq.part")
  } else {
    result <- eta2 %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "variable") %>%
      tibble::as_tibble()
  }

  result <- result %>%
    data.frame() %>%
    dplyr::mutate(row_type = "label")%>%
    dplyr::mutate(across(c(eta.sq, eta.sq.part),
                         ~ MOTE::apa(value = .,
                                     decimals = (d + 1),
                                     leading = FALSE)))

  return(result)
}
