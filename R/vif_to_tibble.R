#' APA: calculate VIF
#'
#' @param x REQUIRED: a models (lm or glm), bare name
#' @param d Optional: Number. Digits after the decimal place
#'
#' @returns a data.frame for use with apaSupp's model tables
#' @import car
#' @import tidyverse
#' @export
#'
#' @examples
#'
#' library(tidyverse)
#'
#' data(mtcars)
#'
#' mtcars <- mtcars %>%
#'   dplyr::mutate(vs = factor(vs,
#'                             levels = 0:1,
#'                             labels = c("V Shaped", "Straight"))) %>%
#'   dplyr::mutate(am = factor(am,
#'                             levels = 0:1,
#'                             labels = c("Automatic", "Manual"))) %>%
#'   dplyr::mutate(cyl  = factor(cyl)) %>%
#'   dplyr::mutate(gear = factor(gear))
#'
#'
#' fit_lm1 <- lm(mpg ~ wt + hp + cyl, data = mtcars)
#'
#' vif_to_tibble(fit_lm1)
#'
vif_to_tibble <- function(x, d = 2) {

  vif <- car::vif(x)

  if (!is.matrix(vif)){
    result <- vif %>%
      tibble::enframe("variable", "vif")
  } else {
    result <- vif %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "variable") %>%
      tibble::as_tibble() %>%
      dplyr::select(variable, vif = GVIF)
  }

  result <- result %>%
    dplyr::mutate(row_type = "label")%>%
    dplyr::mutate(vif = MOTE::apa(value = vif,
                                  decimals = d,
                                  leading = TRUE))

  return(result)
}
