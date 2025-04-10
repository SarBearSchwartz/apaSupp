#' @title
#' APA: flextable for Comparing the Performance of Linear models
#'
#' @description
#' Create a flextable for Comparing the Performance of Linear models via Several Metrics
#'
#'
#' @param x REQUIRED: List. at least 2 lm models, bare names, If named list, then names appear in the table
#' @param caption Optional: Text. Caption for the table
#' @param general_note Optional: Text. General note for footer of APA table
#' @param sort Optional: metrics to sort by, default = "AIC", but may use: "AIC", "BIC", "R2", "R2_adjusted", "RMSE"
#' @param d Optional: Number. Digits after the decimal place
#'
#' @returns a flextable object
#' @import gtsummary
#' @import flextable
#' @import tidyverse
#' @import broom.helpers
#' @import performance
#' @export
#'
#' @examples
#'
#' m1 <- lm(dist ~ 1, cars)
#' m2 <- lm(dist ~ speed, cars)
#'
#' apaSupp::tab_lm_fits(list(m1, m2))
#' apaSupp::tab_lm_fits(list("null" = m1, "main" = m2))
#'
tab_lm_fits <- function(x,
                         caption = "Comparison of Linear Model Performane Metrics",
                         general_note = NA,
                         sort = "AIC",
                         d = 2){

  ns <- sapply(x,function(y)length (y$residuals))
  nparams <- sapply(x,function(y) length(y$coefficients))

  if (length(unique(ns)) == 1){
    n <- unique(ns)
    note_sample <- glue::glue("N = {n}. ")
  } else {
    note_sample = "Models fit to different samples. "
  }

  final_note <- flextable::as_paragraph(flextable::as_i("Note. "),
                                        flextable::as_chunk(ifelse(length(unique(ns)) == 1,
                                                                   NA,
                                                                   "Models fit to different samples. ")),
                                        flextable::as_i("k"),
                                        " = number of parameters estimated in each model. ",
                                        "Larger values indicated better performance for R-squared values. Smaller values indicated better performance for Akaike's Information Criteria (AIC), Bayesian information criteria (BIC), and Root Mean Squared Error (RMSE).",
                                        flextable::as_chunk(general_note))

  df <- performance::compare_performance(x) %>%
    data.frame() %>%
    dplyr::mutate(N = ns) %>%
    dplyr::mutate(k = nparams) %>%
    dplyr::select(Model = Name,
                  N, k,
                  mult = R2,
                  adj = R2_adjusted,
                  AIC, BIC,
                  RMSE) %>%
    dplyr::arrange(sort) %>%
    dplyr::mutate(across(c(mult, adj),
                         ~ apaSupp::p_num(., d = d + 1, stars = FALSE)))



  tab <- df %>%
    flextable::flextable() %>%
    apaSupp::theme_apa(caption = caption,
                       p_note = NULL) %>%
    flextable::colformat_double(j = c("AIC", "BIC", "RMSE"), big.mark = "", digits = d) %>%
    flextable::align(part = "all", j = c("N", "mult","AIC"), align = "right") %>%
    flextable::align(part = "all", j = c("k", "adj", "BIC"), align = "left") %>%
    flextable::add_header_row(values = c(NA, "R2",NA),
                              colwidths = c(3, 2, 3)) %>%
    flextable::hline(part = "header", i = 1,
                     border = flextable::fp_border_default(width = 0)) %>%
    flextable::hline(part = "header", i = 1, j = 4:5) %>%
    flextable::italic(part = "header", i = 2, j = c(2:3)) %>%
    flextable::compose(part = "header", i = 1, j = 4,
                       value = flextable::as_paragraph(flextable::as_i(
                         flextable::as_chunk("R\u00B2")))) %>%
    flextable::align(part = "header", i = 1, align = "center") %>%
    flextable::add_footer_lines("") %>%
    flextable::compose(i = 1, j = 1,
                       value = final_note,
                       part = "footer")

  return(tab)
}


