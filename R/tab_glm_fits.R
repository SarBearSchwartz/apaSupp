#' @title
#' APA: flextable for Comparing the Performance of Generalize Linear models
#'
#' @description
#' Create a flextable for Comparing the Performance of Linear models via Several Metrics
#'
#'
#' @param x REQUIRED: List. at least 2 glm models, bare names, If named list, then names appear in the table
#' @param caption Optional: Text. Caption for the table
#' @param docx Optional: filename. must end with ".docx"
#' @param general_note Optional: Text. General note for footer of APA table
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
#' library(tidyverse)
#'
#' mtcars <- mtcars %>% dplyr::mutate(cyl = factor(cyl))
#'
#' fit_glm1 <- glm(vs ~ wt, data = mtcars, family = "binomial")
#' fit_glm2 <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#'
#' apaSupp::tab_glm_fits(list(fit_glm1, fit_glm2))
#' apaSupp::tab_glm_fits(list("Univar" = fit_glm1, "Multivar" = fit_glm2))
#'
tab_glm_fits <- function(x,
                         caption      = "Comparison of Generalized Linear Model Performane Metrics",
                         docx         = NA,
                         general_note = NA,
                         d            = 2){

  ns <- sapply(x,function(y) length(y$residuals))

  nparams <- sapply(x,function(y) length(y$coefficients))

  if (family(x[[1]])$link == "logit"){
    R2_McF = sapply(x, performance::r2_mcfadden)
  }

  main_note <- flextable::as_paragraph(
    flextable::as_i("Note. "),
    flextable::as_chunk(ifelse(length(unique(ns)) == 1, NA, "Models fit to different samples. ")),
    flextable::as_i("k"), " = number of parameters estimated in each model. ",
    "Larger ", flextable::as_equation("R^2")," values indicated better performance. ",
    "Smaller values indicated better performance for Akaike's Information Criteria (AIC), Bayesian information criteria (BIC), and Root Mean Squared Error (RMSE).",
    flextable::as_chunk(general_note)
  )


  table <- performance::compare_performance(x) %>%
    data.frame() %>%
    dplyr::mutate(N = ns) %>%
    dplyr::mutate(k = nparams) %>%
    dplyr::mutate(McFadden = unlist(R2_McF[1,], use.names = FALSE)) %>%
    dplyr::select(Model = Name,
                  N, k, McFadden,
                  Tjur = R2_Tjur,
                  AIC, BIC, RMSE) %>%
    dplyr::mutate(across(c(McFadden, Tjur),  ~ apaSupp::p_num(., d = d + 1, stars = FALSE))) %>%
    flextable::flextable() %>%
    apaSupp::theme_apa(caption   = caption,
                       main_note = main_note,
                       d         = d) %>%
    flextable::add_header_row(values = c(NA, "R2",NA), colwidths = c(3, 2, 3)) %>%
    flextable::colformat_double(j = c("N", "k"),             big.mark = "", digits = 0) %>%
    flextable::colformat_double(j = c("AIC", "BIC", "RMSE"), big.mark = "", digits = d) %>%
    flextable::hline(  part = "header", i = 1,        border = flextable::fp_border_default(width = 0)) %>%
    flextable::compose(part = "header", i = 1, j = 4, value  = flextable::as_paragraph("pseudo-", flextable::as_equation("R^2"))) %>%
    flextable::hline(  part = "header", i = 1, j = 4:5) %>%
    flextable::italic( part = "header", i = 2, j = c(2:3)) %>%
    flextable::align(  part = "all",    j = c("N", "McFadden","AIC"), align = "right") %>%
    flextable::align(  part = "all",    j = c("k", "Tjur", "BIC"),    align = "left") %>%
    flextable::align(  part = "header", i = 1,                        align = "center") %>%
    flextable::autofit()

  if (!is.na(docx)){
    flextable::save_as_docx(table,
                            path = docx)
  }

  return(table)
}


