#' APA: flextable for a GLM models (specifically logisitc right now)
#'
#' @param x REQUIRED: a glm models, bare name
#' @param var_labels Optional: Vector. Text replacements for model terms, "old" = "new"
#' @param type Optional: default = "logistic", more to come soon...
#' @param caption Optional: Text. Caption for the table
#' @param p_note Optional: Text. (default = NULL) Significance note for APA table, If p_note = "apa" then the standard "* p < .05. ** p < .01. *** p < .001." will be used
#' @param general_note Optional: Text. General note for footer of APA table
#' @param fit Optional: Text. fit statistics: (default = NA) "nobs", null.deviance", "df.null", "deviance", "df.residual", "logLik", "AIC", "BIC"
#' @param pr2 Optional: character vector.  (default = TRUE) include Tjur's pseudo R-squared
#' @param d Optional: Number. Digits after the decimal place
#' @param vif Optional: Logical. (default = TRUE) Include variance inflation factors
#' @param lrt Optional: Logical. (default = TRUE) Include LRT for single-predictor deletion
#'
#' @returns a flextable object
#' @import gtsummary
#' @import flextable
#' @import tidyverse
#' @import broom.helpers
#' @export
#'
#' @examples
#'
#'library(tidyverse)
#'
#' mtcars <- mtcars %>% dplyr::mutate(am = factor(am))
#'
#' fit_glm1 <- glm(vs ~ am, data = mtcars, family = "binomial")
#' fit_glm2 <- glm(vs ~ wt + mpg + am, data = mtcars, family = "binomial")
#'
#' apaSupp::tab_glm(fit_glm1)
#' apaSupp::tab_glm(fit_glm2)
#' apaSupp::tab_glm(fit_glm2, vif = FALSE)
#' apaSupp::tab_glm(fit_glm2, lrt = FALSE)
#' apaSupp::tab_glm(fit_glm2, vif = FALSE, lrt = FALSE)
#' apaSupp::tab_glm(fit_glm2, vif = FALSE, lrt = FALSE, pr2 = FALSE)
#'
tab_glm <- function(x,
                    var_labels = NULL,
                    caption = "Generalized Regression Model",
                    p_note = "apa",
                    general_note = NA,
                    fit = NULL,
                    pr2 = TRUE,
                    d = 2,
                    vif = TRUE,
                    lrt = TRUE){

  n_obs   <- length(x$resid)
  n_param <- length(coef(x))
  n_fit   <- sum(!is.na(fit), pr2)

  if (n_param <= 2) {
    vif <- FALSE
    lrt <- FALSE
  }


  if (family(x)$link == "logit"){
    back_trans <- "exp"
    abr <- c("Odds Ratio","Logit Scale")
    sym <- c("OR", "b")
  }



  main_note <- flextable::as_paragraph(
    flextable::as_i("Note. "),
    flextable::as_chunk(glue::glue("N = {n_obs}. ")),
    flextable::as_chunk(ifelse(vif == TRUE,
                               "CI = confidence interval; VIF = variance inflation factor. ",
                               "CI = confidence interval. ")),
    flextable::as_chunk(ifelse(lrt == TRUE,
                               "Significance denotes Wald t-tests for individual parameter estimates, as well as Likelihood Ratio Tests (LRT) for single-predictor deletion. ",
                               "Significance denotes Wald t-tests for parameter estimates. ")),
    flextable::as_chunk(general_note)
  )



  if (is.null(p_note)){
    p_note <- NULL
  } else if (p_note == "apa"){
    p_note <- "* p < .05. ** p < .01. *** p < .001."
  } else {
    p_note <- p_note
  }


  if (back_trans == "exp"){
    get_tran <- x %>%
      gtsummary::tbl_regression(intercept = TRUE,
                                conf.int = TRUE,
                                exponentiate = TRUE,
                                tidy_fun = broom.helpers::tidy_with_broom_or_parameters) %>%
      gtsummary::modify_column_hide(column = std.error) %>%
      gtsummary::modify_column_hide(column = p.value) %>%
      gtsummary::modify_fmt_fun(estimate ~
                                  gtsummary::label_style_number(digits = d)) %>%
      gtsummary::modify_fmt_fun(conf.low ~
                                  gtsummary::label_style_number(digits = d,
                                                                prefix = "[")) %>%
      gtsummary::modify_fmt_fun(conf.high ~
                                  gtsummary::label_style_number(digits = d,
                                                                suffix = "]")) %>%
      gtsummary::remove_abbreviation("OR = Odds Ratio") %>%
      gtsummary::remove_abbreviation("CI = Confidence Interval") %>%
      gtsummary::remove_abbreviation("SE = Standard Error")  %>%
      gtsummary::modify_table_body(~.x %>%
                                     dplyr::mutate(estimate = ifelse(variable == "(Intercept)", NA, estimate)) %>%
                                     dplyr::mutate(conf.low = ifelse(variable == "(Intercept)", NA, conf.low))) %>%
      gtsummary::modify_table_body(~.x %>%
                                     dplyr::mutate(bk = NA)) %>%
      gtsummary::modify_header(label = "Variable",
                               estimate = sym[1],
                               conf.low = "95% CI",
                               bk = "blank")
  }

  if (!is.null(fit)){
    get_tran <- get_tran %>%
    gtsummary::add_glance_table(include = fit)
  }


  get <- x %>%
    gtsummary::tbl_regression(intercept = TRUE,
                              conf.int = FALSE,
                              exponentiate = FALSE,
                              pvalue_fun = function(x) apaSupp::p_num(x, d = d + 1),
                              tidy_fun = broom.helpers::tidy_with_broom_or_parameters) %>%
    gtsummary::modify_column_unhide(column = std.error) %>%
    gtsummary::remove_abbreviation("OR = Odds Ratio") %>%
    gtsummary::remove_abbreviation("SE = Standard Error")  %>%
    gtsummary::remove_footnote_header() %>%
    gtsummary::modify_fmt_fun(estimate ~
                                gtsummary::label_style_number(digits = d)) %>%
    gtsummary::modify_fmt_fun(std.error ~
                                gtsummary::label_style_number(digits = d,
                                                              prefix = "(",
                                                              suffix = ")"))




  if (lrt == TRUE){
    get <- get %>%
      gtsummary::modify_table_body(dplyr::left_join,
                                   apaSupp::pdr1_to_tibble(x, test = "Chisq", d = d),
                                   by = c("variable", "row_type")) %>%
      gtsummary::modify_header(label = "Variable",
                               estimate = sym[2],
                               std.error = "(SE)",
                               p.value = "Wald",
                               p_dr1 = "LRT") %>%
      gtsummary::modify_fmt_fun(p_dr1 ~ function(x) apaSupp::p_num(x, d = d + 1))

  } else {
    get <- get %>%
      gtsummary::modify_header(label = "Variable",
                               estimate = sym[2],
                               std.error = "(SE)",
                               p.value = "p")
  }

  get_orig <- get %>%
    gtsummary::modify_column_hide(column = starts_with("p")) %>%
    gtsummary::modify_table_body(~.x %>%
                                   dplyr::mutate(bk = NA)) %>%
    gtsummary::modify_header(label = "Variable",
                             estimate = sym[2],
                             conf.low = "95% CI",
                             bk = "blank")

  get_sig <- get %>%
    gtsummary::modify_column_hide(column = estimate) %>%
    gtsummary::modify_column_hide(column = std.error)


  if (vif == TRUE){
    get_vif <- get %>%
      gtsummary::modify_column_hide(column = estimate) %>%
      gtsummary::modify_column_hide(column = std.error)  %>%
      gtsummary::modify_column_hide(column = starts_with("p")) %>%
      gtsummary::modify_table_body(dplyr::left_join,
                                   apaSupp::vif_to_tibble(x, d = d),
                                   by = c("variable", "row_type")) %>%
      gtsummary::modify_header(vif ~ "VIF")

    table <- list(get_tran, get_orig, get_sig, get_vif) %>%
      gtsummary::tbl_merge(tab_spanner = c(abr[1], abr[2], "p", NA)) %>%
      gtsummary::as_flex_table() %>%
      flextable::compose(part = "all", j = c(4, 7),
                         value = flextable::as_paragraph(NA))

  } else {
    table <- list(get_tran,  get_orig,  get_sig) %>%
      gtsummary::tbl_merge(tab_spanner = c(abr[1],  abr[2],  "p")) %>%
      gtsummary::as_flex_table() %>%
      flextable::compose(part = "header", j = c(4, 7),
                         value = flextable::as_paragraph(NA)) %>%
      flextable::compose(par = "body", j = c(4, 7),
                         value = flextable::as_paragraph(NA))
  }

  n_col <- flextable::ncol_keys(table)
  n_rows <- flextable::nrow_part(table, part = "body")

  if (pr2 == TRUE){

    r2 <- performance::r2(x) %>%
      unlist(use.names = FALSE) %>%
      p_num(d = d + 1, stars = FALSE)

    table <- table %>%
      flextable::add_body_row(top = FALSE,
                              values = NA) %>%
      flextable::compose(part = "body", i = (n_rows + 1), j = 1,
                         value = flextable::as_paragraph("Tjur's R",
                                                         flextable::as_chunk("\u00B2"))) %>%
      flextable::compose(part = "body", i = (n_rows + 1), j = 2,
                         value = flextable::as_paragraph(r2))
  }

  n_col <- flextable::ncol_keys(table)
  n_rows <- flextable::nrow_part(table, part = "body")

  table <- table  %>%
    flextable::delete_rows(part = "header", i = 1) %>%
    flextable::add_header_row(values = c(NA, abr[1], NA, abr[2], rep(NA, n_col - 6)),
                              colwidths = c(1, 2, 1, 2, rep(1, n_col - 6))) %>%
    apaSupp::theme_apa(caption = caption,
                       no_notes = FALSE,
                       d = d) %>%
    flextable::italic(part = "header", i = 2, j = 4:5) %>%
    flextable::align(part = "all", j = c(2, 5), align = "right") %>%
    flextable::align(part = "all", j = c(3, 6), align = "left") %>%
    flextable::align(part = "header", i = 1, align = "center") %>%
    flextable::align(part = "footer", align = "left") %>%
    flextable::hline(part = "header", i = 1,
                     border = flextable::fp_border_default(width = 0)) %>%
    flextable::add_footer_lines("") %>%
    flextable::compose(i = 1, j = 1,
                       value = main_note,
                       part = "footer") %>%
    flextable::hline(part = "header", i = 1, j = 2:3) %>%
    flextable::hline(part = "header", i = 1, j = 5:6)

  if (lrt == TRUE){
    table <- table %>%
      flextable::align(part = "all", j = c(8), align = "right") %>%
      flextable::align(part = "all", j = c(9), align = "left") %>%
      flextable::align(part = "header", i = 1, align = "center") %>%
      flextable::merge_at(part = "header", i = 1, j = 8:9) %>%
      flextable::compose(part = "header", i = 1, j = 8,
                         value = flextable::as_paragraph(flextable::as_i("p"))) %>%
      flextable::hline(part = "header", i = 1, j = 8:9)
  } else {
    table <- table %>%
      flextable::compose(part = "header", i = 1, j = 8,
                         value = flextable::as_paragraph(NA)) %>%
      flextable::compose(part = "header", i = 2, j = 8,
                         value = flextable::as_paragraph(flextable::as_i("p")))

  }


  n_rows <- flextable::nrow_part(table, part = "body")

  if (n_fit > 0) {
    table <- table %>%
  flextable::italic(part = "body", i = (n_rows - n_fit + 1):(n_rows)) %>%
    flextable::hline(i = n_rows - n_fit)
  }

  if(!is.null(var_labels)){
    table <- table %>%
      flextable::labelizor(part = "body",
                           labels = var_labels)
  }

  if (!is.null(p_note)){
    table <- table %>%
      flextable::add_footer_lines("") %>%
      flextable::compose(i = 2, j = 1,
                         value = flextable::as_paragraph(flextable::as_chunk(p_note)),
                         part = "footer")
  }

  return(table)
}
