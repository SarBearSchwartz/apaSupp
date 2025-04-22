#' APA: flextable for a GLM models (specifically logisitc right now)
#'
#' @param x REQUIRED: a glm models, bare name
#' @param var_labels Optional: Vector. Text replacements for model terms, "old" = "new"
#' @param caption Optional: Text. Caption for the table
#' @param general_note Optional: Text. General note for footer of APA table
#' @param p_note Optional: Text. (default = NULL) Significance note for APA table, If `p_note = "apa123"` then the standard `"* p < .05. ** p < .01. *** p < .001."` will be used
#' @param no_notes REQUIRED: Logical.  Defaults to `FALSE`, if `TRUE` will ignore `genderal_note` and `p_note`
#' @param d Optional: Number. Digits after the decimal place
#' @param fit Optional: Text. fit statistics: (default = NA) "nobs", null.deviance", "df.null", "deviance", "df.residual", "logLik", "AIC", "BIC"
#' @param pr2 Optional: character.  (default = "both") Include pseudo R-squared: "tjur", "mcfadden", "both", or "none" for logistic or "nagelkerke" for poisson
#' @param vif Optional: Logical. (default = TRUE) Include variance inflation factors
#' @param lrt Optional: Logical. (default = TRUE) Include LRT for single-predictor deletion
#' @param show_single_row	 a variable is dichotomous (e.g. Yes/No) and you wish to print the regression coefficient on a single row, include the variable name(s) here.
#' @param breaks Optional: numeric vector of p-value cut-points
#' @param symbols Optional: character vector for symbols denoting p-value cut-points
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
#'
#' apaSupp::tab_glm(fit_glm2, vif = FALSE)
#' apaSupp::tab_glm(fit_glm2, lrt = FALSE)
#' apaSupp::tab_glm(fit_glm2, vif = FALSE, lrt = FALSE)
#'
#' apaSupp::tab_glm(fit_glm2, pr2 = "both")
#' apaSupp::tab_glm(fit_glm2, pr2 = "tjur")
#' apaSupp::tab_glm(fit_glm2, pr2 = "mcfadden")
#' apaSupp::tab_glm(fit_glm2, pr2 = "none")
#'
#'
#' apaSupp::tab_glm(fit_glm2, fit = c("AIC", "BIC"))
#' apaSupp::tab_glm(fit_glm2, pr2 = "both", fit = c("AIC", "BIC"))
#'
#'
tab_glm <- function(x,
                    var_labels      = NULL,
                    caption         = "Parameter Estimates for Generalized Linear Regression",
                    general_note    = NA,
                    p_note          = "apa123",
                    no_notes        = FALSE,
                    d               = 2,
                    fit             = NULL,
                    pr2             = "tjur",
                    vif             = TRUE,
                    lrt             = TRUE,
                    show_single_row = NULL,
                    breaks          = c(.05, .01, .001),
                    symbols         = c("*", "**", "***")){

  n_obs   <- length(x$resid)
  n_param <- length(coef(x))
  n_fit   <- sum(!is.na(fit),
                 3*(pr2 == "both"),
                 1*(pr2 == "tjur"),
                 1*(pr2 == "mcfadden"))

  if (n_param <= 2) {
    vif <- FALSE
    lrt <- FALSE
  }


  if (family(x)$link == "logit"){
    back_trans <- "exp"
    abr <- c("Odds Ratio","Logit Scale")
    sym <- c("OR", "b")
  } else if (family(x)$family == "poisson" & family(x)$link == "log") {
    back_trans <- "exp"
    abr <- c("Incident Rate Ratio","Log Scale")
    sym <- c("IRR", "b")
    pr2 <- "nagelkerke"
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
    flextable::as_chunk(case_when(pr2 == "both"     ~ "Coefficient of deterination included for both Tjur and McFadden's ",
                                  pr2 == "tjur"     ~ "Coefficient of deterination displays Tjur's ",
                                  pr2 == "mcfadden" ~ "Coefficient of deterination displays McFadden's ",
                                  pr2 == "nagelkerke" ~ "Coefficient of deterination displays Nagelkerke's")),
    flextable::as_i(ifelse(pr2 == "none", NA, "pseudo-R\u00B2. ")),
    flextable::as_chunk(general_note)
  )

  if (back_trans == "exp"){
    get_tran <- x %>%
      gtsummary::tbl_regression(intercept = TRUE,
                                conf.int = TRUE,
                                exponentiate = TRUE,
                                tidy_fun = broom.helpers::tidy_with_broom_or_parameters,
                                show_single_row = show_single_row) %>%
      gtsummary::modify_column_hide(column = std.error) %>%
      gtsummary::modify_column_hide(column = p.value) %>%
      gtsummary::modify_fmt_fun(estimate  ~ gtsummary::label_style_number(digits = d)) %>%
      gtsummary::modify_fmt_fun(conf.low  ~ gtsummary::label_style_number(digits = d, prefix = "[")) %>%
      gtsummary::modify_fmt_fun(conf.high ~ gtsummary::label_style_number(digits = d, suffix = "]")) %>%
      gtsummary::remove_abbreviation("CI = Confidence Interval") %>%
      gtsummary::remove_abbreviation("SE = Standard Error")  %>%
      gtsummary::modify_table_body(~.x %>%
                                     dplyr::mutate(estimate = ifelse(variable == "(Intercept)", NA, estimate)) %>%
                                     dplyr::mutate(conf.low = ifelse(variable == "(Intercept)", NA, conf.low))) %>%
      gtsummary::modify_table_body(~.x %>% dplyr::mutate(bk = NA)) %>%
      gtsummary::modify_header(label = "Variable",
                               estimate = sym[1],
                               conf.low = "95% CI",
                               bk = "blank")
  }

  if (family(x)$link == "logit"){
    get_tran <- get_tran %>%
      gtsummary::remove_abbreviation("OR = Odds Ratio")
  } else if (family(x)$family == "poisson"){
    get_tran <- get_tran %>%
      gtsummary::remove_abbreviation("IRR = Incidence Rate Ratio")
  }

  if (!is.null(fit)){
    get_tran <- get_tran %>% gtsummary::add_glance_table(include = all_of(fit))
  }


  get <- x %>%
    gtsummary::tbl_regression(intercept       = TRUE,
                              conf.int        = FALSE,
                              exponentiate    = FALSE,
                              pvalue_fun      = function(x) apaSupp::p_num(x, d = d + 1),
                              tidy_fun        = broom.helpers::tidy_with_broom_or_parameters,
                              show_single_row = show_single_row) %>%
    gtsummary::modify_column_unhide(column = std.error) %>%
    gtsummary::remove_abbreviation("SE = Standard Error")  %>%
    gtsummary::remove_footnote_header() %>%
    gtsummary::modify_fmt_fun(estimate  ~ function(x) apaSupp::p_num(x, d = (d + 1), stars = FALSE), rows =  stringr::str_detect(variable, "r.")) %>%
    gtsummary::modify_fmt_fun(estimate  ~ function(x) apaSupp::p_num(x, d = (d - 1), stars = FALSE), rows = !stringr::str_detect(variable, "r.")) %>%
    gtsummary::modify_fmt_fun(estimate  ~ gtsummary::label_style_number(digits = d),        rows = (row_type == "label") ) %>%
    gtsummary::modify_fmt_fun(std.error ~ gtsummary::label_style_number(digits = d,         prefix = "(", suffix = ")"))

  if (family(x)$link == "logit"){
    get <- get %>%
      gtsummary::remove_abbreviation("OR = Odds Ratio")
  } else if (family(x)$family == "poisson"){
    get <- get %>%
      gtsummary::remove_abbreviation("IRR = Incidence Rate Ratio")
  }


  if (lrt == TRUE){
    get <- get %>%
      gtsummary::modify_table_body(dplyr::left_join,
                                   apaSupp::pdr1_to_tibble(x, test = "Chisq", d = d),
                                   by = c("variable", "row_type")) %>%
      gtsummary::modify_header(label     = "Variable",
                               estimate  = sym[2],
                               std.error = "(SE)",
                               p.value   = "Wald",
                               p_dr1     = "LRT") %>%
      gtsummary::modify_fmt_fun(p_dr1 ~ function(x) apaSupp::p_num(x, d = d + 1))

  } else {
    get <- get %>%
      gtsummary::modify_header(label     = "Variable",
                               estimate  = sym[2],
                               std.error = "(SE)",
                               p.value   = "p")
  }

  get_orig <- get %>%
    gtsummary::modify_column_hide(column = starts_with("p")) %>%
    gtsummary::modify_table_body(~.x %>% dplyr::mutate(bk = NA)) %>%
    gtsummary::modify_header(label    = "Variable",
                             estimate = sym[2],
                             conf.low = "95% CI",
                             bk       = "blank")

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
      flextable::compose(part = "all", j = c(4, 7), value = flextable::as_paragraph(NA))

  } else {
    table <- list(get_tran,  get_orig,  get_sig) %>%
      gtsummary::tbl_merge(tab_spanner = c(abr[1],  abr[2],  "p")) %>%
      gtsummary::as_flex_table() %>%
      flextable::compose(part = "header", j = c(4, 7), value = flextable::as_paragraph(NA)) %>%
      flextable::compose(part = "body",   j = c(4, 7), value = flextable::as_paragraph(NA))
  }

  if(!is.null(var_labels)){ table <- table %>% flextable::labelizor(part = "body", labels = var_labels)}

  n_col  <- flextable::ncol_keys(table)
  n_rows <- flextable::nrow_part(table, part = "body")


  if (family(x)$link == "logit"){
    r2_tjur     <- performance::r2_tjur(x)     %>% unlist(use.names = FALSE) %>% apaSupp::p_num(d = d + 1, stars = FALSE)
    r2_mcfadden <- performance::r2_mcfadden(x) %>% unlist(use.names = FALSE) %>% apaSupp::p_num(d = d + 1, stars = FALSE)

  } else if (family(x)$family == "poisson"){
    r2_nagelkerke     <- performance::r2_nagelkerke(x)     %>% unlist(use.names = FALSE) %>% apaSupp::p_num(d = d + 1, stars = FALSE)
  }

  if (pr2 == "both"){
    table <- table %>%
      flextable::add_body_row(top = FALSE, values = NA) %>%
      flextable::compose(part  = "body", i = (n_rows + 1), j = 1,
                         value = flextable::as_paragraph(flextable::as_i("pseudo-R\u00B2"))) %>%
      flextable::add_body_row(top = FALSE, values = NA) %>%
      flextable::compose(part  = "body", i = (n_rows + 2), j = 2, value = flextable::as_paragraph("Tjur")) %>%
      flextable::compose(part  = "body", i = (n_rows + 2), j = 3, value = flextable::as_paragraph(r2_tjur)) %>%
      flextable::add_body_row(top = FALSE, values = NA) %>%
      flextable::compose(part  = "body", i = (n_rows + 3), j = 2, value = flextable::as_paragraph("McFadden")) %>%
      flextable::compose(part  = "body", i = (n_rows + 3), j = 3, value = flextable::as_paragraph(r2_mcfadden[1]))
  } else if (pr2 == "tjur"){
    table <- table %>%
      flextable::add_body_row(top = FALSE, values = NA) %>%
      flextable::compose(part  = "body", i = (n_rows + 1), j = 1, value = flextable::as_paragraph(flextable::as_i("pseudo-R\u00B2"))) %>%
      flextable::compose(part =  "body", i = (n_rows + 1), j = 2, value = flextable::as_paragraph(r2_tjur))
  } else if (pr2 == "mcfadden"){
    table <- table %>%
      flextable::add_body_row(top = FALSE, values = NA) %>%
      flextable::compose(part = "body", i = (n_rows + 1), j = 1, value = flextable::as_paragraph(flextable::as_i("pseudo-R\u00B2"))) %>%
      flextable::compose(part = "body", i = (n_rows + 1), j = 2, value = flextable::as_paragraph(r2_mcfadden[1]))
  } else if (pr2 == "nagelkerke"){
    table <- table %>%
      flextable::add_body_row(top = FALSE, values = NA) %>%
      flextable::compose(part  = "body", i = (n_rows + 1), j = 1, value = flextable::as_paragraph(flextable::as_i("pseudo-R\u00B2"))) %>%
      flextable::compose(part =  "body", i = (n_rows + 1), j = 2, value = flextable::as_paragraph(r2_nagelkerke))
  }

  if (lrt == TRUE){
    table <- table %>%
      flextable::align(   part = "all",           j = c(8), align = "right") %>%
      flextable::align(   part = "all",           j = c(9), align = "left") %>%
      flextable::align(   part = "header", i = 1,           align = "center") %>%
      flextable::merge_at(part = "header", i = 1, j = 8:9) %>%
      flextable::compose( part = "header", i = 1, j = 8, value = flextable::as_paragraph(flextable::as_i("p"))) %>%
      flextable::hline(   part = "header", i = 1, j = 8:9)
  } else {
    table <- table %>%
      flextable::compose(part = "header", i = 1, j = 8, value = flextable::as_paragraph(NA)) %>%
      flextable::compose(part = "header", i = 2, j = 8, value = flextable::as_paragraph(flextable::as_i("p")))
  }

  n_rows <- flextable::nrow_part(table, part = "body")

  table <- table  %>%
    flextable::delete_rows(part = "header", i = 1) %>%
    flextable::add_header_row(values    = c(NA, abr[1], NA, abr[2], rep(NA, n_col - 6)),
                              colwidths = c( 1,     2,   1,     2,  rep( 1, n_col - 6))) %>%
    apaSupp::theme_apa(caption      = caption,
                       general_note    = main_note,
                       p_note       = p_note,
                       d            = d,
                       breaks       = breaks,
                       symbols      = symbols) %>%
    flextable::italic(part = "header", i = 2, j = 4:5) %>%
    flextable::align( part = "all",           j = c(2, 5), align = "right") %>%
    flextable::align( part = "all",           j = c(3, 6), align = "left") %>%
    flextable::align( part = "header", i = 1,              align = "center") %>%
    flextable::align( part = "footer",                     align = "left") %>%
    flextable::hline( part = "header", i = 1, border = flextable::fp_border_default(width = 0)) %>%
    flextable::hline( part = "header", i = 1, j = 2:3) %>%
    flextable::hline( part = "header", i = 1, j = 5:6) %>%
    flextable::autofit()

  if (n_fit > 0) {
    table <- table %>%
      flextable::italic(part = "body", i = (n_rows - n_fit + 1):(n_rows)) %>%
      flextable::hline( part = "body", i =  n_rows - n_fit)
  }

  return(table)
}
