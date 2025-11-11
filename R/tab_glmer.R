#' APA: flextable for ONE generarlized linear mixed effects models
#'
#' @param x REQUIRED: 1 glmer model, bare name
#' @param var_labels Optional: Vector. Text replacements for model terms, "old" = "new"
#' @param caption Optional: Text. Caption for the table
#' @param docx Optional: filename. must end with ".docx"
#' @param general_note Optional: Text. General note for footer of APA table
#' @param p_note Optional: Text. (default = NULL) Significance note for APA table, If `p_note = "apa123"` then the standard `"* p < .05. ** p < .01. *** p < .001."` will be used
#' @param no_notes REQUIRED: Logical.  Defaults to `FALSE`, if `TRUE` will ignore `general_note` and `p_note`
#' @param d Optional: Number. Digits after the decimal place
#' @param ci Optional: logical. (default = FALSE) Include a confidence interval for the estimated beta
#' @param show_single_row  Optional: If a variable is dichotomous (e.g. Yes/No) and you wish to print the regression coefficient on a single row, include the variable name(s) here.
#' @param breaks Optional: numeric vector of p-value cut-points
#' @param symbols Optional: character vector for symbols denoting p-value cut-points
#'
#' @returns a flextable object
#' @import gtsummary
#' @import flextable
#' @import tidyverse
#' @import broom.mixed
#' @import parameters
#' @import DescTools
#' @import car
#' @import MOTE
#' @import merDeriv
#' @export
#'
#' @examples
#'
#'library(tidyverse)
#'library(lme4)
#'library(lmerTest)
#'library("HSAUR3")
#'
#'fit1 <- glmer(outcome~treatment*visit+(1|patientID), data=toenail,family=binomial,nAGQ=20)
#'
#'
#'
#' apaSupp::tab_glmer(fit1)
#'
#'
#' apaSupp::tab_glmer(fit1, show_single_row = c("treatment","treatment:visit"), docx = "test.docx")
#'

tab_glmer <- function(x,
                      var_labels      = NULL,
                      caption         = "Parameter Estimates for Generalized Mixed Effects Regression",
                      docx            = NA,
                      general_note    = NA,
                      p_note          = "apa123",
                      no_notes        = FALSE,
                      d               = 2,
                      ci              = FALSE,
                      show_single_row = NULL,
                      breaks          = c(.05, .01, .001),
                      symbols         = c("*", "**", "***")){

  if (family(x)$link == "logit"){
    back_trans <- "exp"
    abr <- c("Odds Ratio","Logit Scale")
    sym <- c("Est", "b")
    int <- FALSE
  } else if (family(x)$family == "poisson" & family(x)$link == "log") {
    back_trans <- "exp"
    abr <- c("Incident Rate Ratio","Log Scale")
    sym <- c("IRR", "b")
    int <- TRUE
  } else if (family(x)$family == "poisson" & family(x)$link == "log"){
    back_trans <- "exp"
    abr <- c("Incident Rate Ratio","Log Scale")
    sym <- c("IRR", "b")
  }

  rand_var <- x %>%
    VarCorr() %>%
    data.frame() %>%
    nrow()

  main_note <- flextable::as_paragraph(
    flextable::as_i("Note. p"),
    flextable::as_chunk(" = significance from Wald t-test for parameter estimate using Satterthwaite's method for degrees of freedom. "),
    flextable::as_chunk(general_note)
  )

  if (back_trans == "exp"){
    get_tran <- x %>%
      gtsummary::tbl_regression(intercept = int,
                                conf.int = TRUE,
                                exponentiate = TRUE,
                                tidy_fun = function(x, ...) broom.mixed::tidy(x, scales = c("vcov", "sdcor"), ...),
                                show_single_row = show_single_row) %>%
      gtsummary::modify_column_hide(column = std.error) %>%
      gtsummary::modify_column_hide(column = p.value) %>%
      gtsummary::modify_fmt_fun(estimate  ~ gtsummary::label_style_number(digits = d)) %>%
      gtsummary::modify_fmt_fun(conf.low  ~ gtsummary::label_style_number(digits = d, prefix = "[")) %>%
      gtsummary::modify_fmt_fun(conf.high ~ gtsummary::label_style_number(digits = d, suffix = "]")) %>%
      gtsummary::remove_abbreviation("CI = Confidence Interval") %>%
      gtsummary::remove_abbreviation("SE = Standard Error")  %>%
      gtsummary::modify_table_body(~.x %>%
                                     dplyr::mutate(estimate = ifelse(variable == "(Intercept)" & int == FALSE, NA, estimate)) %>%
                                     dplyr::mutate(conf.low = ifelse(variable == "(Intercept)" & int == FALSE, NA, conf.low))) %>%
      gtsummary::modify_table_body(~.x %>%
                                     dplyr::mutate(bk = NA) %>%
                                     dplyr::filter(var_type != "ran_pars")) %>%
      gtsummary::modify_header(label = "Variable",
                               estimate = sym[1],
                               conf.low = "[95% CI]",
                               bk = "blank")
  }

  if (family(x)$link == "logit"){
    get_tran <- get_tran %>%
      gtsummary::remove_abbreviation("OR = Odds Ratio")
  } else if (family(x)$family == "poisson"){
    get_tran <- get_tran %>%
      gtsummary::remove_abbreviation("IRR = Incidence Rate Ratio")
  }


  get <- x %>%
    gtsummary::tbl_regression(intercept = FALSE,
                              conf.int = FALSE,
                              pvalue_fun = function(x) apaSupp::p_num(x, d = d + 1),
                              tidy_fun = function(x, ...) broom.mixed::tidy(x, scales = c("vcov", "sdcor"), ...),
                              show_single_row = show_single_row) %>%
    gtsummary::modify_column_unhide(column = std.error)   %>%
    gtsummary::remove_footnote_header() %>%
    gtsummary::remove_abbreviation("SE = Standard Error")  %>%
    gtsummary::modify_fmt_fun(estimate  ~ gtsummary::label_style_number(digits = d),
                              rows = (row_type %in% c("label", "level"))) %>%
    gtsummary::modify_fmt_fun(std.error ~ gtsummary::label_style_number(digits = d, prefix = "(", suffix = ")")) %>%
    gtsummary::modify_header(label     = "Variable",
                             estimate  = "Est",
                             std.error = "(SE)",
                             p.value   = "p") %>%
    gtsummary::modify_table_body(~.x %>%
                                   dplyr::arrange(row_type == "glance_statistic")) %>%
    gtsummary::modify_header(label     = "Variable",
                             estimate  = sym[2],
                             std.error = "(SE)",
                             p.value   = "p")


  if (family(x)$link == "logit"){
    get <- get %>%
      gtsummary::remove_abbreviation("OR = Odds Ratio")
  } else if (family(x)$family == "poisson"){
    get <- get %>%
      gtsummary::remove_abbreviation("IRR = Incidence Rate Ratio")
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

  table <- list(get_tran,  get_orig,  get_sig) %>%
    gtsummary::tbl_merge(tab_spanner = c(abr[1],  abr[2],  "p")) %>%
    gtsummary::as_flex_table() %>%
    flextable::compose(part = "header", j = c(4, 7), value = flextable::as_paragraph(NA)) %>%
    flextable::compose(part = "body",   j = c(4, 7), value = flextable::as_paragraph(NA))


  if(!is.null(var_labels)){ table <- table %>% flextable::labelizor(part = "body", labels = var_labels)}

  n_col  <- flextable::ncol_keys(table)
  n_rows <- flextable::nrow_part(table, part = "body")

  table <- table  %>%
    flextable::delete_rows(part = "header", i = 1) %>%
    flextable::add_header_row(values    = c(NA, abr[1], NA, abr[2], rep(NA, n_col - 6)),
                              colwidths = c( 1,     2,   1,     2,  rep( 1, n_col - 6))) %>%
    apaSupp::theme_apa(caption      = caption,
                       main_note    = main_note,
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
    flextable::italic(part = "header", i = 2) %>%
    flextable::italic(part = "body",   i = (n_rows - rand_var + 1):(n_rows)) %>%
    flextable::hline( part = "body",   i =  n_rows - rand_var) %>%
    flextable::autofit()

  if (!is.na(docx)){
    flextable::save_as_docx(table,
                            path = docx)
  }

  return(table)
}

