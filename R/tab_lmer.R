#' APA: flextable for ONE linear mixed effects models
#'
#' @param x REQUIRED: 1 lmer model, bare name
#' @param var_labels Optional: Vector. Text replacements for model terms, "old" = "new"
#' @param caption Optional: Text. Caption for the table
#' @param docx Optional: filename. must end with ".docx"
#' @param tab_width Optional: numberic value (default is .9) % of available width
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
#'
#'fm1 <- lmerTest::lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
#'fm2 <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#'fm3 <- lmerTest::lmer(Reaction ~ Days + (1 | Subject), sleepstudy %>% dplyr::mutate(Days = factor(Days)))
#'
#' apaSupp::tab_lmer(fm1)
#'
#'
#' apaSupp::tab_lmer(fm2, ci = TRUE, docx = "test.docx")
#'

tab_lmer <- function(x,
                     var_labels      = NULL,
                     caption         = "Parameter Estimates for Mixed Effects Regression",
                     docx            = NA,
                     tab_width       = .9,
                     general_note    = NA,
                     p_note          = "apa123",
                     no_notes        = FALSE,
                     d               = 2,
                     ci              = FALSE,
                     show_single_row = NULL,
                     breaks          = c(.05, .01, .001),
                     symbols         = c("*", "**", "***")){

  rand_var <- x %>%
    VarCorr() %>%
    data.frame() %>%
    nrow()

  main_note <- flextable::as_paragraph(
    flextable::as_i("Note. Est"),
    flextable::as_chunk(" = estimated regresssion coefficient for fixed effects and varaince/covariance estimates for random effects. "),
    flextable::as_i("p"),
    flextable::as_chunk(" = significance from Wald t-test for parameter estimate using Satterthwaite's method for degrees of freedom. "),
    flextable::as_chunk(general_note)
  )

  get <- x %>%
    gtsummary::tbl_regression(intercept = TRUE,
                              conf.int = ci,
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
                                   dplyr::arrange(row_type == "glance_statistic"))

  if (ci == TRUE){
    table <- table %>%
      gtsummary::modify_fmt_fun(conf.low  ~ gtsummary::label_style_number(digits = d, prefix = "[", suffix = "")) %>%
      gtsummary::modify_fmt_fun(conf.high ~ gtsummary::label_style_number(digits = d, prefix = " ", suffix = "]"))
  }


  table <- get %>%
    gtsummary::as_flex_table() %>%
    apaSupp::theme_apa(caption      = caption,
                       main_note    = main_note,
                       p_note       = p_note,
                       d            = d,
                       breaks       = breaks,
                       symbols      = symbols) %>%
    flextable::align( part = "all",   j = 2, align = "right") %>%
    flextable::align( part = "all",   j = 3, align = "left") %>%
    flextable::align( part = "footer",       align = "left")


  if(!is.null(var_labels)){ table <- table %>%
    flextable::labelizor(part = "body", labels = var_labels)}

  n_rows <- flextable::nrow_part(table, part = "body")

  table <- table %>%
    flextable::compose(part = "header", i = 1, j = 1, value = flextable::as_paragraph(NA)) %>%
    flextable::italic( part = "header") %>%
    flextable::hline(  part = "body",  i = (n_rows - rand_var)) %>%
    flextable::italic( part = "body",  i = (n_rows - rand_var + 1):(n_rows)) %>%
    flextable::set_table_properties(layout = "autofit",
                                    width = tab_width)

  if (!is.na(docx)){
    flextable::save_as_docx(table,
                            path = docx)
  }

  return(table)
}

