#' APA: flextable for ONElinear models
#'
#' @param x REQUIRED: 1 lm model, bare name
#' @param var_labels Optional: Vector. Text replacements for model terms, "old" = "new"
#' @param caption Optional: Text. Caption for the table
#' @param general_note Optional: Text. General note for footer of APA table
#' @param p_note Optional: Text. (default = NULL) Significance note for APA table, If `p_note = "apa123"` then the standard `"* p < .05. ** p < .01. *** p < .001."` will be used
#' @param no_notes REQUIRED: Logical.  Defaults to `FALSE`, if `TRUE` will ignore `general_note` and `p_note`
#' @param d Optional: Number. Digits after the decimal place
#' @param fit Optional: vector. quoted names of fit statistics to include, can be: "r.squared", "adj.r.squared", "sigma", "statistic","p.value", "df", "logLik", "AIC", "BIC", "deviance", "df.residual", and "nobs"
#' @param ci Optional: logical. (default = FALSE) Include a confidence interval for the estimated beta
#' @param show_single_row  Optional: If a variable is dichotomous (e.g. Yes/No) and you wish to print the regression coefficient on a single row, include the variable name(s) here.
#' @param tab_width Optional: numberic value (default is .9) % of available width
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
#' apaSupp::tab_lmer(fm3)
#'
#'
#' apaSupp::tab_lmer(fm2, ci = TRUE)
#'

tab_lmer <- function(x,
                     var_labels      = NULL,
                     caption         = "Parameter Estimates for Mixed Effects Regression",
                     general_note    = NA,
                     p_note          = "apa123",
                     no_notes        = FALSE,
                     d               = 2,
                     ci              = FALSE,
                     show_single_row = NULL,
                     tab_width       = .9,
                     breaks          = c(.05, .01, .001),
                     symbols         = c("*", "**", "***")){


  n_param <- x %>%
    fixef() %>%
    length()

  fix_var <- x %>%
    fixef() %>%
    names()


  main_note <- flextable::as_paragraph(
    flextable::as_i("Note. Est."),
    flextable::as_chunk(" = estimated regresssion coefficient for fixed effects and varaince/covariance estimates for random effects. "),
    flextable::as_i("p"),
    flextable::as_chunk(" = significance from Wald t-test for parameter estimate using Satterthwaite's method for degrees of freedom. "),
    flextable::as_chunk(general_note)
  )

  get <- x %>%
    gtsummary::tbl_regression(intercept = TRUE,
                              conf.int = ci,
                              pvalue_fun = function(x) apaSupp::p_num(x, d = d + 1),
                              # tidy_fun = function(x, ...) broom.mixed::tidy(x, scales = c("vcov", "sdcor"), ...),
                              show_single_row = show_single_row) %>%
    gtsummary::modify_column_unhide(column = std.error)   %>%
    # gtsummary::modify_column_unhide(column = effect) %>%
    # gtsummary::add_variable_group_header(header = "FIXED EFFECTS",
    #                                      variables = (effect == "fixed")) #%>%
    # gtsummary::add_variable_group_header(header = "RANDOM EFFECTS",
    #                                      variables =  -fix_var) %>%
    gtsummary::remove_footnote_header() %>%
    gtsummary::remove_abbreviation("SE = Standard Error")  %>%
    gtsummary::modify_fmt_fun(std.error ~ gtsummary::label_style_number(digits = d, prefix = "(", suffix = ")")) %>%
    gtsummary::modify_header(label     = "Variable",
                             estimate  = "Est.",
                             std.error = "(SE)",
                             p.value   = "p")



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


  if(!is.null(var_labels)){ table <- table %>% flextable::labelizor(part = "body", labels = var_labels)}

  n_rows <- flextable::nrow_part(table, part = "body")

  table <- table %>%
    flextable::compose(part = "header", i = 1, j = 1, value = flextable::as_paragraph(NA)) %>%
    flextable::italic( part = "header") %>%
    flextable::hline(  part = "body",  i = n_param+1) %>%
    flextable::italic( part = "body",  i = (n_param + 3):(n_rows))

 for (r in (n_rows - n_fit + 1):n_rows){
    table <- table %>%
      flextable::merge_at(i = r, j = 2:3) %>%
      flextable::align(   i = r, j = 1,   align = "left") %>%
      flextable::align(   i = r, j = 2:3, align = "center")
  }

  table <- table %>%
    flextable::set_table_properties(layout = "autofit",
                                    width = tab_width)

  return(table)
}


