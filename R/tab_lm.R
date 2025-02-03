#' APA: flextable for ONElinear models
#'
#' @param x REQUIRED: 1 lm model, bare names
#' @param var_labels Optional: Vector. Text replacements for model terms, "old" = "new"
#' @param caption Optional: Text. Caption for the table
#' @param p_note Optional: Text. (default = NULL) Significance note for APA table, If p_note = "apa" then the standard "* p < .05. ** p < .01. *** p < .001." will be used
#' @param general_note Optional: Text. General note for footer of APA table
#' @param fit Optional: vector. quoted names of fit statistics to include, can be: "r.squared", "adj.r.squared", "sigma", "statistic","p.value", "df", "logLik", "AIC", "BIC", "deviance", "df.residual", and "nobs"
#' @param d Optional: Number. Digits after the decimal place
#'
#' @returns a flextable object
#' @import gtsummary
#' @import purrr
#' @import magrittr
#' @import tidyverse
#' @import broom.helpers
#' @export
#'
#' @examples
#'
#' m <- lm(dist ~ speed, cars)
#' tab_lm(m)
#'

tab_lm <- function(x,
                   var_labels = NULL,
                   caption = "Regression Model",
                   p_note = "apa",
                   general_note = NULL,
                   fit = c("r.squared",
                           "adj.r.squared"),
                   d = 2){

  n_param <- x %>%
    coef() %>%
    length()

  n_fit <- length(fit)

  get <- x %>%
    gtsummary::tbl_regression(intercept = TRUE,
                              conf.int = TRUE,
                              pvalue_fun = apaSupp::p_num,
                              tidy_fun = broom.helpers::tidy_with_broom_or_parameters) %>%
    gtsummary::add_glance_table(include = fit) %>%
    gtsummary::modify_column_unhide(column = std.error) %>%
    gtsummary::remove_footnote_header() %>%
    gtsummary::remove_abbreviation("SE = Standard Error")  %>%
    gtsummary::modify_fmt_fun(estimate ~
                                label_style_number(digits = d)) %>%
    gtsummary::modify_fmt_fun(std.error ~
                                label_style_number(digits = d,
                                                   prefix = "(",
                                                   suffix = ")")) %>%
    gtsummary::modify_fmt_fun(conf.low ~
                                label_style_number(digits = d,
                                                   prefix = "[")) %>%
    gtsummary::modify_fmt_fun(conf.high ~
                                label_style_number(digits = d,
                                                   suffix = "]")) %>%
    gtsummary::modify_header(label = "Variable",
                             estimate = "b",
                             std.error = "(SE)",
                             conf.low = "[95% CI]",
                             p.value = "p")

  table <- get %>%
    gtsummary::as_flex_table()

  n_rows <- flextable::nrow_part(table, part = "body")

  rows_fit <- (n_rows - n_fit + 1):(n_rows)

  table <- table %>%
    apaSupp::theme_apa(caption = caption,
                       p_note = p_note,
                       general_note = general_note) %>%
    flextable::hline(i = n_rows - n_fit) %>%
    flextable::italic(part = "header", i = 1) %>%
    flextable::italic(part = "body", i = rows_fit)

  if(!is.null(var_labels)){
    table <- table %>%
      flextable::labelizor(part = "body",
                           labels = var_labels)
  }

  return(table)

}
