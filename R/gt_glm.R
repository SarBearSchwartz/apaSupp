#' APA: gtsummary for a single linear model
#'
#' @param x REQUIRED: bare name. a single 'glm' object'
#' @param narrow  Optional. Logical. Default = FALSE, but TRUE will exclude p-vlaues from the table to make it narrower
#' @param fit Optional: vector. quoted names of fit statistics to include, can be: "r.squared", "adj.r.squared", "sigma", "statistic","p.value", "df", "logLik", "AIC", "BIC", "deviance", "df.residual", and "nobs"
#' @param d Optional: number. digits after the decimal, default = 2
#'
#' @return a gtsummary object
#' @import gtsummary
#' @import tidyverse
#' @import broom.helpers
#' @export
#'
#' @examples
#' library(datasets)
#' library(tidyverse)
#'
#' mtcars <- mtcars %>% dplyr::mutate(vs = factor(vs))
#' m <- lm(mpg ~ vs + disp, data = mtcars)
#'gt_lm(m)
#'
#'
#'
gt_glm <- function(x,
                   narrow = FALSE,
                   fit = c("r.squared",
                           "adj.r.squared"),
                   d = 2){

  if (narrow == FALSE){
    p_fun <- apaSupp::p_num
  } else {
    p_fun <- apaSupp::p_star
  }

  table <- x %>%
    gtsummary::tbl_regression(intercept = TRUE,
                              conf.int = FALSE,
                              pvalue_fun = p_fun,
                              tidy_fun = broom.helpers::tidy_with_broom_or_parameters)  %>%
    # gtsummary::add_glance_table(include = fit) %>%
    gtsummary::modify_column_unhide(column = std.error) %>%
    gtsummary::remove_footnote_header() %>%
    gtsummary::remove_abbreviation("SE = Standard Error")  %>%
    gtsummary::modify_fmt_fun(estimate ~
                                gtsummary::label_style_number(digits = d)) %>%
    gtsummary::modify_fmt_fun(std.error ~
                                gtsummary::label_style_number(digits = d,
                                                              prefix = "(",
                                                              suffix = ")"))
  if (narrow == TRUE){
    table <- table %>%
      gtsummary::modify_column_merge(pattern = "{std.error} {p.value}",
                                     row = !is.na(std.error)) %>%
      gtsummary::modify_header(label = "Variable",
                               estimate = "b",
                               std.error = "(SE)")
  } else {
    table <- table %>%
      gtsummary::modify_header(label = "Variable",
                               estimate = "b",
                               std.error = "(SE)",
                               p.value = "p")
  }

  return(table)
}
