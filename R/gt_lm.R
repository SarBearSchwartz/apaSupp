#' APA: gtsummary for a single linear model
#'
#' @param x REQUIRED: bare name. a single 'lm' object'
#' @param narrow  Optional. Logical. Default = FALSE, but TRUE will exclude p-vlaues from the table to make it narrower
#' @param fit Optional: Character vector. Default  = c("r.squared", "adj.r.squared").  May include quoted names of fit statistics listed from broom::glimpse(x)
#' @param d Optional: number. digits after the decimal, default = 2
#' @param show_single_row	(tidy-select) By default categorical variables are printed on multiple rows. If a variable is dichotomous (e.g. Yes/No) and you wish to print the regression coefficient on a single row, include the variable name(s) here.
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
#'
#' m <- lm(mpg ~ vs + disp, data = mtcars)
#'
#' apaSupp::gt_lm(m, d = 3)
#' apaSupp::gt_lm(m, narrow = TRUE)
#'
gt_lm <- function(x,
                  narrow          = FALSE,
                  fit             = c("r.squared", "adj.r.squared"),
                  d               = 2,
                  show_single_row = NULL){

  if (narrow == FALSE){ p_fun <- function(x, d) apaSupp::p_num(x, d = d + 1)
  } else {              p_fun <- function(x, d) apaSupp::p_star(x)
  }

  table <- x %>%
    gtsummary::tbl_regression(intercept = TRUE,
                              conf.int = FALSE,
                              pvalue_fun = ~ p_fun(.x, d = d),
                              tidy_fun = broom.helpers::tidy_with_broom_or_parameters,
                              show_single_row = show_single_row)  %>%
    gtsummary::add_glance_table(include = all_of(fit)) %>%
    gtsummary::modify_column_unhide(column = std.error) %>%
    gtsummary::remove_footnote_header() %>%
    gtsummary::remove_abbreviation("SE = Standard Error")  %>%
    gtsummary::modify_fmt_fun(estimate ~  gtsummary::label_style_number(digits = d)) %>%
    gtsummary::modify_fmt_fun(std.error ~ gtsummary::label_style_number(digits = d, prefix = "(", suffix = ")")) %>%
    gtsummary::modify_fmt_fun(estimate  ~ apaSupp::p_num(d = d + 1, stars = FALSE), rows   =  stringr::str_detect(variable, "r.")) %>%
    gtsummary::modify_fmt_fun(estimate  ~ apaSupp::p_num(d = d - 1, stars = FALSE), rows   = !stringr::str_detect(variable, "r."))


  if (narrow == TRUE){
    table <- table %>%
      gtsummary::modify_column_merge(pattern = "{std.error} {p.value}", row = !is.na(std.error)) %>%
      gtsummary::modify_header(label     = "Variable",
                               estimate  = "b",
                               std.error = "(SE)")
  } else {
    table <- table %>%
      gtsummary::modify_header(label     = "Variable",
                               estimate  = "b",
                               std.error = "(SE)",
                               p.value   = "p")
  }

  return(table)
}


