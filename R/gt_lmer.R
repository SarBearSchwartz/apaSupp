#' APA: gtsummary for a single linear mixed effects model
#'
#' @param x REQUIRED: bare name. a single 'lmer' object'
#' @param narrow  Optional. Logical. Default = FALSE, but TRUE will exclude p-vlaues from the table to make it narrower
#' @param fit Optional: Character vector. Default  = c("AIC", "BIC", "logLik).  May include quoted names of fit statistics listed from broom::glimpse(x)
#' @param d Optional: number. digits after the decimal, default = 2
#' @param show_single_row	(tidy-select) By default categorical variables are printed on multiple rows. If a variable is dichotomous (e.g. Yes/No) and you wish to print the regression coefficient on a single row, include the variable name(s) here.
#'
#' @return a gtsummary object
#' @import gtsummary
#' @import tidyverse
#' @import broom.mixed
#' @import stringr
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
#' apaSupp::gt_lmer(fm1)
#' apaSupp::gt_lmer(fm2, d = 3)
#' apaSupp::gt_lmer(fm3, narrow = TRUE)
#'
gt_lmer <- function(x,
                    narrow          = FALSE,
                    fit             = c("AIC", "BIC", "logLik"),
                    d               = 2,
                    show_single_row = NULL){

  if (narrow == FALSE){ p_fun <- function(x, d) apaSupp::p_num(x, d = d + 1)
  } else {              p_fun <- function(x, d) apaSupp::p_star(x)
  }

  rand_var <- x %>%
    VarCorr() %>%
    data.frame() %>%
    nrow()

  table <- x %>%
    gtsummary::tbl_regression(intercept = TRUE,
                              conf.int = FALSE,
                              pvalue_fun = ~ p_fun(.x, d = d),
                              tidy_fun = function(x, ...) broom.mixed::tidy(x, scales = c("vcov", "sdcor"), ...),
                              show_single_row = show_single_row)  %>%
    gtsummary::add_glance_table(include = all_of(fit)) %>%
    gtsummary::modify_column_unhide(column = std.error) %>%
    gtsummary::remove_footnote_header() %>%
    gtsummary::remove_abbreviation("SE = Standard Error") %>%
    gtsummary::modify_fmt_fun(estimate  ~ gtsummary::label_style_number(digits = d),
                              rows = (row_type %in% c("label", "level"))) %>%
    gtsummary::modify_fmt_fun(std.error ~ gtsummary::label_style_number(digits = d, prefix = "(", suffix = ")"))

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


