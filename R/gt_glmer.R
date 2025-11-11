#' APA: gtsummary for a single generalized linear mioxed effects model
#'
#' @param x REQUIRED: bare name. a single 'glmer' object'
#' @param narrow  Optional. Logical. Default = FALSE, but TRUE will exclude p-vlaues from the table to make it narrower
#' @param fit Optional: vector. quoted names of fit statistics to include, can be: "r.squared", "adj.r.squared", "sigma", "statistic","p.value", "df", "logLik", "AIC", "BIC", "deviance", "df.residual", and "nobs"
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
#'
#'library(tidyverse)
#'library(lme4)
#'library(lmerTest)
#'library("HSAUR3")
#'
#'fit1 <- glmer(outcome~treatment+(1|patientID), data=toenail,family=binomial,nAGQ=20)
#'fit2 <- glmer(outcome~treatment+visit+(1|patientID), data=toenail,family=binomial,nAGQ=20)
#'fit3 <- glmer(outcome~treatment*visit+(1|patientID), data=toenail,family=binomial,nAGQ=20)
#'
#'
#' apaSupp::gt_glmer(fit1)
#' apaSupp::gt_glmer(fit2)
#' apaSupp::gt_glmer(fit3)
#'
#'
gt_glmer <- function(x,
                     narrow          = FALSE,
                     fit             = NA,
                     d               = 2,
                     show_single_row = NULL){

  if (family(x)$link == "logit"){
    back_trans <- "exp"
    abr <- c("Odds Ratio","Logit Scale")
    sym <- c("OR", "b")
    int <- FALSE
  } else if (family(x)$family == "poisson" & family(x)$link == "log") {
    back_trans <- "exp"
    abr <- c("Incidence Rate Ratio","Log Scale")
    sym <- c("IRR", "b")
    int <- TRUE
  }

  if (narrow == FALSE){ p_fun <- function(x, d = d) apaSupp::p_num(x, d = d + 1)
  } else {              p_fun <- function(x, d = d) apaSupp::p_star(x)
  }


  table <- x %>%
    gtsummary::tbl_regression(intercept       = int,
                              conf.int        = TRUE,
                              exponentiate    = TRUE,
                              pvalue_fun      = ~ p_fun(.x, d = d),
                              tidy_fun = broom.helpers::tidy_with_broom_or_parameters,
                              show_single_row = all_of(show_single_row))


  table <- table %>%
    gtsummary::modify_column_hide(column = std.error) %>%
    gtsummary::modify_fmt_fun(estimate  ~ gtsummary::label_style_number(digits = d, rows = (row_type == "label"))) %>%
    gtsummary::modify_fmt_fun(conf.low  ~ gtsummary::label_style_number(digits = d, prefix = "[")) %>%
    gtsummary::modify_fmt_fun(conf.high ~ gtsummary::label_style_number(digits = d, suffix = "]")) %>%
    gtsummary::remove_abbreviation("CI = Confidence Interval") %>%
    gtsummary::remove_abbreviation("SE = Standard Error")

  if (family(x)$link == "logit"){
    table <- table %>%
      gtsummary::remove_abbreviation("OR = Odds Ratio")
  } else if (family(x)$family == "poisson"){
    table <- table %>%
      gtsummary::remove_abbreviation("IRR = Incidence Rate Ratio")
  }

  if (narrow == TRUE){
    table <- table %>%
      gtsummary::modify_column_merge(pattern = "{conf.low}, {conf.high}{p.value}", row = !is.na(std.error)) %>%
      gtsummary::modify_header(label    = "Variable",
                               estimate = sym[1],
                               conf.low = "[95% CI]")
  } else {
    table <- table %>%
      gtsummary::modify_header(label    = "Variable",
                               estimate = sym[1],
                               conf.low = "[95% CI]",
                               p.value  = "p")
  }

  return(table)
}
