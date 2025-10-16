#' APA: gtsummary for a single GEE model
#'
#' @param x REQUIRED: bare name. a single 'geeglm' object'
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
#'library(HSAUR)
#'library(tidyverse)
#'library(geepack)
#'
#'data("respiratory", package = "HSAUR")
#'
#'data_wide <- respiratory %>%
#'  tidyr::spread(key = month,
#'                value = status,
#'                sep = "_") %>%
#'  dplyr::rename("BL_status" = "month_0") %>%
#'  dplyr::arrange(subject) %>%
#'  dplyr::select(subject, centre,
#'                sex, age, treatment,
#'                BL_status, starts_with("month"))
#'
#'data_long <- data_wide%>%
#'  tidyr::gather(key = month,
#'                value = status,
#'                starts_with("month")) %>%
#'  dplyr::mutate(month = str_sub(month, start = -1) %>% as.numeric) %>%
#'  dplyr::mutate(status = case_when(status == "poor" ~ 0,
#'                                   status == "good" ~ 1)) %>%
#'  dplyr::arrange(subject, month) %>%
#'  dplyr::select(subject, centre, sex, age, treatment, BL_status, month, status)
#'
#'
#'resp_geeglm_ex <- geepack::geeglm(status ~ centre + treatment + sex + BL_status +
#'                                    I(age-33) + I((age-33)^2),
#'                                  data = data_long,
#'                                  family = binomial(link = "logit"),
#'                                  id = subject,
#'                                  waves = month,
#'                                  corstr = "exchangeable")
#'
#'gt_gee(resp_geeglm_ex)
#'
#'
#'resp_geeglm2_ex <- geepack::geeglm(status ~ treatment + sex + BL_status,
#'                                   data = data_long,
#'                                   family = binomial(link = "logit"),
#'                                   id = subject,
#'                                   waves = month,
#'                                   corstr = "exchangeable")
#'
#'gt_gee(resp_geeglm2_ex, narrow = TRUE)
#'
#'
#'data(BtheB, package = "HSAUR")
#'
#'btb_wide <- BtheB %>%
#'  dplyr::mutate(id = row_number()) %>%           # create a new variable to ID participants
#'  dplyr::select(id, treatment,                    # specify that ID variable is first
#'                drug, length,
#'                bdi.pre, bdi.2m, bdi.4m, bdi.6m, bdi.8m)
#'
#'btb_long <- btb_wide %>%
#'  tidyr::pivot_longer(cols = c(bdi.2m, bdi.4m, bdi.6m, bdi.8m),  # all existing variables (not quoted)
#'                      names_to = "month",
#'                      names_pattern = "bdi.(.)m",
#'                      values_to = "bdi") %>%
#'  dplyr::mutate(month = as.numeric(month)) %>%
#'  dplyr::filter(complete.cases(id, bdi, treatment, month)) %>%
#'  dplyr::arrange(id, month) %>%
#'  dplyr::select(id, treatment, drug, length, bdi.pre, month, bdi)
#'
#'btb_geeglm_ex_1 <- geepack::geeglm(bdi ~ bdi.pre*length + drug + treatment + month,
#'                                   data = btb_long,
#'                                   id = id,
#'                                   wave = month,
#'                                   family = gaussian,
#'                                   corstr = 'exchangeable')
#'
#'
#'gt_gee(btb_geeglm_ex_1)
#'gt_gee(btb_geeglm_ex_1, narrow = TRUE)
#'
gt_gee <- function(x,
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
    pr2 <- "nagelkerke"
    int <- TRUE
  } else if (family(x)$link == "identity") {
    back_trans <- "id"
    abr <- c(NA, NA)
    sym <- c(NA, "b")
    int <- TRUE
  }

  if (narrow == FALSE){ p_fun <- function(x, d = d) apaSupp::p_num(x, d = d + 1)
  } else {              p_fun <- function(x, d = d) apaSupp::p_star(x)
  }


  if (family(x)$link != "identity"){

    table <- x %>%
      gtsummary::tbl_regression(intercept       = int,
                                conf.int        = TRUE,
                                exponentiate    = TRUE,
                                pvalue_fun      = ~ p_fun(.x, d = d),
                                tidy_fun        = broom.helpers::tidy_with_broom_or_parameters,
                                show_single_row = all_of(show_single_row)) %>%
      gtsummary::modify_column_hide(column = std.error) %>%
      gtsummary::modify_fmt_fun(estimate  ~ gtsummary::label_style_number(digits = d, rows = (row_type == "label"))) %>%
      gtsummary::modify_fmt_fun(conf.low  ~ gtsummary::label_style_number(digits = d, prefix = "[")) %>%
      gtsummary::modify_fmt_fun(conf.high ~ gtsummary::label_style_number(digits = d, suffix = "]")) %>%
      gtsummary::remove_abbreviation("CI = Confidence Interval") %>%
      gtsummary::remove_abbreviation("SE = Standard Error") %>%
      gtsummary::modify_table_body(~.x %>%
                                     dplyr::mutate(estimate = ifelse(variable == "(Intercept)" & int == FALSE, NA, estimate)) %>%
                                     dplyr::mutate(conf.low = ifelse(variable == "(Intercept)" & int == FALSE, NA, conf.low))) %>%
      gtsummary::modify_header(label = "",
                               estimate = sym[1],
                               conf.low = "95% CI")


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
        gtsummary::modify_header(label    = "",
                                 estimate = sym[1],
                                 conf.low = "95% CI")
    } else {
      table <- table %>%
        gtsummary::modify_header(label    = "",
                                 estimate = sym[1],
                                 conf.low = "95% CI",
                                 p.value  = "p")
    }
  } else if(family(x)$link == "identity"){

    table <- x %>%
      gtsummary::tbl_regression(intercept       = int,
                                conf.int        = TRUE,
                                exponentiate    = FALSE,
                                pvalue_fun      = ~ p_fun(.x, d = d),
                                tidy_fun        = broom.helpers::tidy_with_broom_or_parameters,
                                show_single_row = all_of(show_single_row)) %>%
      gtsummary::modify_column_hide(column = std.error) %>%
      gtsummary::modify_fmt_fun(estimate  ~ gtsummary::label_style_number(digits = d, rows = (row_type == "label"))) %>%
      gtsummary::modify_fmt_fun(conf.low  ~ gtsummary::label_style_number(digits = d, prefix = "[")) %>%
      gtsummary::modify_fmt_fun(conf.high ~ gtsummary::label_style_number(digits = d, suffix = "]")) %>%
      gtsummary::remove_abbreviation("CI = Confidence Interval") %>%
      gtsummary::remove_abbreviation("SE = Standard Error") %>%
      gtsummary::modify_table_body(~.x %>%
                                     dplyr::mutate(estimate = ifelse(variable == "(Intercept)" & int == FALSE, NA, estimate)) %>%
                                     dplyr::mutate(conf.low = ifelse(variable == "(Intercept)" & int == FALSE, NA, conf.low)))

    if (narrow == TRUE){
      table <- table %>%
        gtsummary::modify_column_merge(pattern = "{conf.low}, {conf.high}{p.value}", row = !is.na(std.error)) %>%
        gtsummary::modify_header(label    = "",
                                 estimate = "b",
                                 conf.low = "95% CI")
    } else {
      table <- table %>%
        gtsummary::modify_header(label    = "",
                                 estimate = "b",
                                 conf.low = "95% CI",
                                 p.value  = "p")
    }
  }

  return(table)
}









