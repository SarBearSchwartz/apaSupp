#' APA: flextable for a GEE models (specifically logisitic right now)
#'
#' @param x REQUIRED: a gee models, bare name
#' @param var_labels Optional: Vector. Text replacements for model terms, "old" = "new"
#' @param clusters Optional: Text. name of macrounits, default = "participants"
#' @param obs Optional: Text. name of microunits, default = "observations"
#' @param caption Optional: Text. Caption for the table
#' @param docx Optional: filename. must end with ".docx"
#' @param general_note Optional: Text. General note for footer of APA table
#' @param p_note Optional: Text. (default = NULL) Significance note for APA table, If `p_note = "apa123"` then the standard `"* p < .05. ** p < .01. *** p < .001."` will be used
#' @param no_notes REQUIRED: Logical.  Defaults to `FALSE`, if `TRUE` will ignore `general_note` and `p_note`
#' @param d Optional: Number. Digits after the decimal place
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
#'
#'library(HSAUR)
#'library(tidyverse)
#'library(geepack)
#'
#'data("respiratory", package = "HSAUR")
#'
#'data_wide <- respiratory %>%
#'  tidyr::spread(key = month,
#'              value = status,
#'              sep = "_") %>%
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
#'tab_gee(resp_geeglm_ex)
#'
#'
#'resp_geeglm2_ex <- geepack::geeglm(status ~ treatment + sex + BL_status,
#'                                   data = data_long,
#'                                   family = binomial(link = "logit"),
#'                                   id = subject,
#'                                   waves = month,
#'                                   corstr = "exchangeable")
#'
#'tab_gee(resp_geeglm2_ex)
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
#'tab_gee(btb_geeglm_ex_1)
#'
#'
tab_gee <- function(x,
                    var_labels      = NULL,
                    obs             = "observations",
                    clusters        = "participants",
                    caption         = "Parameter Estimates for GEE",
                    docx            = NA,
                    general_note    = NA,
                    p_note          = "apa123",
                    no_notes        = FALSE,
                    d               = 2,
                    show_single_row = NULL,
                    breaks          = c(.05, .01, .001),
                    symbols         = c("*", "**", "***")){



  if (family(x)$link == "logit"){
    back_trans <- "exp"
    abr <- c("Odds Ratio","Logit Scale")
    sym <- c("OR", "b")
    int <- FALSE
  } else if (family(x)$family == "poisson" & family(x)$link == "log") {
    back_trans <- "exp"
    abr <- c("Incident Rate Ratio","Log Scale")
    sym <- c("IRR", "b")
    pr2 <- "nagelkerke"
    int <- TRUE
  } else if (family(x)$link == "identity") {
    back_trans <- "id"
    abr <- c(NA, NA)
    sym <- c(NA, "b")
    int <- TRUE
  }

  n_clust <- length(unique(x$id))
  n_obs   <- length(x$resid)

  main_note <- flextable::as_paragraph(
    flextable::as_i("Note. "),
    flextable::as_chunk(glue::glue("N = {n_obs} {obs} on {n_clust} {clusters}. Correlation structure: {x$corstr}")),
    flextable::as_chunk(general_note)
  )

  if (back_trans == "exp"){
    get_tran <- x %>%
      gtsummary::tbl_regression(intercept = int,
                                conf.int = TRUE,
                                exponentiate = TRUE,
                                tidy_fun = broom.helpers::tidy_with_broom_or_parameters,
                                show_single_row = all_of(show_single_row)) %>%
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
      gtsummary::modify_table_body(~.x %>% dplyr::mutate(bk = NA)) %>%
      gtsummary::modify_header(label = "",
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
    gtsummary::modify_fmt_fun(estimate  ~ gtsummary::label_style_number(digits = d)) %>%
    gtsummary::modify_fmt_fun(std.error ~ gtsummary::label_style_number(digits = d, prefix = "(", suffix = ")")) %>%
    gtsummary::modify_header(std.error = "(SE)")

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
                             bk       = "blank")

  get_sig <- get %>%
    gtsummary::modify_column_hide(column = estimate) %>%
    gtsummary::modify_column_hide(column = std.error)



  if (family(x)$link  == "logit"){
    table <- list(get_tran,  get_orig,  get_sig) %>%
      gtsummary::tbl_merge(tab_spanner = c(abr[1],  abr[2],  "p")) %>%
      gtsummary::as_flex_table() %>%
      flextable::compose(part = "header", j = c(4, 7), value = flextable::as_paragraph(NA)) %>%
      flextable::compose(part = "body",   j = c(4, 7), value = flextable::as_paragraph(NA))
  } else if (family(x)$family == "poisson"){
    table <- list(get_tran,  get_orig,  get_sig) %>%
      gtsummary::tbl_merge(tab_spanner = c(abr[1],  abr[2],  "p")) %>%
      gtsummary::as_flex_table() %>%
      flextable::compose(part = "header", j = c(4, 7), value = flextable::as_paragraph(NA)) %>%
      flextable::compose(part = "body",   j = c(4, 7), value = flextable::as_paragraph(NA))
  } else {
    table <- get %>%
      gtsummary::as_flex_table()
  }


  if(!is.null(var_labels)){
    table <- table %>%
      flextable::labelizor(part = "body", labels = var_labels)
  }

  n_col  <- flextable::ncol_keys(table)
  n_rows <- flextable::nrow_part(table, part = "body")


  if (family(x)$link != "identity"){
  table <- table %>%
    flextable::compose(part = "header", i = 1, j = 8, value = flextable::as_paragraph(NA)) %>%
    flextable::compose(part = "header", i = 2, j = 8, value = flextable::as_paragraph(flextable::as_i("p")))


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
    flextable::autofit()
  } else {
    n_rows <- flextable::nrow_part(table, part = "body")

    table <- table  %>%
      flextable::delete_rows(part = "header", i = 1) %>%
      flextable::add_header_row(values    = c(NA,  "b", "(SE)", "p"),
                                colwidths = c( 1,   1, 1, 1)) %>%
      apaSupp::theme_apa(caption      = caption,
                         main_note    = main_note,
                         p_note       = p_note,
                         d            = d,
                         breaks       = breaks,
                         symbols      = symbols) %>%
      flextable::italic(part = "header", i = 1) %>%
      flextable::align( part = "all",           j = 2, align = "right") %>%
      flextable::align( part = "all",           j = 3, align = "left") %>%
      flextable::align( part = "footer",               align = "left") %>%
      flextable::autofit()
  }

  if (!is.na(docx)){
    flextable::save_as_docx(table,
                            path = docx)
  }

  return(table)
}




library(HSAUR)
library(tidyverse)
library(geepack)

data("respiratory", package = "HSAUR")

data_wide <- respiratory %>%
  tidyr::spread(key = month,
              value = status,
              sep = "_") %>%
  dplyr::rename("BL_status" = "month_0") %>%
  dplyr::arrange(subject) %>%
  dplyr::select(subject, centre,
                sex, age, treatment,
                BL_status, starts_with("month"))

data_long <- data_wide%>%
  tidyr::gather(key = month,
                value = status,
                starts_with("month")) %>%
  dplyr::mutate(month = str_sub(month, start = -1) %>% as.numeric) %>%
  dplyr::mutate(status = case_when(status == "poor" ~ 0,
                                   status == "good" ~ 1)) %>%
  dplyr::arrange(subject, month) %>%
  dplyr::select(subject, centre, sex, age, treatment, BL_status, month, status)


resp_geeglm_ex <- geepack::geeglm(status ~ centre + treatment + sex + BL_status +
                                    I(age-33) + I((age-33)^2),
                                  data = data_long,
                                  family = binomial(link = "logit"),
                                  id = subject,
                                  waves = month,
                                  corstr = "exchangeable")

gt_gee(resp_geeglm_ex)


resp_geeglm2_ex <- geepack::geeglm(status ~ treatment + sex + BL_status,
                                   data = data_long,
                                   family = binomial(link = "logit"),
                                   id = subject,
                                   waves = month,
                                   corstr = "exchangeable")

gt_gee(resp_geeglm2_ex)


data(BtheB, package = "HSAUR")

btb_wide <- BtheB %>%
  dplyr::mutate(id = row_number()) %>%           # create a new variable to ID participants
  dplyr::select(id, treatment,                    # specify that ID variable is first
                drug, length,
                bdi.pre, bdi.2m, bdi.4m, bdi.6m, bdi.8m)

btb_long <- btb_wide %>%
  tidyr::pivot_longer(cols = c(bdi.2m, bdi.4m, bdi.6m, bdi.8m),  # all existing variables (not quoted)
                      names_to = "month",
                      names_pattern = "bdi.(.)m",
                      values_to = "bdi") %>%
  dplyr::mutate(month = as.numeric(month)) %>%
  dplyr::filter(complete.cases(id, bdi, treatment, month)) %>%
  dplyr::arrange(id, month) %>%
  dplyr::select(id, treatment, drug, length, bdi.pre, month, bdi)

btb_geeglm_ex_1 <- geepack::geeglm(bdi ~ bdi.pre*length + drug + treatment + month,
                                   data = btb_long,
                                   id = id,
                                   wave = month,
                                   family = gaussian,
                                   corstr = 'exchangeable')


gt_gee(btb_geeglm_ex_1)


