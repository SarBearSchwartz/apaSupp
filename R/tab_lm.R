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
#' @param std Optional: Logical. (default = TRUE) Include standardized coefficients?
#' @param vif Optional: Logical. (default = FALSE) Include variance inflation factors?
#' @param eta2 Optional: logical. (default = TRUE) Include eta-squared (semi-partial correlations) and partial eta-squared (partial correlations)
#' @param ci Optional: logical. (default = FALSE) Include a confidence interval for the estimated beta
#' @param show_single_row  Optional: If a variable is dichotomous (e.g. Yes/No) and you wish to print the regression coefficient on a single row, include the variable name(s) here.
#' @param breaks Optional: numeric vector of p-value cut-points
#' @param symbols Optional: character vector for symbols denoting p-value cut-points
#'
#' @returns a flextable object
#' @import gtsummary
#' @import flextable
#' @import tidyverse
#' @import broom.helpers
#' @import parameters
#' @import DescTools
#' @import car
#' @import MOTE
#' @export
#'
#' @examples
#'
#' m <- lm(mpg ~ disp + wt + hp, mtcars)
#'
#' apaSupp::tab_lm(m)
#'
#'
#'

tab_lm <- function(x,
                   var_labels      = NULL,
                   caption         = "Parameter Estimates for Linear Regression",
                   general_note    = NA,
                   p_note          = "apa123",
                   no_notes        = FALSE,
                   d               = 2,
                   fit             = c("r.squared", "adj.r.squared"),
                   std             = TRUE,
                   vif             = FALSE,
                   eta2            = TRUE,
                   ci              = FALSE,
                   show_single_row = NULL,
                   breaks          = c(.05, .01, .001),
                   symbols         = c("*", "**", "***")){


  n_param <- x %>%
    coef() %>%
    length()

  if (n_param < 2) {
    vif  <- FALSE
    eta2 <- FALSE
  }

  n_fit <- length(fit)

  main_note <- flextable::as_paragraph(
    flextable::as_i(    "Note. "),
    flextable::as_i(    "N"),
    flextable::as_chunk(glue::glue(" = {length(x$resid)}. ")),
    flextable::as_equation(ifelse(eta2 == FALSE, NA, "b^*")),
    flextable::as_chunk(   ifelse(std  == FALSE, NA, " = standardize coefficient; ")),
    flextable::as_i(       ifelse(vif  == FALSE, NA, "VIF")),
    flextable::as_chunk(   ifelse(vif  == FALSE, NA, " = variance inflation factor; ")),
    flextable::as_equation(ifelse(eta2 == FALSE, NA, "\\eta^2")),
    flextable::as_chunk(   ifelse(eta2 == FALSE, NA, " = semi-partial correlation; ")),
    flextable::as_equation(ifelse(eta2 == FALSE, NA, "\\eta^2_p")),
    flextable::as_chunk(   ifelse(eta2 == FALSE, NA, " = partial correlation; ")),
    flextable::as_i("p"),
    flextable::as_chunk(" = significance from Wald t-test for parameter estimate. "),
    flextable::as_chunk(general_note)
  )

  get <- x %>%
    gtsummary::tbl_regression(intercept = TRUE,
                              conf.int = ci,
                              pvalue_fun = function(x) apaSupp::p_num(x, d = d + 1),
                              tidy_fun = broom.helpers::tidy_with_broom_or_parameters,
                              show_single_row = show_single_row) %>%
    gtsummary::add_glance_table(include = all_of(fit)) %>%
    gtsummary::modify_column_unhide(column = std.error) %>%
    gtsummary::remove_footnote_header() %>%
    gtsummary::remove_abbreviation("SE = Standard Error")  %>%
    gtsummary::modify_fmt_fun(estimate  ~ function(x) MOTE::apa(x, decimals = d + 1, leading = FALSE),
                              rows =  stringr::str_detect(variable, "r.squared")) %>%
    gtsummary::modify_fmt_fun(estimate  ~ function(x) MOTE::apa(x, decimals = d, leading = TRUE),
                              rows = !stringr::str_detect(variable, "r.squared")) %>%
    gtsummary::modify_fmt_fun(estimate  ~ gtsummary::label_style_number(digits = d),
                              rows = (row_type %in% c("label", "level"))) %>%
    gtsummary::modify_fmt_fun(std.error ~ gtsummary::label_style_number(digits = d, prefix = "(", suffix = ")")) %>%
    gtsummary::modify_header(label     = "Variable",
                             estimate  = "b",
                             std.error = "(SE)",
                             p.value   = "p")

  if(std == TRUE){
    get <- get %>%
      gtsummary::modify_table_body(dplyr::left_join, apaSupp::bstd_to_tibble(x, d = d), by = c("variable", "row_type")) %>%
      gtsummary::modify_header(bs ~ "bs")
  }

  if(vif == TRUE){
    get <- get %>%
      gtsummary::modify_table_body(dplyr::left_join, apaSupp::vif_to_tibble(x, d = d), by = c("variable", "row_type")) %>%
      gtsummary::modify_header(vif ~ "VIF")
  }

  if(eta2 == TRUE){
    get <- get %>%
      gtsummary::modify_table_body(dplyr::left_join, apaSupp::eta2_to_tibble(x, d = d), by = c("variable", "row_type")) %>%
      gtsummary::modify_header(eta.sq      ~ "n2",
                               eta.sq.part ~ "n2p")
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


  if(!is.null(var_labels)){ table <- table %>% flextable::labelizor(part = "body", labels = var_labels)}

  n_rows <- flextable::nrow_part(table, part = "body")

  table <- table %>%
    flextable::compose(part = "header", i = 1, j = 1, value = flextable::as_paragraph(NA)) %>%
    flextable::italic( part = "header") %>%
    flextable::hline(  part = "body",  i =  n_rows - n_fit) %>%
    flextable::italic( part = "body",  i = (n_rows - n_fit + 1):(n_rows))%>%
    flextable::align(  part = "body",  i = (n_rows - n_fit + 1):(n_rows), align = "center")

  if (std == TRUE){
    table <- table %>%
      flextable::compose(part = "header",  j = "bs",
                         value = flextable::as_paragraph(flextable::as_equation("b^*")))
  }

  if (eta2 == TRUE){
    table <- table %>%
      flextable::compose(part = "header", j = "eta.sq",
                         value = flextable::as_paragraph(flextable::as_equation("\\eta^2"))) %>%
      flextable::compose(part = "header", j = "eta.sq.part",
                         value = flextable::as_paragraph(flextable::as_equation("\\eta^2_p")))
  }

  for (r in (n_rows - n_fit + 1):n_rows){
      table <- table %>%
        flextable::merge_at(i = r, j = 2:3) %>%
        flextable::align(   i = r, j = 1,   align = "left") %>%
        flextable::align(   i = r, j = 2:3, align = "center")
  }

  table <- table %>%
    flextable::width(         width = 0.50, unit = "in") %>%
    flextable::width(j = 1,   width = 1.75, unit = "in") %>%
    flextable::width(j = 2:4, width = 0.75, unit = "in")

  return(table)
}















