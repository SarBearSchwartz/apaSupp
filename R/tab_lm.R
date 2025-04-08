#' APA: flextable for ONElinear models
#'
#' @param x REQUIRED: 1 lm model, bare name
#' @param var_labels Optional: Vector. Text replacements for model terms, "old" = "new"
#' @param caption Optional: Text. Caption for the table
#' @param p_note Optional: Text. (default = NULL) Significance note for APA table, If p_note = "apa" then the standard "* p < .05. ** p < .01. *** p < .001." will be used
#' @param general_note Optional: Text. General note for footer of APA table
#' @param fit Optional: vector. quoted names of fit statistics to include, can be: "r.squared", "adj.r.squared", "sigma", "statistic","p.value", "df", "logLik", "AIC", "BIC", "deviance", "df.residual", and "nobs"
#' @param d Optional: Number. Digits after the decimal place
#' @param std Optional: Logical. (default = TRUE) Include standardized coefficients?
#' @param vif Optional: Logical. (default = FALSE) Include variance inflation factors?
#' @param eta2 Optional: logical. (default = TRUE) Include eta-squared (semi-partial correlations) and partial eta-squared (partial correlations)
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
#' m <- lm(dist ~ speed, cars)
#' tab_lm(m)
#'

tab_lm <- function(x,
                   var_labels = NULL,
                   caption = "Regression Model",
                   p_note = "apa",
                   general_note = NA,
                   fit = c("r.squared",
                           "adj.r.squared"),
                   d = 2,
                   std = TRUE,
                   vif = FALSE,
                   eta2 = TRUE){

  n_param <- x %>%
    coef() %>%
    length()

  n_fit <- length(fit)


  if (is.null(p_note)){
    p_note <- NULL
  } else if (p_note == "apa"){
    p_note <- "* p < .05. ** p < .01. *** p < .001."
  } else {
    p_note <- p_note
  }


  if (std == TRUE & vif == TRUE & eta2 == TRUE){
    main_note <- flextable::as_paragraph(flextable::as_i("Note. "),
                                         flextable::as_chunk(glue::glue("N = {length(x$resid)}. ")),
                                         flextable::as_i("p"),
                                         " = significance from Wald t-test for parameter estimate; ",
                                         flextable::as_i("b\U002A"),
                                         " = standardized estimate; VIF = variance inflation factor; ",
                                         flextable::as_i("\U03B7\U00B2"),
                                         "= semi-partial correlation; ",
                                         flextable::as_i("\U03B7\U209A\U00B2"),
                                         "= partial correlation.",
                                         flextable::as_chunk(general_note))
  } else if (std == FALSE & vif == TRUE & eta2 == TRUE){
    main_note <- flextable::as_paragraph(flextable::as_i("Note. "),
                                         flextable::as_chunk(glue::glue("N = {length(x$resid)}. ")),
                                         flextable::as_i("p"),
                                         " = significance from Wald t-test for parameter estimate; ",
                                         "VIF = variance inflation factor; ",
                                         flextable::as_i("\U03B7\U00B2"),
                                         "= semi-partial correlation; ",
                                         flextable::as_i("\U03B7\U209A\U00B2"),
                                         "= partial correlation.",
                                         flextable::as_chunk(general_note))
  } else if (std == TRUE & vif == FALSE & eta2 == TRUE){
    main_note <- flextable::as_paragraph(flextable::as_i("Note. "),
                                         flextable::as_chunk(glue::glue("N = {length(x$resid)}. ")),
                                         flextable::as_i("p"),
                                         " = significance from Wald t-test for parameter estimate; ",
                                         flextable::as_i("b\U002A"),
                                         " = standardized estimate; ",
                                         flextable::as_i("\U03B7\U00B2"),
                                         "= semi-partial correlation; ",
                                         flextable::as_i("\U03B7\U209A\U00B2"),
                                         "= partial correlation.",
                                         flextable::as_chunk(general_note))
  } else if (std == TRUE & vif == TRUE & eta2 == FALSE){
    main_note <- flextable::as_paragraph(flextable::as_i("Note. "),
                                         flextable::as_chunk(glue::glue("N = {length(x$resid)}. ")),
                                         flextable::as_i("p"),
                                         " = significance from Wald t-test for parameter estimate; ",
                                         flextable::as_i("b\U002A"),
                                         " = standardized estimate; VIF = variance inflation factor. ",
                                         flextable::as_chunk(general_note))
  } else if (std == TRUE & vif == FALSE & eta2 == FALSE){
    main_note <- flextable::as_paragraph(flextable::as_i("Note. "),
                                         flextable::as_chunk(glue::glue("N = {length(x$resid)}. ")),
                                         flextable::as_i("p"),
                                         " = significance from Wald t-test for parameter estimate; ",
                                         flextable::as_i("b\U002A"),
                                         " = standardized estimate. ",
                                         flextable::as_chunk(general_note))
  } else if (std == FALSE & vif == TRUE & eta2 == FALSE){
    main_note <- flextable::as_paragraph(flextable::as_i("Note. "),
                                         flextable::as_chunk(glue::glue("N = {length(x$resid)}. ")),
                                         flextable::as_i("p"),
                                         " = significance from Wald t-test for parameter estimate; ",
                                         "VIF = variance inflation factor.",
                                         flextable::as_chunk(general_note))
  } else if (std == FALSE & vif == FALSE & eta2 == TRUE){
    main_note <- flextable::as_paragraph(flextable::as_i("Note. "),
                                         flextable::as_chunk(glue::glue("N = {length(x$resid)}. ")),
                                         flextable::as_i("p"),
                                         " = significance from Wald t-test for parameter estimate; ",
                                         flextable::as_i("\U03B7\U00B2"),
                                         "= semi-partial correlation; ",
                                         flextable::as_i("\U03B7\U209A\U00B2"),
                                         "= partial correlation.",
                                         flextable::as_chunk(general_note))
  } else {
    main_note <- flextable::as_paragraph(flextable::as_i("Note. "),
                                         flextable::as_chunk(glue::glue("N = {length(x$resid)}. ")),
                                         flextable::as_i("p"),
                                         " = significance from Wald t-test for parameter estimate. ",
                                         flextable::as_chunk(general_note))
  }

  get <- x %>%
    gtsummary::tbl_regression(intercept = TRUE,
                              conf.int = FALSE,
                              pvalue_fun = apaSupp::p_num,
                              tidy_fun = broom.helpers::tidy_with_broom_or_parameters) %>%
    gtsummary::add_glance_table(include = fit) %>%
    gtsummary::modify_column_unhide(column = std.error) %>%
    gtsummary::remove_footnote_header() %>%
    gtsummary::remove_abbreviation("SE = Standard Error")  %>%
    gtsummary::modify_fmt_fun(estimate ~
                                gtsummary::label_style_number(digits = d)) %>%
    gtsummary::modify_fmt_fun(std.error ~
                                gtsummary::label_style_number(digits = d,
                                                              prefix = "(",
                                                              suffix = ")")) %>%
    gtsummary::modify_header(label = "Variable",
                             estimate = "b",
                             std.error = "(SE)",
                             p.value = "p")

  if(std == TRUE){
    get <- get %>%
      gtsummary::modify_table_body(dplyr::left_join,
                                   bstd_to_tibble(x, d = d),
                                   by = c("variable", "row_type")) %>%
      gtsummary::modify_header(bs ~ "bs")
  }

  if(vif == TRUE){
    get <- get %>%
      gtsummary::modify_table_body(dplyr::left_join,
                                   vif_to_tibble(x, d = d),
                                   by = c("variable", "row_type")) %>%
      gtsummary::modify_header(vif ~ "VIF")
  }

  if(eta2 == TRUE){
    get <- get %>%
      gtsummary::modify_table_body(dplyr::left_join,
                                   eta2_to_tibble(x),
                                   by = c("variable", "row_type")) %>%
      gtsummary::modify_header(eta.sq ~ "n2",
                               eta.sq.part ~ "n2p")
  }

  table <- get %>%
    gtsummary::as_flex_table()

  n_rows <- flextable::nrow_part(table, part = "body")

  rows_fit <- (n_rows - n_fit + 1):(n_rows)

  table <- table %>%
    apaSupp::theme_apa(caption = caption,
                       no_notes = FALSE,
                       d = d) %>%
    flextable::hline(i = n_rows - n_fit) %>%
    flextable::italic(part = "header", i = 1) %>%
    flextable::italic(part = "body", i = rows_fit)

  if (std == TRUE){
    table <- table %>%
      flextable::compose(part = "header",
                         j = "bs",
                         value = flextable::as_paragraph("b\U002A"))
  }


  if (eta2 == TRUE){
    table <- table %>%
      flextable::compose(part = "header",
                         j = "eta.sq",
                         value = flextable::as_paragraph("\u03B7\U00B2")) %>%
      flextable::compose(part = "header",
                         j = "eta.sq.part",
                         value = flextable::as_paragraph("\u03B7\u209A\U00B2"))

  }

  if(!is.null(var_labels)){
    table <- table %>%
      flextable::labelizor(part = "body",
                           labels = var_labels)
  }

  table <- table %>%
    flextable::align(part = "all", j = 2, align = "right") %>%
    flextable::align(part = "all", j = 3, align = "left") %>%
    flextable::align(part = "footer", align = "left") %>%
    flextable::add_footer_lines("") %>%
    flextable::compose(i = 1, j = 1,
                       value = main_note,
                       part = "footer")


  if (!is.null(p_note)){
    table <- table %>%
      flextable::add_footer_lines("") %>%
      flextable::compose(i = 2, j = 1,
                         value = flextable::as_paragraph(flextable::as_chunk(p_note)),
                         part = "footer")
  }


  return(table)
  }















