#' APA: flextable for ONElinear models
#'
#' @param model REQUIRED: 1 lm model, bare name
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
#' @import purrr
#' @import magrittr
#' @import tidyverse
#' @import broom.helpers
#' @import DescTools
#' @export
#'
#' @examples
#'
#' m <- lm(dist ~ speed, cars)
#' tab_lm(m)
#'

tab_lm <- function(model,
                   var_labels = NULL,
                   caption = "Regression Model",
                   p_note = "apa",
                   general_note = NULL,
                   fit = c("r.squared",
                           "adj.r.squared"),
                   d = 2,
                   std = TRUE,
                   vif = FALSE,
                   eta2 = TRUE){

  n_param <- model %>%
    coef() %>%
    length()

  n_fit <- length(fit)

  get <- model %>%
    gtsummary::tbl_regression(intercept = TRUE,
                              conf.int = FALSE,
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
    gtsummary::modify_header(label = "Variable",
                             estimate = "b",
                             std.error = "(SE)",
                             p.value = "p")

  if(std == TRUE){
    get <- get %>%
      modify_table_body(dplyr::left_join,
                        bstd_to_tibble(model, d = d),
                        by = c("variable", "row_type")) %>%
      modify_header(bs ~ "bs")

    general_note <- paste(general_note, "b\U002A = standardized estimate.")
  }

  if(vif == TRUE){
    get <- get %>%
      modify_table_body(dplyr::left_join,
                        vif_to_tibble(model, d = d),
                        by = c("variable", "row_type")) %>%
      modify_header(vif ~ "VIF")

    general_note <- paste(general_note, "VIF = variance inflation factor.")
  }

  if(eta2 == TRUE){
    get <- get %>%
      modify_table_body(dplyr::left_join,
                        eta2_to_tibble(model),
                        by = c("variable", "row_type")) %>%
      modify_header(eta.sq ~ "n2",
                    eta.sq.part ~ "n2p")

    general_note <- paste(general_note,
                          "\U1D702\U00B2 = semi-partial correlation.",
                          "\U1D702\U209A\U00B2 = partial correlation.")
  }

  table <- get %>%
    gtsummary::as_flex_table()

  n_rows <- flextable::nrow_part(table, part = "body")

  rows_fit <- (n_rows - n_fit + 1):(n_rows)

  table <- table %>%
    apaSupp::theme_apa(caption = caption,
                       p_note = p_note,
                       general_note = general_note,
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
                       value = flextable::as_paragraph("\U1D702\U00B2")) %>%
    flextable::compose(part = "header",
                       j = "eta.sq.part",
                       value = flextable::as_paragraph("\U1D702\U209A\U00B2"))

  }

  if(!is.null(var_labels)){
    table <- table %>%
      flextable::labelizor(part = "body",
                           labels = var_labels)
  }

  return(table)
}




bstd_to_tibble <- function(model, d = 2) {

  result <- model %>%
    parameters::standardise_parameters() %>%
    as.data.frame() %>%
    dplyr::filter(Parameter != "(Intercept)") %>%
    dplyr::select("variable" = "Parameter",
                  "bs" = "Std_Coefficient") %>%
    dplyr::mutate(row_type = "label")%>%
    dplyr::mutate(across(c(bs),
                         ~ MOTE::apa(value = .,
                                     decimals = d,
                                     leading = TRUE)))

  return(result)
}




vif_to_tibble <- function(model, d = 2) {

  vif <- car::vif(model)

  if (!is.matrix(vif)){
    result <- vif %>%
      tibble::enframe("variable", "vif")
  } else {
    result <- vif %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "variable") %>%
      tibble::as_tibble() %>%
      dplyr::select(variable, vif = GVIF)
  }

  result <- result %>%
    dplyr::mutate(row_type = "label")%>%
    dplyr::mutate(vif = MOTE::apa(value = vif,
                                  decimals = d,
                                  leading = TRUE))

  return(result)
}



eta2_to_tibble <- function(model) {
  eta2 <- DescTools::EtaSq(model)

  if (!is.matrix(eta2)){
    result <- eta2 %>%
      enframe("variable", "eta.sq", "eta.sq.part")
  } else {
    result <- eta2 %>%
      as.data.frame() %>%
      rownames_to_column(var = "variable") %>%
      as_tibble()
  }

  result <- result %>%
    dplyr::mutate(row_type = "label")%>%
    dplyr::mutate(across(c(eta.sq, eta.sq.part),
                  ~ MOTE::apa(value = .,
                                  decimals = 3,
                                  leading = FALSE)))

  return(result)
}
