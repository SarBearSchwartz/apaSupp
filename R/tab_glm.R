#' APA: flextable for a GLM models (specifically logisitc right now)
#'
#' @param x REQUIRED: a glm models, bare name
#' @param var_labels Optional: Vector. Text replacements for model terms, "old" = "new"
#' @param type Optional: default = "logistic", more to come soon...
#' @param caption Optional: Text. Caption for the table
#' @param p_note Optional: Text. (default = NULL) Significance note for APA table, If p_note = "apa" then the standard "* p < .05. ** p < .01. *** p < .001." will be used
#' @param general_note Optional: Text. General note for footer of APA table
#' @param fit Optional: Text. fit statistics:  "nobs", null.deviance", "df.null", "deviance", "df.residual", "logLik", "AIC", "BIC"
#' @param d Optional: Number. Digits after the decimal place
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
#' mtcars <- mtcars %>% dplyr::mutate(cyl = factor(cyl))
#'
#' fit_glm2 <- glm(vs ~ wt + mpg + cyl, data = mtcars, family = "binomial")
#'
#' apaSupp::tab_glm(fit_glm2)
#'
tab_glm <- function(x,
                    var_labels = NULL,
                    caption = "Generalized Regression Model",
                    p_note = "apa",
                    general_note = NA,
                    fit = c("null.deviance",
                            "deviance"),
                    d = 2,
                    vif = TRUE){

  n_obs   <- length(x$resid)
  n_param <- length(coef(x))
  n_fit   <- length(fit)

  if (n_param < 2) {vif <- FALSE}


  if (family(x)$link == "logit"){
    back_trans <- "exp"
    abr1 <- "Odds Ratio"
    abr2 <- "Logit Scale"
  }


  if (vif == TRUE){
    main_note <- flextable::as_paragraph(
      flextable::as_i("Note. "),
      flextable::as_chunk(glue::glue("N = {n_obs}. ")),
      "CI = confidence interval; VIF = variance inflation factor; ",
      flextable::as_i("p"),
      " = significance from Wald t-test for parameter estimate. ",
      flextable::as_chunk(general_note)
    )
  } else {
    main_note <- flextable::as_paragraph(
      flextable::as_i("Note. "),
      flextable::as_chunk(glue::glue("N = {length(x$resid)}. ")),
      "CI = confidence interval; ",
      flextable::as_i("p"),
      " = significance from Wald t-test for parameter estimate. ",
      flextable::as_chunk(general_note)
    )
  }


  if (is.null(p_note)){
    p_note <- NULL
  } else if (p_note == "apa"){
    p_note <- "* p < .05. ** p < .01. *** p < .001."
  } else {
    p_note <- p_note
  }


  if (back_trans == "exp"){
    get_tran <- x %>%
      gtsummary::tbl_regression(intercept = TRUE,
                                conf.int = TRUE,
                                exponentiate = TRUE,
                                tidy_fun = broom.helpers::tidy_with_broom_or_parameters) %>%
      gtsummary::add_glance_table(include = fit) %>%
      gtsummary::modify_column_hide(column = std.error) %>%
      gtsummary::modify_column_hide(column = p.value) %>%
      gtsummary::modify_fmt_fun(estimate ~
                                  gtsummary::label_style_number(digits = d)) %>%
      gtsummary::modify_fmt_fun(conf.low ~
                                  gtsummary::label_style_number(digits = d,
                                                                prefix = "[")) %>%
      gtsummary::modify_fmt_fun(conf.high ~
                                  gtsummary::label_style_number(digits = d,
                                                                suffix = "]")) %>%
      gtsummary::remove_abbreviation("OR = Odds Ratio") %>%
      gtsummary::remove_abbreviation("CI = Confidence Interval") %>%
      gtsummary::remove_abbreviation("SE = Standard Error")  %>%
      gtsummary::modify_header(label = "Variable",
                               estimate = "OR",
                               conf.low = "95% CI")
  }


  get <- x %>%
    gtsummary::tbl_regression(intercept = TRUE,
                              conf.int = FALSE,
                              exponentiate = FALSE,
                              pvalue_fun = apaSupp::p_num,
                              tidy_fun = broom.helpers::tidy_with_broom_or_parameters) %>%
    gtsummary::modify_column_unhide(column = std.error) %>%
    gtsummary::remove_abbreviation("OR = Odds Ratio") %>%
    # gtsummary::remove_abbreviation("CI = Confidence Interval") %>%
    gtsummary::remove_abbreviation("SE = Standard Error")  %>%
    gtsummary::remove_footnote_header() %>%
    gtsummary::modify_fmt_fun(estimate ~
                                gtsummary::label_style_number(digits = d)) %>%
    gtsummary::modify_fmt_fun(std.error ~
                                gtsummary::label_style_number(digits = d,
                                                              prefix = "(",
                                                              suffix = ")")) %>%
    gtsummary::modify_header(label = "Variable",
                             estimate = "b",
                             std.error = "(SE)",
                             p.value = "p") %>%
    gtsummary::modify_table_body(dplyr::left_join,
                                 apaSupp::vif_to_tibble(x, d = d),
                                 by = c("variable", "row_type")) %>%
    gtsummary::modify_header(vif ~ "VIF")



  get_orig <- get %>%
    gtsummary::modify_column_hide(column = p.value) %>%
    gtsummary::modify_column_hide(column = vif)

  get_end <- get %>%
    gtsummary::modify_column_hide(column = estimate) %>%
    gtsummary::modify_column_hide(column = std.error)


  if (vif == FALSE){
    get_end <- get_end %>%
      gtsummary::modify_column_hide(column = vif)
    col_end <- 6
  } else {
    col_end <- 6:7
  }


  table <- list(get_tran, get_orig, get_end) %>%
    gtsummary::tbl_merge(tab_spanner = c(abr1, abr2, "end")) %>%
    gtsummary::as_flex_table()



  n_rows <- flextable::nrow_part(table, part = "body")

  rows_fit <- (n_rows - n_fit + 1):(n_rows)



  table <- table %>%
    apaSupp::theme_apa(caption = caption,
                       no_notes = FALSE,
                       d = d) %>%
    flextable::hline(i = n_rows - n_fit) %>%
    flextable::italic(part = "body", i = rows_fit)


  n_rows <- flextable::nrow_part(table, part = "body")

  n_fit <- length(fit)

  rows_fit <- (n_rows - n_fit + 1):(n_rows)



  if(!is.null(var_labels)){
    table <- table %>%
      flextable::labelizor(part = "body",
                           labels = var_labels)
  }

  table <- table %>%
    flextable::align(part = "all", j = c(2, 4), align = "right") %>%
    flextable::align(part = "all", j = c(3, 5), align = "left") %>%
    flextable::align(part = "header", i = 2, j = 3, align = "center") %>%
    flextable::align(part = "header", i = 1, align = "center") %>%
    flextable::italic(part = "header", i = 2, j = 4:6) %>%
    flextable::align(part = "footer", align = "left") %>%
    flextable::compose(part = "header", i = 1, j = col_end,
                       value = flextable::as_paragraph(NA)) %>%
    flextable::compose(part = "body", i = 1, j = 2:3,
                       value = flextable::as_paragraph(NA)) %>%
    flextable::hline(part = "header", i = 1,
                     border = flextable::fp_border_default(width = 0)) %>%
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
