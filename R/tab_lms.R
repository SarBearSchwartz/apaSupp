#' APA: flextable for 2-3 linear models
#'
#' @param x REQUIRED: List. at least 2 lm models, bare names
#' @param mod_names Optional: Vector. Text names for each model
#' @param var_labels Optional: Vector. Text replacements for model terms, "old" = "new"
#' @param caption Optional: Text. Caption for the table
#' @param p_note Optional: Text. (default = NULL) Significance note for APA table, If p_note = "apa" then the standard "* p < .05. ** p < .01. *** p < .001." will be used
#' @param general_note Optional: Text. General note for footer of APA table
#' @param d Optional: Number. Digits after the decimal place
#'
#' @returns a flextable object
#' @import gtsummary
#' @import purrr
#' @import tidyverse
#' @import broom.helpers
#' @export
#'
#' @examples
#'
#' m1 <- lm(dist ~ 1, cars)
#' m2 <- lm(dist ~ speed, cars)
#' tab_lms(list(m1, m2))
#'
tab_lms <- function(x, # list of model names
                    mod_names = NA,
                    var_labels = NULL,
                    caption = "Compare Regression Models",
                    p_note = "apa",
                    general_note = NULL,
                    d = 2){

  n_iv <- x %>%
    purrr::map(coef) %>%
    purrr::map(length) %>%
    unlist() %>%
    max()

  n_models <- length(x)

  if(length(mod_names) != length(x)){
    mod_names <- paste("Model", 1:n_models)
  }

  table <- x %>%
    purrr::map(gt_lm, d) %>%
    gtsummary::tbl_merge(tab_spanner = mod_names) %>%
    gtsummary::modify_table_body(~ .x %>%
                                   dplyr::arrange(row_type == "glance_statistic")) %>%
    gtsummary::as_flex_table() %>%
    apaSupp::theme_apa(caption = caption,
                       p_note = p_note,
                       general_note = general_note) %>%
    flextable::hline(i = n_iv) %>%
    flextable::bold(part = "header", i = 1) %>%
    flextable::italic(part = "header", i = 2) %>%
    flextable::italic(part = "body", i = c(n_iv+1, n_iv+2))

  if(!is.null(var_labels)){
    table <- table %>%
      flextable::labelizor(part = "body",
                           labels = var_labels)
  }

  return(table)

}
