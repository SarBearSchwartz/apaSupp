#' APA: flextable for 2-3 generalized linear models
#'
#' @param x REQUIRED: List. at least 2 glm models, bare names
#' @param var_labels Optional: Vector. Text replacements for model terms, "old" = "new"
#' @param caption Optional: Text. Caption for the table
#' @param narrow  Optional. Logical. Default = FALSE, but TRUE will exclude p-vlaues from the table to make it narrower
#' @param p_note Optional: Text. (default = NULL) Significance note for APA table, If p_note = "apa" then the standard "* p < .05. ** p < .01. *** p < .001." will be used
#' @param general_note Optional: Text. General note for footer of APA table
#' @param fit Optional: vector. quoted names of fit statistics to include, can be: "r.squared", "adj.r.squared", "sigma", "statistic","p.value", "df", "logLik", "AIC", "BIC", "deviance", "df.residual", and "nobs"
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
#' library(tidyverse)
#'
#' mtcars <- mtcars %>% dplyr::mutate(cyl = factor(cyl))
#'
#' fit_glm1 <- glm(vs ~ wt, data = mtcars, family = "binomial")
#' fit_glm2 <- glm(vs ~ wt + mpg + cyl, data = mtcars, family = "binomial")
#'
#' apaSupp::tab_glms(list(fit_glm1, fit_glm2))
#' apaSupp::tab_glms(list("M1" = fit_glm1, "M2" = fit_glm2))
#' apaSupp::tab_glms(list("M1" = fit_glm1, "M2" = fit_glm2), narrow = TRUE)
#' apaSupp::tab_glms(list("M1" = fit_glm1, "M2" = fit_glm2), narrow = TRUE, fit = c("AIC", "BIC"))
#'
#'
tab_glms <- function(x,
                    var_labels = NULL,
                    caption = "Compare Regression Models",
                    narrow = FALSE,
                    p_note = "apa",
                    general_note = NULL,
                    fit = NA,
                    d = 2){



  n_param <- x %>%
    purrr::map(coef) %>%
    purrr::map(length) %>%
    unlist() %>%
    max()

  n_models <- length(x)


  n_fit   <- sum(!is.na(fit))

  if(is.null(names(x))){
    mod_names <- paste("Model", 1:n_models)
  }else{
    mod_names <- names(x)
  }

  get <- x %>%
    purrr::map(apaSupp::gt_glm,
               narrow = narrow,
               fit = fit,
               d = d) %>%
    gtsummary::tbl_merge(tab_spanner = mod_names) %>%
    gtsummary::modify_table_body(~ .x %>%
                                   dplyr::arrange(row_type == "glance_statistic"))

  table <- get %>%
    gtsummary::as_flex_table()

  n_rows <- flextable::nrow_part(table, part = "body")

  table <- table %>%
    apaSupp::theme_apa(caption = caption,
                       p_note = p_note,
                       general_note = general_note) %>%
    flextable::bold(part = "header", i = 1) %>%
    flextable::italic(part = "header", i = 2) %>%
    flextable::align(part = "header", i = 1, align = "center")

  if(n_fit > 0){
    table <- table %>%
      flextable::italic(part = "body", i = (n_rows + 1 - n_fit):(n_rows)) %>%
      flextable::hline(part = "body", i = (n_rows - n_fit))
  }

  n_rows <- flextable::nrow_part(table, part = "body")

  if (narrow == FALSE){
    table <- table %>%
      flextable::align(part = "all",
                       j = seq(from = 2, to = 3*n_models, by = 3),
                       align = "right") %>%
      flextable::align(part = "all",
                       j = seq(from = 3, to = (3*n_models + 1), by = 3),
                       align = "left")
  } else {
    table <- table %>%
      flextable::align(part = "all",
                       j = seq(from = 2, to = 2*n_models, by = 2),
                       align = "right") %>%
      flextable::align(part = "all",
                       j = seq(from = 3, to = (2*n_models + 1), by = 2),
                       align = "left")
  }

  if(!is.null(var_labels)){
    table <- table %>%
      flextable::labelizor(part = "body",
                           labels = var_labels)
  }

  table <- table %>%
    flextable::align(part = "header", i = 1, align = "center") %>%
    flextable::align(part = "footer", align = "left") %>%
    flextable::hline(part = "header", i = 1,
                     border = flextable::fp_border_default(width = 0))

  return(table)

}
