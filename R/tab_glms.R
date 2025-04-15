#' APA: flextable for 2-3 generalized linear models
#'
#' @param x REQUIRED: List. at least 2 glm models, bare names
#' @param var_labels Optional: Vector. Text replacements for model terms, "old" = "new"
#' @param caption Optional: Text. Caption for the table
#' @param general_note Optional: Text. General note for footer of APA table
#' @param p_note Optional: Text. (default = NULL) Significance note for APA table, If p_note = "apa" then the standard "* p < .05. ** p < .01. *** p < .001." will be used
#' @param d Optional: Number. Digits after the decimal place
#' @param narrow  Optional. Logical. Default = FALSE, but TRUE will exclude p-vlaues from the table to make it narrower
#' @param fit Optional: vector. quoted names of fit metrics from `performance::performance()` for glm (max of 4)
#' @param pr2 Optional: character.  (default = "both") Include pseudo R-squared: "tjur", "mcfadden", "both", or "none"
#' @param show_single_row	(tidy-select) By default categorical variables are printed on multiple rows. If a variable is dichotomous (e.g. Yes/No) and you wish to print the regression coefficient on a single row, include the variable name(s) here.
#'
#'
#' @returns a flextable object
#' @import gtsummary
#' @import flextable
#' @import tidyverse
#' @import performance
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
#' apaSupp::tab_glms(list("M1" = fit_glm1, "M2" = fit_glm2), pr2 = "both")
#' apaSupp::tab_glms(list("M1" = fit_glm1, "M2" = fit_glm2), narrow = TRUE)
#' apaSupp::tab_glms(list("M1" = fit_glm1, "M2" = fit_glm2), narrow = TRUE, fit = "RMSE")
#' apaSupp::tab_glms(list("M1" = fit_glm1, "M2" = fit_glm2), narrow = TRUE, fit = NA)
#' apaSupp::tab_glms(list("M1" = fit_glm1, "M2" = fit_glm2), narrow = TRUE, fit = NA, pr2 = "tjur")
#' apaSupp::tab_glms(list("M1" = fit_glm1, "M2" = fit_glm2), narrow = TRUE, fit = "AIC", pr2 = "tjur")
#'
#'
#' fit_glm3 <- glm(vs ~ wt + mpg + cyl, data = mtcars[1:30,], family = "binomial")
#'
#' apaSupp::tab_glms(list(fit_glm1, fit_glm2, fit_glm3), narrow = TRUE)
#' apaSupp::tab_glms(list(fit_glm1, fit_glm2, fit_glm3), narrow = TRUE, fit = "AIC", pr2 = "tjur")
#'
#'
#'
tab_glms <- function(x,
                     var_labels      = NULL,
                     caption         = "Compare Generalized Regression Models",
                     general_note    = NA,
                     p_note          = "apa123",
                     no_notes        = FALSE,
                     d               = 2,
                     narrow          = FALSE,
                     fit             = c("AIC", "BIC"),
                     pr2             = "both",
                     show_single_row = NULL,
                     breaks          = c(.05, .01, .001),
                     symbols         = c("*", "**", "***")){

  ns <- sapply(x,function(y) length(y$residuals))

  n_param <- x %>%
    purrr::map(coef) %>%
    purrr::map(length) %>%
    unlist() %>%
    max()

  n_models <- length(x)

  pad_fit  <- NULL
  pad_pr2  <- NULL

  n_fit   <- sum(!is.na(fit)*length(fit),
                 length(unique(ns)) > 1,
                 1*(length(fit)> 1),
                 3*(pr2 == "both"),
                 1*(pr2 == "tjur"),
                 1*(pr2 == "mcfadden"))


  main_note <- flextable::as_paragraph(
    flextable::as_i("Note. "),
    flextable::as_chunk(ifelse(length(unique(ns)) == 1,
                               glue::glue("N = {unique(ns)}. "),
                               "Models fit to different samples. ")),
    "CI = confidence interval.",
    flextable::as_chunk(case_when(pr2 == "both" ~ "Coefficient of deterination estiamted with both Tjur and McFadden's psuedo R-squared",
                                  pr2 == "tjur" ~ "Coefficient of deterination estiamted by Tjur's psuedo R-squared",
                                  pr2 == "mcfadden" ~ "Coefficient of deterination estimated by McFadden's psuedo R-squared",)),
    flextable::as_chunk(general_note)
  )


  if(is.null(names(x))){ mod_names <- paste("Model", 1:n_models)
  } else{                mod_names <- names(x)
  }

  table <- x %>%
    purrr::map(gt_glm,
               narrow          = narrow,
               fit             = fit,
               d               = d,
               show_single_row = show_single_row) %>%
    gtsummary::tbl_merge(tab_spanner = mod_names) %>%
    gtsummary::as_flex_table()


  n_rows <- flextable::nrow_part(table, part = "body")

  if (length(unique(ns)) > 1){
    table <- table %>%
      flextable::add_body_row(top = FALSE, values = NA) %>%
      flextable::compose(part = "body", i = (n_rows + 1), j = 1,
                         value = flextable::as_paragraph("N"))

    for (i_mod in 1:n_models){
      table <- table %>%
        flextable::compose(part = "body", i = (n_rows + 1), j = 2 + (i_mod-1)*(3-narrow),
                           value = flextable::as_paragraph(unlist(ns[i_mod])))
    }
  }



  if (sum(!is.na(fit)) > 0 ){
    df_fit <- performance::compare_performance(x, verbose = FALSE) %>%
      data.frame() %>%
      dplyr::select(fit)
  }


  n_rows <- flextable::nrow_part(table, part = "body")

  if (sum(!is.na(fit))  == 1){

    table <- table %>%
      flextable::add_body_row(top = FALSE, values = NA) %>%
      flextable::compose(part = "body", i = (n_rows + 1), j = 1,
                         value = flextable::as_paragraph(names(df_fit[1])))

    for (i_mod in 1:n_models){
      table <- table %>%
        flextable::compose(part = "body", i = (n_rows + 1), j = 2 + (i_mod-1)*(3-narrow),
                           value = flextable::as_paragraph(apaSupp::apan(df_fit[i_mod, 1], 2)))
    }

  } else if (length(fit) >= 2){

    pad_fit <- n_rows + (2:(1+length(fit)))

    table <- table %>%
      flextable::add_body_row(top = FALSE, values = NA) %>%
      flextable::compose(part = "body", i = (n_rows + 1), j = 1,
                         value = flextable::as_paragraph("Fit Metrics"))

    for (i_fit in 1:length(fit)){
      table <- table %>%
        flextable::add_body_row(top = FALSE, values = NA) %>%
        flextable::compose(part = "body", i = (n_rows + 1 + i_fit), j = 1,
                           value = flextable::as_paragraph(names(df_fit[i_fit])))

      for (i_mod in 1:n_models){
        table <- table %>%
          flextable::compose(part = "body", i = (n_rows + 1 + i_fit), j = 2 + (i_mod-1)*(3-narrow),
                             value = flextable::as_paragraph(df_fit[i_mod, i_fit]))
      }
    }
  }


  df_r2 <- data.frame(num = 1:n_models) %>%
    dplyr::mutate(mod = x) %>%
    dplyr::mutate(tjur     = purrr::map_dbl(x, ~performance::r2_tjur(.x))) %>%
    dplyr::mutate(mcfadden = purrr::map_dbl(x, ~performance::r2_mcfadden(.x)[[1]])) %>%
    dplyr::select(tjur, mcfadden) %>%
    dplyr::mutate(tjur     = apaSupp::p_num(tjur,     d = d + 1, stars = FALSE)) %>%
    dplyr::mutate(mcfadden = apaSupp::p_num(mcfadden, d = d + 1, stars = FALSE))


  n_rows <- flextable::nrow_part(table, part = "body")


  if (pr2 == "tjur" | pr2 == "mcfadden"){
    table <- table %>%
      flextable::add_body_row(top = FALSE, values = NA) %>%
      flextable::compose(part = "body", i = (n_rows + 1), j = 1,
                         value = flextable::as_paragraph(flextable::as_i("pseudo-R"),
                                                         flextable::as_chunk("\u00B2")))

    for (i_mod in 1:n_models){
      table <- table %>%
        flextable::compose(part = "body", i = (n_rows + 1), j = 2 + (i_mod-1)*(3-narrow),
                           value = flextable::as_paragraph(unlist(df_r2[i_mod, 1])))
    }

  } else if (pr2 == "both"){

    pad_pr2 <- n_rows + (2:3)

    table <- table %>%
      flextable::add_body_row(top = FALSE, values = NA) %>%
      flextable::compose(part = "body", i = (n_rows + 1), j = 1,
                         value = flextable::as_paragraph(flextable::as_i("pseudo-R"),
                                                         flextable::as_chunk("\u00B2"))) %>%
      flextable::add_body_row(top = FALSE, values = NA) %>%
      flextable::compose(part = "body", i = (n_rows + 2), j = 1,
                         value = flextable::as_paragraph("Tjur")) %>%
      flextable::add_body_row(top = FALSE, values = NA) %>%
      flextable::compose(part = "body", i = (n_rows + 3), j = 1,
                         value = flextable::as_paragraph("McFadden"))

    for (i_mod in 1:n_models){
      table <- table %>%
        flextable::compose(part = "body", i = (n_rows + 2), j = 2 + (i_mod-1)*(3-narrow),
                           value = flextable::as_paragraph(unlist(df_r2[i_mod, 1]))) %>%
        flextable::compose(part = "body", i = (n_rows + 3), j = 2 + (i_mod-1)*(3-narrow),
                           value = flextable::as_paragraph(unlist(df_r2[i_mod, 2])))
    }


  }


  table <- table %>%
    apaSupp::theme_apa(caption      = caption,
                       main_note    = main_note,
                       p_note       = p_note,
                       d            = d,
                       breaks       = breaks,
                       symbols      = symbols) %>%
    flextable::bold(  part = "header", i = 1) %>%
    flextable::italic(part = "header", i = 2) %>%
    flextable::align( part = "header", i = 1, align = "center")


  n_rows <- flextable::nrow_part(table, part = "body")

  if(n_fit > 0){
    table <- table %>%
      flextable::italic(part = "body", i = (n_rows + 1 - n_fit):(n_rows)) %>%
      flextable::hline( part = "body", i = (n_rows - n_fit))
  }


  if (narrow == FALSE){
    table <- table %>%
      flextable::align(part = "all", j = seq(from = 2, to =  3*n_models,      by = 3), align = "right") %>%
      flextable::align(part = "all", j = seq(from = 3, to = (3*n_models + 1), by = 3), align = "left")
  } else {
    table <- table %>%
      flextable::align(part = "all", j = seq(from = 2, to =  2*n_models,      by = 2),  align = "right") %>%
      flextable::align(part = "all", j = seq(from = 3, to = (2*n_models + 1), by = 2), align = "left")
  }

  if(!is.null(var_labels)){ table <- table %>% flextable::labelizor(part = "body", labels = var_labels) }

  pad_lines <- c(pad_fit, pad_pr2)

  if (!is.null(pad_lines)){
    table <- table %>%
      flextable::padding(i = pad_lines, padding.left = 20)
  }

  table <- table %>%
    flextable::align(part = "header", i = 1, align = "center") %>%
    flextable::align(part = "footer",        align = "left") %>%
    flextable::hline(part = "header", i = 1, border = flextable::fp_border_default(width = 0)) %>%
    flextable::autofit()

  return(table)

}
