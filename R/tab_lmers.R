#' APA: flextable for 2-3 linear mixed effects models
#'
#' @param x REQUIRED: List. at least 2 lmer models, bare names
#' @param var_labels Optional: Vector. Text replacements for model terms, "old" = "new"
#' @param caption Optional: Text. Caption for the table
#' @param docx Optional: filename. must end with ".docx"
#' @param general_note Optional: Text. General note for footer of APA table
#' @param p_note Optional: Text. (default = NULL) Significance note for APA table, If p_note = "apa" then the standard "* p < .05. ** p < .01. *** p < .001." will be used
#' @param no_notes REQUIRED: Logical.  Defaults to `FALSE`, if `TRUE` will ignore `general_note` and `p_note`
#' @param d Optional: Number. Digits after the decimal place
#' @param narrow  Optional. Logical. Default = FALSE, but TRUE will exclude p-vlaues from the table to make it narrower
#' @param fit Optional: vector. quoted names of fit statistics to include, can be: "AIC", "BIC", "logLik", "statistic","p.value", "df", "logLik", "AIC", "BIC", "deviance", "df.residual", and "nobs"
#' @param show_single_row	 If a variable is dichotomous (e.g. Yes/No) and you wish to print the regression coefficient on a single row, include the variable name(s) here.
#' @param breaks Optional: numeric vector of p-value cut-points
#' @param symbols Optional: character vector for symbols denoting p-value cut-points
#'
#' @returns a flextable object
#' @import gtsummary
#' @import flextable
#' @import tidyverse
#' @import broom.mixed
#' @export
#'
#' @examples
#'
#'library(tidyverse)
#'library(lme4)
#'library(lmerTest)
#'
#'fm1 <- lmerTest::lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
#'fm2 <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#'fm3 <- lmerTest::lmer(Reaction ~ Days + (1 | Subject), sleepstudy %>% dplyr::mutate(Days = factor(Days)))
#'fm4 <- lmerTest::lmer(Reaction ~ Days + I(Days^2) + (1|Subject), sleepstudy)
#'
#' apaSupp::tab_lmers(list(fm1, fm2))
#' apaSupp::tab_lmers(list(fm1, fm2, fm4), var_labels = c("Subject.var__(Intercept)" = "rand int"))
#'
tab_lmers <- function(x,
                      var_labels      = NULL,
                      caption         = "Compare MLM Models",
                      docx            = NA,
                      general_note    = NA,
                      p_note          = "apa123",
                      no_notes        = FALSE,
                      d               = 2,
                      narrow          = FALSE,
                      fit             = c("AIC", "BIC", "logLik"),
                      show_single_row = NULL,
                      breaks          = c(.05, .01, .001),
                      symbols         = c("*", "**", "***")){


  ns <- sapply(x, function(y) length(resid(y)))

  n_fix_param <- x %>%
    purrr::map(fixef) %>%
    purrr::map(length) %>%
    unlist() %>%
    max()

  n_models <- length(x)

  k     <- ifelse(narrow == TRUE, 2, 3)
  n_col <- 1 + k*n_models

  main_note <- flextable::as_paragraph(
    flextable::as_i("Note. "),
    flextable::as_chunk(ifelse(length(unique(ns)) == 1,
                               NA,
                               "Models fit to different samples. ")),
    flextable::as_chunk(general_note)
  )


  if(is.null(names(x))){ mod_names <- paste("Model", 1:n_models)
  } else{                mod_names <- names(x)
  }

  get <- x %>%
    purrr::map(apaSupp::gt_lmer,
               narrow          = narrow,
               fit             = fit,
               d               = d,
               show_single_row = show_single_row) %>%
    gtsummary::tbl_merge(tab_spanner = mod_names) %>%
    gtsummary::modify_table_body(
      ~ .x %>%
        dplyr::mutate(section = case_when(
          variable == "(Intercept)" ~ 0,
          stringr::str_detect(variable, ".var_") ~ 2,
          stringr::str_detect(variable, ".cov_") ~ 2,
          row_type   == "glance_statistic" ~ 3,
          TRUE ~ 1
        )) %>%
        dplyr::arrange(section))

  n_rand_param <- sum(get$table_body$section == 2)

  table <- get %>%
    gtsummary::as_flex_table()

  n_rows <- flextable::nrow_part(table, part = "body")
  rows_fit <- (n_rows - n_fit + 1):(n_rows)
  rows_rand <- (n_rows - n_fit - n_rand_param + 1):(n_rows - n_fit)


  table <- table %>%
    apaSupp::theme_apa(caption      = caption,
                       main_note    = main_note,
                       p_note       = p_note,
                       d            = d,
                       breaks       = breaks,
                       symbols      = symbols) %>%
    flextable::bold(  part = "header", i = 1) %>%
    flextable::italic(part = "header", i = 2) %>%
    flextable::italic(part = "body",   i = rows_fit) %>%
    flextable::align( part = "header", i = 1, align = "center") %>%
    flextable::align( part = "footer",        align = "left") %>%
    flextable::hline( part = "header", i = 1,
                      border = flextable::fp_border_default(width = 0))


  if(!is.null(var_labels)){
    table <- table %>%
      flextable::labelizor(part = "body", labels = var_labels)
  }


  if (narrow == TRUE){
    table <- table %>%
      flextable::align(part = "all", j = seq(from = 2, to = n_col, by = k), align = "right") %>%
      flextable::align(part = "all", j = seq(from = 3, to = n_col, by = k), align = "left")  %>%
      flextable::width(j = seq(from = 2, to = n_col, by = k), width = .40)     %>%
      flextable::width(j = seq(from = 3, to = n_col, by = k), width = .75)
  } else if (narrow == FALSE) {
    table <- table %>%
      flextable::align(part = "all", j = seq(from = 2, to = n_col, by = k), align = "right") %>%
      flextable::align(part = "all", j = seq(from = 3, to = n_col, by = k), align = "left")  %>%
      flextable::align(part = "all", j = seq(from = 4, to = n_col, by = k), align = "left")  %>%
      flextable::width(j = seq(from = 2, to = n_col, by = k), width = .40)     %>%
      flextable::width(j = seq(from = 3, to = n_col, by = k), width = .40)     %>%
      flextable::width(j = seq(from = 4, to = n_col, by = k), width = .75)
  }


  for (r in rows_fit){
    for (m in 1:n_models){
      table <- table %>%
        flextable::merge_at(i = r, j = (2 + (m-1)*k):(3 + (m-1)*k)) %>%
        flextable::align(   i = r,          align = "center")
    }
  }



  table <- table %>%
    flextable::align( part = "header", i = 1, align = "center") %>%
    flextable::align( part = "header", i = 1, align = "center") %>%
    flextable::align( part = "body",   j = 1, align = "left") %>%
    flextable::align( part = "footer",        align = "left") %>%
    flextable::hline( part = "body",   i = rows_fit[1] - 1) %>%
    flextable::hline( part = "body",   i = rows_rand[1] - 1) %>%
    flextable::hline( part = "header", i = 1,
                      border = flextable::fp_border_default(width = 0)) %>%
    flextable::line_spacing(part = "header", space = 1.5) %>%
    flextable::line_spacing(part = "body",   space = 0.5) %>%
    flextable::line_spacing(part = "footer", space = 1.5) %>%
    flextable::autofit()

  if (!is.na(docx)){
    flextable::save_as_docx(table,
                            path = docx)
  }

  return(table)

}
