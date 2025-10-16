#' APA: flextable for 2-3 GEE models
#'
#' @param x REQUIRED: List. at least 2 geeglm models, bare names
#' @param var_labels Optional: Vector. Text replacements for model terms, "old" = "new"
#' @param caption Optional: Text. Caption for the table
#' @param docx Optional: filename. must end with ".docx"
#' @param general_note Optional: Text. General note for footer of APA table
#' @param p_note Optional: Text. (default = NULL) Significance note for APA table, If p_note = "apa" then the standard "* p < .05. ** p < .01. *** p < .001." will be used
#' @param no_notes REQUIRED: Logical.  Defaults to `FALSE`, if `TRUE` will ignore `general_note` and `p_note`
#' @param d Optional: Number. Digits after the decimal place
#' @param narrow  Optional. Logical. Default = FALSE, but TRUE will exclude p-vlaues from the table to make it narrower
#' @param show_single_row	(tidy-select) By default categorical variables are printed on multiple rows. If a variable is dichotomous (e.g. Yes/No) and you wish to print the regression coefficient on a single row, include the variable name(s) here.
#' @param breaks Optional: numeric vector of p-value cut-points
#' @param symbols Optional: character vector for symbols denoting p-value cut-points
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
#'library(tidyverse)
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
#'
#'
#'
#'resp_geeglm2_ex <- geepack::geeglm(status ~ treatment + sex + BL_status,
#'                                   data = data_long,
#'                                   family = binomial(link = "logit"),
#'                                   id = subject,
#'                                   waves = month,
#'                                   corstr = "exchangeable")
#'
#'
#'tab_gees(list(resp_geeglm_ex, resp_geeglm2_ex))
#'tab_gees(list(resp_geeglm_ex, resp_geeglm2_ex), narrow = TRUE)
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
#'btb_geeglm_ex_1 <- geepack::geeglm(bdi ~ bdi.pre + length + drug + treatment + month,
#'                                   data = btb_long,
#'                                   id = id,
#'                                   wave = month,
#'                                   family = gaussian,
#'                                   corstr = 'exchangeable')
#'
#'btb_geeglm_ex_2 <- geepack::geeglm(bdi ~ bdi.pre*length + drug + treatment + month,
#'                                   data = btb_long,
#'                                   id = id,
#'                                   wave = month,
#'                                   family = gaussian,
#'                                   corstr = 'exchangeable')
#'
#'
#'tab_gees(list(btb_geeglm_ex_1, btb_geeglm_ex_2))
#'tab_gees(list(btb_geeglm_ex_1, btb_geeglm_ex_2), narrow = TRUE)
#'
#'
tab_gees <- function(x,
                     var_labels      = NULL,
                     caption         = "Compare GEE Models",
                     docx            = NA,
                     general_note    = NA,
                     p_note          = "apa123",
                     no_notes        = FALSE,
                     d               = 2,
                     narrow          = FALSE,
                     show_single_row = NULL,
                     breaks          = c(.05, .01, .001),
                     symbols         = c("*", "**", "***")){

  n_models <- length(x)

  if(is.null(names(x))){
    mod_names <- paste("Model", 1:n_models)
  } else{
    mod_names <- names(x)
  }

  main_note <- flextable::as_paragraph(
    flextable::as_i("Note. "),
    flextable::as_chunk(general_note)
  )

  table <- x %>%
    purrr::map(~ apaSupp::gt_gee(.x,
                                 narrow          = narrow,
                                 fit             = NA,
                                 d               = d,
                                 show_single_row = show_single_row)) %>%
    gtsummary::tbl_merge(tab_spanner = mod_names) %>%
    gtsummary::as_flex_table() %>%
    apaSupp::theme_apa(caption      = caption,
                       main_note    = main_note,
                       p_note       = p_note,
                       d            = d,
                       breaks       = breaks,
                       symbols      = symbols) %>%
    flextable::bold(  part = "header", i = 1) %>%
    flextable::italic(part = "header", i = 2) %>%
    flextable::align( part = "header", i = 1, align = "center")



  if (narrow == FALSE){
    table <- table %>%
      flextable::align(part = "all", j = seq(from = 2, to =  3*n_models,      by = 3), align = "right") %>%
      flextable::align(part = "all", j = seq(from = 3, to = (3*n_models + 1), by = 3), align = "left")
  } else {
    table <- table %>%
      flextable::align(part = "all", j = seq(from = 2, to =  2*n_models,      by = 2), align = "right") %>%
      flextable::align(part = "all", j = seq(from = 3, to = (2*n_models + 1), by = 2), align = "left")
  }

  if(!is.null(var_labels)){
    table <- table %>% flextable::labelizor(part = "body", labels = var_labels)
  }


  table <- table %>%
    flextable::align(part = "header", i = 1, align = "center") %>%
    flextable::align(part = "footer",        align = "left") %>%
    flextable::hline(part = "header", i = 1, border = flextable::fp_border_default(width = 0)) %>%
    flextable::autofit()

  if (!is.na(docx)){
    flextable::save_as_docx(table,
                            path = docx)
  }

  return(table)

}

















