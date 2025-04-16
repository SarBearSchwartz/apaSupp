#' Convert a `furniture::table1()` to a `flextable::flextable()` with APA formatting
#'
#' @param df REQUIRED: Data frame
#' @param split REQUIRED: Bare varaiable name
#' @param caption REQUIRED: Text. Caption for the table
#' @param total Optional: Logical. Defaults to add a total column
#' @param test Optional: Logical. Defaults to test groups differences
#' @param na.rm Optional: Logical. Defaults to NOT list-wise delete cases
#' @param general_note Optional: Text. General note for footer of APA table
#' @param p_note Optional: Text. (default = NULL) Significance note for APA table, If `p_note = "apa"` then the standard `"* p < .05. ** p < .01. *** p < .001."` will be used
#' @param digits Optional: Number. Digits after the decimal place
#' @export
#' @import tidyverse
#' @import furniture
#' @import flextable
#'
#' @examples
#' library(tidyverse)
#'
#' # Simple
#' mtcars
#'
table1_apa <- function(df,
                       split,
                       caption = "Summary Statistics",
                       total = TRUE,
                       test = TRUE,
                       na.rm = FALSE,
                       general_note = NULL,
                       p_note = "apa",
                       digits = 2){

  split <- rlang::enquo(split)

  vars <- df %>%
    dplyr::select(-!!split) %>%
    names()

  n_tot <- df %>%
    dplyr::select(-!!split) %>%
    ncol()

  n_factors <- df %>%
    dplyr::select(-!!split) %>%
    dplyr::select_if(is.factor) %>%
    ncol()

  n_numeric <-  df %>%
    dplyr::select(-!!split) %>%
    dplyr::select_if(is.numeric) %>%
    ncol()

  warnings <- df %>%
    dplyr::group_by(!!split) %>%
    furniture::table1(total = total,
                      test = test,
                      na.rm = na.rm,
                      digits = digits) %>%
    testthat::capture_messages()

  warn <- data.frame(warn = warnings) %>%
    dplyr::filter(stringr::str_sub(warn,
                                   start = 1,
                                   end = 7) == "Breusch") %>%
    dplyr::mutate(failHOV = stringr::str_sub(warn,
                                             start = 92,
                                             end = -2)) %>%
    dplyr::mutate(failHOV = stringr::str_replace(failHOV,
                                                 pattern = "\\.",
                                                 replacement = " ")) %>%
    dplyr::mutate(failHOV = stringr::str_trim(failHOV)) %>%
    dplyr::pull(failHOV) %>%
    unlist() %>%
    paste(collapse = ", ")


  df_tab1 <- df %>%
    dplyr::group_by(!!split) %>%
    furniture::table1(total = total,
                      test = test,
                      na.rm = na.rm,
                      digits = digits) %>%
    as.data.frame() %>%
    dplyr::rename(Variable = !!names(.[1]))


  factor_rows <- df_tab1 %>%
    tibble::rowid_to_column() %>%
    dplyr::filter(!Variable %in% vars) %>%
    dplyr::filter(Variable != " ") %>%
    dplyr::pull(rowid)


  c <- ncol(df_tab1)
  n <- names(df_tab1)[2:(c - 1)]

  if (n_factors > 0 & n_numeric > 0){
    standard_note <- glue::glue("NA = not available or missing. Categorical variables are summarized by counts (percentages) and are compared via Chi-squred Test(s) for Independence and continuous variables summarized by means (standard deviations) and are compared via independent ANOVA.")
  } else if (n_factors > 0 & n_numeric == 0){
    standard_note <- glue::glue("NA = not available or missing. Categorical variables are summarized by counts (percentages) and are compared via Chi-squred Test(s) for Independence.")
  } else if (n_factors == 0 & n_numeric > 0){
    standard_note <- glue::glue("Continuous variables summarized by means (standard deviations) and are compared via independent ANOVA.")
  }

  if (is.null(general_note)){
    main_note <- standard_note
  } else {
    main_note <- paste(general_note, standard_note, sep = " ")
  }

  # if (!is.null(warn)){
  #   main_note <- paste(main_note,
  #                      "Evidence of unequal variances adjusted for when comparing means for the following variables:",
  #                      warn,
  #                      sep = " ")
  # }

  tab <- df_tab1 %>%
    dplyr::rename(p = "P.Value") %>%
    dplyr::mutate(p = ifelse(p == "<.001", ".0009", p)) %>%
    dplyr::mutate(p = as.numeric(p)) %>%
    dplyr::mutate(stars = case_when(p <= .001 ~ "***",
                                    p <= .010 ~ "**",
                                    p <= .050 ~ "*")) %>%
    dplyr::mutate(p = apaSupp::p_num(p,
                                     symbols = c("", "", ""))) %>%
    flextable::flextable() %>%
    flextable::delete_part(part = "header") %>%
    flextable::add_header_row(values = c("", n, "p", ""),
                              colwidths = rep(1, times = c + 1)) %>%
    apaSupp::theme_apa(caption = caption,
                       general_note = main_note,
                       p_note = p_note,
                       digits = digits) %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::align(j = 1, align = "left", part = "body") %>%
    flextable::align(j = -1, align = "right", part = "body") %>%
    flextable::align(j = "p", align = "right", part = "all") %>%
    flextable::align(j = "stars", align = "left", part = "all") %>%
    flextable::align(i = 1, align = "center", part = "body") %>%
    flextable::padding(padding.right = 0, j = "p",     part  = "all")  %>%
    flextable::padding(padding.left = 0, j = "stars", part  = "all") %>%
    flextable::padding(padding.left = 15, j = 1, i = factor_rows) %>%
    flextable::italic(j = 1, i = factor_rows)

  return(tab)
}
