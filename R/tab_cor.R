#' Tabulate Pairwise Pearson's Cofrrelation
#'
#' @param df REQUIRED: A dataframe with selected numeric variables
#' @param caption REQUIRED: Text. Caption for the table
#' @param general_note Optional: Text. General note for footer of APA table
#' @param p_note Optional: Text. (default = NULL) Significance note for APA table, If `p_note = "apa"` then the standard `"* p < .05. ** p < .01. *** p < .001."` will be used
#' @param max_width_in = Optional: Number.  Inches wide take can be
#' @param digits Optional: Number. Digits after the decimal place
#'
#' @return table
#' @import tidyverse
#' @import dplyr
#' @import flextable
#' @import rstatix
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(flextable)
#'
#' mtcars
#'
tab_cor <- function(df,
                    caption,
                    general_note = NULL,
                    p_note = "* p < .05. ** p < .01. *** p < .001.",
                    max_width_in = 6,
                    digits = 2){

  if (is.null(general_note)){
    main_notes <- glue::glue("r = Pearson's Product-Moment correlation coefficient. N = {nrow(df)}.")
  } else {
    main_notes <- glue::glue("{general_note} r = Pearson's Product-Moment correlation coefficient. N = {nrow(df)}.")
  }

  x <- df %>%
    rstatix::cor_mat() %>%
    rstatix::pull_lower_triangle() %>%
    rstatix::cor_gather() %>%
    dplyr::rename(r = cor) %>%
    dplyr::mutate(stars = case_when(p <= .001 ~ "***",
                                    p <= .010 ~ "**",
                                    p <= .050 ~ "*")) %>%
    dplyr::mutate(p = apaSupp::p_num(p,
                                     symbols = c("", "", ""))) %>%
    dplyr::select(var2, everything())

  tab <- x %>%
    flextable::as_flextable(max_row = 100) %>%
    flextable::delete_part(part = "header") %>%
    flextable::add_header_row(values = c("Variables", "r", "p"),
                              colwidths = c(2, 1, 2)) %>%
    theme_apa(caption = caption,
              general_note = main_notes,
              p_note = p_note,
              digits = digits,
              max_width_in = max_width_in) %>%
    flextable::colformat_double(j = "r", digits = 3) %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::align(j = 1:2, align = "left", part = "body") %>%
    flextable::align(j = 3:4, align = "right", part = "body") %>%
    flextable::align(j = 5, align = "left", part = "body") %>%
    flextable::padding(padding.right = 0, j = "p",     part  = "all")  %>%
    flextable::padding(padding.left = 0, j = "stars", part  = "all")

  return(tab)

}
