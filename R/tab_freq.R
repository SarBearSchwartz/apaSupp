#' Frequency Table, total or split by a factor
#'
#' @param df REQUIRED: Data frame
#' @param split Optional: Quoted variable name
#' @param caption REQUIRED: Text. Caption for the table
#' @param general_note Optional: Text. General note for footer of APA table
#' @param d Optional: Number. Digits after the decimal place
#' @param max_width_in = Optional: Number.  Inches wide the table can be
#'
#' @return a `flextable` table with caption
#' @import tidyverse
#' @import flextable
#' @export
#'
#' @examples
#' library(tidyverse)
#'
#' mtcars
#' apaSupp::tab_freq(mtcars)
#'
tab_freq <- function(df,
                     split = NULL,
                     caption = "Summary of Categorical Variables",
                     general_note = NA,
                     d = 2,
                     max_width_in = 6){

  table <- df %>%
    dplyr::select_if(is.factor) %>%
    dplyr::mutate_if(is.factor, forcats::fct_drop) %>%
    flextable::summarizor(by = split,
                          overall_label = "Total") %>%
    flextable::as_flextable(spread_first_col = TRUE,
                            max_rows = 100) %>%
    flextable::style(i = ~!is.na(variable),
                     pr_t = fp_text_default(bold = TRUE),
                     pr_p = officer::fp_par(text.align = "left",
                                            padding = 5,
                                            line_spacing = 1.5)) %>%
    flextable::prepend_chunks(i = ~is.na(variable),
                              j = 1,
                              as_chunk("\t")) %>%
    apaSupp::theme_apa(caption = caption,
                       general_note = general_note,
                       p_note = NULL,
                       d = d,
                       max_width_in = max_width_in) %>%
    flextable::align(align = "right", part = "body") %>%
    flextable::align(j = 1:2, align = "left",  part = "all") %>%
    flextable::padding(j =  1, padding.left = 0, padding.right = 0) %>%
    flextable::padding(j = -1, padding.left = 5, padding.right = 0) %>%
    flextable::fit_to_width(max_width = max_width_in, unit = "in") %>%
    flextable::autofit()

  return(table)
}
