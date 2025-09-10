#' Frequency Table, total or split by a factor
#'
#' @param df REQUIRED: Data frame
#' @param split Optional: Quoted variable name
#' @param caption REQUIRED: Text. Caption for the table
#' @param docx Optional: filename. must end with ".docx"
#' @param tab_width Optional: numberic value (default is .9) % of available width
#' @param general_note Optional: Text. General note for footer of APA table
#' @param d Optional: Number. Digits after the decimal place
#'
#' @return a `flextable` table with caption
#' @import tidyverse
#' @import flextable
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(forcats)
#'
#' data(mtcars)
#'
#' mtcars <- mtcars %>%
#'   dplyr::mutate(vs = factor(vs,
#'                             levels = 0:1,
#'                             labels = c("V Shaped", "Straight"))) %>%
#'   dplyr::mutate(am = factor(am,
#'                             levels = 0:1,
#'                             labels = c("Automatic", "Manual"))) %>%
#'   dplyr::mutate(cyl  = factor(cyl)) %>%
#'   dplyr::mutate(gear = factor(gear))
#'
#'
#' tab_freq(mtcars)
#'
#' mtcars %>%
#'   dplyr::select(cyl,
#'                 "Engine Type" = vs,
#'                 "Transmission" = am,
#'                 "Forward Gears" = gear) %>%
#'   tab_freq(split = "cyl",
#'            caption = "Summary of Automobile Engines by Number of Cylinders",
#'            general_note = "Data from the 1974 Motor Trend US magazine.")
#'
tab_freq <- function(df,
                     split        = NULL,
                     caption      = "Summary of Categorical Variables",
                     docx         = NA,
                     tab_width    = .9,
                     general_note = NA,
                     d            = 2){

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
    apaSupp::theme_apa(caption      = caption,
                       general_note = general_note,
                       d            = d) %>%
    flextable::align(  part = "body",         align = "right") %>%
    flextable::align(  part = "all", j = 1:2, align = "left") %>%
    flextable::padding(j =  1, padding.left = 0, padding.right = 0) %>%
    flextable::padding(j = -1, padding.left = 5, padding.right = 0) %>%
    flextable::set_table_properties(layout = "autofit",
                                    width = tab_width)

  if (!is.na(docx)){
    flextable::save_as_docx(table,
                            path = docx)
  }

  return(table)
}
