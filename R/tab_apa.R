#' APA Formatted Table of a data.frame
#'
#' @param df REQUIRED: data frame
#' @param caption REQUIRED: Text. Caption for the table
#' @param general_note Optional: Text. General note for footer of APA table
#' @param p_note Optional: Text. Significance note for APA table
#' @param valign Optional: Text. vertical alignment ("center")
#' @param digits Optional: Number. Digits after the decimal place
#' @param fontname Optional: Text.  Font used in table
#' @param space Optional: Number. Line spacing in the body of the table
#'
#' @return table
#' @import tibble
#' @import rempsyc
#' @import flextable
#' @import officer
#' @import tidyverse
#' @import rlang
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(psych)
#'
#' tab_apa(head(cars), caption = "Cars Dataset")
#'
#' cars %>%
#' head() %>%
#' tab_apa(caption = "Speed and Stopping Distances of Cars in the 1920's",
#'         general_note = "Example cases from the dataset.",
#'         p_note = NULL)
#'
#' cars %>%
#'   psych::headTail() %>%
#'   tibble::rownames_to_column(var = "ID") %>%
#'   dplyr::rename("Speed, mph" = speed,
#'                 "Stopping Distance, ft" = dist) %>%
#'   tab_apa(caption = "Speed and Stopping Distances of Cars in the 1920's",
#'           general_note = "Head and tail of a dataset from:
#'              McNeil, D. R. (1977) Interactive Data Analysis. Wiley.",
#'           p_note = NULL)

tab_apa <- function(df,
                    caption,
                    general_note = NULL,
                    p_note = "* p < .05. ** p < .01. *** p < .001.",
                    valign = "center",
                    digits = 2,
                    fontname = "serif",
                    space = .5){

  border.thick <- list("width" = 2.5, color = "black", style = "solid")
  border.thin  <- list("width" = 1.0, color = "black", style = "solid")

  tab <- df %>%
    flextable::flextable() %>%
    flextable::border_remove() %>%
    flextable::hline_top(border = border.thick,
                         part = "header") %>%
    flextable::hline(border = border.thin,
                     part = "header") %>%
    flextable::hline_top(border = border.thick,
                         part = "body") %>%
    flextable::hline_bottom(part = "head", border = border.thin) %>%
    flextable::hline_bottom(part = "body", border = border.thick) %>%
    flextable::fix_border_issues() %>%
    flextable::autofit() %>%
    flextable::line_spacing(space = 1.5, part = "header") %>%
    flextable::align(j = -1, align = "center", part = "all") %>%
    flextable::set_caption(caption = caption)

  if (!is.null(general_note) & !is.null(p_note)){
    note <- flextable::as_paragraph(flextable::as_i("Note. "),
                                    general_note,
                                    p_note)
    tab <- tab %>%
      flextable::add_footer_lines("") %>%
      flextable::compose(i = 1, j = 1, value = note, part = "footer")

  } else if (!is.null(general_note) & is.null(p_note)){
    note <- flextable::as_paragraph(flextable::as_i("Note. "),
                                    general_note)
    tab <- tab %>%
      flextable::add_footer_lines("") %>%
      flextable::compose(i = 1, j = 1, value = note, part = "footer")

  } else if (is.null(general_note) & !is.null(p_note)){
    note <- flextable::as_paragraph(flextable::as_i("Note. "),
                                    p_note)
    tab <- tab %>%
      flextable::add_footer_lines("") %>%
      flextable::compose(i = 1, j = 1, value = note, part = "footer")

  }


  return(tab)
}
