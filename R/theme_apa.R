#' APA Formatted Table of a data.frame
#' @param tab REQUIRED: A pre-created `flextable` object
#' @param caption REQUIRED: Text. Caption for the table
#' @param general_note Optional: Text. General note for footer of APA table
#' @param p_note Optional: Text. (default = NULL) Significance note for APA table, If `p_note = "apa"` then the standard `"* p < .05. ** p < .01. *** p < .001."` will be used
#' @param no_notes REQUIRED: Logical.  Defaults to `FALSE`, if `TRUE` will ignore `genderal_note` and `p_note`
#' @param max_width_in = Optional: Number.  Inches wide take can be
#' @param digits Optional: Number. Digits after the decimal place
#'
#' @return table
#' @import tidyverse
#' @import dplyr
#' @import flextable
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(flextable)
#'
#' mtcars %>%
#`   dplyr::select(mpg, disp, hp, drat, wt) %>%
#'   flextable::flextable() %>%
#'   theme_apa(caption = "Summary of Some Variables")

theme_apa <- function(tab,
                      caption = "Replace this Table Caption",
                      general_note = NULL,
                      p_note = NULL,
                      no_notes = FALSE,
                      max_width_in = 6,
                      digits = 2){

  border.thick <- list("width" = 2.5, color = "black", style = "solid")
  border.thin  <- list("width" = 1.0, color = "black", style = "solid")

  if (no_notes == TRUE){
    main_note <- NULL
    p_note    <- NULL
  } else if (is.null(general_note)){
    main_note <- NULL
  } else if (!is.null(general_note)){
    main_note <- general_note
  }

  if (is.null(p_note)){
    p_note <- NULL
  } else if (p_note == "apa"){
    p_note <- "* p < .05. ** p < .01. *** p < .001."
  } else {
    p_note <- p_note
  }

  k <- sum(!is.null(main_note), !is.null(p_note))

  tab <- tab %>%
    flextable::border_remove() %>%
    flextable::hline_top(border = border.thick, part = "header") %>%
    flextable::hline(border = border.thin, part = "header") %>%
    flextable::hline_top(border = border.thick, part = "body") %>%
    flextable::hline_bottom(part = "head", border = border.thin) %>%
    flextable::hline_bottom(part = "body", border = border.thick) %>%
    flextable::fix_border_issues() %>%
    flextable::autofit() %>%
    flextable::line_spacing(space = 1.5, part = "header") %>%
    flextable::line_spacing(space = 0.5, part = "body") %>%
    flextable::valign(valign = "center", part = "all") %>%
    flextable::align(align = "left",     part = "footer") %>%
    flextable::align(j = -1, align = "center", part = "all") %>%
    flextable::align(j = 1, align = "left") %>%
    flextable::colformat_double(digits = digits)  %>%
    flextable::set_caption(caption = caption,
                           autonum = TRUE) %>%
    flextable::fit_to_width(max_width = max_width_in, unit = "in")

  if (!is.null(main_note)){
    tab <- tab %>%
      flextable::add_footer_lines("") %>%
      flextable::compose(i = 1, j = 1,
                         value = as_paragraph(flextable::as_i("Note. "),
                                              as_chunk(main_note)),
                         part = "footer")
  }

  if (!is.null(p_note)){
    tab <- tab %>%
      flextable::add_footer_lines("") %>%
      flextable::compose(i = k, j = 1,
                         value = as_paragraph(as_chunk(p_note)),
                         part = "footer")
  }

  return(tab)
}
