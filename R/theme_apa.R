#' APA Formatted Table of a data.frame
#' @param x REQUIRED: A pre-created `flextable` object
#' @param caption REQUIRED: Text. Caption for the table
#' @param general_note Optional: Text. General note for footer of APA table
#' @param p_note Optional: Text. (default = NULL) Significance note for APA table, If `p_note = "apa"` then the standard `"* p < .05. ** p < .01. *** p < .001."` will be used
#' @param no_notes REQUIRED: Logical.  Defaults to `FALSE`, if `TRUE` will ignore `genderal_note` and `p_note`
#' @param d Optional: Number. Digits after the decimal place
#' @param max_width_in = Optional: Number.  Inches wide take can be
#'
#' @return table
#' @import tidyverse
#' @import flextable
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(flextable)
#'
#' mtcars %>%
#'   dplyr::select(mpg, disp, hp, drat, wt) %>%
#'   flextable::flextable() %>%
#'   theme_apa(caption = "Summary of Some Variables")

theme_apa <- function(x,
                      caption      = "Table Caption",
                      general_note = NA,
                      p_note       = NA,
                      no_notes     = FALSE,
                      d            = 2,
                      max_width_in = 6,
                      main_note    = NA,
                      sig_note     = NA){

  border.thick <- list("width" = 2.5, color = "black", style = "solid")
  border.thin  <- list("width" = 1.0, color = "black", style = "solid")

  if (no_notes == TRUE){
    general_note <- NA
    p_note       <- NA
  }

  if (!is.na(general_note)){
    main_note <- flextable::as_paragraph(flextable::as_i("Note. "),
                                         flextable::as_chunk(general_note))
  }

  if (!is.na(p_note) & p_note == "apa"){
    sig_note  <- flextable::as_paragraph("* p < .05. ** p < .01. *** p < .001.")
  } else if (!is.na(p_note)){
    sig_note  <- flextable::as_paragraph(p_note)
  }

  table <- x %>%
    flextable::border_remove() %>%
    flextable::hline_top(part    = "header", border = border.thick) %>%
    flextable::hline_bottom(part = "header", border = border.thin) %>%
    flextable::hline_bottom(part = "body",   border = border.thick) %>%
    flextable::fix_border_issues() %>%
    flextable::line_spacing(part = "header", space = 1.5) %>%
    flextable::line_spacing(part = "body",   space = 0.5) %>%
    flextable::line_spacing(part = "footer", space = 1.5) %>%
    flextable::valign(part = "all",       valign = "center") %>%
    flextable::align( part = "all",        align = "center", ) %>%
    flextable::align( part = "all", j = 1, align = "left") %>%
    flextable::align( part = "footer",     align = "left") %>%
    flextable::colformat_double(digits = d)  %>%
    flextable::set_caption(caption = caption, autonum = TRUE)

  if (!is.na(main_note)){
    table <- table %>%
      flextable::add_footer_lines("") %>%
      flextable::compose(part = "footer", i = 1, j = 1, value = main_note)
  }

  f <- flextable::nrow_part(table, part = "footer")

  if (!is.na(p_note)){
    table <- table %>%
      flextable::add_footer_lines("") %>%
      flextable::compose(part = "footer", i = f + 1, j = 1, value = sig_note)
  }

  table <- table %>%
    flextable::fit_to_width(max_width = max_width_in, unit = "in") %>%
    flextable::autofit()

  return(table)
}
