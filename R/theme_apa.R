#' APA Formatted Table of a data.frame
#' @param x REQUIRED: A pre-created `flextable` object
#' @param caption REQUIRED: Text. Caption for the table
#' @param docx Optional: filename. must end with ".docx"
#' @param tab_width Optional: numberic value (default is .9) % of available width
#' @param general_note Optional: Text. General note for footer of APA table
#' @param main_note Optional: alternative to `general_note` already in paragraph form
#' @param p_note Optional: Text. (default = NULL) Significance note for APA table, If `p_note = "apa123"` then the standard `"* p < .05. ** p < .01. *** p < .001."` will be used
#' @param no_notes REQUIRED: Logical.  Defaults to `FALSE`, if `TRUE` will ignore `genderal_note` and `p_note`
#' @param d Optional: Number. Digits after the decimal place
#' @param breaks Optional: numeric vector of p-value cut-points
#' @param symbols Optional: character vector for symbols denoting p-value cut-points
#' @param d Optional: Number. Digits after the decimal place
#' @param main_note Optional: alternative to `general_note` already in paragraph form

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
#'   apaSupp::theme_apa(caption = "Summary of Some Variables")

theme_apa <- function(x,
                      caption      = "Table Caption",
                      docx         = NA,
                      tab_width    = .9,
                      general_note = NA,
                      main_note    = NA,
                      p_note       = NA,
                      no_notes     = FALSE,
                      d            = 2,
                      breaks       = c(.05, .01, .001),
                      symbols      = c("*", "**", "***")){

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

  if (sum(breaks == c(.05, .01, .001)) == length(breaks)) {ch_bk <- c(".05", ".01", ".001")}

  if (!is.na(p_note)){
    sig_note  <- flextable::as_paragraph(
      flextable::as_chunk(ifelse(stringr::str_detect(p_note, "1"), symbols[1]  , NA)),
      flextable::as_i(    ifelse(stringr::str_detect(p_note, "1"), " p < "     , NA)),
      flextable::as_chunk(ifelse(stringr::str_detect(p_note, "1"), ch_bk[1]    , NA)),
      flextable::as_chunk(ifelse(stringr::str_detect(p_note, "1"), ". "        , NA)),
      flextable::as_chunk(ifelse(stringr::str_detect(p_note, "2"), symbols[2]  , NA)),
      flextable::as_i(    ifelse(stringr::str_detect(p_note, "2"), " p < "     , NA)),
      flextable::as_chunk(ifelse(stringr::str_detect(p_note, "2"), ch_bk[2]    , NA)),
      flextable::as_chunk(ifelse(stringr::str_detect(p_note, "2"), ". "        , NA)),
      flextable::as_chunk(ifelse(stringr::str_detect(p_note, "3"), symbols[3]  , NA)),
      flextable::as_i(    ifelse(stringr::str_detect(p_note, "3"), " p < "     , NA)),
      flextable::as_chunk(ifelse(stringr::str_detect(p_note, "3"), ch_bk[3]    , NA)),
      flextable::as_chunk(ifelse(stringr::str_detect(p_note, "3"), ". "        , NA))
    )
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
    flextable::set_table_properties(layout = "autofit",
                                    width = tab_width)

  if (!is.na(docx)){
    flextable::save_as_docx(table,
                            path = docx)
  }

  return(table)
}
