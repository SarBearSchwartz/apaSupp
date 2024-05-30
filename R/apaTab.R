#' APA Formatted Table of a data.frame
#'
#' @param x a data frame
#' @param caption the `flextable` caption for the table
#' @param general_note general note for footer of APA table
#' @param p_note significance note for APA table
#' @param fontname name of font for table
#' @param align alignment
#' @param valign vertial alignment
#' @param digits digits after the decimal place for numeric values
#' @param space line spacing in the body of the table, default is single
#' @param ... extra arguments
#'
#' @return table
#' @import rempsyc
#' @import flextable
#' @import officer
#' @export
#'
#' @examples
#' apaTab(head(cars), caption = "Cars Dataset")
#'
#' cars %>%
#' head() %>%
#' apaTab(caption = "Speed and Stopping Distances of Cars in the 1920's",
#'        general_note = "Example cases from the dataset.",
#'        p_note = NULL)
#'
#' cars %>%
#'   psych::headTail() %>%
#'   tibble::rownames_to_column(var = "ID") %>%
#'   dplyr::rename("Speed, mph" = speed,
#'                 "Stopping Distance, ft" = dist) %>%
#'   apaTab(caption = "Speed and Stopping Distances of Cars in the 1920's",
#'          general_note = "Head and tail of a dataset from: McNeil, D. R. (1977) Interactive Data Analysis. Wiley.",
#'          p_note = NULL)
apaTab <- function(x,
                   caption,
                   general_note = NULL,
                   p_note = "* p < .05. ** p < .01. *** p < .001.",
                   fontname = "Times New Roman",
                   align = "right",
                   valign = "center",
                   digits = 2,
                   space = 1,
                   ...){

  if (!is.null(general_note)){
    x <- x %>%
      rempsyc::nice_table(note = general_note)
  } else {
    x <- x %>%
      rempsyc::nice_table()
  }

  if (!is.null(p_note)){
    x <- x %>%
      flextable::add_footer_lines(p_note)
    }

  border.thick <- list("width" = 2.5, color = "black", style = "solid")
  border.thin <- list("width" = 1, color = "black", style = "solid")

  x <- x %>%
    flextable::set_caption(caption = caption) %>%
    flextable::autofit() %>%
    flextable::font(part = "all",
                    fontname = fontname) %>%
    flextable::line_spacing(space = space, part = "all") %>%
    flextable::border_remove() %>%
    flextable::hline_top(part = "head", border = border.thick) %>%
    flextable::hline_bottom(part = "head", border = border.thin) %>%
    flextable::hline_bottom(part = "body", border = border.thick) %>%
    flextable::align(align = align,
                     part = "all") %>%
    flextable::align(align = "left",
                     part = "footer") %>%
    flextable::align(align = "center",
                     part = "header") %>%
    flextable::valign(valign = valign,
                      part = "all") %>%
    flextable::colformat_double(digits = digits) %>%
    flextable::align(j = 1,
                     align = "left")

  return(x)
}
