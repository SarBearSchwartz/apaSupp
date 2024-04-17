#' APA Formatted Table of a data.frame
#'
#' @param x a data frame
#' @param caption text for the caption
#' @param note text for a general note
#' @param sig text for a p-value significance indicators
#'
#' @return table
#' @import rempsyc
#' @import flextable
#' @export
#'
#' @examples
#' df <- head(cars)
#' apaTab(df, caption = "Testing")
apaTab <- function(x, caption,
                   note = NULL,
                   sig = "* p < .05. ** p < .01. *** p < .001."){

  x <- x %>%
    rempsyc::nice_table(note = c(general_note, p_note)) %>%
    flextable::set_caption(caption = caption)

  apa.border <- list("width" = flextable_global$defaults$border.width,
                     color = "black",
                     style = "solid")

  x <- flextable::font(x, part = "all", fontname = "Times New Roman")
  x <- flextable::line_spacing(x, space = 2, part = "all")
  x <- flextable::hline_top(x, part = "head", border = apa.border)
  x <- flextable::hline_bottom(x, part = "head", border = apa.border)
  x <- flextable::hline_top(x, part = "body", border = apa.border)
  x <- flextable::hline_bottom(x, part = "body", border = apa.border)
  x <- flextable::align(x, align = "left", part = "all")
  x <- flextable::valign(x, valign = "center", part = "all")
  x <- flextable::colformat_double(x, digits = 2)
  flextable::fix_border_issues(x)
  return(x)
}
