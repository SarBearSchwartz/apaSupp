#' APA Formatted Table of a data.frame
#' @param tab A pre-created `flextable`
#' @param valign Optional: Text. vertical alignment ("center")
#' @param digits Optional: Number. Digits after the decimal place
#' @param fontname Optional: Text.  Font used in table
#' @param space Optional: Number. Line spacing in the body of the table
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
#'   flextable::flextable() %>%
#'   theme_apa()

theme_apa <- function(tab,
                      valign = "center",
                      digits = 2,
                      fontname = "serif",
                      space = .5){

  border.thick <- list("width" = 2.5, color = "black", style = "solid")
  border.thin  <- list("width" = 1.0, color = "black", style = "solid")

  tab <- tab %>%
    flextable::colformat_double(digits = digits)  %>%
    flextable::autofit() %>%
    flextable::line_spacing(space = space, part = "body") %>%
    flextable::border_remove() %>%
    flextable::hline_top(part = "head", border = border.thick) %>%
    flextable::hline_bottom(part = "head", border = border.thin) %>%
    flextable::hline_bottom(part = "body", border = border.thick) %>%
    flextable::valign(valign = valign, part = "all") %>%
    flextable::align(align = "left",   part = "footer") %>%
    flextable::align(j = 1, align = "left") %>%
    flextable::font(fontname = fontname, part = "all")

  return(tab)
}
