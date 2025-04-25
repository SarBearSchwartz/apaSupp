#' Tabulate Pairwise Pearson's Correlation
#'
#' @param x REQUIRED: A dataframe with selected numeric variables
#' @param caption REQUIRED: Text. Caption for the table
#' @param general_note Optional: Text. General note for footer of APA table
#' @param p_note Optional: Text. (default = NULL) Significance note for APA table, If `p_note = "apa"` then the standard `"* p < .05. ** p < .01. *** p < .001."` will be used
#' @param d Optional: Number. Digits after the decimal place
#' @param breaks statistical significance break points
#' @param symbols symbols to assign to each break point
#'
#' @return table
#' @import tidyverse
#' @import flextable
#' @import rstatix
#' @import equatags
#' @export
#'
#' @examples
#' library(tidyverse)
#'
#' apaSupp::tab_cor(mtcars %>% dplyr::select(cyl, mpg, disp, hp))
#'
tab_cor <- function(x,
                    caption      = "Pairwise Correlations",
                    general_note = NA,
                    p_note       = "apa123",
                    d            = 2,
                    breaks       = c(.05, .01, .001),
                    symbols      = c("*", "**", "***")){

  main_note <- flextable::as_paragraph(
    flextable::as_i("Note. "),
    flextable::as_i("N"),
    flextable::as_chunk(glue::glue(" = {nrow(x)}. ")),
    flextable::as_i("r"),
    flextable::as_chunk(" = Pearson's Product-Moment correlation coefficient."),
    flextable::as_chunk(general_note)
  )


  table <- x %>%
    rstatix::cor_mat() %>%
    rstatix::pull_lower_triangle() %>%
    rstatix::cor_gather() %>%
    dplyr::mutate(r = MOTE::apa(cor, decimals = d + 1, leading = FALSE)) %>%
    dplyr::mutate(p = apaSupp::p_num(p, breaks = breaks, symbols = symbols)) %>%
    dplyr::select("Variable Pair" = var2, var3, r, p) %>%
    flextable::flextable() %>%
    apaSupp::theme_apa(caption      = caption,
                       main_note    = main_note,
                       p_note       = p_note,
                       breaks       = breaks,
                       symbols      = symbols,
                       d            = d) %>%
    flextable::merge_at(part = "header", i = 1, j = 1:2) %>%
    flextable::italic(  part = "header", i = 1, j = 3:4) %>%
    flextable::align(   part = "header",                 align = "center") %>%
    flextable::align(   part = "body",          j = 1:2, align = "left") %>%
    flextable::align(   part = "body",          j = 3:4, align = "right") %>%
    flextable::align(   part = "footer",                 align = "left") %>%
    flextable::autofit()

  return(table)

}
