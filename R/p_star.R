#' P-value Stars: only stars with no numbers
#'
#' @param value a numeric value for a statistical significance (p-value)
#' @param breaks statistical significance break points
#' @param symbols symbols to assign to each break point
#'
#'
#' @return character of the form ".231", ".022**", or "< .001***"
#' @import tidyverse
#' @import glue
#' @import MOTE
#' @export
#'
#' @examples
#' x <- c(.36521, .02456, .0000000056)
#' p_star(x)
p_star <- function(value,
                  breaks = c(.05, .01, .001),
                  symbols = c("*", "**", "***")){

  dplyr::case_when(value <= breaks[3] ~ glue::glue("{symbols[3]}"),
                   value <= breaks[2] ~ glue::glue("{symbols[2]}"),
                   value <= breaks[1] ~ glue::glue("{symbols[1]}"),
                   value  > breaks[1] ~ glue::glue(""))
}
