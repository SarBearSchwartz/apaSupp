#' P-value Numbers: 3-decimal places, no leading zero, and stars
#'
#' @param value a numeric value for a statistical significance (p-value)
#' @param breaks statistical significance break points
#' @param symbols symbols to assign to each break point
#' @param decimals number of digits after the decimal point
#' @param leading keep the leading zero in front of the decimal
#'
#'
#' @return character of the form ".231", ".022**", or "< .001***"
#' @import dplyr
#' @import glue
#' @import MOTE
#' @export
#'
#' @examples
#' x <- c(.36521, .02456, .0000000056)
#' p_num(x)
p_num <- function(value,
                         breaks = c(.05, .01, .001),
                         symbols = c("*", "**", "***"),
                         decimals = 3,
                         leading = FALSE){

  value_apa = MOTE::apa(value = value,
                        decimals = decimals,
                        leading = leading)

  value_apa_min = MOTE::apa(value = breaks[3],
                            decimals = decimals,
                            leading = leading)

  dplyr::case_when(value <  breaks[3] ~ glue::glue("< {value_apa_min} {symbols[3]}"),
                   value == breaks[3] ~ glue::glue("{value_apa_min} {symbols[3]}"),
                   value <=  breaks[2] ~ glue::glue("{value_apa} {symbols[2]}"),
                   value <=  breaks[1] ~ glue::glue("{value_apa} {symbols[1]}"),
                   value > breaks[1] ~ glue::glue("{value_apa}"))
}
