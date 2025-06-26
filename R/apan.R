#' APA: Round a numeric value to `n` decimal places, keeping leading zero
#'
#' @param x a numeric value
#' @param d numbr of decimal places
#'
#' @return a numeric value
#' @import MOTE
#' @export
#'
#' @examples
#' n <- 1/3
#' apan(n, d = 2)
#'
apan <- function(x, d){
  MOTE::apa(value = x,
            decimals = d,
            leading = TRUE)
}
