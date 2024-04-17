#' APA: Round a numeric value to 2 decimal places, keeping leading zero
#'
#' @param x a numeric value
#'
#' @return a numeric value
#' @import MOTE
#' @export
#'
#' @examples
#' n <- 1/3
#' apa2(n)
#'
apa2 <- function(x){
  MOTE::apa(value = x,
            decimals = 2,
            leading = TRUE)
}
