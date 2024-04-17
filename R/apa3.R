#' APA: Round a numeric value to 3 decimal places, keeping leading zero
#'
#' @param x a numeric value
#'
#' @return a numeric value
#' @import MOTE
#' @export
#'
#' @examples
#' n <- 1/3
#' apa3(n)
#'
apa3 <- function(x){
  MOTE::apa(value = x,
            decimals = 3,
            leading = TRUE)
}
