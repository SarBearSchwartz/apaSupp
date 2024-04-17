#' APA Formatted Table of a data.frame
#'
#' @param x a data frame
#' @param caption text for the caption
#' @param general_note text for a general note
#' @param p_note text for a p-value significance indicators
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
                   general_note = NULL,
                   p_note = "* p < .05, ** p < .01, *** p < .001"){

  x %>%
    rempsyc::nice_table(note = c(general_note,p_note)) %>%
    flextable::set_caption(caption = caption)
}
