#' Basic Descriptive Summary: n, M (SD), [Min, Max], Median with flextable
#'
#' @param df a data frame with relevant variables selected
#' @param caption the `flextable` caption for the table
#' @param lab the text to place above the variable names
#'
#' @return a `flextable` table with caption
#' @import tidyverse
#' @import flextable
#' @export
#'
#' @examples
#' library(tidyverse)
#'
#' tab_desc(df = cars, caption = "Demographics Summary of Cars", lab = "Variable")
#'
#' cars %>%
#' dplyr::rename("Speed, mph" = speed,
#' "Stopping Distance, ft" = dist) %>%
#' tab_desc(caption = "Demographics Summary of Cars", lab = "Variable")
#'
tab_desc <- function(df,
                     caption,
                     lab = "Scale"){
  df %>%
    psych::describe(quant = c(.25, .75)) %>%
    data.frame() %>%
    tibble::rownames_to_column(var = lab) %>%
    dplyr::mutate(M_SD = glue::glue("{apaSupp::apa2(mean)} ({apaSupp::apa2(sd)})")) %>%
    dplyr::mutate(min_max = glue::glue("[{apaSupp::apa2(min)}, {apaSupp::apa2(max)}]")) %>%
    dplyr::select(lab,
                  n,
                  "M (SD)" = M_SD,
                  "[Min, Max]" = min_max,
                  "Median" = median) %>%
    flextable::flextable() %>%
    flextable::colformat_double(j = c("n"), digits = 0) %>%
    flextable::autofit() %>%
    flextable::set_caption(caption)
}
