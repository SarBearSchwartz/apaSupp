#' Basic Descriptive Summary: n, M (SD), [Min, Max], Median with flextable
#'
#' @param df a data frame with relevant variables selected
#' @param caption the `flextable` caption for the table
#' @param general_note general note for footer of APA table
#' @param p_note significance note for APA table
#' @param fontname name of font for table
#' @param align alignment
#' @param valign vertial alignment
#' @param digits digits after the decimal place for numeric values
#' @param space line spacing in the body of the table, default is single
#' @param ... extra arguments
#'
#' @return a `flextable` table with caption
#' @import tidyverse
#' @import flextable
#' @import officer
#' @import naniar
#' @export
#'
#' @examples
#' library(tidyverse)
#'
#' tab_desc(cars)
#'
#' cars %>%
#' dplyr::rename("Speed, mph" = speed,
#'               "Stopping Distance, ft" = dist) %>%
#' tab_desc(caption = "Speed and Stopping Distances of Cars in the 1920's",
#'          general_note = "McNeil, D. R. (1977) Interactive Data Analysis. Wiley.")
#'
#'
tab_desc <- function(df,
                     caption = "Descriptive Summary",
                     general_note = NULL,
                     p_note = NULL,
                     fontname = "Times New Roman",
                     align = "center",
                     valign = "center",
                     digits = 2,
                     space = 1,
                     ...){

    n <- nrow(df)

    if (is.null(general_note)){
      general_note <- glue::glue("N = {n}.")
    } else {
      general_note <- glue::glue("{general_note} N = {n}.")
    }

    x <- df %>%
    dplyr::summarise(across(
      .cols = is.numeric,
      .fns = list(valid = ~ n(),
                  nmiss = ~ naniar::n_miss(.x),
                  M = ~ mean(.x, na.rm = TRUE),
                  SD = ~ sd(.x, na.rm = TRUE),
                  min = ~ min(.x, na.rm = TRUE),
                  q1 = ~ quantile(.x, p = .25, na.rm = TRUE),
                  Mdn = ~ median(.x, na.rm = TRUE),
                  q3 = ~ quantile(.x, p = .75, na.rm = TRUE),
                  max = ~ max(.x, na.rm = TRUE)),
      .names = "{col}__{fn}"
    )) %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to = c("var", ".value"),
                        names_pattern = "(.*)__(.*)") %>%
    dplyr::mutate(miss = glue::glue("{nmiss}/{valid}")) %>%
    dplyr::mutate_if(is.numeric, apa2) %>%
    dplyr::mutate(M_SD = glue::glue("{M} ({SD})")) %>%
    dplyr::mutate(min_max = glue::glue("{min} - {max}")) %>%
    dplyr::mutate(qs = glue::glue("[{q1}, {q3}]")) %>%
    dplyr::mutate(nmiss = as.integer(as.numeric(nmiss))) %>%
    dplyr::select("Measure" = var,
                  "Missing" = nmiss,
                  "M (SD)" = M_SD,
                  "Median" = Mdn,
                  "[Min, Max]" = min_max,
                  "Quantiles" = qs) %>%
      apaTab(caption = caption,
             general_note = general_note,
             p_note = p_note,
             fontname = fontname,
             align = align,
             valign = valign,
             digits = digits,
             space = space,
             ...) %>%
      flextable::align(j = 1,
                       align = "left") %>%
      flextable::fit_to_width(max_width = 6)

  return(x)
}
