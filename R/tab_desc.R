#' Basic Descriptive Summary: n, M (SD), Median, etc. with flextable
#'
#' @param df REQUIRED: data frame
#' @param total Optional: Logical. Calculate total? default is no (FALSE)
#' @param caption REQUIRED: Text. Caption for the table
#' @param general_note Optional: Text. General note for footer of APA table
#' @param p_note Optional: Text. Significance note for APA table
#' @param valign Optional: Text. vertical alignment ("center")
#' @param digits Optional: Number. Digits after the decimal place
#' @param fontname Optional: Text.  Font used in table
#' @param space Optional: Number. Line spacing in the body of the table
#'
#' @return a `flextable` table with caption
#' @import tidyverse
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @import purrr
#' @import glue
#' @import flextable
#' @import officer
#' @import naniar
#' @export
#'
#' @examples
#' library(tidyverse)
#'
#' # Simple
#' tab_desc(cars)
#'
#' # Complex
#' cars %>%
#'     dplyr::rename("Speed, mph" = speed,
#'                   "Stopping Distance, ft" = dist) %>%
#'     tab_desc(caption = "Speed and Stopping Distances of Cars in the 1920's",
#'              general_note = "McNeil, D. R. (1977) Interactive Data Analysis. Wiley.")
#'
#'
tab_desc <- function(df,
                     total = FALSE,
                     caption = "Descriptive Summary",
                     general_note = NULL,
                     p_note = NULL,
                     valign = "center",
                     digits = 2,
                     fontname = "serif",
                     space = .5){

  n <- nrow(df)

  if (is.null(general_note)){
    general_note <- glue::glue("N = {n}.")
  } else {
    general_note <- glue::glue("{general_note} N = {n}.")
  }

  x <- df %>%
    dplyr::select(where(is.numeric)) %>%
    dplyr::summarise(across(
      .cols = is.numeric,
      .fns = list(valid = ~ n(),
                  nmiss = ~ naniar::n_miss(.x),
                  M     = ~ mean(.x, na.rm = TRUE),
                  SD    = ~ sd(.x, na.rm = TRUE),
                  min   = ~ min(.x, na.rm = TRUE),
                  q1    = ~ quantile(.x, p = .25, na.rm = TRUE),
                  Mdn   = ~ median(.x, na.rm = TRUE),
                  q3    = ~ quantile(.x, p = .75, na.rm = TRUE),
                  max   = ~ max(.x, na.rm = TRUE),
                  sum   = ~ sum(.x, na.rm = TRUE)),
      .names = "{col}__{fn}"
    )) %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to = c("var", ".value"),
                        names_pattern = "(.*)__(.*)") %>%
    dplyr::mutate(miss = glue::glue("{nmiss}/{valid}")) %>%
    dplyr::mutate_if(is.numeric, apa2) %>%
    dplyr::mutate(min_p = glue::glue("[{min}")) %>%
    dplyr::mutate(max_p = glue::glue("{max}]")) %>%
    dplyr::mutate(nmiss = as.integer(as.numeric(nmiss))) %>%
    dplyr::select("Measure" = var,
                  "Missing" = nmiss,
                  "Mean" = M,
                  "(SD)" = SD,
                  "[min" = min_p,
                  "Q1" = q1,
                  "Median" = Mdn,
                  "Q3" = q3,
                  "max]" = max_p,
                  "Total" = sum
    )

  if (total == FALSE){
    x <- x %>%
      dplyr::select(-Total)
  }

  tab <- x %>%
    tab_apa(caption = caption,
            general_note = general_note,
            p_note = p_note,
            valign = valign,
            digits = digits,
            fontname = fontname,
            space = space) %>%
    flextable::align(j = 1, align = "left")

  return(tab)
}
