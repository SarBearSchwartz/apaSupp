#' Basic Descriptive Summary: n, M (SD), Median, etc. with flextable
#'
#' @param df REQUIRED: Data frame
#' @param caption REQUIRED: Text. Caption for the table
#' @param general_note Optional: Text. General note for footer of APA table
#' @param p_note Optional: Text. (default = NULL) Significance note for APA table, If `p_note = "apa"` then the standard `"* p < .05. ** p < .01. *** p < .001."` will be used
#' @param no_notes REQUIRED: Logical.  Defaults to `FALSE`, if `TRUE` will ignore `genderal_note` and `p_note`
#' @param max_width_in Optional: Number.  Inches wide table can be
#' @param digits Optional: Number. Digits after the decimal place
#'
#' @return a `flextable` table with caption
#' @import tidyverse
#' @import glue
#' @import flextable
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
                     caption = "Summary of Quantiatative Variables",
                     general_note = NULL,
                     p_note = NULL,
                     no_notes = FALSE,
                     max_width_in = 6,
                     digits = 2){

  n <- nrow(df)

  standard_note <- glue::glue("NA = not available or missing. Mdn = median. Q1 = 25th percentile, Q3 = 75th percentile. N = {n}.")

  if (no_notes == TRUE){
    main_note <- NULL
  } else if (is.null(general_note)){
    main_note <- standard_note
  } else {
    main_note <- paste(general_note, standard_note, sep = " ")
  }

  x <- df %>%
    dplyr::select(where(is.numeric)) %>%
    dplyr::summarise(across(
      .cols = is.numeric,
      .fns = list(nmiss = ~ naniar::n_miss(.x),
                  M     = ~ base::mean(.x, na.rm = TRUE),
                  SD    = ~ stats::sd(.x, na.rm = TRUE),
                  min   = ~ base::min(.x, na.rm = TRUE),
                  q1    = ~ stats::quantile(.x, p = .25, na.rm = TRUE),
                  Mdn   = ~ stats::median(.x, na.rm = TRUE),
                  q3    = ~ stats::quantile(.x, p = .75, na.rm = TRUE),
                  max   = ~ base::max(.x, na.rm = TRUE),
                  sum   = ~ base::sum(.x, na.rm = TRUE)),
      .names = "{col}__{fn}")) %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to = c("var", ".value"),
                        names_pattern = "(.*)__(.*)") %>%
    dplyr::mutate(nmiss = as.integer(as.numeric(nmiss))) %>%
    dplyr::select("Variable" = var,
                  "NA" = nmiss,
                  "M" = M,
                  "SD" = SD,
                  "min" = min,
                  "Q1" = q1,
                  "Mdn" = Mdn,
                  "Q3" = q3,
                  "max" = max) %>%
    as.data.frame()

  tab <- x %>%
    flextable::flextable() %>%
    apaSupp::theme_apa(caption,
                       general_note = main_note,
                       p_note = p_note,
                       no_notes = no_notes,
                       max_width_in = max_width_in,
                       digits = digits) %>%
    flextable::align(j = 1,   align = "left",  part = "all") %>%
    flextable::align(j = 2:9, align = "right", part = "all") %>%
    flextable::bold(j = c(3, 4, 7), part = "all")

  return(tab)
}
