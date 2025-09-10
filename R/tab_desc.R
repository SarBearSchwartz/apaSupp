#' Basic Descriptive Summary: n, M (SD), Median, etc. with flextable
#'
#' @param df REQUIRED: Data frame
#' @param caption REQUIRED: Text. Caption for the table
#' @param docx Optional: filename. must end with ".docx"
#' @param tab_width Optional: numberic value (default is .9) % of available width
#' @param general_note Optional: Text. General note for footer of APA table
#' @param d Optional: Number. Digits after the decimal place
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
                     caption      = "Summary of Quantiatative Variables",
                     docx         = NA,
                     tab_width    = .9,
                     general_note = NA,
                     d            = 2){

  n <- nrow(df)

  main_note <- flextable::as_paragraph(
    flextable::as_i("Note. "),
    flextable::as_i("N"),
    flextable::as_chunk(glue::glue(" = {n}. ")),
    flextable::as_i("NA"),  " = not available or missing; ",
    flextable::as_i("Mdn"), " = median; ",
    flextable::as_i("Q1"),  " = 25th percentile; " ,
    flextable::as_i("Q3"),  " = 75th percentile. ",
    flextable::as_chunk(general_note)
  )


  table <- df %>%
    dplyr::summarise(across(
      .cols = where(is.numeric),
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
                  "NA"       = nmiss,
                  "M"        = M,
                  "SD"       = SD,
                  "min"      = min,
                  "Q1"       = q1,
                  "Mdn"      = Mdn,
                  "Q3"       = q3,
                  "max"      = max) %>%
    as.data.frame()%>%
    flextable::flextable() %>%
    apaSupp::theme_apa(caption   = caption,
                       main_note = main_note,
                       d         = d) %>%
    flextable::align(  part = "all", j = 1,   align = "left") %>%
    flextable::align(  part = "all", j = 2:9, align = "right") %>%
    flextable::bold(   part = "all", j = c(3, 4, 7)) %>%
    flextable::italic( part = "header") %>%
    flextable::compose(part = "header", i = 1, j = 1, value = flextable::as_paragraph(NA)) %>%
    flextable::autofit() %>%
    flextable::width(j = 1,   width = 1.75) %>%
    flextable::width(j = 2,   width = 0.25) %>%
    flextable::width(j = 2:9, width = 0.50) %>%
    flextable::width(j = 5,   width = 0.75) %>%
    flextable::line_spacing(part = "header", space = 1.5) %>%
    flextable::line_spacing(part = "body",   space = 0.5) %>%
    flextable::line_spacing(part = "footer", space = 1.5) %>%
    flextable::set_table_properties(layout = "autofit",
                                    width = tab_width)

  if (!is.na(docx)){
    flextable::save_as_docx(table,
                            path = docx)
  }

  return(table)
}
