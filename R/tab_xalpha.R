#' Tabulate Internal Consistency
#'
#' @param df REQUIRED: data frame
#' @param measure Optional: Bare variable name. Measurement tool name
#' @param domain Optional: Bare variable name. Domain or sub-score name
#' @param item REQUIRED: Bare variable name. Item distinction
#' @param value REQUIRED: Bare variable name. Values or rating of each item
#' @param id REQUIRED: Bare variable name. Identify cases/subjects
#' @param caption REQUIRED: Text. Caption for the table
#' @param general_note Optional: Text. General note for footer of APA table
#' @param d Optional: Number. Digits after the decimal place
#'
#' @return a `flextable` table with caption
#' @import tidyverse
#' @import glue
#' @import psych
#' @import flextable
#' @import lavaan
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(lavaan)
#'
#' data(HolzingerSwineford1939, package = "lavaan")
#'
#' HolzingerSwineford1939 %>%
#'   tidyr::pivot_longer(cols = starts_with("x"),
#'              names_to = "it",
#'              values_to = "val") %>%
#'   dplyr::mutate(Domain = case_when(it %in% c("x1", "x2", "x3") ~ "Visual",
#'                                    it %in% c("x4", "x5", "x6") ~ "Verbal",
#'                                   it %in% c("x7", "x8", "x9") ~ "Speed")) %>%
#'   dplyr::mutate(Measure = case_when(Domain %in% c("Visual", "Verbal") ~
#'                                    "Scale V Made Up",
#'                                     Domain == "Speed"  ~
#'                                    "Scale S With A Very Long Name for Demo")) %>%
#'   tab_xalpha(measure = Measure,
#'              domain = Domain,
#'              item = it,
#'              value = val,
#'              id = id,
#'              caption = "Internal Consistency for the Holzinger and Swineford",
#'              general_note = "Scales are for example only.")
#'
tab_xalpha <- function(df,
                       measure = NULL,
                       domain = NULL,
                       item,
                       value,
                       id,
                       caption = "Internal Consistency",
                       general_note = NA,
                       d = 2){

  measure <- rlang::enquo(measure)
  domain  <- rlang::enquo(domain)
  item    <- rlang::enquo(item)
  value   <- rlang::enquo(value)
  id      <- rlang::enquo(id)

  level_names <- df %>%
    dplyr::select(!!measure, !!domain) %>%
    names()

  level_num <- length(level_names)

  if (level_num == 0) level_names = NULL

  dfc <- df %>%
    dplyr::select(!!measure, !!domain, !!item, !!value, !!id) %>%
    dplyr::filter(complete.cases(.))

  vars <- names(dfc)

  n <- dfc %>%
    dplyr::pull(var = !!id) %>%
    unique() %>%
    length()

  main_note <- flextable::as_paragraph(
    flextable::as_i(    "Note. "),
    flextable::as_i(    "N"),
    flextable::as_chunk(glue::glue(" = {n}. ")),
    flextable::as_i(    "n"),   " = complete observations; ",
    flextable::as_i(    "Std"), " = standardized; ",
    flextable::as_i(    "Mdn"), " = median. ",
    flextable::as_chunk(general_note)
  )


  x <- dfc %>%
    tidyr::nest(data = c(!!item, !!value, !!id)) %>%
    dplyr::mutate(data_wide = data %>%
                    purrr::map(~ data.frame(.x) %>%
                                 tidyr::pivot_wider(names_from = !!item,
                                                    names_prefix = "item_",
                                                    values_from = !!value))) %>%
    dplyr::mutate(psych_alpha = purrr::map(data_wide,
                                           ~ data.frame(.x) %>%
                                             dplyr::select(-!!id) %>%
                                             psych::alpha(discrete = FALSE))) %>%
    dplyr::mutate(nids = psych_alpha %>%
                    purrr::map(~ min(.x$item.stats$n)) %>%
                    as.character()) %>%
    dplyr::mutate(nvar = psych_alpha %>%
                    purrr::map_dbl(~.x$nvar) %>%
                    as.character()) %>%
    dplyr::mutate(lo = purrr::map_dbl(data, ~ min(.x %>% dplyr::select(!!value)))) %>%
    dplyr::mutate(hi = purrr::map_dbl(data, ~ max(.x %>% dplyr::select(!!value)))) %>%
    dplyr::mutate(range = paste0(floor(lo), " - ", ceiling(hi))) %>%
    dplyr::mutate(a_tot = purrr::map(psych_alpha,
                                     ~ .x$total)) %>%
    tidyr::unnest(a_tot) %>%
    dplyr::mutate(across(c(raw_alpha, std.alpha, `G6(smc)`, average_r, median_r),
                         ~ apaSupp::p_num(.x, d = d + 1, stars = FALSE))) %>%
    dplyr::select(!!measure,
                  !!domain,
                  "n" = nids,
                  "Num\nItems" = nvar,
                  "Value\nRange" = range,
                  "Chronbach's\nAlpha_Raw" = raw_alpha,
                  "Chronbach's\nAlpha_Std" = std.alpha,
                  "Guttman's\nLamdba\nG6" = `G6(smc)`,
                  "Interitem\nCorrelation_M" = average_r,
                  "Interitem\nCorrelation_Mdn" = median_r) %>%
    as.data.frame()

  if (level_num == 2){
    grp_lines <- x %>%
      flextable::as_grouped_data(groups = names(x)[1]) %>%
      tibble::rowid_to_column() %>%
      dplyr::filter(!is.na(.[, 2])) %>%
      dplyr::pull(rowid)
    y <- x  %>%
      flextable::as_grouped_data(groups = names(x)[1]) %>%
      flextable::as_flextable(hide_grouplabel = TRUE) %>%
      flextable::bold(part = "body", i = grp_lines) %>%
      flextable::merge_h_range(part = "body", i = grp_lines, j1 = 1, j2 = 9)
  } else {
    grp_lines <- NULL
    y <- x %>% flextable::flextable()
  }

  table <- y  %>%
    flextable::separate_header() %>%
    apaSupp::theme_apa(caption      = caption,
                       main_note    = main_note,
                       d            = d) %>%
    flextable::line_spacing(space = 1, part = "header") %>%
    flextable::italic( part = "header", i = 2, j = c(2, 5, 6, 8, 9)) %>%   #added
    flextable::align(  part = "all",  j = c(1, 6, 9),    align = "left") %>%
    flextable::align(  part = "all",  j = c(5, 8),       align = "right") %>%
    flextable::align(  part = "all",  j = c(2, 3, 4, 7), align = "center") %>%
    flextable::align(  part = "header", i = 1, j = c(5:6, 8:9), align = "center") %>%
    flextable::hline(  part = "header", i = 1, j = c(5:6, 8:9)) %>%
    flextable::valign( part = "header", j = 1:4, valign = "bottom") %>%
    flextable::compose(part = "header", j = 1, value = flextable::as_paragraph(NA)) %>%
    flextable::autofit()


  return(table)

}


