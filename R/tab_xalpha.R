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
#' @param max_width_in = Optional: Number.  Inches wide take can be
#' @param digits Optional: Number. Digits after the decimal place
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
                       general_note = NULL,
                       max_width_in = 6,
                       digits = 2){

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

  standard_note <- glue::glue("Std = standardized. Mdn = median. N = {n}.")

  if (is.null(general_note)){
    main_note <- standard_note
  } else {
    main_note <- paste(general_note, standard_note, sep = " ")
  }


  x <- dfc %>%
    tidyr::nest(data = c(!!item, !!value, !!id)) %>%
    dplyr::mutate(data_wide = data %>%
                    map(~ data.frame(.x) %>%
                          tidyr::pivot_wider(names_from = !!item,
                                             names_prefix = "item_",
                                             values_from = !!value))) %>%
    dplyr::mutate(lo = purrr::map_dbl(data,
                                      ~ min(.x %>% dplyr::select(!!value)))) %>%
    dplyr::mutate(hi = purrr::map_dbl(data,
                                      ~ max(.x %>% dplyr::select(!!value)))) %>%
    dplyr::mutate(psych_alpha = purrr::map(data_wide,
                                           ~ data.frame(.x) %>%
                                             dplyr::select(-!!id) %>%
                                             psych::alpha(discrete = FALSE))) %>%
    dplyr::mutate(nvar = psych_alpha %>%
                    purrr::map_dbl(~.x$nvar) %>%
                    as.character()) %>%
    dplyr::mutate(a_tot = purrr::map(psych_alpha,
                                     ~ .x$total)) %>%
    tidyr::unnest(a_tot) %>%
    dplyr::mutate(range = paste0(floor(lo), " - ", ceiling(hi))) %>%
    dplyr::select(!!measure,
                  !!domain,
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
      flextable::bold(i = grp_lines, part = "body") %>%
      flextable::merge_h_range(i = grp_lines, j1 = 1, j2 = 8, part = "body")
  } else {
    grp_lines <- NULL
    y <- x %>% flextable::flextable()
  }

  tab <- y  %>%
    flextable::separate_header() %>%
    theme_apa(caption = caption,
              general_note = main_note,
              p_note = NULL,
              digits = digits) %>%
    flextable::align(j = -1, align = "center", part = "all") %>%
    flextable::line_spacing(space = 1, part = "header") %>%
    flextable::padding(j = c(6),
                       padding.left = 5,
                       padding.right = 5,
                       part = "all") %>%
    flextable::padding(j = c(4, 7),
                       padding.left = 10,
                       padding.right = 1,
                       part = "all") %>%
    flextable::padding(j = c(5, 8),
                       padding.left = 1,
                       padding.right = 10,
                       part = "all") %>%
    flextable::fit_to_width(max_width = max_width_in, unit = "in")

  return(tab)

}


