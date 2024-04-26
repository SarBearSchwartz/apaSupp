#' Tabulate Internal Consistency
#'
#' @param df_long data frame with the following `id`, `item`, `value`, `scale`,
#' @param caption the `flextable` caption for the table
#' @param scale the variable that denotes the subscale for each item
#'
#' @return a `flextable` table with caption
#' @import tidyverse
#' @import psych
#' @import flextable
#' @export
#'
#' @examples
#' library(lavaan)
#' data(HolzingerSwineford1939, package = "lavaan")
#' df <- HolzingerSwineford1939
#' df %>%
#'   tidyr::pivot_longer(cols = starts_with("x"),
#'                       names_to = "item",
#'                       values_to = "value") %>%
#'   dplyr::mutate(scale = case_when(item %in% c("x1", "x2", "x3") ~ "Visual",
#'                                   item %in% c("x4", "x5", "x6") ~ "Verbal",
#'                                   item %in% c("x7", "x8", "x9") ~ "Speed")) %>%
#'   dplyr::select(id, item, value, scale) %>%
#'   tab_xalpha(caption = "Internal Consistency for Holzinger and Swineford Dataset, n = 301")
#'
tab_xalpha <- function(df_long,
                      caption,
                      scale = "scale"){
  df_long %>%
    tidyr::nest(.by = scale) %>%
    dplyr::mutate(data_wide = map(data, ~ data.frame(.x) %>%
                                    tidyr::pivot_wider(names_from = item,
                                                       names_prefix = "item_",
                                                       values_from = value) %>%
                                    data.frame())) %>%
    dplyr::mutate(psych_alpha = map(data_wide, ~ .x %>%
                                      dplyr::select(-id) %>%
                                      psych::alpha())) %>%
    dplyr::mutate(alpha = map(psych_alpha, ~.x$total)) %>%
    dplyr::mutate(nvar = map_dbl(psych_alpha, ~.x$nvar)) %>%
    tidyr::unnest(alpha) %>%
    dplyr::select("Scale" = scale,
                  "Items" = nvar,
                  "Alpha, raw" = raw_alpha,
                  "Alpha, std." = std.alpha) %>%
    flextable::flextable() %>%
    flextable::colformat_double(j = c("Items"), digits = 0) %>%
    flextable::autofit() %>%
    flextable::set_caption(caption)
}
