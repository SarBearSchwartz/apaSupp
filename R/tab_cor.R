#' Tabulate Pairwise Pearson's Correlation
#'
#' @param x REQUIRED: A dataframe with selected numeric variables
#' @param caption REQUIRED: Text. Caption for the table
#' @param general_note Optional: Text. General note for footer of APA table
#' @param p_note Optional: Text. (default = NULL) Significance note for APA table, If `p_note = "apa"` then the standard `"* p < .05. ** p < .01. *** p < .001."` will be used
#' @param d Optional: Number. Digits after the decimal place
#' @param method Optional: character string indicating which correlation coefficient is to be used for the test. One of "pearson", "kendall", or "spearman", can be abbreviated.
#' @param alternative Optional: indicates the alternative hypothesis and must be one of "two.sided", "greater" or "less". You can specify just the initial letter. "greater" corresponds to positive association, "less" to negative association.
#' @param drop.na Optional.  logical. If TRUE, drop rows containing missing values after gathering the data.
#' @param breaks statistical significance break points
#' @param symbols symbols to assign to each break point
#'
#' @return table
#' @import tidyverse
#' @import flextable
#' @import rstatix
#' @import equatags
#' @export
#'
#' @examples
#' library(tidyverse)
#'
#' mtcars2 <- mtcars %>% dplyr::mutate(hp = ifelse(mpg > 22, NA, hp)) %>% dplyr::select(cyl, mpg, disp, hp)
#'
#' apaSupp::tab_cor(mtcars2)
#'
#' apaSupp::tab_cor(mtcars2, miss = "list)
#'
#' apaSupp::tab_cor(mtcars2, p_note = "apa3")
#'
tab_cor <- function(x,
                    caption      = "Pairwise Correlations",
                    general_note = NA,
                    p_note       = "apa123",
                    d            = 2,
                    miss         = "pair",
                    method       = "pearson",
                    alternative  = "two.sided",
                    breaks       = c(.05, .01, .001),
                    symbols      = c("*", "**", "***")){


  n_org <- nrow(x)

  if (miss == "list"){ x <- x %>% dplyr::filter(complete.cases(.)) }

  df_cmp <- x %>% dplyr::filter(complete.cases(.))
  n_cmp <- nrow(df_cmp)

  main_note <- flextable::as_paragraph(
    flextable::as_i("Note. "),
    flextable::as_i("N"),
    flextable::as_chunk(glue::glue(" = {n_org}")),
    flextable::as_chunk(ifelse(n_org == n_cmp,
                               " (no missing values). ",
                               NA)),
    flextable::as_chunk(ifelse(n_cmp < n_org & miss == "list",
                               glue::glue(", reduced to {n_cmp} due to listwise deletion of any observation having at least one of the variables missing/unknown. "),
                               NA)),
    flextable::as_chunk(ifelse(n_cmp < n_org & miss == "pair",
                               ", reduced by pairwise deletion. NA = number of observations excluded, provided at least one of the two variables in the pair are missing/unknown. ",
                               NA)),
    flextable::as_i("r"),
    flextable::as_chunk(" = Pearson's Product-Moment correlation coefficient."),
    flextable::as_chunk(general_note)
  )


  df <- x %>%
    rstatix::cor_mat(method      = method,
                     alternative = alternative) %>%
    rstatix::pull_lower_triangle() %>%
    rstatix::cor_gather() %>%
    dplyr::mutate(r = MOTE::apa(cor,
                                decimals = d + 1,
                                leading = FALSE)) %>%
    dplyr::mutate(p = apaSupp::p_num(p,
                                     d = d + 1,
                                     breaks = breaks,
                                     symbols = symbols)) %>%
    dplyr::mutate(n = purrr::map2_dbl(.x = var1,
                                      .y = var2,
                                      .f = function(v1, v2){
                                        x %>%
                                          dplyr::select(v1, v2) %>%
                                          dplyr::filter(complete.cases(.)) %>%
                                          nrow()
                                      })) %>%
    dplyr::mutate(na = ifelse(n == nrow(x), NA, nrow(x) - n)) %>%
    dplyr::mutate(na = as.character(round(na, 0))) %>%
    dplyr::select("Variable Pair" = var2, var1, "NA" = na, r, p)


  if (n_cmp == n_org | miss == "list"){
    df <- df %>%
      dplyr::select(-"NA")
  }

  table <- df %>%
    flextable::flextable() %>%
    apaSupp::theme_apa(caption      = caption,
                       main_note    = main_note,
                       p_note       = p_note,
                       breaks       = breaks,
                       symbols      = symbols,
                       d            = d) %>%
    flextable::italic(  part = "header", i = 1, j = c("r", "p")) %>%
    flextable::merge_at(part = "header", i = 1, j = 1:2) %>%
    flextable::align(   part = "all",                     align = "center") %>%
    flextable::align(   part = "all",    j = 1:2,         align = "left") %>%
    flextable::align(   part = "body",   j = c("r", "p"), align = "right") %>%
    flextable::align(   part = "footer",                  align = "left") %>%
    flextable::autofit()


  return(table)

}
