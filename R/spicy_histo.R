#' Descriptive Summary of a Continuous Variable, Histogram and Table
#'
#' @param df REQUIRED: data frame
#' @param var REQUIRED: Bare variable name.
#' @param lab Optional: Text. Label for x-axis of plot
#' @param digits Optional: Number. Digits after the decimal place
#' @param binwidth Optional: Number. Width of bins
#' @param bins Optional: Number. Quantity of bins (default = 10)
#' @param fontname Optional: Text.  Font used in table
#'
#' @return a `ggplot` object
#' @import tidyverse
#' @import rlang
#' @import dplyr
#' @import ggplot2
#' @import patchwork
#' @export
#'
#' @examples
#' # Simple
#' spicy_histo(mtcars, mpg)
#'
#' # More Complex
#' spicy_histo(df = mtcars,
#'             var = mpg,
#'             lab = "Fuel Economy, mpg",
#'             binwidth = 2)
#'
spicy_histo <- function(df,
                        var,
                        lab = NULL,
                        binwidth = NULL,
                        bins = 10,
                        digits = 2,
                        fontname = "serif"){

  var <- rlang::enquo(var)

  if (is.null(lab)){lab <- var}

  border.thick <- list("width" = 2, color = "black", style = "solid")
  border.thin  <- list("width" = 1, color = "black", style = "solid")

  layout <- "
              AAA
              #B#
              "

  df_sum <- df %>%
    dplyr::filter(complete.cases(!!var)) %>%
    dplyr::summarise(M   = base::mean(!!var),
                     Q1  = stats::quantile(!!var, .25),
                     Mdn = stats::median(!!var),
                     Q3  = stats::quantile(!!var, .75)) %>%
    dplyr::mutate_if(is.numeric,
                     ~ round(.x, digits)) %>%
    data.frame()


  tab <- df %>%
    dplyr::select(!!var) %>%
    tab_desc(caption = NULL,
             no_notes = TRUE) %>%
    flextable::delete_columns(j = 1)

  p <- df %>%
    dplyr::filter(complete.cases(!!var)) %>%
    ggplot(aes(!!var)) +
    geom_histogram(fill = "gray",
                   color = "black",
                   alpha = .5,
                   binwidth = binwidth,
                   bins = bins) +
    theme_bw() +
    geom_vline(data = df_sum,
               aes(xintercept = M),
               color = "black",
               linetype = "solid",
               linewidth = 2) +
    geom_vline(data = df_sum,
               aes(xintercept = Mdn),
               color = "gray45",
               linetype = "solid",
               linewidth = 1.25) +
    geom_vline(data = df_sum,
               aes(xintercept = Q1),
               color = "gray60",
               linetype = "solid",
               linewidth = .75) +
    geom_vline(data = df_sum,
               aes(xintercept = Q3),
               color = "gray60",
               linetype = "solid",
               linewidth = .75) +
    theme(legend.position = "none") +
    labs(x = lab,
         y = "Density")

  x <- p /gen_grob(tab, fit = "width") +
    patchwork::plot_layout(design = layout,
                           widths = c(.5, 20, .5),
                           heights = c(5, 1.5))

  return(x)
}
