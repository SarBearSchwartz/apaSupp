
#' Descriptive Summary of a Continuous Variable, Violins and Table
#'
#' @param df REQUIRED: data frame
#' @param var REQUIRED: Bare variable name.
#' @param split REQUIRED: Bare variable name. Row by a factor
#' @param lab Optional: Text. Label for x-axis of plot
#' @param digits Optional: Number. Digits after the decimal place
#' @param fontname Optional: Text.  Font used in table
#' @param heights Optional: Vector of 2 numbers. figure vs. table
#'
#' @return a `ggplot` object
#' @import tidyverse
#' @import rlang
#' @import dplyr
#' @import forcats
#' @import ggplot2
#' @import patchwork
#' @export
#'
#' @examples
#'
#' spicy_violins(df = mtcars,
#'               var = mpg,
#'               lab = "Fuel Economy, mpg",
#'               split = vs)
#'
#' spicy_violins(df = chickwts,
#'               var = weight,
#'               lab = "Chick Weight at Six Weeks (g)",
#'               split = feed,
#'               heights = c(2,1))
#'
spicy_violins <- function(df,
                          var,
                          lab = NULL,
                          split = NULL,
                          digits = 2,
                          fontname = "serif",
                          heights = c(5, 1.5)){


  var   <- rlang::enquo(var)
  split <- rlang::enquo(split)

  if (is.null(lab)){lab <- var}

  border.thick <- list("width" = 2, color = "black", style = "solid")
  border.thin  <- list("width" = 1, color = "black", style = "solid")

  layout <- "
              AAA
              #B#
              "
  df <- df %>%
    dplyr::mutate(across(c(!!split), factor))


  df_sum <- df  %>%
    dplyr::filter(complete.cases(!!var, !!split)) %>%
    dplyr::group_by(!!split) %>%
    dplyr::summarise(n = dplyr::n(),
                     M = stats::mean(!!var),
                     SD = stats::sd(!!var),
                     Min = base::min(!!var),
                     Q1 = stats::quantile(!!var, .25),
                     Mdn = stats::median(!!var),
                     Q3 = stats::quantile(!!var, .75),
                     Max = base::max(!!var)) %>%
    dplyr::mutate_if(is.numeric,
                     ~ round(.x, digits)) %>%
    dplyr::ungroup() %>%
    data.frame()

  tab <- df_sum %>%
    flextable::flextable() %>%
    flextable::line_spacing(space = .75, part = "all") %>%
    flextable::bold(j = c(3, 4, 7),      part = "all") %>%
    flextable::align(align = "center",   part = "all") %>%
    flextable::border_remove() %>%
    flextable::hline_top(part = "head", border = border.thick) %>%
    flextable::hline_bottom(part = "head", border = border.thin) %>%
    flextable::hline_bottom(part = "body", border = border.thick) %>%
    flextable::font(fontname = fontname, part = "all")


  p <- df %>%
    dplyr::filter(complete.cases(!!var, !!split)) %>%
    ggplot(aes(x = !!var,
               y = forcats::fct_rev(!!split))) +
    geom_violin(fill = "gray",
                color = "black",
                alpha = .5) +
    geom_boxplot(fill = "white",
                 color = "black",
                 alpha = .5,
                 width = .2) +
    stat_summary(fun = mean,
                 geom = "point",
                 shape = 18,
                 size = 5) +
    theme_bw() +
    labs(x = lab,
         y = NULL)

  x <- p /gen_grob(tab, fit = "width") +
    patchwork::plot_layout(design = layout,
                           widths = c(.5, 20, .5),
                           heights = heights)

  return(x)
}
