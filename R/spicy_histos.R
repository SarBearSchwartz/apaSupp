#' Descriptive Summary of a Continuous Variable, Histograms and Table
#'
#' @param df REQUIRED: data frame
#' @param var REQUIRED: Bare variable name. Variable to compare
#' @param split REQUIRED: Bare variable name. Row by a factor
#' @param lab Optional: Text. Label for x-axis of plot
#' @param binwidth Optional: Number. Width of bins
#' @param bins Optional: Number. Quantity of bins (default = 10)
#' @param digits Optional: Number. Digits after the decimal place for numeric values
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
#'  spicy_histos(df = mtcars,
#'               var = mpg,
#'               split = am)
#'
#' # Complex
#' spicy_histos(df = mtcars,
#'              var = mpg,
#'              lab = "Fuel Economy, mpg",
#'              split = vs,
#'              binwidth = 2)
#'
spicy_histos <- function(df,
                         var,
                         split = NULL,
                         lab = NULL,
                         binwidth = NULL,
                         bins = 10,
                         digits = 2,
                         fontname = "serif"){

  var   <- rlang::enquo(var)
  split <- rlang::enquo(split)

  if (is.null(lab)){lab <- var}

  if (!is.null(binwidth)){bins = NULL}

  border.thick <- list("width" = 2, color = "black", style = "solid")
  border.thin  <- list("width" = 1, color = "black", style = "solid")

  layout <- "
              AAA
              #B#
              "

  df_sum <- df  %>%
    dplyr::filter(stats::complete.cases(!!var, !!split)) %>%
    dplyr::group_by(!!split) %>%
    dplyr::summarise(n = n(),
                     M = base::mean(!!var),
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

  # tab <- df %>%
  #   dplyr::select(!!var, !!split) %>%
  #   flextable::as_grouped_data(group = !!split) %>%
  #   tab_desc(caption = NULL,
  #            no_notes = TRUE)



  p <- df %>%
    dplyr::filter(complete.cases(!!var, !!split)) %>%
    ggplot(aes(!!var)) +
    geom_histogram(fill = "gray",
                   color = "black",
                   alpha = .5,
                   binwidth = binwidth,
                   bins = bins) +
    theme_bw() +
    facet_wrap(vars(!!split), ncol = 1) +
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
