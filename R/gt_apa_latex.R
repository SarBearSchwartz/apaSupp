#' APA Formatted Table of a data.frame in Latex (for .pdf)
#'
#' @param x a data frame
#' @param caption text for the caption
#' @param lab text for the label
#' @param ... Other options to pass to gt::gt()
#'
#' @return latex code for a table
#' @import dplyr
#' @import gt
#' @export
#'
#' @examples
#' df <- head(cars)
#' gt_apa_latex(df, caption = "Testing", lab = "tab1")
gt_apa_latex <- function(x, caption, lab, ...) {

  x <- gt::gt(x, ...) %>%
    gt::tab_options(
      table.background.color = "white",
      table.border.top.color = "white",
      table.border.bottom.color = "white",
      heading.title.font.size = gt::px(16),
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table.width = gt::pct(100)
    ) %>%
    gt::cols_align(align="center") %>%
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = c("top", "bottom", "left", "right"),
          color = "white",
          weight = gt::px(1)
        ),
        gt::cell_text(
          align="center"
        ),
        gt::cell_fill(color = "white", alpha = NULL)
      ),
      locations = list(gt::cells_body(columns = everything(),
                                      rows = everything()),
                       gt::cells_stub()
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_text(align = "left"),
      locations = gt::cells_stub()
    ) %>%
    gt::opt_align_table_header(align = "left") %>%
    gt::sub_missing(columns = everything(),
                    missing_text = "")

  x <- gt::as_latex(x)
  cap <- paste0("\\caption{", caption, "}\n \\label{tab:", lab, "}\\\\")
  latex <- strsplit(x[1], split = "\n")[[1]]
  latex <- c(latex[1], cap, latex[-1])
  latex <- paste(latex, collapse = "\n")
  x[1] <- latex
  return(x)
}
