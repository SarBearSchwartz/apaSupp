#' APA Formatted Table of a data.frame in non-Latex (for screen
#'
#' @param x a data frame
#' @param caption text for the caption
#' @param ... Other options to pass to gt::gt()
#'
#' @return a table
#' @import dplyr
#' @import gt
#' @export
#'
#' @examples
#' df <- head(cars)
#' gt_apa_screen(df, caption = "Testing")
gt_apa_screen <- function(x, caption, ...) {
  gt::gt(x, ...) %>%
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
                    missing_text = "") %>%
    gt::tab_caption(caption = caption)
}
