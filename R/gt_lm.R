#' APA: gtsummary for a single linear model
#'
#' @param x REQUIRED: a single 'lm' object, bare name
#' @param d OPTIONAL: number of digits, default = 2
#'
#' @return a gtsummary object
#' @import gtsummary
#' @import tidyverse
#' @import broom.helpers
#' @export
#'
#' @examples
#' m <- lm(dist ~ speed, data = cars)
#'gt_lm(m)
#'
#'
#'
gt_lm <- function(x,
                  d = 2){

  table <- x %>%
    gtsummary::tbl_regression(intercept = TRUE,
                              conf.int = FALSE,
                              pvalue_fun = apaSupp::p_num,
                              tidy_fun = broom.helpers::tidy_with_broom_or_parameters) %>%
    gtsummary::add_glance_table(include = c("r.squared",
                                            "adj.r.squared")) %>%
    gtsummary::modify_column_unhide(column = std.error) %>%
    gtsummary::remove_footnote_header() %>%
    gtsummary::remove_abbreviation("SE = Standard Error")  %>%
    gtsummary::modify_fmt_fun(c(estimate, std.error) ~
                                label_style_number(digits = d)) %>%
    gtsummary::modify_header(label = "Variable",
                             estimate = "b",
                             std.error = "SE",
                             p.value = "p")

  return(table)
}


