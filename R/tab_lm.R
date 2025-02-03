

tab_lm <- function(x,
                   var_labels = NULL,
                   caption = "Regression Model",
                   p_note = "apa",
                   general_note = NULL,
                   fit = c("r.squared",
                           "adj.r.squared"),
                   d = 2){

  n_param <- x %>%
    coef() %>%
    length()


  get <- x %>%
    gtsummary::tbl_regression(intercept = TRUE,
                              conf.int = TRUE,
                              pvalue_fun = apaSupp::p_num,
                              tidy_fun = broom.helpers::tidy_with_broom_or_parameters) %>%
    gtsummary::add_glance_table(include = fit) %>%
    gtsummary::modify_column_unhide(column = std.error) %>%
    gtsummary::remove_footnote_header() %>%
    gtsummary::remove_abbreviation("SE = Standard Error")  %>%
    gtsummary::modify_fmt_fun(estimate ~
                                label_style_number(digits = d)) %>%
    gtsummary::modify_fmt_fun(std.error ~
                                label_style_number(digits = d,
                                                   prefix = "(",
                                                   suffix = ")")) %>%
    gtsummary::modify_fmt_fun(conf.low ~
                                label_style_number(digits = d,
                                                   prefix = "[")) %>%
    gtsummary::modify_fmt_fun(conf.high ~
                                label_style_number(digits = d,
                                                   suffix = "]")) %>%
    gtsummary::modify_header(label = "Variable",
                             estimate = "b",
                             std.error = "(SE)",
                             conf.low = "[95% CI]",
                             p.value = "p")

  table <- get %>%
    gtsummary::as_flex_table()

  n_rows <- flextable::nrow_part(table, part = "body")

  table <- table %>%
    apaSupp::theme_apa(caption = caption,
                       p_note = p_note,
                       general_note = general_note) %>%
    flextable::hline(i = n_rows - 2) %>%
    flextable::italic(part = "header", i = 1) %>%
    flextable::italic(part = "body", i = c(n_rows - 1,
                                           n_rows - 0))

  if(!is.null(var_labels)){
    table <- table %>%
      flextable::labelizor(part = "body",
                           labels = var_labels)
  }

  return(table)

}
