tab_glm_exp <- function(x,
                        var_labels = NULL,
                        caption = "Generalized Regression Model",
                        p_note = "apa",
                        type = "logistic",
                        general_note = NA,
                        d = 2){

  n_param <- x %>%
    coef() %>%
    length()

  # n_fit <- length(fit)


  if (is.null(p_note)){
    p_note <- NULL
  } else if (p_note == "apa"){
    p_note <- "* p < .05. ** p < .01. *** p < .001."
  } else {
    p_note <- p_note
  }

  if (type == "logistic"){
    abr1 <- "Odds Ratio, OR"
    abr2 <- "Logit Scale, b"
    main_note <- flextable::as_paragraph(flextable::as_i("Note. "),
                                         flextable::as_chunk(glue::glue("N = {length(x$resid)}. ")),
                                         flextable::as_i("CI"),
                                         " = confidence interval; ",
                                         flextable::as_i("p"),
                                         " = significance from Wald t-test for parameter estimate. ",
                                         flextable::as_chunk(general_note))
  } else {
    abr1 = "exp(b)"
    abr2 = "Log Scale, b"
    main_note <- flextable::as_paragraph(flextable::as_i("Note. "),
                                         flextable::as_chunk(glue::glue("N = {length(x$resid)}. ")),
                                         flextable::as_i("CI"),
                                         " = confidence interval; ",
                                         flextable::as_i("p"),
                                         " = significance from Wald t-test for parameter estimate. ",
                                         flextable::as_chunk(general_note))
  }



  get_exp <- x %>%
    gtsummary::tbl_regression(intercept = FALSE,
                              conf.int = TRUE,
                              exponentiate = TRUE,
                              tidy_fun = broom.helpers::tidy_with_broom_or_parameters) %>%
    gtsummary::modify_column_hide(column = std.error) %>%
    gtsummary::modify_column_hide(column = p.value) %>%
    gtsummary::modify_fmt_fun(estimate ~
                                gtsummary::label_style_number(digits = d)) %>%
    gtsummary::modify_fmt_fun(conf.low ~
                                gtsummary::label_style_number(digits = d,
                                                              prefix = "[")) %>%
    gtsummary::modify_fmt_fun(conf.high ~
                                gtsummary::label_style_number(digits = d,
                                                              suffix = "]")) %>%
    gtsummary::remove_abbreviation("OR = Odds Ratio") %>%
    gtsummary::remove_abbreviation("CI = Confidence Interval") %>%
    gtsummary::remove_abbreviation("SE = Standard Error")  %>%
    gtsummary::modify_header(label = "Variable",
                             estimate = "Est",
                             conf.low = "95% CI")

  get <- x %>%
    gtsummary::tbl_regression(intercept = TRUE,
                              conf.int = FALSE,
                              exponentiate = FALSE,
                              pvalue_fun = apaSupp::p_num,
                              tidy_fun = broom.helpers::tidy_with_broom_or_parameters) %>%
    # gtsummary::add_glance_table(include = fit) %>%
    gtsummary::modify_column_unhide(column = std.error) %>%
    gtsummary::remove_abbreviation("OR = Odds Ratio") %>%
    # gtsummary::remove_abbreviation("CI = Confidence Interval") %>%
    gtsummary::remove_abbreviation("SE = Standard Error")  %>%
    gtsummary::remove_footnote_header() %>%
    gtsummary::modify_fmt_fun(estimate ~
                                gtsummary::label_style_number(digits = d)) %>%
    gtsummary::modify_fmt_fun(std.error ~
                                gtsummary::label_style_number(digits = d,
                                                              prefix = "(",
                                                              suffix = ")")) %>%
    gtsummary::modify_header(label = "Variable",
                             estimate = "Est",
                             std.error = "(SE)",
                             p.value = "p")


  get_logit <- get %>%
    gtsummary::modify_column_hide(p.value)

  get_p <- get %>%
    gtsummary::modify_column_hide(c(estimate, std.error))


  table <- list(get_exp, get_logit, get_p) %>%
    gtsummary::tbl_merge(tab_spanner = c(abr1, abr2, "p")) %>%
    gtsummary::as_flex_table()

  n_rows <- flextable::nrow_part(table, part = "body")

  n_fit = 0

  rows_fit <- (n_rows - n_fit + 1):(n_rows)

  table <- table %>%
    apaSupp::theme_apa(caption = caption,
                       no_notes = FALSE,
                       d = d) %>%
    # flextable::hline(i = n_rows) %>%
    flextable::italic(part = "header", i = 2) #%>%
  # flextable::italic(part = "body", i = rows_fit)

  if(!is.null(var_labels)){
    table <- table %>%
      flextable::labelizor(part = "body",
                           labels = var_labels)
  }

  table <- table %>%
    flextable::align(part = "all", j = c(2, 4), align = "right") %>%
    flextable::align(part = "all", j = c(3, 5), align = "left") %>%
    flextable::align(part = "header", i = 2, j = 3, align = "center") %>%
    flextable::align(part = "header", i = 1, align = "center") %>%
    flextable::align(part = "footer", align = "left") %>%
    flextable::compose(part = "header", i = 1, j = 6,
                       value = flextable::as_paragraph(NA)) %>%
    flextable::hline(part = "header", i = 1,
                     border = flextable::fp_border_default(width = 0)) %>%
    flextable::add_footer_lines("") %>%
    flextable::compose(i = 1, j = 1,
                       value = main_note,
                       part = "footer")


  if (!is.null(p_note)){
    table <- table %>%
      flextable::add_footer_lines("") %>%
      flextable::compose(i = 2, j = 1,
                         value = flextable::as_paragraph(flextable::as_chunk(p_note)),
                         part = "footer")
  }


  return(table)
}
