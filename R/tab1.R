#' Descriptive Table, total or split by a factor
#'
#' @param df (`data.frame`)\cr A data frame.
#' @param split Optional: Quoted variable name
#' @param caption Optional: Text. Caption for the table
#' @param docx Optional: Text.  Word file to store the table.  Must end with ".docx"
#' @param tab_width Optional: numberic value (default is .9) % of available width
#' @param no_notes Optional: Logical.  Defaults to `FALSE`, if `TRUE` will ignore `general_note` and `p_note`
#' @param general_note Optional: Text. General note for footer of APA table
#' @param p_note Optional: Text. (default = NULL) Significance note for APA table, If `p_note = "apa123"` then the standard `"* p < .05. ** p < .01. *** p < .001."` will be used
#' @param total Optional: Logical. Default = TRUE to include a total column in a split table
#' @param var_lab Optional: Text. Label to print above the variable names in the header
#' @param total_lab Optional: Text. Label above the total column in a split table
#' @param total_last Optional: Logical. Default = TRUE to include the total column last vs first
#' @param test Optional: Logical. Default = TRUE to run comparisons in a split table
#' @param na.rm Optional: Logical. Default = FALSE, do not remove instances with a missing value from the entire table
#' @param breaks Optional: numeric vector of p-value cut-points
#' @param symbols Optional: character vector for symbols denoting p-value cut-points
#' @param d Optional: Number. Digits after the decimal place
#' @param statistic ([`formula-list-selector`][syntax])\cr
#'   Used to specify the summary statistics for each variable.
#'   The default is
#'   `list(all_continuous()  ~ "{mean} ({sd})", all_categorical() ~ "{n}    ({p}%)")`.
#' @param type ([`formula-list-selector`][syntax])\cr
#'   Specifies the summary type. Accepted value are
#'   `c("continuous", "continuous2", "categorical", "dichotomous")`.
#'   If not specified, default type is assigned via
#'   `assign_summary_type()`.
#' @param value ([`formula-list-selector`][syntax])\cr
#'   Specifies the level of a variable to display on a single row.
#'   The gtsummary type selectors, e.g. `all_dichotomous()`, cannot be used
#'   with this argument. Default is `NULL`.
#' @param missing,missing_text,missing_stat
#'   Arguments dictating how and if missing values are presented:
#'   - `missing`: must be one of `c("ifany", "no", "always")`.
#'   - `missing_text`: string indicating text shown on missing row. Default is `"Unknown"`.
#'   - `missing_stat`: statistic to show on missing row. Default is `"{N_miss}"`.
#'     Possible values are `N_miss`, `N_obs`, `N_nonmiss`, `p_miss`, `p_nonmiss`.
#' @param sort ([`formula-list-selector`][syntax])\cr
#'   Specifies sorting to perform for categorical variables.
#'   Values must be one of `c("alphanumeric", "frequency")`.
#'   Default is `all_categorical(FALSE) ~ "alphanumeric"`.
#' @param percent (`string`)\cr
#'   Indicates the type of percentage to return.
#'   Must be one of `c("column", "row", "cell")`. Default is `"column"`.
#'
#'   In rarer cases, you may need to define/override the typical denominators.
#'   In these cases, pass an integer or a data frame. Refer to the
#'   [`?cards::ard_categorical(denominator)`][cards::ard_categorical] help file for details.
#' @param include ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Variables to include in the summary table. Default is `everything()`.
#'
#'
#'
#'
#' @return a `flextable` table with caption
#' @import tidyverse
#' @import flextable
#' @import gtsummary
#' @import cardx
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(gtsummary)
#'
#' data(mtcars, package = "datasets")
#'
#' mtcars <- mtcars %>%
#'   dplyr::mutate(cyl = factor(cyl)) %>%
#'   dplyr::mutate(vs = factor(vs,
#'                             levels = c(0, 1),
#'                             labels = c("V-shaped", "Straight"))) %>%
#'   dplyr::mutate(am = factor(am,
#'                             levels = c(0, 1),
#'                             labels = c("Automatic",
#'                                        "Manual")))
#'
#'
#' ##########################
#'
#' tab1(mtcars)
#'
#'
#' mtcars %>%
#'   dplyr::select(cyl,
#'                 "Fuel Economy, mpg" = mpg,
#'                 "Dispacement, cu-in" = disp,
#'                 "Gross Horsepower" = hp,
#'                 "Real Axel Ratio" = drat,
#'                 "Weight, 1000 lbs" = wt,
#'                 "Engine Type" = vs) %>%
#'   tab1(split        = "cyl",
#'        total_last   = FALSE,
#'        caption      = "Descriptive Summary of Car Road Tests",
#'        general_note = "Data extracted from the 1974 Motor Trend US magazine.",
#'        p_note       = "apa3")

tab1 <- function(df,
                 split        = NULL,
                 caption      = "Summary of Variables",
                 docx         = NA,
                 tab_width    = .9,
                 no_notes     = FALSE,
                 general_note = NA,
                 p_note       = "apa123",
                 total        = TRUE,
                 var_lab      = " ",
                 test         = TRUE,
                 total_lab    = "Total\nN = {N}",
                 total_last   = TRUE,
                 na.rm        = FALSE,
                 breaks       = c(.05, .01, .001),
                 symbols      = c("*", "**", "***"),
                 d            = 2,
                 statistic    = list(all_continuous()  ~ "{mean} ({sd})",
                                     all_categorical() ~ "{n}    ({p}%)"),
                 value        = NULL,
                 type         = NULL,
                 missing      = c("ifany", "no", "always"),
                 missing_text = "Unknown",
                 missing_stat = "{N_miss}",
                 sort         = all_categorical(FALSE) ~ "alphanumeric",
                 percent      = c("column", "row", "cell"),
                 include      = everything()){


  if (!is.null(split)){
    n_grps <- df %>%
      dplyr::pull(split) %>%
      levels() %>%
      length()
  } else {
    total  <- FALSE
    test   <- FALSE
    n_grps <- 1
    p_note <- NA
  }

    if(test == FALSE) p_note <- NA

    vars <- df %>%
      dplyr::select(-!!split) %>%
      names()

    n_tot <- df %>%
      dplyr::select(-!!split) %>%
      ncol()

    n_factors <- df %>%
      dplyr::select(-!!split) %>%
      dplyr::select_if(is.factor) %>%
      ncol()

    n_numeric <-  df %>%
      dplyr::select(-!!split) %>%
      dplyr::select_if(is.numeric) %>%
      ncol()

    if (no_notes == TRUE){
      general_note <- NA
      p_note       <- NA
    }


  main_note <- flextable::as_paragraph(
    flextable::as_i("Note. "),
    flextable::as_chunk(ifelse(n_numeric > 0,
                               "Continuous variables are summarised with means ",
                               NA)),
    flextable::as_i(ifelse(n_numeric > 0,
                           "(SD)",
                           NA)),
    flextable::as_chunk(ifelse(n_numeric > 0 & test == TRUE,
                               " and significant group differences assessed via ",
                               ". ")),
    flextable::as_chunk(case_when(n_numeric > 0 & test == TRUE & n_grps > 2
                               ~ "independent one-way analysis of vaiance (ANOVA). ",
                               n_numeric > 0 & test == TRUE & n_grps == 2
                               ~ "independent t-tests. ")),
    flextable::as_chunk(ifelse(n_numeric > 0,
                               "Categorical variables are summarised with counts ",
                               NA)),
    flextable::as_i(ifelse(n_numeric > 0,
                           "(%)",
                           NA)),
    flextable::as_chunk(ifelse(n_numeric > 0 & test == TRUE,
                               " and significant group differences assessed via Chi-squared tests for independence. ",
                               ". ")),
    flextable::as_chunk(ifelse(!is.na(general_note), general_note, NA))
  )


  gt <- df %>%
    gtsummary::tbl_summary(by           = split,
                           statistic    = statistic,
                           type         = type,
                           value        = value,
                           digits       = list(all_continuous()  ~ c(d, d),
                                               all_categorical() ~ c(0, 1)),
                           missing      = missing,
                           missing_text = missing_text,
                           missing_stat = missing_stat,
                           sort         = sort,
                           percent      = percent,
                           include      = include) %>%
    gtsummary::modify_header(label      = var_lab)

  if(n_grps > 1){
    gt <- gt %>%
      gtsummary::modify_header(all_stat_cols() ~ "{level}\nn = {n} ({style_percent(p, digits = 1)}%)")
  }


  if(total == TRUE){
    gt <- gt %>%
      gtsummary::add_overall(last = total_last,
                             col_label = total_lab)
  }

  if(test == TRUE){
    gt <- gt %>%
      gtsummary::add_p(
        test = list(all_continuous()  ~ "oneway.test",
                    all_categorical() ~ "chisq.test"),
        pvalue_fun = ~ apaSupp::p_num(.x,
                                      d       = d + 1,
                                      breaks  = breaks,
                                      symbols = symbols))
  }

  table <- gt %>%
    gtsummary::remove_footnote_header(columns = everything()) %>%
    gtsummary::as_flex_table() %>%
    apaSupp::theme_apa(caption      = caption,
                       main_note    = main_note,
                       p_note       = p_note,
                       breaks       = breaks,
                       symbols      = symbols) %>%
    flextable::set_table_properties(layout = "autofit",
                                    width = tab_width)

  if (!is.na(docx)){
    flextable::save_as_docx(table,
                            path = docx)
  }

  return(table)
}
