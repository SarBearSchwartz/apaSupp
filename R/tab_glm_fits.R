#' @title
#' APA: flextable for Comparing the Performance of Generalize Linear models
#'
#' @description
#' Create a flextable for Comparing the Performance of Linear models via Several Metrics
#'
#' @details
#' Model performance metrics
#'
#' In regression model, the most commonly known evaluation metrics include:
#' * **Akaike's Information Criteria (AIC)** is a metric developed by the Japanese Statistician, Hirotugu Akaike, 1970. The basic idea of AIC is to penalize the inclusion of additional variables to a model. It adds a penalty that increases the error when including additional terms. The **lower** the AIC, the better the model.
#' * **Bayesian information criteria (BIC)** is a variant of AIC with a stronger penalty for including additional variables to the model. The basic idea of AIC is to penalize the inclusion of additional variables to a model. It adds a penalty that increases the error when including additional terms. The **lower** the BIC, the better the model.
#' * **R-squared (R2)**, which is the proportion of variation in the outcome that is explained by the predictor variables. In multiple regression models, R2 corresponds to the squared correlation between the observed outcome values and the predicted values by the model. The **Higher** the R-squared, the better the model.
#' * **Adjusted R-squared (adj-R2)** adjusts the R2 for having too many variables in the model.  **Larger** values are better.
#' * **Root Mean Squared Error (RMSE)**, which measures the average error performed by the model in predicting the outcome for an observation. Mathematically, the RMSE is the square root of the mean squared error (MSE), which is the average squared difference between the observed actual outcome values and the values predicted by the model. So, MSE = mean((observed - predicted)^2) and RMSE = sqrt(MSE). The **lower** the RMSE, the better the model.
#'
#' Including additional variables in the model will **always** increase the R2 and reduce the RMSE.  Conversely, AIC, BIC, and adjusted-R2 penalize for model complexity and are more commonly used for model evaluation and selection, as these are unbiased estimated fo the model prediction error, and thus should be the basis of model comparison and optimal model selection.
#' Note: regression metrics are all internal measures, that is they have been computed on the **same data** that was used to build the regression model. They tell you how well the model fits to the data in hand.
#'
#'
#' @param x REQUIRED: List. at least 2 glm models, bare names, If named list, then names appear in the table
#' @param caption Optional: Text. Caption for the table
#' @param general_note Optional: Text. General note for footer of APA table
#' @param sort Optional: metrics to sort by, default = "AIC", but may use: "R2_Tjur", "R2_Nag", "AIC",  "BIC", "RMSE"
#' @param d Optional: Number. Digits after the decimal place
#'
#' @returns a flextable object
#' @import gtsummary
#' @import flextable
#' @import tidyverse
#' @import broom.helpers
#' @import performance
#' @export
#'
#' @examples
#'
#' fit1 <- glm(vs ~ wt, data = mtcars, family = "binomial")
#' fit2 <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' tab_glm_fits(list(fit1, fit2))
#'
tab_glm_fits <- function(x,
                         caption = "Comparison of Generalized Linear Model Performane Metrics",
                         general_note = NA,
                         sort = "AIC",
                         d = 2){

  ns <- sapply(x,function(y) length(y$residuals))

  nparams <- sapply(x,function(y) length(y$coefficients))

  nagR2 <- sapply(x, performance::r2_nagelkerke)

  if (length(unique(ns)) == 1){
    n <- unique(ns)
    note_sample <- glue::glue("N = {n}. ")
  } else {
    note_sample = "Models fit to different samples. "
  }

  final_note <- flextable::as_paragraph(flextable::as_i("Note. "),
                                        note_sample,
                                        flextable::as_i("k"),
                                        " = number of parameters estimated in each model. ",
                                        "Larger values indicated better performance for pseudo R-squared, both Tjur's  (",
                                        flextable::as_i(flextable::as_chunk("Tjur-R\u00B2")),
                                        ") and Nagelkerke's (",
                                        flextable::as_i(flextable::as_chunk("Nag-R\u00B2")),
                                        "). Smaller values indicated better performance for Akaike's Information Criteria (AIC), Bayesian information criteria (BIC), and Root Mean Squared Error (RMSE).",
                                        flextable::as_chunk(general_note))

  df <- performance::compare_performance(x) %>%
    data.frame() %>%
    dplyr::mutate(N = ns) %>%
    dplyr::mutate(k = nparams) %>%
    dplyr::mutate(R2_Nag = nagR2) %>%
    dplyr::select(Model = Name,
                  N, k,
                  R2_Tjur,
                  R2_Nag,
                  AIC,
                  BIC,
                  RMSE) %>%
    dplyr::arrange(sort) %>%
    dplyr::mutate(across(c(R2_Tjur, R2_Nag),
                         ~ apaSupp::p_num(., decimals = d + 1, stars = FALSE)))

  if (length(unique(ns)) == 1){
    df <- df %>%
      dplyr::select(-N)
  }

  tab <- df %>%
    flextable::flextable() %>%
    apaSupp::theme_apa(caption = caption,
                       p_note = NULL) %>%
    flextable::colformat_double(j = c("k"), big.mark = "", digits = 0) %>%
    flextable::colformat_double(j = c("AIC", "BIC", "RMSE"), big.mark = "", digits = d) %>%
    flextable::align(part = "all", j = c("R2_Tjur", "AIC"), align = "right") %>%
    flextable::align(part = "all", j = c("k", "R2_Nag", "BIC"), align = "left") %>%
    flextable::italic(part = "header", j = "k") %>%
    flextable::compose(part = "header",
                       j = "R2_Tjur",
                       value = flextable::as_paragraph(flextable::as_i(flextable::as_chunk("Tjur-R\u00B2"))))%>%
    flextable::compose(part = "header",
                       j = "R2_Nag",
                       value = flextable::as_paragraph(flextable::as_i(flextable::as_chunk("Nag-R\u00B2"))))%>%
    flextable::add_footer_lines("") %>%
    flextable::compose(i = 1, j = 1,
                       value = final_note,
                       part = "footer")

  return(tab)
}


