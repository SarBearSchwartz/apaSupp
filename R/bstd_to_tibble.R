bstd_to_tibble <- function(model, d = 2) {

  result <- model %>%
    parameters::standardise_parameters() %>%
    as.data.frame() %>%
    dplyr::filter(Parameter != "(Intercept)") %>%
    dplyr::select("variable" = "Parameter",
                  "bs" = "Std_Coefficient") %>%
    dplyr::mutate(row_type = "label")%>%
    dplyr::mutate(across(c(bs),
                         ~ MOTE::apa(value = .,
                                     decimals = d,
                                     leading = TRUE)))

  return(result)
}
