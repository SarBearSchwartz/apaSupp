vif_to_tibble <- function(model, d = 2) {

  vif <- car::vif(model)

  if (!is.matrix(vif)){
    result <- vif %>%
      tibble::enframe("variable", "vif")
  } else {
    result <- vif %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "variable") %>%
      tibble::as_tibble() %>%
      dplyr::select(variable, vif = GVIF)
  }

  result <- result %>%
    dplyr::mutate(row_type = "label")%>%
    dplyr::mutate(vif = MOTE::apa(value = vif,
                                  decimals = d,
                                  leading = TRUE))

  return(result)
}
