eta2_to_tibble <- function(model) {
  eta2 <- DescTools::EtaSq(model)

  if (!is.matrix(eta2)){
    result <- eta2 %>%
      enframe("variable", "eta.sq", "eta.sq.part")
  } else {
    result <- eta2 %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "variable") %>%
      tibble::as_tibble()
  }

  result <- result %>%
    dplyr::mutate(row_type = "label")%>%
    dplyr::mutate(across(c(eta.sq, eta.sq.part),
                         ~ MOTE::apa(value = .,
                                     decimals = 3,
                                     leading = FALSE)))

  return(result)
}
