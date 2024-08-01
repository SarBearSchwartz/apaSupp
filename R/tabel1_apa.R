table1_apa <- function(tab1,
                       caption,
                       general_note = NULL,
                       p_note = "apa",
                       var_type = "factor",
                       max_width_in = 6,
                       digits = 2){
  # tab1 is the output from `furniture::table1()` without caption
  df <- tab1 %>%
    as.data.frame()

  c <- ncol(df)
  n <- names(df)[2:(c - 1)]

  if (var_type == "factor"){
    standard_note <- glue::glue("NA = not available or missing. Categorical variables are compared via Chi-squred Test(s) for Independence.")
  } else if (var_type == "numeric"){
    standard_note <- glue::glue("Continuous variables are compared via independent ANOVA.")
  } else if (var_type == "both"){
    standard_note <- glue::glue("NA = not available or missing. Categorical variables are compared via Chi-squred Test(s) for Independence and continuous variables are compared via independent ANOVA.")
  }

  if (is.null(general_note)){
    main_note <- standard_note
  } else {
    main_note <- paste(general_note, standard_note, sep = " ")
  }


  tab <- df %>%
    dplyr::rename(p = "P.Value") %>%
    dplyr::mutate(p = as.numeric(p)) %>%
    dplyr::mutate(stars = case_when(p <= .001 ~ "***",
                                    p <= .010 ~ "**",
                                    p <= .050 ~ "*")) %>%
    dplyr::mutate(p = apaSupp::p_num(p,
                                     symbols = c("", "", ""))) %>%
    flextable::flextable() %>%
    flextable::delete_part(part = "header") %>%
    flextable::add_header_row(values = c("", n, "p", ""),
                              colwidths = rep(1, times = c + 1)) %>%
    theme_apa(caption = caption,
              general_note = main_note,
              p_note = p_note,
              digits = digits,
              max_width_in = max_width_in) %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::align(j = "p", align = "right", part = "all") %>%
    flextable::align(j = "stars", align = "left", part = "all") %>%
    flextable::padding(padding.right = 0, j = "p",     part  = "all")  %>%
    flextable::padding(padding.left = 0, j = "stars", part  = "all")

  return(tab)
}
