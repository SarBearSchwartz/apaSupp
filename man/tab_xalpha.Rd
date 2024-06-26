% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab_xalpha.R
\name{tab_xalpha}
\alias{tab_xalpha}
\title{Tabulate Internal Consistency}
\usage{
tab_xalpha(df_long, caption, scale = "scale")
}
\arguments{
\item{df_long}{data frame with the following \code{id}, \code{item}, \code{value}, \code{scale},}

\item{caption}{the \code{flextable} caption for the table}

\item{scale}{the variable that denotes the subscale for each item}
}
\value{
a \code{flextable} table with caption
}
\description{
Tabulate Internal Consistency
}
\examples{
library(lavaan)
data(HolzingerSwineford1939, package = "lavaan")
df <- HolzingerSwineford1939
df \%>\%
  tidyr::pivot_longer(cols = starts_with("x"),
                      names_to = "item",
                      values_to = "value") \%>\%
  dplyr::mutate(scale = case_when(item \%in\% c("x1", "x2", "x3") ~ "Visual",
                                  item \%in\% c("x4", "x5", "x6") ~ "Verbal",
                                  item \%in\% c("x7", "x8", "x9") ~ "Speed")) \%>\%
  dplyr::select(id, item, value, scale) \%>\%
  tab_xalpha(caption = "Internal Consistency for Holzinger and Swineford Dataset, n = 301")

}
