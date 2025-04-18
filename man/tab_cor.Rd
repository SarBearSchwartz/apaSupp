% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab_cor.R
\name{tab_cor}
\alias{tab_cor}
\title{Tabulate Pairwise Pearson's Correlation}
\usage{
tab_cor(
  x,
  caption = "Pairwise Correlations",
  general_note = NA,
  p_note = "apa123",
  d = 2,
  breaks = c(0.05, 0.01, 0.001),
  symbols = c("*", "**", "***")
)
}
\arguments{
\item{x}{REQUIRED: A dataframe with selected numeric variables}

\item{caption}{REQUIRED: Text. Caption for the table}

\item{general_note}{Optional: Text. General note for footer of APA table}

\item{p_note}{Optional: Text. (default = NULL) Significance note for APA table, If \code{p_note = "apa"} then the standard \code{"* p < .05. ** p < .01. *** p < .001."} will be used}

\item{d}{Optional: Number. Digits after the decimal place}

\item{breaks}{statistical significance break points}

\item{symbols}{symbols to assign to each break point}
}
\value{
table
}
\description{
Tabulate Pairwise Pearson's Correlation
}
\examples{
library(tidyverse)

apaSupp::tab_cor(mtcars \%>\% dplyr::select(cyl, mpg, disp, hp))

}
