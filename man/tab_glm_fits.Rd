% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab_glm_fits.R
\name{tab_glm_fits}
\alias{tab_glm_fits}
\title{APA: flextable for Comparing the Performance of Generalize Linear models}
\usage{
tab_glm_fits(
  x,
  caption = "Comparison of Generalized Linear Model Performane Metrics",
  general_note = NA,
  d = 2
)
}
\arguments{
\item{x}{REQUIRED: List. at least 2 glm models, bare names, If named list, then names appear in the table}

\item{caption}{Optional: Text. Caption for the table}

\item{general_note}{Optional: Text. General note for footer of APA table}

\item{d}{Optional: Number. Digits after the decimal place}
}
\value{
a flextable object
}
\description{
Create a flextable for Comparing the Performance of Linear models via Several Metrics
}
\examples{

library(tidyverse)

mtcars <- mtcars \%>\% dplyr::mutate(cyl = factor(cyl))

fit_glm1 <- glm(vs ~ wt, data = mtcars, family = "binomial")
fit_glm2 <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")

apaSupp::tab_glm_fits(list(fit_glm1, fit_glm2))
apaSupp::tab_glm_fits(list("Univar" = fit_glm1, "Multivar" = fit_glm2))

}
