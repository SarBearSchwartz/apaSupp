% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vif_to_tibble.R
\name{vif_to_tibble}
\alias{vif_to_tibble}
\title{APA: calculate VIF}
\usage{
vif_to_tibble(x, d = 2)
}
\arguments{
\item{x}{REQUIRED: a models (lm or glm), bare name}

\item{d}{Optional: Number. Digits after the decimal place}
}
\value{
a data.frame for use with apaSupp's model tables
}
\description{
APA: calculate VIF
}
\examples{

library(tidyverse)

data(mtcars)

mtcars <- mtcars \%>\%
  dplyr::mutate(vs = factor(vs,
                            levels = 0:1,
                            labels = c("V Shaped", "Straight"))) \%>\%
  dplyr::mutate(am = factor(am,
                            levels = 0:1,
                            labels = c("Automatic", "Manual"))) \%>\%
  dplyr::mutate(cyl  = factor(cyl)) \%>\%
  dplyr::mutate(gear = factor(gear))


fit_lm1 <- lm(mpg ~ wt + hp + cyl, data = mtcars)

vif_to_tibble(fit_lm1)

}
