% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/p_lab.R
\name{p_lab}
\alias{p_lab}
\title{P-value Labels: 3-decimal places, no leading zero, and stars}
\usage{
p_lab(
  value,
  breaks = c(0.05, 0.01, 0.001),
  symbols = c("*", "**", "***"),
  d = 3,
  leading = FALSE
)
}
\arguments{
\item{value}{a numeric value for a statistical significance (p-value)}

\item{breaks}{statistical significance break points}

\item{symbols}{symbols to assign to each break point}

\item{d}{number of digits after the decimal point}

\item{leading}{keep the leading zero in front of the decimal}
}
\value{
character of the form "p = .231", "p = .022**", or "< .001***"
}
\description{
P-value Labels: 3-decimal places, no leading zero, and stars
}
\examples{
nums <- c(.36521, .02456, .0000000056)
p_lab(nums)
}
