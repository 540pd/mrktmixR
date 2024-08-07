% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reporting.R
\name{saturation_curve}
\alias{saturation_curve}
\title{Saturation Curve}
\usage{
saturation_curve(
  x_axis,
  alpha,
  beta,
  channels,
  round_digit_mapping = setNames(2, "M"),
  y_axis_func = function(x, alpha, beta) {
     alpha * (x^beta)
 }
)
}
\arguments{
\item{x_axis}{Numeric vector representing the x-axis values, typically impressions or spend.}

\item{alpha}{Numeric vector representing the coefficient values for each channel.}

\item{beta}{Numeric vector representing the power values for each channel.}

\item{channels}{Character vector representing the channel names.
If character, it represents the channel names.
If factor, it allows changing the order of legend levels based on the factor levels.}

\item{round_digit_mapping}{A named numeric vector specifying the number of digits to round each notation to (default is set to 2 for "M" notation).}

\item{y_axis_func}{function for mapping on y axis}
}
\value{
A ggplot object visualizing the saturation curves.
}
\description{
Calculate and visualize saturation curves based on input parameters.
}
\details{
This function calculates and visualizes saturation curves based on the input parameters: x-axis values, alpha values (coefficients), beta values (powers), and channel names.
The contribution for each channel is calculated using the formula: contribution = alpha * (x_axis^beta).
The resulting plot includes points, text labels, and line segments representing the saturation curves for each channel.

This round_digit_mapping parameter formats numeric vectors based on its parameter.
It rounds the numbers according to the specified digits for each notation and adds a prefix string if provided.
The 'notation' parameter should be an ordered factor with levels representing the notation for each number in 'numeric_vector'.
The 'round_digit_mapping' parameter is a named numeric vector where the names represent the notations and the values represent the number of digits to round to.
The default rounding is set to 2 digits for "M" notation.

Numbers with corresponding notation greater than the name of 'round_digit_mapping' will be rounded to 0.
Numbers with corresponding notation equal to the name of 'round_digit_mapping' will be rounded to the value of 'round_digit_mapping'.
Numbers greater than or equal to 1 with corresponding notation less than the name of 'round_digit_mapping' will be rounded to the value of 'round_digit_mapping'.
Numbers less than 1 will be converted to a significant number of 'round_digit_mapping'.
}
