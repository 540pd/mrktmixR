% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_transformations.R
\name{aggregate_columns}
\alias{aggregate_columns}
\title{Aggregate Columns in a Data Frame}
\usage{
aggregate_columns(modeling_df, aggregated_variables, delimiter = "|")
}
\arguments{
\item{modeling_df}{A data frame containing the input variables.}

\item{aggregated_variables}{A character vector specifying the columns to
aggregate. The columns are expected to be delimited strings representing
the individual variables to aggregate.}

\item{delimiter}{A string used as a delimiter to split the names in
\code{aggregated_variables}. Default is "|".}
}
\value{
A data frame with columns specified in \code{aggregated_variables} aggregated.
}
\description{
Aggregates specified columns in a data frame and returns a new data frame with
these aggregated values. It takes a character vector of column names,
possibly representing aggregated variables separated by a delimiter, and
computes the sum of these variables row-wise.
}
\examples{
\dontrun{
  advertising <- data.frame(tv = runif(10), radio = runif(10), newspaper = runif(10),
   sales = runif(10))
  aggregate_columns(advertising, c("Sales", "TV|Radio|Newspaper"), delimiter = "|")
}

}
