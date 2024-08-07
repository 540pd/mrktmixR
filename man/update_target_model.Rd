% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auxiliary_functions.R
\name{update_target_model}
\alias{update_target_model}
\title{Update Target Model}
\usage{
update_target_model(
  target_model,
  new_target_model,
  independent_variables,
  try_append_feasible_models = FALSE,
  verbose = FALSE
)
}
\arguments{
\item{target_model}{Original target model data frame.}

\item{new_target_model}{Data frame of new feasible target models to be added.}

\item{independent_variables}{Character vector of independent variables in the model.}

\item{try_append_feasible_models}{Logical, whether to append target model parameters during reverse modeling. Only relevant for hierarchical models. Default is FALSE. This is applied when re-running the model and the same variable is part of stage 0.}

\item{verbose}{Logical indicating whether to print details during processing. Default is FALSE.}
}
\value{
Updated target model data frame.
}
\description{
Update the target model based on new feasible target models.
}
