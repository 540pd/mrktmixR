% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_iteration.R
\name{collate_models}
\alias{collate_models}
\title{Collate Models}
\usage{
collate_models(
  candidate_variables_list,
  model_df,
  dep_var_info,
  with_intercept,
  base_variables,
  pos_sign_variables = NA,
  neg_sign_variables = NA,
  mdl_start_date = NA,
  mdl_end_date = NA,
  var_agg_delimiter = "|",
  apl_delimiter = "_",
  var_apl_delimiter = "|",
  drop_flexi_vars = TRUE,
  run_up_to_flexi_vars = NA,
  vif_threshold = 10,
  pvalue_thresholds = c(intercept = 0.15, fixed = 0.15, flexible = 0.15),
  drop_pvalue_precision = 2,
  drop_discard_estimate_sign = TRUE,
  drop_highest_estimate = FALSE,
  get_model_object = FALSE,
  defer_intercept_test = FALSE,
  always_check_vif = FALSE,
  verbose = FALSE
)
}
\arguments{
\item{candidate_variables_list}{A list of lists, where each sublist contains
candidate predictors for a model. Each sublist should have a structure similar
to the \code{candidate_predictors} parameter in \code{assemble_base_models}.}

\item{model_df}{A dataframe containing independent variables for all models.}

\item{dep_var_info}{Information about the dependent variable, which can be
provided in one of the following formats:
\enumerate{
\item \strong{String}: A string representing the name of the dependent variable.
This variable name must be present in the modeling dataset (model_df).
\item \strong{Named Vector}: A named vector where names represent the variable
along with its adstock, power, and lag information, denoted by consistent
delimiters (e.g., "var_adstock_0.5_power_2_lag_3"). The values in the
vector represent the coefficient of the variable.
\item \strong{List}: A list that can take two forms:
\enumerate{
\item A list of named vectors, where each named vector follows the
format specified in the Named Vector option.
\item A list containing lists of adstock, power, and lag information.
Each of these (adstock, power, lag) is itself a list containing
named vectors with 'start', 'stop', and 'step', representing the
parameters for adstock, power, and lag transformations.
}
}
This parameter is crucial for specifying how the dependent variable is treated
in the model, including any transformations or lags applied.}

\item{with_intercept}{Logical; if TRUE, an intercept is included in the model.
Default is TRUE.}

\item{base_variables}{A character vector of variable names representing base
variables in the model. The coefficients of these variables are set to 1.
Use \code{NA} if no base variable is required in the model.}

\item{pos_sign_variables}{A vector of variable names expected to have a positive
sign in the model.}

\item{neg_sign_variables}{A vector of variable names expected to have a negative
sign in the model.}

\item{mdl_start_date}{The start date for filtering the data before model assembly.
Should be of class \code{Date}. Default is NA.}

\item{mdl_end_date}{The end date for filtering the data before model assembly.
Should be of class \code{Date}. Default is NA.}

\item{var_agg_delimiter}{A character string used as the delimiter to aggregate
variables. Default is "\\|".}

\item{apl_delimiter}{The delimiter used in variable names to separate APL components.
Default is "_".}

\item{var_apl_delimiter}{The delimiter used between variable names and APL attributes.
Default is "|".}

\item{drop_flexi_vars}{Logical; specifies the method for handling flexible variables.
If TRUE, flexible variables are dropped iteratively. If FALSE, independent
variables are tested only once in the model.}

\item{run_up_to_flexi_vars}{An integer representing the number of flexible
variables to consider up to. Default is 0.}

\item{vif_threshold}{A numeric threshold for the Variance Inflation Factor (VIF)
above which variables are dropped. Default is 10.}

\item{pvalue_thresholds}{A named list containing p-value thresholds for
intercept, fixed, and flexible variables. Default is c(intercept = 0.15,
fixed = 0.15, flexible = 0.15).}

\item{drop_pvalue_precision}{The precision for the p-value to drop variables.
Default is 2.}

\item{drop_discard_estimate_sign}{Logical; if TRUE, the sign of the estimate is
ignored when dropping variables. Default is TRUE.}

\item{drop_highest_estimate}{Logical; if TRUE, the variable with the highest
estimate is dropped. Default is FALSE.}

\item{get_model_object}{Logical; if TRUE, the function returns the linear model
object instead of the default list. Default is FALSE.}

\item{defer_intercept_test}{Logical; if TRUE, the intercept test is deferred.}

\item{always_check_vif}{Logical; if TRUE, the Variance Inflation Factor (VIF)
is always checked. If FALSE, VIF will only be checked if there are no flags
for p-value and no signs for the estimate. Default is FALSE.}

\item{verbose}{Logical indicating whether to print time and number of models. Default is \code{FALSE}.}
}
\value{
A list containing the collated models after filtering and preprocessing,
along with associated statistics. This list includes details about the dependent
variable transformations, summary statistics, model coefficients, and linear model objects.
}
\description{
Extends the \code{collate_base_models} function to include additional preprocessing
of the dependent variable. This function collates models for linear regression
from a list of candidate variable sets, applying preprocessing steps to the
dependent variable and assembling the models.
}
\examples{
\dontrun{
candidate_variables_list <- list(
  list(fixed = c("var1"), flexible = c("var3")),
  list(fixed = c("var2"), flexible = c("var4"))
)
independent_df <- data.frame(var1 = rnorm(100), var2 = rnorm(100))
dependent_df <- data.frame(Y = rnorm(100))
dep_info <- list(transformation = "log", lag = 1)
filter_start <- as.Date("2000-01-01")
filter_end <- as.Date("2005-12-31")

results <- collate_models(
  candidate_variables_list,
  model_df = independent_df,
  model_dep_df = dependent_df,
  dep_var_info = dep_info,
  mdl_start_date = filter_start,
  mdl_end_date = filter_end
  # Additional parameters can be set as needed
)
}

}
