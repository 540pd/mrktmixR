#' Compose Variable Names with Adstock, Power, and Lag (APL) Attributes
#'
#' Constructs standardized variable names by incorporating Adstock, Power, and Lag
#' (APL) attributes into each variable's name. This function is useful for creating
#' clear and descriptive variable names in models where APL transformations are applied.
#'
#' @param variables_wt_named_apl A list where each element is a named vector representing
#'        a variable. The names of the elements are the variable names, and each named
#'        vector contains the APL attributes: adstock, power, and lag.
#' @param apl_delimiter The delimiter to use between the variable name and its APL values
#'        (default is "_").
#' @param delimiter The delimiter to use between different APL attributes
#'        (default is "|").
#'
#' @return A character vector where each element is a variable name composed with
#'         its APL attributes. These names follow the format:
#'         `variable_name[delimiter]adstock_value[apl_delimiter]power_value[apl_delimiter]lag_value`.
#'
#' @examples
#' \dontrun{
#'   variables_wt_named_apl <- list(
#'     "variable1" = c("adstock" = 0.5, "power" = 2, "lag" = 1),
#'     "variable2" = c("adstock" = 0.3, "power" = 1, "lag" = 0)
#'   )
#'   composed_names <- compose_variable_apl(variables_wt_named_apl,
#'                                          apl_delimiter = "_",
#'                                          delimiter = "|")
#'   print(composed_names)
#' }
#'
compose_variable_apl <-
  function(variables_wt_named_apl,
           apl_delimiter = "_",
           delimiter = "|") {
    # Compose variable names with their APL attributes into a standardized format
    result <- mapply(function(name, values) {
      # Concatenate each value with the variable name using the provided delimiters
      paste(name, paste(unlist(values), collapse = apl_delimiter), sep = delimiter)
    },
    names(variables_wt_named_apl),
    variables_wt_named_apl,
    SIMPLIFY = FALSE)

    # Combine the result into a single character vector without names
    unname(unlist(result))
  }

#' Parse Variable with Adstock, Power, and Lag
#'
#' Parses a vector of strings containing variable information along with Adstock,
#' Power, and Lag (APL) attributes, separated by specified delimiters. Each string
#' in the vector should contain a variable name followed by up to three numeric
#' values representing the APL attributes, in a consistent order and separated
#' by the delimiters.
#'
#' @param variables_wt_apl A character vector where each element contains a
#'   variable name and up to three numeric values for adstock, power, and lag,
#'   separated by `delimiter` and `apl_delimiter`.
#' @param apl_delimiter A string representing the delimiter used between the
#'   adstock, power, and lag values in each string. It cann't be period (.)
#' @param delimiter A string representing the main delimiter used between the
#'   variable name and the APL attributes in each string. It cann't be period (.)
#'
#' @return A data frame where each row corresponds to an element in
#'   `variables_wt_apl`, with columns for the variable name, adstock value,
#'   power value, and lag value.
#'
#' @examples
#' \dontrun{
#'   parsed_data <- parse_variable_wt_apl(c("TV_Smart|0.8_0.22_0.11", "Radio|0.5_0.15"),
#'                                        apl_delimiter = "_",
#'                                        delimiter = "|")
#'   print(parsed_data)
#' }
#'
#' @importFrom stats na.omit
#'
parse_variable_wt_apl <-
  function(variables_wt_apl,
           apl_delimiter = "_",
           delimiter = "|") {
    # Escape the special characters in regular expression
    escaped_delimiter <-
      gsub("([.|()\\[^$?*+])", "\\\\\\1", delimiter)
    escaped_apl_delimiter <-
      gsub("([.|()\\[^$?*+])", "\\\\\\1", apl_delimiter)

    regex_pattern <-
      paste0(
        "(.+?)",
        escaped_delimiter,
        "([0-9.]+",
        escaped_apl_delimiter,
        "?[0-9.]*",
        escaped_apl_delimiter,
        "?[0-9.]*)$"
      )

    matches <- stringr::str_match(variables_wt_apl, regex_pattern)

    if (any(!stats::complete.cases(matches))) {
      matches[!stats::complete.cases(matches),] <-
        c(variables_wt_apl[!stats::complete.cases(matches)],
          variables_wt_apl[!stats::complete.cases(matches)],
          paste(0, 1, 0, sep = apl_delimiter))
    }
    if (any(is.na(matches))) {
      warning("Some inputs did not match the expected format and will be omitted.")
      matches <- na.omit(matches)
    }

    data.frame(
      variable = matches[, 2],
      adstock = as.numeric(stringr::str_extract(matches[, 3], "^[0-9.]+")),
      power = as.numeric(stringr::str_extract(
        stringr::str_replace(matches[, 3], "^[0-9.]+_", ""), "^[0-9.]+"
      )),
      lag = as.numeric(stringr::str_extract(matches[, 3], "[0-9.]+$"))
    )
  }

#' Aggregate Columns in a Data Frame
#'
#' Aggregates specified columns in a data frame and returns a new data frame with
#' these aggregated values. It takes a character vector of column names,
#' possibly representing aggregated variables separated by a delimiter, and
#' computes the sum of these variables row-wise.
#'
#' @param modeling_df A data frame containing the input variables.
#' @param aggregated_variables A character vector specifying the columns to
#'   aggregate. The columns are expected to be delimited strings representing
#'   the individual variables to aggregate.
#' @param delimiter A string used as a delimiter to split the names in
#'   `aggregated_variables`. Default is "|".
#'
#' @return A data frame with columns specified in `aggregated_variables` aggregated.
#'
#' @examples
#' \dontrun{
#'   advertising <- data.frame(tv = runif(10), radio = runif(10), newspaper = runif(10),
#'    sales = runif(10))
#'   aggregate_columns(advertising, c("Sales", "TV|Radio|Newspaper"), delimiter = "|")
#' }
#'
aggregate_columns <-
  function(modeling_df,
           aggregated_variables,
           delimiter = "|") {
    individual_variables <-
      stringr::str_split(aggregated_variables, stringr::fixed(delimiter))
    aggregated_vals <-
      lapply(individual_variables, function(x)
        apply(modeling_df[, x, drop = FALSE], 1, sum, na.rm = TRUE))
    aggregated_df <-
      as.data.frame(do.call(
        cbind,
        stats::setNames(aggregated_vals, aggregated_variables)
      ))
    return(aggregated_df)
  }

#' Decompose Model Component with Adstock, Power, and Lag (APL) Transformations
#'
#' Applies Adstock, Power, and Lag (APL) transformations to variables within a
#' model data frame. It handles the decomposition of model components based on
#' provided weights, which can be treated either as coefficients or contributions.
#'
#' @param variables_wt_weights A named vector of weights for the variables, where
#'   names include APL information.
#' @param model_df A data frame containing the variables to be transformed.
#' @param is_weight_coefficient Logical; if TRUE, weights are treated as
#'   coefficients. If FALSE, they are treated as contributions.
#' @param apl_delimiter Delimiter used in variable names to separate APL components.
#' @param delimiter Delimiter used in variable names to denote different variables.
#' @param var_agg_delimiter Delimiter used for aggregating variables (default "|").
#'
#' @return A transformed data frame with variables weighted and APL transformations
#'   applied.
#'
#' @examples
#' \dontrun{
#'   advertising <- data.frame(TV = rnorm(100), Radio = rnorm(100))
#'   event <- data.frame(Date2021_01_08 = sample(0:1, 100, replace = TRUE),
#'                       Date2021_01_01 = sample(0:1, 100, replace = TRUE))
#'   model_df <- cbind(advertising, event)
#'
#'   variables_wt_weights <- setNames(1:5, c("TV|0.8_0.22_0", "Radio|0.5_0.15_1",
#'                                           "Intercept", "Date2021_01_08", "Date2021_01_01"))
#'
#'   transformed_df <- decompose_model_component(variables_wt_weights, model_df,
#'                                               is_weight_coefficient = FALSE,
#'                                               apl_delimiter = "_",
#'                                               delimiter = "|",
#'                                               var_agg_delimiter="|")
#' }
#'
#' @importFrom dplyr mutate across any_of bind_cols select if_else cur_column
#' @importFrom purrr pmap
#' @importFrom magrittr "%>%"
#' @importFrom stats setNames
#' @importFrom stringr str_replace str_split
#' @importFrom tidyselect everything
#'
decompose_model_component <-
  function(variables_wt_weights,
           model_df,
           is_weight_coefficient = TRUE,
           apl_delimiter = "_",
           delimiter = "|",
           var_agg_delimiter = "|") {
    # Treat weights as coefficients if is_weight_coefficient is TRUE, otherwise as contributions

    model_df_selected <- model_df %>%
      dplyr::select(any_of(names(variables_wt_weights)))

    # Remaining variables after selection
    variables_wt_weights_left <-
      variables_wt_weights[!names(variables_wt_weights) %in% names(model_df_selected)]

    # Check for the presence of an intercept term and include it if present
    intercept_key <- c("Intercept", "intercept", "(Intercept)")
    intercept_exists <-
      names(variables_wt_weights_left) %in% intercept_key
    if (any(intercept_exists)) {
      # model_df_selected[,names(variables_wt_weights_left[intercept_exists])] <- variables_wt_weights_left[intercept_exists]
      model_df_selected[, names(variables_wt_weights_left[intercept_exists])] <-
        1
      variables_wt_weights_left <-
        variables_wt_weights_left[!intercept_exists]
    }

    if (length(variables_wt_weights_left)) {
      # Parse variables for APL information
      variable_info <-
        parse_variable_wt_apl(names(variables_wt_weights_left),
                              apl_delimiter,
                              delimiter)
      # update variables_wt_weights names
      variables_wt_weights_captured <-
        variables_wt_weights[!names(variables_wt_weights) %in% names(variables_wt_weights_left)]
      variables_wt_weights_apl <-
        setNames(variables_wt_weights_left, variable_info$variable)
      names(variables_wt_weights)[names(variables_wt_weights) %in% names(variables_wt_weights_left)] <-
        names(variables_wt_weights_apl)

      # Create APL information list using pmap
      apl_info <-
        purrr::pmap(variable_info, function(variable, adstock, power, lag) {
          setNames(c(adstock, power, lag), names(variable_info)[-1])
        }) %>%
        setNames(variable_info$variable)

      # Dependent Data - create aggregation if required
      vars_in_model_df_logical <-
        names(apl_info) %in%  names(model_df)
      vars_expected_model_df <-
        names(apl_info)[!vars_in_model_df_logical]
      model_df_rel <-
        aggregate_columns(model_df,
                          c(vars_expected_model_df, names(apl_info)[vars_in_model_df_logical]) ,
                          var_agg_delimiter)


      # Apply APL transformations and recombine with the selected data
      model_df_transformed <- apply_apl(model_df_rel, apl_info)
      model_df_combined <- model_df_selected %>%
        dplyr::bind_cols(model_df_transformed) %>%
        dplyr::select(tidyr::all_of(names(variables_wt_weights)))
    } else {
      model_df_combined <- model_df_selected %>%
        dplyr::select(tidyr::all_of(names(variables_wt_weights)))
    }

    model_df_combined_est <- model_df_combined %>%
      dplyr::mutate(dplyr::across(any_of(names(
        variables_wt_weights
      )),
      ~ .x * variables_wt_weights[dplyr::cur_column()]),
      .keep = "used")


    # Adjust for weight coefficients if not treating as coefficients
    if (!is_weight_coefficient) {
      model_df_combined_est <- model_df_combined_est %>%
        dplyr::mutate(dplyr::across(everything(), ~ .x / sum(.x, na.rm = TRUE) * {
          {
            variables_wt_weights
          }
        }[dplyr::cur_column()]))
    }

    # Return the final model dataframe with applied transformations
    return(model_df_combined_est)
  }

#' Generate Model-Dependent Data Frames with APL Transformations
#'
#' Applies Adstock-Power-Lag (APL) transformations to variable information within
#' a model data frame. The function returns a list containing APL information
#' and a list of data frames with applied transformations. The `apl_delimiter`
#' and `var_apl_delimiter` are used to interpret and construct variable names when
#' `var_info` is a named numeric vector. When `var_info` is a list, the function
#' generates variable combinations and applies APL transformations to `model_df`.
#'
#' @param var_info A named numeric vector, character vector, or a list detailing the variables and
#'   their respective APL transformations. The interpretation of `var_info` depends on its format:
#'
#'   - Named Numeric Vector: If `var_info` is a named numeric vector, each named element represents
#'     a variable to which APL transformations will be applied. The format of each name should be
#'     "VariableName|Adstock_Power_Lag", where "Adstock", "Power", and "Lag" are numeric values
#'     separated by `var_apl_delimiter`. Example: "TV|0.8_0.22_0".
#'
#'   - Character Vector: If `var_info` is a character vector, it represents one or more variables
#'     to which APL transformations will be applied. Variables can be specified individually or
#'     aggregated using `var_agg_delimiter`. Example: "TV" or "TV|Radio".
#'
#'   - List: If `var_info` is a list, it should contain detailed specifications for each variable.
#'     This format allows you to specify APL attributes (Adstock, Power, Lag, Constraints) for each
#'     variable individually. Example:
#'     ```
#'     var_info_list <- list(
#'       TV = list(
#'         adstock = setNames(c(.1, .2, .1), c("start", "end", "step")),
#'         power = setNames(c(.2, .3, .1), c("start", "end", "step")),
#'         lag = setNames(c(0, 1, 1), c("start", "end", "step")),
#'         constraints = "adstock <= power"
#'       ),
#'       Radio = list(
#'         adstock = setNames(c(.2, .3, .1), c("start", "end", "step")),
#'         power = setNames(c(.1, .2, .1), c("start", "end", "step")),
#'         lag = setNames(c(0, 0, 1), c("start", "end", "step")),
#'         constraints = "adstock <= power"
#'       )
#'     )
#'     ```
#'
#' @param model_df A data frame containing model variables to which the APL
#'   transformations are to be applied.
#' @param apl_delimiter Delimiter used for concatenating variable names with
#'   their APL attributes when `var_info` is a named numeric vector (default "_").
#' @param var_apl_delimiter Delimiter used for separating variable names from their
#'   APL attributes when `var_info` is a named numeric vector (default "|").
#' @param var_agg_delimiter Delimiter used for aggregating variables (default "|").
#'
#' @return A list containing a tibble of APL information and a list of data
#'   frames with APL transformations applied. The tibble includes variable names
#'   and their APL attributes, while each data frame in the list represents a
#'   model-dependent transformation.
#'
#' @examples
#' \dontrun{
#' # Named vector input with single variable
#' var_info_vec <- "TV"
#' result_vec <- generate_model_dependent(var_info_vec, model_df)
#'
#' # Named vector input with multiple variables aggregated
#' var_info_vec <- "TV|Radio"
#' result_vec <- generate_model_dependent(var_info_vec, model_df)
#' var_info_vec <- c("TV|Radio","Radio|TV")
#' result_vec <- generate_model_dependent(var_info_vec, model_df)
#'
#' var_info_vec <- setNames(5, "TV|0.8_0.22_0")
#' result_vec <- generate_model_dependent(var_info_vec, model_df)
#'
#' var_info_vec <- setNames(c(1,5), c("TV|0.8_0.22_0", "Radio|.3_.2_0"))
#' result_vec <- generate_model_dependent(var_info_vec, model_df)
#'
#' # List input with APL specifications
#' var_info_list <- list(
#'   TV = list(
#'     adstock = setNames(c(.1, .2, .1), c("start", "end", "step")),
#'     power = setNames(c(.2, .3, .1), c("start", "end", "step")),
#'     lag = setNames(c(0, 1, 1), c("start", "end", "step")),
#'     constraints = "adstock <= power"
#'   )
#' )
#' result_list <- generate_model_dependent(var_info_list, model_df)
#'
#' var_info_list <- list(
#'   TV = list(
#'     adstock = setNames(c(.1, .2, .1), c("start", "end", "step")),
#'     power = setNames(c(.2, .3, .1), c("start", "end", "step")),
#'     lag = setNames(c(0, 1, 1), c("start", "end", "step")),
#'     constraints = "adstock <= power"
#'   ),
#'   Radio = list(
#'     adstock = setNames(c(.2, .3, .1), c("start", "end", "step")),
#'     power = setNames(c(.1, .2, .1), c("start", "end", "step")),
#'     lag = setNames(c(0, 0, 1), c("start", "end", "step")),
#'     constraints = "adstock <= power"
#'   )
#' )
#' result_list <- generate_model_dependent(var_info_list, model_df)
#'
#' }
#'
#' @importFrom purrr map flatten map2
#' @importFrom tibble enframe as_tibble
#' @importFrom tidyr unnest_wider
#'
generate_model_dependent <- function(var_info,
                                     model_df,
                                     apl_delimiter = "_",
                                     var_apl_delimiter = "|",
                                     var_agg_delimiter = "|") {
  if (is.vector(var_info) && is.character(var_info)) {
    var_info <-
      setNames(
        #        sapply(model_df[, var_info, drop = FALSE], sum, na.rm = T),
        sapply(aggregate_columns(model_df, var_info), sum, na.rm = T),

        paste(
          var_info,
          var_apl_delimiter,
          0,
          apl_delimiter,
          1,
          apl_delimiter,
          0,
          sep = ""
        )
      )
  }

  if (is.vector(var_info) &&
      is.numeric(var_info) && all(!is.na(names(var_info)))) {
    # Process named vector
    apl_df_list <- list(
      decompose_model_component(
        var_info,
        model_df,
        is_weight_coefficient = FALSE,
        apl_delimiter = apl_delimiter,
        delimiter = var_apl_delimiter,
        var_agg_delimiter = var_agg_delimiter
      ) %>%
        dplyr::mutate(dplyr::across(
          everything(), ~ tidyr::replace_na(.x, 0)
        ))
    )
    var_apl_info <-
      list(parse_variable_wt_apl(names(var_info), apl_delimiter, var_apl_delimiter))

  } else {
    if (all(c("adstock", "power", "lag", "constraints") %in% names(var_info[[1]]))) {
      # Dependent Data - create aggregation if required
      model_df_rel <-
        aggregate_columns(model_df, names(var_info), var_agg_delimiter)

      # Process list
      var_wt_apl <- generate_variable_combination(var_info)
      apl_df_list <-
        purrr::map(var_wt_apl,
                   ~ apply_apl(model_df_rel, .x) %>%
                     dplyr::mutate(dplyr::across(
                       everything(), ~ tidyr::replace_na(.x, 0)
                     )))

      var_apl_info <- lapply(var_wt_apl, function(x)
        data.frame(matrix(
          x[[1]],
          nrow = 1,
          dimnames = list(names(x[1]), names(x[[1]]))
        )) %>% tibble::rownames_to_column("variable"))

    } else {
      apl_df_list <- lapply(var_info, function(x)
        decompose_model_component(
          x,
          model_df,
          is_weight_coefficient = FALSE,
          apl_delimiter = apl_delimiter,
          delimiter = var_apl_delimiter,
          var_agg_delimiter = var_agg_delimiter
        ) %>%
          dplyr::mutate(dplyr::across(
            everything(), ~ tidyr::replace_na(.x, 0)
          )))

      var_apl_info <-
        lapply(var_info, function(x)
          parse_variable_wt_apl(names(x), apl_delimiter, var_apl_delimiter))

    }
  }
  dep_sum <- lapply(apl_df_list, function(x)
    sapply(x, sum))
  list(purrr::map2(var_apl_info, dep_sum , ~ cbind(.x, sum = unname(.y))), apl_df_list)
}

#' Get Dependent and Independent Variables
#'
#' Determines the type of a model based on the presence of characters '+' and '-',
#' and processes the model variable to extract dependent and independent variables.
#' It classifies models into types like "Remodel", "Aggregate", "Segregate",
#' or "Aggregate & Segregate" based on the syntax of the model variable.
#'
#' @param model_variable A character string representing the model variable,
#'   typically a formula or expression where variables may be aggregated or
#'   segregated using '+' and '-'.
#' @param var_agg_delimiter A character string representing the delimiter for
#'   aggregated variables. Default is "|".
#' @param trim A logical indicating whether to trim whitespace from the output.
#'   Default is TRUE.
#' @param print_model_type A logical indicating whether to print the determined
#'   model type to the console. Default is TRUE.
#'
#' @return A list with two elements: 'dependent_var' containing the processed
#'   dependent variable(s) and 'independent_var' containing the processed
#'   independent variable(s).
#'
#' @examples
#' \dontrun{
#'   # Aggregation
#'   get_dep_indep_vars("TV_0_1_0+Sales_0_1_0")
#'   # Remodel
#'   get_dep_indep_vars("TV_0_1_0")
#'   # Segregation
#'   get_dep_indep_vars("TV-Sales_0_1_0")
#'   # Aggregation & Segregation
#'   get_dep_indep_vars("TV_0_1_0+-Sales_0_1_0")
#'   # Aggregation & Segregation, segregation is evaluated first and then aggregation
#'   get_dep_indep_vars("TV_0_1_0+Sales-Radio_0_1_0")
#'   # Aggregation with alternate delimiter
#'   get_dep_indep_vars("TV_0_1_0+Sales|Radio_0_1_0", var_agg_delimiter = "|")
#' }
#'
get_dep_indep_vars <-
  function(model_variable,
           var_agg_delimiter = "|",
           trim = TRUE,
           print_model_type = TRUE) {
    # Determine model type based on the presence of characters + and -
    model_type <-
      if (!stringr::str_detect(model_variable, "[+-]")) {
        "Remodel"
      } else if (stringr::str_detect(model_variable, "\\+") &
                 stringr::str_detect(model_variable, "-")) {
        "Aggregate & Segregate"
      } else if (stringr::str_detect(model_variable, "-")) {
        "Segregate"
      } else if (stringr::str_detect(model_variable, "\\+")) {
        "Aggregate"
      }

    # Print the determined model type if requested
    if (print_model_type) {
      cat("Model Type:", model_type, "\n")
    }

    # Initialize variables for dependent and independent variables
    dep_var_rel <- indep_vars <- character()

    # Based on the model type, process the model variable
    if (model_type == "Remodel") {
      dep_var_rel <- model_variable
      indep_vars <- model_variable
    } else if (model_type == "Aggregate") {
      dep_var_rel <- unlist(str_split(model_variable, "\\+"))
      indep_vars <-
        stringr::str_replace_all(model_variable, "\\+", var_agg_delimiter)
    } else if (model_type == "Aggregate & Segregate") {
      dep_var_rel <-
        stringr::str_replace_all(str_replace(
          unlist(str_split(model_variable, "\\+")),
          paste0("^-", var_agg_delimiter, "-$"),
          ""
        ), "-", var_agg_delimiter)
      indep_vars <-
        stringr::str_replace_all(str_replace(
          unlist(str_split(model_variable, "-")),
          paste0("^\\+", var_agg_delimiter, "\\+$"),
          ""
        ),
        "\\+",
        var_agg_delimiter)
    } else if (model_type == "Segregate") {
      dep_var_rel <-
        stringr::str_replace_all(model_variable, "-", var_agg_delimiter)
      indep_vars <- unlist(stringr::str_split(model_variable, "-"))
    }

    if (trim) {
      dep_var_rel <-
        unlist(lapply(stringr::str_split(dep_var_rel, stringr::fixed(var_agg_delimiter)), function(x)
          paste0(stringr::str_trim(x, side = c("both")), collapse = var_agg_delimiter)))
      indep_vars <-
        unlist(lapply(stringr::str_split(indep_vars, stringr::fixed(var_agg_delimiter)), function(x)
          paste0(stringr::str_trim(x, side = c("both")), collapse = var_agg_delimiter)))
    }

    # Return a list containing the processed dependent and independent variables
    return(list(dependent_var = dep_var_rel, independent_var = indep_vars))
  }

#' Determine the Expected Sign of Model Coefficients
#'
#' This function assesses whether the coefficients of given variables are
#' expected to be positive or negative. It is particularly useful for variables
#' that may have aggregated names, determining the expected sign based on the
#' segregation of each part of the variable name.
#'
#' @param variable A character string representing the name of the variable
#'                 whose coefficient sign is to be determined. For aggregated
#'                 variable names, the function segregates the parts based on
#'                 the provided delimiter.
#' @param pos_vars A character vector of variable names that are expected to
#'                 have a positive impact. Coefficients of these variables
#'                 are expected to be positive.
#' @param neg_vars A character vector of variable names that are expected to
#'                 have a negative impact. Coefficients of these variables
#'                 are expected to be negative.
#' @param var_agg_delimiter A character string delimiter used in the variable
#'                          names for aggregation purposes. This delimiter
#'                          is used to split the variable names if they are
#'                          aggregated.
#'
#' @return A logical value; returns TRUE if all segregated parts of the variable
#'         are expected to have a positive sign, FALSE if all are expected to
#'         have a negative sign. If the segregated parts of the variable do not
#'         uniformly align with either positive or negative expectations, the
#'         function returns NA.
#'
#' @details
#' In the case of an aggregated variable (a variable name composed of multiple
#' parts separated by the delimiter), the function determines the expected sign
#' based on the segregation of each part. The output will be TRUE only if all
#' segregated parts have an expected positive sign; similarly, it will be FALSE
#' only if all parts have an expected negative sign. If there is any inconsistency
#' among the segregated parts, the expected sign will be NA.
#'
#' @examples
#' \dontrun{
#'   pos_vars <- c("sales", "marketing")
#'   neg_vars <- c("costs", "returns")
#'   determine_expected_sign("sales|Q1", pos_vars, neg_vars, "|") # Returns TRUE
#'   determine_expected_sign("costs|Q1", pos_vars, neg_vars, "|") # Returns FALSE
#'   determine_expected_sign("sales|other", pos_vars, neg_vars, "|") # Returns NA
#' }
#'
determine_expected_sign <-
  function(variable,
           pos_vars,
           neg_vars,
           var_agg_delimiter) {
    expected_pos <-
      lapply(stringr::str_split(variable, stringr::fixed(var_agg_delimiter)),
             `%in%`,
             pos_vars)
    expected_neg <-
      lapply(stringr::str_split(variable, stringr::fixed(var_agg_delimiter)),
             `%in%`,
             neg_vars)

    dplyr::if_else(unlist(lapply(expected_pos, all)),
                   TRUE,
                   dplyr::if_else(unlist(lapply(expected_pos, any)), NA,
                                  dplyr::if_else(unlist(
                                    lapply(expected_neg, all)
                                  ), FALSE, NA)))
  }

#' Flag P-Values Based on Predefined Thresholds
#'
#' This function evaluates p-values of predictors and flags them based on
#' predefined thresholds specific to their types (intercept, fixed, or flexible).
#'
#' @param type A character string indicating the type of the predictor.
#'             Valid options are 'intercept', 'fixed', or 'flexible'.
#' @param pvalue A numeric value representing the p-value of the predictor.
#' @param pvalue_thresholds A named numeric vector of thresholds for each
#'                          predictor type. The names of the vector should
#'                          be 'intercept', 'fixed', and 'flexible'.
#'
#' @return A logical value; TRUE if the p-value is above the threshold for its
#'         respective type. This function helps in identifying statistically
#'         significant predictors based on their p-values and predefined criteria.
#'
#' @examples
#' \dontrun{
#'   pvalue_thresholds <- c(intercept = 0.05, fixed = 0.05, flexible = 0.1)
#'   determine_pvalue_flag("fixed", 0.04, pvalue_thresholds)     # Returns FALSE
#'   determine_pvalue_flag("flexible", 0.08, pvalue_thresholds)  # Returns TRUE
#'   determine_pvalue_flag("intercept", 0.06, pvalue_thresholds) # Returns TRUE
#' }
#'
determine_pvalue_flag <- function(type, pvalue, pvalue_thresholds) {
  dplyr::if_else(
    type == "intercept",
    pvalue > pvalue_thresholds["intercept"],
    dplyr::if_else(
      type == "fixed",
      pvalue > pvalue_thresholds["fixed"],
      pvalue > pvalue_thresholds["flexible"]
    )
  )
}

#' Determine VIF Flag for Predictors
#'
#' This function evaluates the Variance Inflation Factor (VIF) for predictors and
#' determines if it exceeds a specified threshold. It accounts for special cases
#' based on the type of the variable.
#'
#' @param type Character string indicating the type of the variable.
#'             Options are 'fixed' or 'flexible'. The function applies special
#'             logic if there is only one 'fixed' variable.
#' @param vif_threshold Numeric value specifying the threshold above which the
#'                      VIF flag should be triggered.
#'
#' @return A numeric value; the function returns \code{Inf} (infinite) if there is only
#'         one 'fixed' variable, otherwise it returns the `vif_threshold`.
#'
#' @details
#' The function uses the following logic:
#' - If the `type` is 'fixed' and there is only one such variable, it returns \code{Inf}.
#'   This accounts for the scenario where a single fixed variable should not trigger
#'   a flag by having an artificially high VIF.
#' - In all other cases, it returns the `vif_threshold`.
#'
#' @importFrom stringr str_count
#' @importFrom dplyr if_else
#'
#' @examples
#' \dontrun{
#'   find_critical_vif("fixed", 5)      # Returns Inf
#'   find_critical_vif("flexible", 5)   # Returns 5
#'   find_critical_vif("fixed|fixed", 5)  # Returns 5
#' }
#'
find_critical_vif <- function(type, vif_threshold) {
  dplyr::if_else(sum(stringr::str_count(type, "fixed")) == 1, Inf, vif_threshold)
}

#' Replicate and Extend Dependent IDs with Factors
#'
#' This function replicates and extends a dataset by applying factors to specific columns
#' based on the provided dependent IDs and factors.
#'
#' @param data The original dataset to be replicated and extended.
#' @param ids_with_factors A named list where each element corresponds to a dependent ID,
#' and the values are vectors of factors to be applied.
#' @param cols_to_apply_factor A character vector specifying the columns in the dataset
#' on which the factors should be applied.
#'
#' @return A replicated and extended dataset with modified values in specified columns.
#'
#' @importFrom dplyr select
#' @importFrom purrr map2_df
#' @examples
#' \dontrun{
#' # Set a seed for reproducibility
#' set.seed(123)
#'
#' # Number of rows in the dataset
#' num_rows <- 100
#'
#' # Generate data
#' data <- data.frame(
#'   dependent_id = 1:num_rows,
#'   age = sample(22:45, num_rows, replace = TRUE),
#'   income = sample(40000:100000, num_rows, replace = TRUE),
#'   expenses = sample(1500:5000, num_rows, replace = TRUE),
#'   savings = sample(18000:75000, num_rows, replace = TRUE)
#' )
#'
#' # Display the first few rows of the dataset
#' head(data)
#'
#' # Define factors to apply for specific dependent IDs and columns
#' ids_with_factors <- list("2" = c(2, 4), "3" = c(3, 4, 5))
#' cols_to_apply_factor <- c("income", "savings")
#'
#' # Replicate and extend the dataset
#' replicate_and_extend_dep_ids(data, ids_with_factors, cols_to_apply_factor)
#' }
#'
replicate_and_extend_dep_ids <-
  function(data,
           ids_with_factors,
           cols_to_apply_factor) {
    if (length(ids_with_factors)) {
      data$dependent_id <- as.numeric(data$dependent_id)
      data_list <-
        lapply(names(ids_with_factors), function(x)
          data[data$dependent_id == x,])
      apply_factor_df <-
        function(df,
                 factors2apply,
                 cols_to_apply_factor) {
          df_list <- replicate(length(factors2apply), df, simplify = F)
          df_collated <-
            purrr::map2_df(df_list, factors2apply, function(df, x) {
              df[, cols_to_apply_factor] <- df[, cols_to_apply_factor] * x
              df
            }, .id = "id")
          df_collated$id <- as.numeric(df_collated$id)
          divisor <- 10 ^ ceiling(log10(length(factors2apply)))
          df_collated$dependent_id <-
            df_collated$dependent_id + df_collated$id / divisor
          df_collated
        }
      df_modified <-
        purrr::map2_df(data_list, ids_with_factors, function(df, id_factors_num) {
          apply_factor_df(df, id_factors_num, cols_to_apply_factor)
        })
      df_modified %>%
        dplyr::select(-"id")
    } else {
      data.frame()
    }
  }

#' Generate Model Decomposition
#'
#' This function decomposes a model into its components, including dependent and independent variables, based on provided transformation parameters. It allows for the decomposition of complex models into simpler components for analysis or interpretation.
#'
#' @param dep_info A list specifying the dependent variable(s) or combinations of dependent variables along with their transformation parameters. Each element of the list should be a numeric value representing the weight coefficient for the corresponding dependent variable(s). The names of dependent variables should include the adstock, power, and lag parameters required for variable transformation, along with variable names and components, separated by the delimiter.
#' @param indep_info A list specifying the independent variable(s) or combinations of independent variables along with their transformation parameters. Each element of the list should be a numeric value representing the weight coefficient for the corresponding independent variable(s). The names of independent variables should include the adstock, power, and lag parameters required for variable transformation, along with variable names and components, separated by the delimiter.
#' @param modeling_df The data frame containing the modeling data.
#' @param dep_info_is_weight_coefficient Logical, indicating whether the dependent variable information represents weight coefficients.
#' @param indep_info_is_weight_coefficient Logical, indicating whether the independent variable information represents weight coefficients.
#' @param apl_delimiter The delimiter used for separating adstock, power, lag parameters from variable names and components.
#' @param delimiter The delimiter used for separating variable components.
#' @param var_agg_delimiter The delimiter used for variable aggregation.
#'
#' @return A list containing the dependent and its decomposition of its components.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   dep_var1 = c(1, 2, 3),
#'   dep_var2 = c(4, 5, 6),
#'   indep_var1 = c(7, 8, 9),
#'   indep_var2 = c(10, 11, 12),
#'   indep_var3 = c(13, 14, 15)
#' )
#' generate_model_decomposition(list(setNames(1,"dep_var1_0_1_0")), list(setNames(c(1:3),
#'   c("indep_var1_0_1_0","indep_var2_0_1_0","indep_var3_0_1_0"))), df)
#' generate_model_decomposition(list(setNames(1,"dep_var1|dep_var2_0_1_0")),
#'   list(setNames(c(1:2),c("indep_var1|indep_var2_0_1_0","indep_var3_0_1_0"))), df)
#' generate_model_decomposition(list(setNames(1:2,c("dep_var1_0_1_0","dep_var2_0_1_0"))),
#'  list(setNames(c(1:3),c("indep_var1_0_1_0","indep_var2_0_1_0","indep_var3_0_1_0"))), df)
#' generate_model_decomposition(list(setNames(1,"dep_var1_0_1_0"),setNames(2,"dep_var2_0_1_0")),
#'  list(setNames(c(1:3),c("indep_var1_0_1_0","indep_var2_0_1_0","indep_var3_0_1_0")),
#'  setNames(c(1:3),c("indep_var1_0_1_0","indep_var2_0_1_0","indep_var3_0_1_0"))), df)
#'}
#' @importFrom purrr map2_vec
#'
#' @export
#'
generate_model_decomposition <-
  function(dep_info,
           indep_info,
           modeling_df,
           dep_info_is_weight_coefficient = FALSE,
           indep_info_is_weight_coefficient = TRUE,
           apl_delimiter = "_",
           delimiter = "_",
           var_agg_delimiter = "|") {
    bp_stage_id <- purrr::map2(dep_info, indep_info, function(y, x) {
      dep_bp <- decompose_model_component(
        y,
        modeling_df,
        is_weight_coefficient = dep_info_is_weight_coefficient,
        apl_delimiter = apl_delimiter,
        delimiter = delimiter,
        var_agg_delimiter = var_agg_delimiter
      )
      indep_bp <- decompose_model_component(
        x,
        modeling_df,
        is_weight_coefficient = indep_info_is_weight_coefficient,
        apl_delimiter = apl_delimiter,
        delimiter = delimiter,
        var_agg_delimiter = var_agg_delimiter
      )
      residual <- rowSums(dep_bp) - rowSums(indep_bp)
      names(residual) <- "residual"
      list(dep_bp, cbind(indep_bp, residual))
    })
    bp_stage_id
  }
