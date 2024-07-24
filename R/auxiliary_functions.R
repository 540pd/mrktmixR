#' Convert String to Named Sequence
#'
#' This function takes a character vector, splits each string by a specified delimiter,
#' converts the split parts into numeric values, and assigns user-defined names
#' to these numeric values.
#'
#' @param char_vector A character vector with strings to be split and converted.
#' @param delimiter A character string specifying the delimiter used for splitting.
#' @param names_vector A character vector specifying the names to be assigned to the numeric values.
#' @return A list of named numeric vectors.
#' @importFrom dplyr %>%
#' @importFrom purrr map
#' @importFrom stringr str_split
#' @examples
#' \dontrun{
#' char_vector <- c("1|3|2", "4|6|2")
#' delimiter <- "|"
#' names_vector <- c("start", "end", "step")
#' convert_to_named_seq(char_vector, delimiter, names_vector)
#' }
convert_to_named_seq <-
  function(char_vector, delimiter, names_vector) {
    char_vector %>%
      stringr::str_split(stringr::fixed(delimiter)) %>%
      purrr::map( ~ setNames(as.numeric(.x), names_vector))
  }

#' Name and Truncate List
#'
#' Takes a list and a vector of variable names, truncates the list
#' to the length of the variable names (if necessary), and sets the names
#' of the list elements to those variable names. If the list is shorter
#' than the names vector, the function truncates the names vector to the
#' length of the list.
#'
#' @param list_to_name A list to be named.
#' @param var_names A vector of names to be assigned to the list elements.
#' @return A named list.
#' @examples
#' \dontrun{
#' list_to_name <- list(1, 2, 3, 4)
#' var_names <- c("var1", "var2")
#' name_and_truncate_list(list_to_name, var_names)
#' }
name_and_truncate_list <- function(list_to_name, var_names) {
  if (length(var_names) > length(list_to_name)) {
    var_names <- var_names[1:length(list_to_name)]
  }
  rel_list <- list_to_name[1:length(var_names)]
  setNames(rel_list, var_names)
}

#' Split String and Convert to Numeric
#'
#' Splits each string in a character vector by a specified delimiter and converts
#' the resulting parts to numeric vectors. Returns a list of these numeric vectors.
#'
#' @param char_vector A character vector with strings to be split and converted to numeric.
#' @param delimiter A character string specifying the delimiter used for splitting.
#' @return A list of numeric vectors.
#' @importFrom dplyr %>%
#' @importFrom purrr map
#' @examples
#' \dontrun{
#' char_vector <- c("1,2,3", "4,5,6")
#' delimiter <- ","
#' split_and_convert_to_numeric(char_vector, delimiter)
#' }
split_and_convert_to_numeric <- function(char_vector, delimiter) {
  char_vector %>%
    stringr::str_split(delimiter) %>%
    map( ~ as.numeric(.x))
}

#' Create Named Lists from Variables with Predefined Pairs
#'
#' Creates a list where each element is a copy of the predefined pairs list.
#' The output list will have the same names as the input `var_names` vector.
#' Note that each element in the output list is a direct copy of `predefined_pairs`,
#' and not a list of individual key-value pairs.
#'
#' @param var_names A character vector of variable names.
#' @param predefined_pairs A list of predefined key-value pairs to be replicated.
#' @return A named list where each element is a copy of the predefined pairs list.
#' @examples
#' \dontrun{
#' var_names <- c("var1", "var2")
#' predefined_pairs <- list(setNames(1:3, c("adstock", "power", "lag")))
#' create_named_lists_from_vars(var_names, predefined_pairs)
#' }
create_named_lists_from_vars <-
  function(var_names, predefined_pairs) {
    if (is.null(var_names) || length(var_names) == 0) {
      stop("var_names cannot be NULL or empty")
    }

    if (is.null(predefined_pairs) || !is.list(predefined_pairs)) {
      stop("predefined_pairs must be a non-empty list")
    }

    named_lists <-
      replicate(length(var_names), predefined_pairs, simplify = TRUE)
    setNames(named_lists, var_names)
  }

#' Replace Values Pairwise
#'
#' Replaces each value in an input vector that matches a condition from a list
#' of conditions with the corresponding value from a list of replacement values.
#' Error if lengths of conditions and replacements are not equal.
#'
#' @param input_vector An input vector with values to be replaced.
#' @param conditions_replacements A vector of conditions to be checked.
#' @param replacement_values A vector of replacement values.
#' @return A vector with replaced values.
#'
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#' input_vector <- c(1, 2, 3, 4, 5)
#' conditions_replacements <- c(2, 4)
#' replacement_values <- c(20, 40)
#' replace_values_pairwise(input_vector, conditions_replacements, replacement_values)
#' }
replace_values_pairwise <-
  function(input_vector,
           conditions_replacements,
           replacement_values) {
    if (length(conditions_replacements) != length(replacement_values)) {
      stop("Length of conditions_replacements and replacement_values should be the same.")
    }
    condition_tracker <- replicate(length(input_vector), F)
    for (i in seq_along(conditions_replacements)) {
      condition <- conditions_replacements[i]
      replacement <- replacement_values[i]
      match_index <- input_vector == condition
      input_vector[match_index & !condition_tracker] <- replacement
      condition_tracker[match_index] <- T
    }
    return(input_vector)
  }

#' Scope for Dependent Variable
#'
#' Determines the feasible dependent variables based on previous and current variable names.
#'
#' @description
#' This function takes the previous named variable as a base and tries to find a new feasible named vector
#' based on whether it involves aggregation, aggregation and segregation, update in the variable,
#' or segregation. It assumes the current variable with APL is based on these cases in the named vector.
#' The names of these vectors will be variables with APL, and their values represent the contribution of the model.
#'
#' @param previous_var_apl Named numeric vector of previous variable names with application suffix.
#'                        The names must contain variables with APL as per the specified delimiter.
#'                        The values represent the contribution.
#' @param current_var_apl Named numeric vector of current variable names with application suffix.
#'                       The names must contain variables with APL as per the specified delimiter.
#'                       The values represent the contribution.
#' @param var_agg_delimiter Delimiter used for variable aggregation (default is "|").
#' @param delimiter Delimiter used for variable parts (default is "_").
#' @return Feasible dependent variables based on the given conditions.
#' @details
#' The function aims to identify feasible dependent variables by comparing the previous and current variable names.
#' It considers cases such as aggregation, aggregation and segregation, variable updates, or segregation in the process.
#' The named vectors generated represent the contribution of the model, with variable names containing APL information.
#'
#' @importFrom stringr str_split
#'
#' @examples
#' \dontrun{
#' # Both matching - aggregation and segregation
#' previous_var_apl <- c("var1|A_.2_2_5" = 0.5, "var1|A_.2_2_9" = 0.5, "var2|B_.5_.4_.2" = 0.3,
#'  "var2|B_.5_.1_.2" = 0.4, "var2|B_.5_.1_9" = 0.4, "afaf_2_3_4" = 0)
#' current_var_apl <- c("var1|A_.2_2_5" = 0.6, "var1|A_.2_2_9" = 0.6, "var2|B_.5_.4_.2" = 0.9,
#'  "var2|B_.5_.4_.2" = 10)
#' scope_for_dependent_variable(previous_var_apl, current_var_apl)
#'
#' # Aggregation and carry forward
#' previous_var_apl <- c("var3_.2_2_5" = 0.5, "B_.5_.4_.2" = 0.3, "var3_.2_9_5" = 0.78,
#'  "B_.9_.4_.2" = 0.33, "d_1_2_4" = 0)
#' current_var_apl <- c("var3|B_2_4_9" = 0.9, "var3|B_2_4_2" = 0.9, "var3|B_2_4_2" = 10,
#'  "var3|B_2_4_2" = 11)
#' scope_for_dependent_variable(previous_var_apl, current_var_apl)
#'
#' # Segregation and carry forward
#' previous_var_apl <- c("var1|c_.2_2_5" = 0.5, "var1|c_.2_2_1" = 0.5, "d_1_2_4" = 0,
#'  "d_1_2_1" = 0)
#' current_var_apl <- c("var1_2_4_2" = 0.9, "c_.5_.4_.2" = 0.9, "var1_2_4_2" = 11,
#'  "c_.5_.4_.2" = 0.10, "c_.5_.4_.2" = 0.10, "c_.5_.4_9" = 0.10)
#' scope_for_dependent_variable(previous_var_apl, current_var_apl)
#'
#' }
#'
scope_for_dependent_variable <-
  function(previous_var_apl,
           current_var_apl,
           var_agg_delimiter = "|",
           delimiter = "_") {
    if (var_agg_delimiter == delimiter) {
      stop("var_agg_delimiter and delimiter cannot be the same.")
    }
    previous_var_wo_apl_ori <- sapply(stringr::str_split(names(previous_var_apl),
                                                         stringr::fixed(delimiter)), function(x)
                                                           x[1])
    previous_var_wo_apl <- unique(previous_var_wo_apl_ori)
    current_var_wo_apl_ori <- sapply(stringr::str_split(names(current_var_apl),
                                                        stringr::fixed(delimiter)), function(x)
                                                          x[1])
    current_var_wo_apl <- unique(current_var_wo_apl_ori)
    previous_vars_wo_apl <-
      stringr::str_split(previous_var_wo_apl, stringr::fixed(var_agg_delimiter))
    current_vars_wo_apl <-
      stringr::str_split(current_var_wo_apl, stringr::fixed(var_agg_delimiter))
    previous_var_in_current_vars <- sapply(previous_vars_wo_apl,
                                           function(x)
                                             all(x %in% unlist(current_vars_wo_apl)))
    feasible_dependent_variables <- if (all(sapply(current_var_wo_apl,
                                                   function(x)
                                                     x %in% previous_var_wo_apl))) {
      previous_var_wo_apl
    } else if (length(current_vars_wo_apl) > 1 &&
               sum(unlist(previous_var_in_current_vars)) ==
               1) {
      c(previous_var_wo_apl[!previous_var_in_current_vars],
        current_var_wo_apl)
    } else if (length(current_vars_wo_apl) == 1 &&
               length(previous_vars_wo_apl) >
               1 && length(purrr::reduce(
                 unlist(current_vars_wo_apl),
                 unlist(previous_vars_wo_apl),
                 setdiff
               )) == 0) {
      c(previous_var_wo_apl[!previous_var_in_current_vars],
        current_var_wo_apl)
    } else {
      previous_var_wo_apl
    }

    return(c(current_var_apl[current_var_wo_apl_ori %in% current_var_wo_apl[current_var_wo_apl %in% feasible_dependent_variables]],
             previous_var_apl[previous_var_wo_apl_ori %in% previous_var_wo_apl[previous_var_wo_apl %in% feasible_dependent_variables[!feasible_dependent_variables %in%
                                                                                                                                       current_var_wo_apl]]]))
  }

#' Create File Path
#'
#' Create a file path based on the specified folder, file name, and relative directory.
#'
#' @param folder The folder in which the file is located.
#' @param file_name The name of the file.
#' @param relative_directory The relative directory path (default is ".").
#' @return A character vector representing the absolute file path.
#' @examples
#' \dontrun{
#' create_file_path("data", "example.csv")
#' create_file_path("output", "result.txt", relative_directory = "project")
#' }
create_file_path <-
  function(folder, file_name, relative_directory = ".") {
    # Check if the suffix indicates a CSV file
    path.expand(file.path(normalizePath(relative_directory),
                          folder,
                          file_name))
  }

#' Slowly Changing Dimension Type 2 Update Function
#'
#' This function updates a base dataset with changes from a current dataset using the
#' Slowly Changing Dimension Type 2 (SCD2) methodology. It handles additions of new
#' records and updates to existing records based on specified identification columns
#' and comparison columns. It assumes that the base data should not contain records
#' ending with '_base', and new data should not contain 'start_date' and 'end_date'.
#' Additionally, it only compares numeric values in `compare_cols` and excludes any
#' columns starting with 'source_' in `current_data`.
#'
#' @param base_data A dataframe representing the base data to be updated.
#' @param current_data A dataframe representing the current data with potential updates.
#' @param changes_ids A vector of column names at which base data will always be obsolete.
#' This is a higher hierarchy of data in which changes are traced and old data will be obsolete.
#' @param row_ids A vector of column names with primary keys.
#' @param compare_cols A vector of column names used for comparing record values.
#' @param create_date The name of the column representing creation date in the base data.
#' @param update_date The name of the column representing update date in the base data.
#' @return A dataframe that is the result of applying SCD2 methodology on the base data
#'         with the updates from the current data.
#' @importFrom dplyr full_join group_by mutate filter left_join
#' @importFrom purrr map
#' @examples
#' \dontrun{
#'   base_data <- data.frame(
#'     model_id = c(1, 1, 2, 3),
#'     version_id = c(1, 2, 3, 4),
#'     name = c("Alice", "Bob", "Charlie", "David"),
#'     value = c(100, 200, 300, 400),
#'     start_date = as.POSIXct(c("2023-01-01", "2023-01-05", "2023-01-05", "2023-01-05")),
#'     end_date = as.POSIXct(NA)
#'   )
#'   current_data <- data.frame(
#'     model_id = c(1),
#'     version_id = c(2),
#'     name = c("Alice"),
#'     value = c(300)
#'   )
#'   changes_ids <- c("model_id")
#'   row_ids <- c("model_id", "version_id")
#'   compare_cols <- "value"
#'   scd_type_2_update(base_data, current_data, changes_ids, row_ids, compare_cols)
#' }
#'
scd_type_2_update <-
  function(base_data,
           current_data,
           changes_ids,
           row_ids,
           compare_cols,
           create_date = "start_date",
           update_date = "end_date") {
    # Initialize start_date and end_date in base_data if not present
    if (!create_date %in% names(base_data)) {
      base_data[[create_date]] <- Sys.time()
    }
    if (!update_date %in% names(base_data)) {
      base_data[[update_date]] <- as.POSIXct(NA)
    }

    if (nrow(current_data) != 0) {
      current_data[[create_date]] <- Sys.time()
      current_data[[update_date]] <- as.POSIXct(NA)
      current_data[["source_current"]] = TRUE

      current_base_data <- current_data %>%
        full_join(
          base_data %>% dplyr::filter(is.na(get(update_date)))  %>% dplyr::mutate(source_base = TRUE),
          by = row_ids,
          suffix = c("", "_base")
        )

      # data present in current but not in base - new data
      new_data_added <-
        current_base_data[is.na(current_base_data$source_base) &
                            !is.na(current_base_data$source_current), names(current_data)]

      # common ids data
      common_data <-
        current_base_data[!is.na(current_base_data$source_base) &
                            !is.na(current_base_data$source_current), ]
      common_data$is_difference <-
        rowSums(abs(common_data[, compare_cols, drop = F] - common_data[, paste(compare_cols, "base", sep =
                                                                                  "_"), drop = F]) != 0)
      common_data <- common_data %>%
        group_by(across(tidyselect::all_of(row_ids))) %>%
        mutate(row_ids_diff = sum(.data$is_difference))
      common_data[common_data$row_ids_diff != 0, paste0(update_date, "_base")] <-
        common_data[common_data$row_ids_diff != 0, create_date]

      # udpate any previous change in changes ids
      current_old_data <-
        current_base_data[!is.na(current_base_data$source_base) &
                            is.na(current_base_data$source_current), c(row_ids, names(current_base_data)[names(current_base_data)  %in%   paste(names(base_data), "base", sep = "_")])]
      current_old_data <- current_old_data %>%
        dplyr::left_join(unique(common_data[, c(changes_ids, create_date)]), by = changes_ids)
      current_old_data[, paste0(update_date, "_base")] <-
        current_old_data[, create_date]
      current_old_data <-
        current_old_data[, names(current_old_data) != create_date]

      old_data_updated <- rbind(current_old_data,
                                common_data[, c(row_ids, names(current_base_data)[names(current_base_data)  %in%   paste(names(base_data), "base", sep = "_")])])
      names(old_data_updated) <-
        c(row_ids, sub("_base", "", names(old_data_updated)[!names(old_data_updated) %in% row_ids]))

      new_data_added <-
        rbind(common_data[common_data$row_ids_diff != 0, names(current_data)],
              current_base_data[is.na(current_base_data$source_base) &
                                  !is.na(current_base_data$source_current), names(current_data)])

      rbind(old_data_updated, new_data_added[,!names(new_data_added) %in% "source_current"])
    }
    else {
      base_data
    }
  }

#' Summarize Recursive Model Coefficients
#'
#' This function summarizes the coefficients of a recursive model, joining relevant
#' datasets and performing necessary data transformations. It takes dependent variable information,
#' model summary, and model coefficient datasets, and consolidates them using unique identifiers
#' to create a comprehensive summary file with both dependent and independent information.
#'
#' @param model_dependent Dataset containing information about the dependent variable in the model.
#' @param model_smry Summary dataset related to the model.
#' @param model_coef Dataset containing coefficients of the model.
#' @param round_digits Number of digits to round up numeric values to. It is important to specify
#' this parameter due to potential approximation issues when merging datasets with different precision.
#' @param ids_separator used to concatenate ids for model
#'
#' @return A summarized dataset containing information about the model coefficients.
#'
#' @importFrom dplyr full_join right_join filter select rename group_by
#'
recursive_model_summary <- function(model_dependent, model_smry, model_coef, round_digits = 10, ids_separator = "|") {
  model_coef <- model_coef %>%
    dplyr::inner_join(
      model_smry %>%
        dplyr::filter(.data$flag_num == 0) %>%
        dplyr::select("stage_id", "version_id", "dependent_id", "model_id", "loop_id", "rmse"),
      by = c("stage_id", "version_id", "dependent_id", "model_id", "loop_id")
    )

  # Create Summary
  mdl_smry_all <- model_dependent %>%
    dplyr::rename("dep_variable" = "variable", "dep_adstock" = "adstock", "dep_power" = "power", "dep_lag" = "lag", "dep_sum" =  "sum") %>%
    dplyr::mutate(dep_sum = round(.data$dep_sum, round_digits)) %>%
    dplyr::right_join(
      model_coef %>%
        dplyr::filter(.data$type == "fixed") %>%
        dplyr::rename("indep_variable" = "variable", "indep_adstock" = "adstock", "indep_power" = "power", "indep_lag" = "lag") %>%
        dplyr::select("version_id", "stage_id", "dependent_id", "model_id", "loop_id", "indep_variable", "indep_adstock", "indep_power", "indep_lag", "dep_sum", "contri", "contri_perc") %>%
        dplyr::mutate(dep_sum = round(.data$dep_sum, round_digits)),
      by = c("stage_id", "version_id", "dependent_id","dep_sum")
    ) %>%
    dplyr::full_join(
      model_coef %>%
        dplyr::filter(.data$type == "flexible") %>%
        dplyr::group_by(.data$version_id, .data$stage_id, .data$dependent_id, .data$model_id, .data$loop_id) %>%
        dplyr::summarise(flexible_vars = paste(.data$variable, collapse = ", "), flexible_vars_count = n(), .groups = "drop"),
      by = c("stage_id", "version_id", "dependent_id", "model_id", "loop_id")
    ) %>%
    dplyr::mutate(
      flexible_vars = if_else(is.na(.data$flexible_vars), "", .data$flexible_vars),
      flexible_vars_count = if_else(is.na(.data$flexible_vars_count), 0, .data$flexible_vars_count)
    ) %>%
    dplyr::full_join(
      model_coef %>%
        dplyr::filter(.data$type == "intercept") %>%
        dplyr::select("version_id", "dependent_id", "stage_id", "model_id", "loop_id") %>%
        dplyr::mutate(intercept = T),
      by = c("stage_id", "version_id", "dependent_id", "model_id", "loop_id")
    ) %>%
    dplyr::mutate(
      intercept = ifelse(is.na(.data$intercept), FALSE, .data$intercept),
      indep_adstock = round(.data$indep_adstock, round_digits),
      indep_power = round(.data$indep_power, round_digits),
      indep_lag = round(.data$indep_lag, round_digits),
      stage_version_dep_mdl_loop_stage = paste(.data$stage_id, .data$version_id, .data$dependent_id, .data$model_id, .data$loop_id, sep = ids_separator)
    )

  mdl_smry_all
}

#' Generates combinations of dependent variables based on provided possible values.
#'
#' @param dep_vars List of dependent variable names.
#' @param possible_values List containing data frames with possible values for each dependent variable.
#'
#' @return A list of named vectors representing all combinations of dependent variables.
#'
#' @importFrom stringr str_detect
#'
#' @examples
#' \dontrun{
#' # Example 1
#' dep_var <- c("a", "b")
#' possible_values <- list(c(a_1_9_2 = 1, b_1_3_4 = 2, a_1_4_4 = 10, b_1_5_3 = 3))
#' get_dep_vars_combinations(dep_var, possible_values)
#'
#' # Example 2
#' dep_var <- c("a")
#' possible_values <- list(c(a_1_4_4 = 10, a_1_4_5 = 10, a_1_1_4 = 10))
#' get_dep_vars_combinations(dep_var, possible_values)
#' }
#'
get_dep_vars_combinations <- function(dep_vars, possible_values) {
  stopifnot(!is.na(unlist(dep_vars)))
  stopifnot(length(unlist(possible_values))>0)

  # Extract relevant columns from each data frame in possible_values
  list_of_combinations <- lapply(dep_vars, function(dep_var) possible_values[[1]][stringr::str_detect(names(possible_values[[1]]), paste0("^", dep_var))])

  # Find all combinations of vectors with and without applied names
  grid_vars_wt_apl <- as.matrix(do.call(expand.grid, lapply(list_of_combinations, names)))
  grid_contri <- as.matrix(do.call(expand.grid, list_of_combinations))

  # Create a list of named vectors
  list_of_named_vectors <- lapply(seq_len(nrow(grid_vars_wt_apl)), function(i) {
    unlist(setNames(as.list(grid_contri[i, ]), as.character(grid_vars_wt_apl[i, ])))
  })

  # Return the list of named vectors
  list_of_named_vectors
}

#' Convert Variable Information to a List
#'
#' Applies a function to convert variable information with contribution values to a list.
#'
#' @param variables_wt_apl_contri Named numeric vector representing variables with weight, application, and contribution values.
#'
#' @return A list of numeric vectors containing variable, weight, application details, and contribution values.
#'
#' @examples
#' \dontrun{
#' # Example 1
#' convert_variable_info2list(c(a_2_4_2 = 9, b_2_4_1 = 6))
#'
#' # Example 2
#' convert_variable_info2list(c(a_2_4_2 = 9))
#' }
#'
convert_variable_info2list <- function(variables_wt_apl_contri) {
  # Parse the variable, weight, and application details using a specific function
  var_wt_apl_df <- parse_variable_wt_apl(names(variables_wt_apl_contri), apl_delimiter = "_", delimiter = "_")

  # Add the contribution values to the parsed dataframe
  var_wt_apl_df$contri <- variables_wt_apl_contri

  # Apply a function to each row of the dataframe, creating a list of numeric vectors
  var_apl_apl_list <- apply(var_wt_apl_df[, -1], 1, function(var_wt_apl) {
    # Extract numeric values from the row and return as a vector
    c(
      var_wt_apl["adstock"], var_wt_apl["power"], var_wt_apl["lag"]
      # , var_wt_apl["contri"]
    )
  }, simplify = FALSE)

  # Assign names to the list based on the 'variable' column of the dataframe
  names(var_apl_apl_list) <- var_wt_apl_df$variable

  # Return the list for this combination
  var_apl_apl_list
}

#' Summarizes target model at stage and variable level.
#'
#' This function takes a dataframe containing information about the target model
#' at different stages and variables. It calculates summary statistics such as
#' model hierarchy, number of unique models (apl), average contribution (contri_avg),
#' and contribution deviation (contri_dev).
#'
#' @param stage_id_target_model A dataframe containing information about the target model.
#' @return A dataframe summarizing the target model at stage and variable level.
#'
#' @importFrom dplyr full_join right_join filter select rename group_by distinct mutate rowwise summarise reframe arrange ungroup
#' @importFrom tidyr separate_rows
#' @importFrom primes generate_n_primes
#' @importFrom stats quantile sd median
#'
generate_reverse_target_smry <- function(stage_id_target_model) {
  reverse_target_smry <- stage_id_target_model %>%
    dplyr::select("stage_id", "variable", "adstock", "power", "lag", "contri_min", "contri_max") %>%
    dplyr::distinct(.data$variable, .data$adstock, .data$power, .data$lag, .data$contri_min, .data$contri_max, .keep_all = T) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      ind_variable = .data$variable,
      contri_avg = (.data$contri_min + .data$contri_max) / 2,
      contri_dev = .data$contri_avg - .data$contri_min
    )

  reverse_target_smry <-
    reverse_target_smry %>%
    dplyr::select("stage_id", "variable", "ind_variable", "adstock", "power", "lag") %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$stage_id, .data$variable, .data$ind_variable) %>%
    dplyr::summarise(n = n(), .groups = "drop") %>%
    dplyr::full_join(
      reverse_target_smry %>%
        dplyr::group_by(.data$stage_id, .data$variable) %>%
        dplyr::reframe(
          quartile_avg =
            list(list(
              Q0 = min(.data$contri_avg),
              Q1 = as.numeric(stats::quantile(.data$contri_avg, 0.25)),
              Q2 = as.numeric(stats::quantile(.data$contri_avg, 0.5)),
              Q3 = as.numeric(stats::quantile(.data$contri_avg, 0.75)),
              Q4 = max(.data$contri_avg)
            )),
          quartile_dev = list(list(
            overall = stats::sd(.data$contri_dev),
            Q1 = stats::sd(.data$contri_dev[.data$contri_avg >= min(.data$contri_avg) & .data$contri_avg <= stats::quantile(.data$contri_avg, 0.25)]),
            Q2 = stats::sd(.data$contri_dev[.data$contri_avg > stats::quantile(.data$contri_avg, 0.25) & .data$contri_avg <= stats::median(.data$contri_avg)]),
            Q3 = stats::sd(.data$contri_dev[.data$contri_avg > stats::median(.data$contri_avg) & .data$contri_avg <= stats::quantile(.data$contri_avg, 0.75)]),
            Q4 = stats::sd(.data$contri_dev[.data$contri_avg > stats::quantile(.data$contri_avg, 0.75) & .data$contri_avg <= max(.data$contri_avg)])
          ))
        ),
      by = c("stage_id", "variable")
    ) %>%
    tidyr::separate_rows("ind_variable", sep = "\\|") %>%
    dplyr::arrange(-.data$stage_id)

  reverse_target_smry <- reverse_target_smry %>%
    dplyr::left_join(
      data.frame(ind_variable = unique(reverse_target_smry$ind_variable), id = primes::generate_n_primes(length(unique(reverse_target_smry$ind_variable)))),
      by = "ind_variable"
    ) %>%
    dplyr::mutate(ind_variable = factor(.data$ind_variable, levels = unique(.data$ind_variable), ordered = T)) %>%
    dplyr::arrange(-.data$stage_id) %>%
    dplyr::mutate(stage_id = factor(.data$stage_id, levels = unique(.data$stage_id), ordered = T)) %>%
    dplyr::group_by(.data$stage_id, .data$variable) %>%
    dplyr::mutate(id = sum(.data$id)) %>%
    dplyr::ungroup()
  reverse_target_smry
}

#' Find Final Dependent Variables
#'
#' This function identifies and returns the final dependent variables from a list of variable scopes based on the provided dependent variable names. It is used to create final dependent named vector for model
#'
#' @param list_of_dep_var_scope A list containing variable scopes.
#' @param dep_vars Character vector of dependent variable names to be matched.
#' @param var_agg_delimiter A character specifying the variable aggregation delimiter. default value is "|".
#' @param apl_delimiter A character specifying the APL delimiter. default value is "_".
#' @param delimiter A character specifying the variable APL delimiter. default value is "_".
#'
#' @examples
#' \dontrun{
#' list_of_vars <- list(a_1_9_2 = 1, b_1_3_4 = 2, c_3_5_2=9, a_1_4_4=10,b_1_5_3=3)
#' dep_vars <- c("a", "b")
#' find_final_dependent_vars(list_of_vars, dep_vars)
#' }
#'
#' @return A subset of list_of_dep_var_scope containing the final dependent variables that match the specified dep_vars.
#'
trim_list_for_dependent_variables <- function(list_of_dep_var_scope, dep_vars, var_agg_delimiter = "|", apl_delimiter = "_", delimiter = "_") {
  names_wo_apl <- parse_variable_wt_apl(names(list_of_dep_var_scope), apl_delimiter = apl_delimiter, delimiter = delimiter)$variable
  names_wo_apl_match <- names_wo_apl %in% dep_vars
  # Tackle segregation of variable scenario
  if (sum(names_wo_apl_match) == 0) {
    names_wo_apl_match <- unlist(lapply(str_split(names_wo_apl, fixed(var_agg_delimiter)), function(x) {
      lapply(str_split(dep_vars, fixed(var_agg_delimiter)), function(y) {
        all(x %in% y)
      })
    }))
  }
  list_of_dep_var_scope[names_wo_apl_match]
}

#' Generate Reverse Target Models
#'
#' This function generates reverse target models based on provided final contributions and model summaries.
#' It takes input dataframes containing final contributions (final_contri), collated models (collated_models),
#' and successful model information (dep_contri_ranges_wt_model_ids), and performs matching and accumulation to generate the models.
#' The resulting dataframe contains all combinations of final contributions from collated models along with intermediate contributions.
#'
#' @param final_contri A dataframe with the following columns:
#'     "variable", "adstock", "power", "lag", "dep_contri_exp".
#'     This dataframe represents the top hierarchy (root) of dependent attributes used to trace the model.
#'     It must comply with the feasible conditions specified in dep_contri_ranges_wt_model_ids.
#' @param collated_models A dataframe containing information on successful models at each model ID.
#'     The columns in collated_models include:
#'     "stage_id", "version_id", "dependent_id", "dep_variable", "dep_adstock", "dep_power", "dep_lag", "model_id", "loop_id",
#'     "indep_variable", "indep_adstock", "indep_power", "indep_lag", "contri_perc", "stage_version_dep_mdl_loop_stage".
#'     Rows must be sorted by stage IDs, where lower row numbers represent granular or leaf models, and higher row numbers indicate top hierarchy in models.
#'     All model IDs must be present in collated_models.
#' @param dep_contri_ranges_wt_model_ids A dataframe containing model IDs, dependent variables, and acceptable contributions (contri_min & contri_max) to achieve benchmark APL and contribution.
#'     This dataframe includes the contribution mix and maximum contribution at each model stage necessary to achieve model benchmark.
#'     Rows must be sorted by stage IDs, where lower row numbers represent granular or leaf models, and higher row numbers indicate top hierarchy in models.
#'     The file is generated as part of the process for reverse modeling.
#' @param acceptable_lower_diff Acceptable lower difference for contribution matching.
#' @param acceptable_upper_diff Acceptable upper difference for contribution matching.
#' @param round_digits Number of digits to which adstock, power, and lag should be rounded to resolve issues of approximation while merging.
#' @param verbose If TRUE, prints information on intermediate steps.
#'
#' @return A dataframe with all combinations from model collation including intermediate contributions.
#'
#' @importFrom dplyr mutate inner_join distinct group_split rowwise filter bind_rows
#' @importFrom purrr reduce
#'
generate_reverse_target_models <- function(final_contri, collated_models, dep_contri_ranges_wt_model_ids, acceptable_lower_diff = 0.0001, acceptable_upper_diff = 0.0001, round_digits = 10, verbose = F) {

  collated_all_model <- collated_all_model %>%
    dplyr::mutate(
      dep_adstock = round(.data$dep_adstock, round_digits),
      dep_power = round(.data$dep_power, round_digits),
      dep_lag = round(.data$dep_lag, round_digits)
    ) %>%
    dplyr::inner_join(dep_contri_ranges_wt_model_ids, by = c("stage_id" = "stage_id", "version_id" = "version_id", "dependent_id" = "dependent_id", "model_id" = "model_id", "loop_id" = "loop_id", "dep_variable" = "variable"), relationship = "many-to-many") %>%
    dplyr::distinct() %>%
    as.data.frame() %>%
    dplyr::group_split(.data$stage_id)

  collated_all_model <- c(list(
    dplyr::bind_rows(collated_all_model[[1]][0,],
              final_contri %>%
                dplyr::rename("indep_variable" = "variable", "indep_adstock" = "adstock", "indep_power" = "power", "indep_lag" = "lag", "indep_contri_exp" = "dep_contri_exp")) %>%
      dplyr::mutate(
        stage_version_dep_mdl_loop_stage = "-999|9|9|9|9|9", stage_id = -999, version_id = 9, dependent_id = 9, model_id = 9, loop_id = 9,
        is_dep_matching = NA, dep_contri_exp = NA, prev_stage_version_dep_mdl_loop_stage = "-999|9|9|9|9|9", contri_check = NA)
  ), collated_all_model)

  accumulated_models <- purrr::reduce(collated_all_model, function(collated_all_model_first, collated_all_model_second) {

    if (verbose) {
      print(paste0("Stage ID : ", unique(collated_all_model_second[1, "stage_id"])))
    }

    next_step_model <- collated_all_model_first %>%
      dplyr::select("indep_variable", "indep_adstock", "indep_power", "indep_lag", "indep_contri_exp", "stage_version_dep_mdl_loop_stage") %>%
      dplyr::rename(
        "dep_variable" = "indep_variable", "dep_adstock" = "indep_adstock", "dep_power" = "indep_power", "dep_lag" = "indep_lag", "dep_contri_exp" = "indep_contri_exp",
        "prev_stage_version_dep_mdl_loop_stage" = "stage_version_dep_mdl_loop_stage"
      ) %>%
      dplyr::distinct()

    next_step_model_matched <-
      next_step_model %>%
      dplyr::inner_join(collated_all_model_second, by = c("dep_variable", "dep_adstock", "dep_power", "dep_lag"), relationship = "many-to-many") %>%
      dplyr::mutate(
        is_dep_matching = !is.na(.data$stage_id),
        indep_contri_exp = if_else(.data$is_dep_matching, .data$dep_contri_exp * .data$contri_perc / 100, .data$dep_contri_exp),
        contri_check = if_else(.data$is_dep_matching, (.data$contri_min <= (.data$dep_contri_exp + acceptable_lower_diff)) & (.data$contri_max >= (.data$dep_contri_exp - acceptable_upper_diff)), FALSE)
      )
    next_step_model_matched<-next_step_model_matched[,names(collated_all_model_first)]

    next_step_model_matched_contri <- next_step_model_matched %>%
      dplyr::rowwise() %>%
      dplyr::filter(identical(.data$is_dep_matching, .data$contri_check)) %>%
      as.data.frame()

    if (nrow(next_step_model_matched_contri) == 0) {
      print("No match found.")
      rbind(
        next_step_model_matched %>% as.data.frame(),
        collated_all_model_first %>% dplyr::mutate(is_dep_matching = F)
      )
    } else {
      rbind(
        next_step_model_matched_contri,
        collated_all_model_first %>% dplyr::mutate(is_dep_matching = F)
      )
    }
  })

  accumulated_models %>% dplyr::select(-"is_dep_matching")
}

#' Get all combinations of variables from a named list
#'
#' This function generates all combinations of variables (flexible variables)
#' from a given named list.
#'
#' @param flexi_vars A named list where each element represents a variable.
#' @return A list containing all combinations of variables from flexi_vars.
#'
#' @examples
#' \dontrun{
#' flexi_vars <- list(a = c(m = 0, n = 1, o = 2), b = c(m = 0.1, n = 2.1, o = 4.2), c = c(m = 3.1, n = 2.7, o = 4.5))
#' all_combination_of_flexi_vars(flexi_vars)
#' }
all_combination_of_flexi_vars <- function(flexi_vars) {
    char_vector <- names(flexi_vars)
    # Get all combinations
    all_combinations <- unlist(lapply(c(0, seq_along(char_vector)), function(x) combn(char_vector, x, simplify = FALSE)), recursive = FALSE)
    all_combinations_with_flexi_vars <- lapply(all_combinations, function(x) {
        flexi_vars[names(flexi_vars) %in% x]
    })
    all_combinations_with_flexi_vars
}

#' Rank Target Models Based on Contribution Metrics
#'
#' This function processes a data frame to rank models based on specified criteria related to marketing contributions.
#' It filters the data to include only relevant columns (stage ID, variable, adstock, power, lag, minimum contribution, and maximum contribution),
#' calculates the range of contribution for each model, and ranks the models based on multiple attributes including adstock, power, and lag.
#'
#' @param data A data frame containing detailed model information including minimum and maximum contributions.
#' @param stage_id_col Name of the column in data representing the stage ID.
#' @param variable_col Name of the column in data representing the variable or factor.
#' @param adstock_col Name of the column in data representing the adstock effect.
#' @param power_col Name of the column in data representing the power of the model.
#' @param lag_col Name of the column in data representing the lag effect.
#' @param contri_min_col Name of the column in data representing the minimum contribution value.
#' @param contri_max_col Name of the column in data representing the maximum contribution value.
#'
#' @return A data frame with models ranked based on the specified criteria. The output data frame excludes the intermediate columns
#' used for calculations such as minimum contribution, contribution range, and count of models per group.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'     stage_id = c(1, 1, 2, 2),
#'     variable = c("A", "B", "A", "B"),
#'     adstock = c(0.5, 0.3, 0.4, 0.6),
#'     power = c(2, 2, 3, 3),
#'     lag = c(1, 2, 1, 2),
#'     contri_min = c(10, 15, 10, 20),
#'     contri_max = c(20, 25, 15, 30)
#' )
#' rank_target_models(data, "stage_id", "variable", "adstock", "power", "lag", "contri_min", "contri_max")
#' }
rank_target_models <- function(data,
                               stage_id_col, variable_col,
                               adstock_col, power_col,
                               lag_col, contri_min_col,
                               contri_max_col) {
    # Select columns from data frame
    data <- data %>%
        select(
            {{ stage_id_col }}, {{ variable_col }},
            {{ adstock_col }}, {{ power_col }},
            {{ lag_col }}, {{ contri_min_col }},
            {{ contri_max_col }}
        )
    processed_model <-
        data %>%
        rowwise() %>%
        mutate(contri_range = .data[[contri_max_col]] - .data[[contri_min_col]]) %>%
        group_by_at(vars(
            {{ stage_id_col }}, {{ variable_col }},
            {{ adstock_col }}, {{ power_col }},
            {{ lag_col }}
        )) %>%
        summarise(
            contri_min = mean(.data[[contri_min_col]]),
            contri_range = round(mean(contri_range, na.rm = TRUE), 2),
            .groups = "drop"
        ) %>%
        group_by_at(vars({{ stage_id_col }}, {{ variable_col }})) %>%
        mutate(n = n()) %>%
        arrange_at(vars(
            {{ stage_id_col }}, {{ variable_col }},
            {{ adstock_col }}, -{{ power_col }},
            {{ lag_col }}, {{ contri_min_col }},
            -contri_range
        )) %>%
        mutate(rank = row_number()) %>%
        select(-contri_min, -contri_range, -n)
    return(processed_model)
}

#' Subset Feasible Target Models
#'
#' Subset target models based on feasibility criteria.
#'
#' This function filters feasible model summaries based on given target model contribution ranges.
#'
#' @param model_summary Data frame summarizing the model, including columns:
#'   "stage_id", "version_id", "dependent_id", "model_id", "loop_id", "dependent_sum", "fixed_contri_perc",
#'   and additional columns for each independent variable indicating adstock, power, lag, contri, and contri_perc.
#' @param target_model Data frame with target model containing columns:
#'   "stage_id", "variable", "adstock", "power", "lag", "contri_min", "contri_max".
#' @param current_stage_id Current stage ID.
#' @param independent_variables Character vector of independent variables in the model.
#' @param reverse_append_remodel Logical indicating whether to append target model parameters during the reverse modeling process. Only meaningful for hierarchical models.
#' @param contri_min_relaxation_factor Numeric value to apply as a factor on the minimum contribution at each stage.
#' @param contri_max_relaxation_factor Numeric value to apply as a factor on the maximum contribution at each stage.
#' @param target_contri_round Numeric value specifying rounding precision for contribution values.
#' @param verbose Logical indicating whether to print details during processing.
#'
#' @return Updated target model data frame with columns representing minimized and maximized contributions for feasible models.
#'
subset_feasible_target_models <- function(
    model_summary,
    target_model,
    current_stage_id,
    independent_variables,
    contri_min_relaxation_factor = 1,
    contri_max_relaxation_factor = 1,
    target_contri_round = 5,
    verbose = FALSE) {
    # subset relevent target models
    current_target_model <- target_model %>%
        filter(stage_id > current_stage_id) %>%
        mutate(
            adstock = round(adstock, 10),
            power = round(power, 10),
            lag = round(lag, 10)
        ) %>%
        select(stage_id, variable, adstock, power, lag, contri_min, contri_max) %>%
        filter(variable %in% independent_variables) %>%
        arrange(-abs(stage_id))
    # remove duplicate vars and convert to list of dataframes of target models
    target_model_list <- current_target_model %>%
        filter(stage_id %in% current_target_model$stage_id[!duplicated(current_target_model[, "variable"])]) %>%
        mutate(
            stage_id = current_stage_id,
            contri_min = contri_min_relaxation_factor * contri_min,
            contri_max = contri_max_relaxation_factor * contri_max
        ) %>%
        distinct() %>%
        group_split(variable)
    target_model_list <- lapply(target_model_list, function(target_model_df) {
        names(target_model_df)[3:7] <- paste(unique(target_model_df$variable), c("adstock", "power", "lag", "contri_min", "contri_max"), sep = "_")
        target_model_df[, -2]
    })
    updated_target_model <- model_summary[, c(
        "stage_id", "version_id", "dependent_id", "model_id", "loop_id", "dependent_sum", "fixed_contri_perc",
        paste(rep(independent_variables, each = 5), rep(c("adstock", "power", "lag", "contri", "contri_perc"), length(independent_variables)), sep = "_")
    )]
    for (target_model_df in target_model_list) {
        suppressMessages(
            updated_target_model <- updated_target_model %>%
                left_join(target_model_df, relationship = "many-to-many")
        )
    }
    if (length(independent_variables) == 1) {
        updated_target_model$dependent_min_sum <-
            updated_target_model$dependent_sum / updated_target_model[, paste0(independent_variables[1], "_contri")] * updated_target_model[, paste0(independent_variables[1], "_contri_min")]
        updated_target_model$dependent_max_sum <-
            updated_target_model$dependent_sum / updated_target_model[, paste0(independent_variables[1], "_contri")] * updated_target_model[, paste0(independent_variables[1], "_contri_max")]
    } else if (length(independent_variables) == 2) {
        updated_target_model[, "dependent_min_1"] <-
            updated_target_model[, "dependent_sum"] / updated_target_model[, paste0(independent_variables[1], "_contri")] * updated_target_model[, paste0(independent_variables[1], "_contri_min")]
        updated_target_model[, "dependent_min_1_contri_2"] <- updated_target_model[, "dependent_min_1"] * updated_target_model[, paste0(independent_variables[2], "_contri_perc")] / 100
        updated_target_model[, "is_min_valid"] <- (updated_target_model[, paste0(independent_variables[2], "_contri_min")] <= updated_target_model[, "dependent_min_1_contri_2"])
        updated_target_model[updated_target_model$is_min_valid, "dependent_min_sum"] <-
            updated_target_model[updated_target_model$is_min_valid, "dependent_sum"] / updated_target_model[updated_target_model$is_min_valid, paste0(independent_variables[1], "_contri")] * updated_target_model[updated_target_model$is_min_valid, paste0(
                independent_variables[1],
                "_contri_min"
            )]
        updated_target_model[!updated_target_model$is_min_valid, "dependent_min_sum"] <-
            updated_target_model[!updated_target_model$is_min_valid, "dependent_sum"] / updated_target_model[!updated_target_model$is_min_valid, paste0(independent_variables[2], "_contri")] * updated_target_model[!updated_target_model$is_min_valid, paste0(
                independent_variables[2],
                "_contri_min"
            )]
        updated_target_model[, "dependent_max_1"] <- updated_target_model[, "dependent_sum"] / updated_target_model[, paste0(independent_variables[1], "_contri")] * updated_target_model[, paste0(independent_variables[1], "_contri_max")]
        updated_target_model[, "dependent_max_1_contri_2"] <- updated_target_model[, "dependent_max_1"] * updated_target_model[, paste0(independent_variables[2], "_contri_perc")] / 100
        updated_target_model[, "is_max_valid"] <- (updated_target_model[, paste0(independent_variables[2], "_contri_max")] >= updated_target_model[, "dependent_max_1_contri_2"])
        updated_target_model[updated_target_model$is_max_valid, "dependent_max_sum"] <-
            updated_target_model[updated_target_model$is_max_valid, "dependent_sum"] / updated_target_model[updated_target_model$is_max_valid, paste0(independent_variables[1], "_contri")] * updated_target_model[updated_target_model$is_max_valid, paste0(
                independent_variables[1],
                "_contri_max"
            )]
        updated_target_model[!updated_target_model$is_max_valid, "dependent_max_sum"] <-
            updated_target_model[!updated_target_model$is_max_valid, "dependent_sum"] / updated_target_model[!updated_target_model$is_max_valid, paste0(independent_variables[2], "_contri")] * updated_target_model[!updated_target_model$is_max_valid, paste0(
                independent_variables[2],
                "_contri_max"
            )]
        if (verbose) {
            cat("Reference Variable: ", independent_variables[1], "\n")
            print(with(updated_target_model, table(is_min_valid, is_max_valid)))
            cat("Valid models")
            print(with(updated_target_model, table(dependent_max_sum > dependent_min_sum)))
            cat("Percentile for  differences", "\n")
            print(with(
                updated_target_model,
                round(quantile(dependent_max_sum - dependent_min_sum, seq(0, 100, by = 10) / 100)), 2
            ))
        }
    } else {
        stop("Not Implemented for more than 2 variables")
    }
    updated_target_model <- updated_target_model %>%
        mutate(
            dependent_min_sum = round(dependent_min_sum, target_contri_round),
            dependent_max_sum = round(dependent_max_sum, target_contri_round)
        ) %>%
        filter(dependent_max_sum >= dependent_min_sum)
    if (nrow(updated_target_model)) {
        updated_target_model <-
            updated_target_model[, c("stage_id", "version_id", "dependent_id", "model_id", "loop_id", "dependent_min_sum", "dependent_max_sum")] %>%
            left_join(model_dependent, by = c("stage_id", "version_id", "dependent_id")) %>%
            select(stage_id, version_id, dependent_id, model_id, loop_id, variable, adstock, power, lag, dependent_min_sum, dependent_max_sum)
        updated_target_model <- updated_target_model %>%
            group_by(stage_id, version_id, dependent_id, model_id, loop_id, variable, adstock, power, lag, dependent_max_sum) %>%
            summarise(dependent_min_sum = min(dependent_min_sum), .groups = "drop") %>%
            group_by(stage_id, version_id, dependent_id, model_id, loop_id, variable, adstock, power, lag, dependent_min_sum) %>%
            summarise(dependent_max_sum = max(dependent_max_sum), .groups = "drop") %>%
            rename("contri_min" = dependent_min_sum, "contri_max" = dependent_max_sum) %>%
            distinct()
    }
    updated_target_model
}

#' Update Target Model
#'
#' Update the target model based on new feasible target models.
#'
#' @param target_model Original target model data frame.
#' @param new_target_model Data frame of new feasible target models to be added.
#' @param independent_variables Character vector of independent variables in the model.
#' @param try_append_feasible_models Logical indicating whether to attempt to append target models during the update process. Default if FALSE
#'   This is applied when re-running the model and the same variable is part of stage 0.
#' @param verbose Logical indicating whether to print details during processing. Default is FALSE.
#'
#' @return Updated target model data frame.
#'
update_target_model <- function(target_model, new_target_model, independent_variables, try_append_feasible_models = FALSE, verbose = FALSE) {
    reverse_append_remodel_upd <- try_append_feasible_models && all(length(independent_variables) == 1 & independent_variables %in% unique(target_model$variable[target_model$stage_id == 0]))
    if (nrow(new_target_model)) {
        if (reverse_append_remodel_upd) {
            new_target_model <- rbind(
                new_target_model,
                target_model %>%
                    filter(variable %in% independent_variables) %>%
                    filter(stage_id != current_stage_id) %>%
                    mutate(stage_id = current_stage_id)
            ) %>%
                distinct()
        }
    } else {
        print("Reverse Model: No models selected")
    }
    if (verbose) {
        print(paste("Reverse Model: ", nrow(new_target_model), "models selected"))
        if (reverse_append_remodel_upd) {
            print(paste("Reverse Model: ", length(unique(new_target_model$dependent_id)) - 1, "Unique dependent APL"))
        } else {
            print(paste("Reverse Model: ", length(unique(new_target_model$dependent_id)), "Unique dependent APL"))
        }
    }
    rbind(target_model, new_target_model)
}

#' Flatten hierarchical paths into a tidy data frame.
#'
#' This function takes a hierarchical path structure represented as a delimited
#' string and flattens it into a tidy data frame format using the tidyverse
#' functions.
#'
#' @param full_path A character vector containing hierarchical paths separated by '||'.
#' @return A tidy data frame with columns representing different levels of the hierarchy.
#'   - \code{aggregated_path}: Separate rows for each aggregated path.
#'   - \code{individual_path}: Paths separated by '||'.
#'   - \code{aggregated_ids}: IDs separated by '>'.
#'   - \code{granular_ids}: IDs separated by '|'.
#'
#' @examples
#' \dontrun{
#' full_path <- "aggregated_path1||aggregated_path2>individual_path1>individual_path2|granular_id1|granular_id2"
#' flatten_path(full_path)
#' }
#' 
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @importFrom stringr str_split fixed
flatten_path <- function(full_path) {
    aggregated_paths <- stringr::str_split(full_path, fixed("||"))
    bind_rows(purrr::map(aggregated_paths, function(aggregated_path) {
        individual_paths <- stringr::str_split(aggregated_path, fixed(">"))
        bind_rows(purrr::map(individual_paths, function(aggregated_ids) {
            bind_rows(purrr::map(aggregated_ids, function(aggregated_id) {
                granular_ids <- stringr::str_split(aggregated_id, fixed("|"))
                granular_ids<-unlist(granular_ids, recursive = F)
                bind_rows(purrr::map(granular_ids, ~ data.frame(id = (.))), .id = "granular_ids")
            }), .id = "aggregated_ids")
        }), .id = "individual_path")
    }), .id = "aggregated_path")
}
