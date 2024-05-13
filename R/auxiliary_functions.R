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
