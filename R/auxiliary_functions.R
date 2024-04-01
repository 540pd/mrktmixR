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
      purrr::map(~ setNames(as.numeric(.x), names_vector))
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
    map(~ as.numeric(.x))
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
    condition_tracker<-replicate(length(input_vector),F)
    for (i in seq_along(conditions_replacements)) {
      condition <- conditions_replacements[i]
      replacement <- replacement_values[i]
      match_index <- input_vector == condition
      input_vector[match_index & !condition_tracker] <- replacement
      condition_tracker[match_index]<-T
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
#' @examples
#' \dontrun{
#' # Both matching - aggregation and segregation
#' previous_var_apl <- c("var1|A_.2_2_5" = 0.5, "var1|A_.2_2_9" = 0.5, "var2|B_.5_.4_.2" = 0.3, "var2|B_.5_.1_.2" = 0.4, "var2|B_.5_.1_9" = 0.4, "afaf_2_3_4" = 0)
#' current_var_apl <- c("var1|A_.2_2_5" = 0.6, "var1|A_.2_2_9" = 0.6, "var2|B_.5_.4_.2" = 0.9, "var2|B_.5_.4_.2" = 10)
#' scope_for_dependent_variable(previous_var_apl, current_var_apl)
#'
#' # Aggregation and carry forward
#' previous_var_apl <- c("var3_.2_2_5" = 0.5, "B_.5_.4_.2" = 0.3, "var3_.2_9_5" = 0.78, "B_.9_.4_.2" = 0.33, "d_1_2_4" = 0)
#' current_var_apl <- c("var3|B_2_4_9" = 0.9, "var3|B_2_4_2" = 0.9, "var3|B_2_4_2" = 10, "var3|B_2_4_2" = 11)
#' scope_for_dependent_variable(previous_var_apl, current_var_apl)
#'
#' # Segregation and carry forward
#' previous_var_apl <- c("var1|c_.2_2_5" = 0.5, "var1|c_.2_2_1" = 0.5, "d_1_2_4" = 0, "d_1_2_1" = 0)
#' current_var_apl <- c("var1_2_4_2" = 0.9, "c_.5_.4_.2" = 0.9, "var1_2_4_2" = 11, "c_.5_.4_.2" = 0.10, "c_.5_.4_.2" = 0.10, "c_.5_.4_9" = 0.10)
#' scope_for_dependent_variable(previous_var_apl, current_var_apl)
#'
#' }
#'
scope_for_dependent_variable <- function(previous_var_apl, current_var_apl, var_agg_delimiter = "|", delimiter = "_") {
  if (var_agg_delimiter == delimiter) {
    stop("var_agg_delimiter and delimiter cannot be the same.")
  }
  previous_var_wo_apl_ori <- sapply(str_split(
    names(previous_var_apl),
    stringr::fixed(delimiter)
  ), function(x) x[1])
  previous_var_wo_apl <- unique(previous_var_wo_apl_ori)
  current_var_wo_apl_ori <- sapply(str_split(
    names(current_var_apl),
    stringr::fixed(delimiter)
  ), function(x) x[1])
  current_var_wo_apl <- unique(current_var_wo_apl_ori)
  previous_vars_wo_apl <- str_split(previous_var_wo_apl, stringr::fixed(var_agg_delimiter))
  current_vars_wo_apl <- str_split(current_var_wo_apl, stringr::fixed(var_agg_delimiter))
  previous_var_in_current_vars <- sapply(
    previous_vars_wo_apl,
    function(x) all(x %in% unlist(current_vars_wo_apl))
  )
  # browser()
  feasible_dependent_variables <- if (all(sapply(
    current_var_wo_apl,
    function(x) x %in% previous_var_wo_apl
  ))) {
    previous_var_wo_apl
  } else if (length(current_vars_wo_apl) > 1 && sum(unlist(previous_var_in_current_vars)) ==
             1) {
    c(
      previous_var_wo_apl[!previous_var_in_current_vars],
      current_var_wo_apl
    )
  } else if (length(current_vars_wo_apl) == 1 && length(previous_vars_wo_apl) >
             1 && length(purrr::reduce(
               unlist(current_vars_wo_apl),
               unlist(previous_vars_wo_apl), setdiff
             )) == 0) {
    c(
      previous_var_wo_apl[!previous_var_in_current_vars],
      current_var_wo_apl
    )
  } else {
    previous_var_wo_apl
  }

  return(c(
    current_var_apl[current_var_wo_apl_ori %in% current_var_wo_apl[current_var_wo_apl %in% feasible_dependent_variables]],
    previous_var_apl[previous_var_wo_apl_ori %in% previous_var_wo_apl[previous_var_wo_apl %in% feasible_dependent_variables[!feasible_dependent_variables %in%
                                                                                                                              current_var_wo_apl]]]
  ))
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
create_file_path <- function(folder, file_name, relative_directory = ".") {
  # Check if the suffix indicates a CSV file
  path.expand(
    file.path(
      normalizePath(relative_directory),
      folder,
      file_name
    )
  )
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

    if(nrow(current_data)!=0){
      current_data[[create_date]] <- Sys.time()
      current_data[[update_date]] <- as.POSIXct(NA)
      current_data[["source_current"]] = TRUE

      current_base_data <- current_data %>%
        full_join(
          base_data %>% filter(is.na(get(update_date)))  %>% mutate(source_base = TRUE),
          by = row_ids,
          suffix = c("", "_base")
        )

      # data present in current but not in base - new data
      new_data_added<-current_base_data[is.na(current_base_data$source_base) & !is.na(current_base_data$source_current),names(current_data)]

      # common ids data
      common_data<-current_base_data[!is.na(current_base_data$source_base) & !is.na(current_base_data$source_current),]
      common_data$is_difference <- rowSums(abs(common_data[,compare_cols, drop = F] - common_data[,paste(compare_cols,"base",sep="_"), drop= F])!=0)
      common_data <- common_data %>%
        group_by(across(tidyselect::all_of(row_ids))) %>%
        mutate(row_ids_diff = sum(.data$is_difference))
      common_data[common_data$row_ids_diff!=0,paste0(update_date,"_base")]<-common_data[common_data$row_ids_diff!=0,create_date]

      # udpate any previous change in changes ids
      current_old_data<-current_base_data[!is.na(current_base_data$source_base) & is.na(current_base_data$source_current),c(row_ids,names(current_base_data)[names(current_base_data)  %in%   paste(names(base_data),"base",sep = "_")])]
      current_old_data<-current_old_data %>%
        dplyr::left_join(unique(common_data[,c(changes_ids,create_date)]), by = changes_ids)
      current_old_data[,paste0(update_date,"_base")]<-current_old_data[,create_date]
      current_old_data<-current_old_data[,names(current_old_data) != create_date]

      old_data_updated<-rbind(
        current_old_data,
        common_data[,c(row_ids,names(current_base_data)[names(current_base_data)  %in%   paste(names(base_data),"base",sep = "_")])])
      names(old_data_updated)<-c(row_ids,sub("_base","",names(old_data_updated)[!names(old_data_updated) %in% row_ids]))

      new_data_added<- rbind(common_data[common_data$row_ids_diff!=0,names(current_data)],
                             current_base_data[is.na(current_base_data$source_base) & !is.na(current_base_data$source_current),names(current_data)])

      rbind(old_data_updated, new_data_added[, !names(new_data_added) %in% "source_current"])
    }
    else {
      base_data
    }
  }

#' Compare Named Vectors
#'
#' This function compares two named numeric vectors. It creates a new vector
#' with names from the base vector and values matched from the comparison vector.
#' If a name in the base vector does not exist in the comparison vector,
#' its corresponding value in the output is set to NA.
#'
#' @param base_vector A named numeric vector that serves as the base for comparison.
#' @param comparison_vector A named numeric vector that is compared against the base vector.
#'
#' @return A named numeric vector where each element from the base vector is matched
#'         with the corresponding element in the comparison vector by name.
#'         If a name from the base vector does not exist in the comparison vector,
#'         the corresponding value in the returned vector is NA.
#'
#' @examples
#' \dontrun{
#' base_vector <- c(Alice = 10, Bob = 15, Charlie = 20)
#' comparison_vector <- c(Alice = 5, David = 18, Bob = 12)
#' compare_named_vectors(base_vector, comparison_vector)
#' }
#'
compare_named_vectors <- function(base_vector, comparison_vector) {
  # Create a new vector 'comparison_matched' with names from 'base_vector' and all values as NA
  comparison_matched <- setNames(rep(NA, length(base_vector)), names(base_vector))

  # Find the matching names between 'base_vector' and 'comparison_vector' for efficient updating
  matching_names <- names(base_vector) %in% names(comparison_vector)

  # Update 'comparison_matched' only for matching names, avoiding unnecessary operations
  comparison_matched[matching_names] <- comparison_vector[names(comparison_matched)[matching_names]]

  return(comparison_matched)
}

#' Compare and print named vectors
#'
#' Compare two named vectors and print the differences between them.
#'
#' @param first_vector A named vector.
#' @param second_vector A named vector.
#' @param acceptable_difference Threshold for acceptable difference.
#'
#' @return This function does not return any value. It prints the differences between the two vectors.
#'
#' @examples
#' first_vector <- c(a = 1, b = 2, c = 3)
#' second_vector <- c(a = 1, b = 4, d = 5)
#' compare_and_print_differences(first_vector, second_vector, acceptable_difference = 1)
#'
#' @import dplyr::filter
#' @export
compare_and_print_differences <- function(first_vector, second_vector, acceptable_difference = NULL) {
  # Extract names of each vector
  names1 <- names(first_vector)
  names2 <- names(second_vector)

  # Find names present in one vector but not the other
  names_only_in_first <- setdiff(names1, names2)
  names_only_in_second <- setdiff(names2, names1)

  # Find elements that have different values in both vectors
  differences_df <- data.frame(
    Name = intersect(names1, names2),
    First_Vector = first_vector[intersect(names1, names2)],
    Second_Vector = second_vector[intersect(names1, names2)],
    Difference = -(first_vector[intersect(names1, names2)] - second_vector[intersect(names1, names2)])
  )
  if (!(is.null(acceptable_difference) || is.na(acceptable_difference))) {
    differences_df <- dplyr::filter(differences_df, abs(Difference) > acceptable_difference)
  }

  # Print differences if any are found
  if (length(names_only_in_first) > 0) {
    cat("Names only in first vector:", names_only_in_first, "\n")
  }
  if (length(names_only_in_second) > 0) {
    cat("Names only in second vector:", names_only_in_second, "\n")
  }
  if (nrow(differences_df) > 0) {
    cat("Elements with different values:\n")
    print(differences_df)
  }
}

#' Perform Optimization
#'
#' This function conducts optimization based on specified constraints and model parameters.
#'
#' @param constr_type Type of constraint: "budget" or "goal".
#' @param constr_value Numeric value for the constraint.
#' @param alpha Numeric vector representing model coefficients.
#' @param beta Numeric vector representing model powers.
#' @param initial_spend Initial spending values for optimization.
#' @param lower_bound_spend Lower bound for spending optimization.
#' @param upper_bound_spend Upper bound for spending optimization.
#'
#' @return Numeric vector representing the optimized solution.
#'
#' @details
#' The function employs the NLOPT_LD_SLSQP algorithm for optimization.
#' For "goal" constraints, the optimization maximizes the sum of spending.
#' For "budget" constraints, the optimization minimizes the negative sum of spending.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' constr_type <- "Goal"
#' constr_value <- 5
#' alpha <- c(1, 2)
#' beta <- c(0.3, 0.4)
#' initial_spend <- c(2, 2)
#' lower_bound_spend <- c(-1000, -10000)
#' upper_bound_spend <- c(10000, 10000)
#' perform_optimization(constr_type, constr_value, alpha, beta, initial_spend, lower_bound_spend, upper_bound_spend)
#' }
#'
#' @importFrom nloptr nloptr
#'
#' @export
perform_optimization <-
  function(constr_type,
           constr_value,
           alpha,
           beta,
           initial_spend,
           lower_bound_spend,
           upper_bound_spend) {
    if (grepl("Goal", constr_type, ignore.case = TRUE)) {
      eval_f1 <- function(spend) {
        k <- spend
        return(sum(k))
      }
      eval_grad <- function(spend) {
        return(rep(1, length(spend)))
      }
      eval_g_eq1 <- function(spend) {
        return(constr_value - sum(alpha * spend ^ beta))
      }
      eval_eq_jac <- function(spend) {
        return(-alpha * beta * spend ^ (beta - 1))
      }
    } else {
      eval_f1 <- function(spend) {
        k <- alpha * spend ^ beta
        return(-sum(k))
      }
      eval_grad <- function(spend) {
        return(-alpha * beta * spend ^ (beta - 1))
      }
      eval_g_eq1 <- function(spend) {
        return(constr_value - sum(spend))
      }
      eval_eq_jac <- function(spend) {
        return(rep(-1, length(spend)))
      }
    }

    opts <- list(
      "algorithm" = "NLOPT_LD_SLSQP",
      "xtol_rel" = 1e-16,
      "print_level" = 0,
      "maxeval" = 50000
    )

    opti = nloptr(
      x0 = initial_spend,
      eval_f = eval_f1,
      eval_grad_f = eval_grad,
      eval_g_eq = eval_g_eq1,
      eval_jac_g_eq = eval_eq_jac,
      lb = lower_bound_spend,
      ub = upper_bound_spend,
      opts = opts
    )

    return(opti$solution)
  }
