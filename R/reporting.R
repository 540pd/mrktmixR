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
#' generate_model_decomposition(list(setNames(1,"dep_var1_0_1_0")), list(setNames(c(1:3),c("indep_var1_0_1_0","indep_var2_0_1_0","indep_var3_0_1_0"))), df)
#' generate_model_decomposition(list(setNames(1,"dep_var1|dep_var2_0_1_0")), list(setNames(c(1:2),c("indep_var1|indep_var2_0_1_0","indep_var3_0_1_0"))), df)
#' generate_model_decomposition(list(setNames(1:2,c("dep_var1_0_1_0","dep_var2_0_1_0"))), list(setNames(c(1:3),c("indep_var1_0_1_0","indep_var2_0_1_0","indep_var3_0_1_0"))), df)
#' generate_model_decomposition(list(setNames(1,"dep_var1_0_1_0"),setNames(2,"dep_var2_0_1_0")), list(setNames(c(1:3),c("indep_var1_0_1_0","indep_var2_0_1_0","indep_var3_0_1_0")),setNames(c(1:3),c("indep_var1_0_1_0","indep_var2_0_1_0","indep_var3_0_1_0"))), df)
#'}
#' @importFrom purrr map2_vec
#'
#' @export
#'
generate_model_decomposition <- function(dep_info, indep_info, modeling_df, dep_info_is_weight_coefficient = FALSE, indep_info_is_weight_coefficient = TRUE, apl_delimiter = "_", delimiter = "_", var_agg_delimiter = "|"){
  bp_stage_id <- purrr::map2(dep_info, indep_info, function(y, x) {
    dep_bp <- decompose_model_component(y,
                                        modeling_df,
                                        is_weight_coefficient = dep_info_is_weight_coefficient,
                                        apl_delimiter = apl_delimiter,
                                        delimiter = delimiter,
                                        var_agg_delimiter = var_agg_delimiter
    )
    indep_bp <- decompose_model_component(x,
                                          modeling_df,
                                          is_weight_coefficient = indep_info_is_weight_coefficient,
                                          apl_delimiter = apl_delimiter,
                                          delimiter = delimiter,
                                          var_agg_delimiter = var_agg_delimiter
    )
    residual <- rowSums(dep_bp) - rowSums(indep_bp)
    names(residual) <- "residual"
    list(dep_bp,cbind(indep_bp, residual))
  })
  bp_stage_id
}


#' Generate arrows based on values relative to a reference point
#'
#' This function generates arrows representing the direction and magnitude of values relative to a reference point.
#' Arrows are HTML entities rendered based on the sign and magnitude of the input values. The size of the arrows is
#' determined by the absolute value of the input values raised to a power, allowing for non-linear scaling.
#' The closer the value to zero, the less relative difference there will be between the sizes of the arrows.
#'
#' @param x A numeric vector of values.
#' @param reference_point The reference point relative to which arrows are generated. Default is 0.
#' @param size The size of the arrows. Default is 40.
#' @param shrinkage_factor A value controlling the shrinkage of arrow width based on the magnitude of the input values.
#'   A smaller shrinkage_factor will result in less relative difference in arrow sizes for values closer to the reference_point.
#'   Default is 0.5.
#' @param positive_color The color of arrows representing values greater than or equal to the reference point. Default is "#00FF00" (green).
#' @param negative_color The color of arrows representing values less than the reference point. Default is "red".
#'
#' @return A character vector containing HTML representations of arrows.
#'
#' @examples
#' \dontrun{
#' x <- c(1, 2, 3, 0, -1, -2, -3)
#' arrows <- generate_arrows(x, reference_point = 0, size = 40, shrinkage_factor = 0.5)
#' print(arrows)
#' }
#'
#' @export
generate_arrows <- function(x, reference_point = 0, size = 40, shrinkage_factor = .5, positive_color = "#00FF00", negative_color = "red") {
  widths <- abs(x)^shrinkage_factor - abs(reference_point)^shrinkage_factor
  widths <- widths * size
  arrows <- ifelse(x >= reference_point, paste0('<span style="color:', positive_color, "; font-size:", abs(widths), 'px">&#129069;</span>'),
                   ifelse(x < reference_point, paste0('<span style="color:', negative_color, "; font-size:", abs(widths), 'px">&#129071;</span>'), "")
  )
  return(arrows)
}

#' Generate circles with static color
#'
#' This function generates circles with a static color based on the values provided in the input vector.
#' The size of the circles is determined by scaling the values linearly between a maximum and minimum radius.
#' The color of all circles remains constant and is specified by the circle_color parameter.
#'
#' @param x A numeric vector of values.
#' @param max_radius The maximum radius of the circles. Default is 10.
#' @param min_radius The minimum radius of the circles. Default is 5.
#' @param circle_color The color of the circles. Default is "blue".
#'
#' @return A character vector containing HTML representations of circles.
#'
#' @examples
#' \dontrun{
#' x <- c(1, 2, 3, 4, 5)
#' circles <- generate_circles(x, max_radius = 20, min_radius = 10, circle_color = "red")
#' print(circles)
#' }
#'
#' @export
generate_circles <- function(x, max_radius = 10, min_radius = 5, circle_color = "blue") {
  radii <- (x - min(x)) / (max(x) - min(x)) * (max_radius - min_radius) + min_radius
  circles <- paste0('<div style="width:', radii, "px; height:", radii, "px; border-radius: 50%; background-color:", circle_color, '; display: inline-block; margin-right: 5px;"></div>')
  return(circles)
}

#' Format numeric vector into human-readable pretty format
#'
#' This function formats a numeric vector into a human-readable format with appropriate prefixes and suffixes.
#'
#' @param spend_vector A numeric vector containing the values to be formatted.
#' @param prefix The prefix to be added before the formatted numbers (default is "$").
#' @param million_round_dp The number of decimal places to round for numbers in millions (default is 1).
#'
#' @return A character vector containing the formatted numbers with prefixes and suffixes.
#'
#' @details
#' The function formats numeric values in the spend_vector into a more readable format with appropriate prefixes and suffixes.
#' The suffixes used are: "B" for billion, "M" for million, "K" for thousand, and "H" for hundred.
#' The function rounds the numbers and adds suffixes accordingly.
#'
#' @examples
#' \dontrun{
#' spend_vector <- c(100, 1000, 10000, 1000000, 10000000)
#' format_number(spend_vector)
#'}
#'
#' @importFrom purrr map2_vec
#' @importFrom dplyr if_else
#' @export
format_number <- function(spend_vector, prefix = "$", million_round_dp = 1) {
  suffix <- dplyr::if_else(spend_vector >= 1e9, "B", dplyr::if_else(spend_vector >= 1e6, "M", dplyr::if_else(spend_vector >= 1e3, "K", "H")))
  round_digit <- dplyr::if_else(suffix == "B", -9, dplyr::if_else(suffix == "M", -6, dplyr::if_else(suffix == "K", -3, 0)))
  round_digit <- dplyr::if_else(suffix %in% c("B", "M"), round_digit + million_round_dp, round_digit)
  approx_spend <- purrr::map2_vec(spend_vector, round_digit, function(x, y) round(x, digits = y))
  formatted_spend <- dplyr::if_else(suffix == "B", format(round(approx_spend / 1e9, 0), nsmall = 0),
                                    dplyr::if_else(suffix == "M", format(round(approx_spend / 1e6, million_round_dp), nsmall = million_round_dp),
                                                   dplyr::if_else(suffix == "K", paste0(approx_spend / 1e3), paste0(round(approx_spend)))
                                    )
  )
  formatted_spend <- purrr::map2_vec(formatted_spend, suffix, function(x, y) {
    paste(trimws(x), y, sep = "")
  })
  return(paste0(prefix, trimws(formatted_spend)))
}
