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
  comparison_matched <-
    setNames(rep(NA, length(base_vector)), names(base_vector))

  # Find the matching names between 'base_vector' and 'comparison_vector' for efficient updating
  matching_names <- names(base_vector) %in% names(comparison_vector)

  # Update 'comparison_matched' only for matching names, avoiding unnecessary operations
  comparison_matched[matching_names] <-
    comparison_vector[names(comparison_matched)[matching_names]]

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
#' @importFrom dplyr filter
#' @export
compare_and_print_differences <-
  function(first_vector,
           second_vector,
           acceptable_difference = NULL) {
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
    if (!(is.null(acceptable_difference) ||
          is.na(acceptable_difference))) {
      differences_df <-
        dplyr::filter(differences_df,
                      abs(.data$Difference) > acceptable_difference)
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
generate_arrows <-
  function(x,
           reference_point = 0,
           size = 40,
           shrinkage_factor = .5,
           positive_color = "#00FF00",
           negative_color = "red") {
    widths <-
      abs(x) ^ shrinkage_factor - abs(reference_point) ^ shrinkage_factor
    widths <- widths * size
    arrows <-
      ifelse(
        x >= reference_point,
        paste0(
          '<span style="color:',
          positive_color,
          "; font-size:",
          abs(widths),
          'px">&#129069;</span>'
        ),
        ifelse(
          x < reference_point,
          paste0(
            '<span style="color:',
            negative_color,
            "; font-size:",
            abs(widths),
            'px">&#129071;</span>'
          ),
          ""
        )
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
generate_circles <-
  function(x,
           max_radius = 10,
           min_radius = 5,
           circle_color = "blue") {
    radii <-
      (x - min(x)) / (max(x) - min(x)) * (max_radius - min_radius) + min_radius
    circles <-
      paste0(
        '<div style="width:',
        radii,
        "px; height:",
        radii,
        "px; border-radius: 50%; background-color:",
        circle_color,
        '; display: inline-block; margin-right: 5px;"></div>'
      )
    return(circles)
  }


#' Abbreviate Number
#'
#' This function abbreviates large numbers using a standardized notation.
#'
#' @param number A numeric vector containing the numbers to be abbreviated.
#'
#' @return A list containing the abbreviated numbers and their corresponding suffixes.
#'
#' @details This function takes a numeric vector and abbreviates large numbers using a standardized notation, such as "K" for thousand, "M" for million, etc. It calculates the appropriate abbreviation based on the magnitude of the input numbers.
#' The following abbreviations are used:
#'   - -: <1
#'   -  : 1 to 100
#'   - H: Hundred (10^2)
#'   - K: Thousand (10^3)
#'   - M: Million (10^6)
#'   - B: Billion (10^9)
#'   - T: Trillion (10^12)
#'   - Q: Quadrillion (10^15)
#'   - QQ: Quintillion (10^18)
#'   - S: Sextillion (10^21)
#'   - SS: Septillion (10^24)
#'   - O: Octillion (10^27)
#'   - N: Nonillion (10^30)
#'   - D: Decillion (10^33)
#'   - UD: Undecillion (10^36)
#'   - DDD: Duodecillion (10^39)
#'   - TD: Tredecillion (10^42)
#'   - QD: Quattuordecillion (10^45)
#'   - QTD: Quindecillion (10^48)
#'   - SD: Sexdecillion (10^51)
#'   - SSD: Septendecillion (10^54)
#'   - OD: Octodecillion (10^57)
#'   - NND: Novemdecillion (10^60)
#'   - V: Vigintillion (10^63)
#'   - UV: Unvigintillion (10^66)
#'   - DV: Duovigintillion (10^69)
#'   - TV: Trevigintillion (10^72)
#'   - QV: Quattuorvigintillion (10^75)
#'   - QTV: Quinvigintillion (10^78)
#'   - SV: Sexvigintillion (10^81)
#'   - SSV: Septenvigintillion (10^84)
#'   - OV: Octovigintillion (10^87)
#'   - NNV: Novemvigintillion (10^90)
#'   - X: Trigintillion (10^93)
#'
#' @examples
#' \dontrun{
#' # Do not run examples because they may take a long time
#' abbreviate_number(c(1000, 1000000, 1000000000))
#' }
#'
#' @importFrom dplyr if_else
#' @export
abbreviate_number <- function(number) {
  # Define the abbreviations
  abbreviations <-
    c(
      "H",
      "K",
      "M",
      "B",
      "T",
      "Q",
      "QQ",
      "S",
      "SS",
      "O",
      "N",
      "D",
      "UD",
      "DDD",
      "TD",
      "QD",
      "QTD",
      "SD",
      "SSD",
      "OD",
      "NND",
      "V",
      "UV",
      "DV",
      "TV",
      "QV",
      "QTV",
      "SV",
      "SSV",
      "OV",
      "NNV",
      "X"
    )

  # Determine the exponent
  exponent <- floor(log10(abs(number)) / 3)
  exponent <- pmin(exponent, length(abbreviations) - 1)

  arrox_number <-
    dplyr::if_else(exponent >= 0 & exponent < length(abbreviations),
                   number / 10 ^ (exponent * 3),
                   number)

  suffix <- rep("", length(number))
  suffix[exponent > 0 &
           exponent < length(abbreviations)] <-
    abbreviations[exponent[exponent > 0 &
                             exponent < length(abbreviations)] + 1]
  suffix[exponent < 0] <- "-"

  return(list(arrox_number, factor(
    suffix,
    levels =
      c("-", "", abbreviations)[c("-", "", abbreviations) %in% suffix],
    ordered = TRUE
  )))
}

#' Format Number
#'
#' This function formats numeric vectors according to specified notation and rounding rules.
#'
#' @param numeric_vector A numeric vector containing the numbers to be formatted.
#' @param notation An ordered factor specifying the notation for each number in numeric_vector.
#' @param round_digit_mapping A named numeric vector specifying the number of digits to round each notation to (default is set to 2 for "M" notation).
#' @param prefix A prefix string to be added before each formatted number (default is empty).
#'
#' @return A character vector containing the formatted numbers with the specified notation and rounding.
#'
#' @details This function formats numeric vectors based on the specified notation and rounding rules.
#' It rounds the numbers according to the specified digits for each notation and adds a prefix string if provided.
#' The 'notation' parameter should be an ordered factor with levels representing the notation for each number in 'numeric_vector'.
#' The 'round_digit_mapping' parameter is a named numeric vector where the names represent the notations and the values represent the number of digits to round to.
#' The default rounding is set to 2 digits for "M" notation.
#'
#' Numbers with corresponding notation greater than the name of 'round_digit_mapping' will be rounded to 0.
#' Numbers with corresponding notation equal to the name of 'round_digit_mapping' will be rounded to the value of 'round_digit_mapping'.
#' Numbers greater than or equal to 1 with corresponding notation less than the name of 'round_digit_mapping' will be rounded to the value of 'round_digit_mapping'.
#' Numbers less than 1 will be converted to a significant number of 'round_digit_mapping'.
#'
#' @examples
#' \dontrun{
#' format_number(c(1234567, 9876543), ordered(c("M", "K")),
#'  round_digit_mapping = c(M = 2), prefix = "$")
#' }
#'
#' @importFrom purrr map_vec
#' @export
format_number <-
  function(numeric_vector,
           notation,
           round_digit_mapping = setNames(2, "M"),
           prefix = "") {
    if (length(round_digit_mapping) > 1) {
      stop("length of round_digit_mapping must not be greater than 1")
    }
    if (!is.na(round_digit_mapping)) {
      if (!names(round_digit_mapping)  %in% notation) {
        stop("round_digit_mapping must be part of notation")
      }

      if (sum(notation > names(round_digit_mapping))) {
        numeric_vector[notation > names(round_digit_mapping)] <-
          round(numeric_vector[notation > names(round_digit_mapping)], 0)
      }
      if (sum(notation == names(round_digit_mapping))) {
        numeric_vector[notation == names(round_digit_mapping)] <-
          round(numeric_vector[notation == names(round_digit_mapping)], as.numeric(round_digit_mapping))
      }
      if (sum(numeric_vector >= 1 &
              notation < names(round_digit_mapping))) {
        numeric_vector[numeric_vector >= 1 &
                         notation < names(round_digit_mapping)] <-
          round(numeric_vector[numeric_vector >= 1 &
                                 notation < names(round_digit_mapping)], as.numeric(round_digit_mapping))
      }
      if (sum(numeric_vector < 1 &
              notation < names(round_digit_mapping))) {
        numeric_vector[numeric_vector < 1 &
                         notation < names(round_digit_mapping)] <-
          signif(numeric_vector[numeric_vector < 1 &
                                  notation < names(round_digit_mapping)], as.numeric(round_digit_mapping))
      }
    }
    notation <- as.character(notation)
    if (sum(notation == "-")) {
      notation[notation == "-"] <- ""
    }
    paste(prefix,
          purrr::map_vec(
            numeric_vector,
            ~ format(
              .x,
              trim = TRUE,
              big.mark = ",",
              scientific = FALSE
            )
          ),
          notation,
          sep = "")
  }

#' Saturation Curve
#'
#' Calculate and visualize saturation curves based on input parameters.
#'
#' @param x_axis Numeric vector representing the x-axis values, typically impressions or spend.
#' @param alpha Numeric vector representing the coefficient values for each channel.
#' @param beta Numeric vector representing the power values for each channel.
#' @param channels Character vector representing the channel names.
#'                 If character, it represents the channel names.
#'                 If factor, it allows changing the order of legend levels based on the factor levels.
#' @param round_digit_mapping A named numeric vector specifying the number of digits to round each notation to (default is set to 2 for "M" notation).
#' @param y_axis_func function for mapping on y axis
#'
#' @return A ggplot object visualizing the saturation curves.
#'
#' @details This function calculates and visualizes saturation curves based on the input parameters: x-axis values, alpha values (coefficients), beta values (powers), and channel names.
#' The contribution for each channel is calculated using the formula: contribution = alpha * (x_axis^beta).
#' The resulting plot includes points, text labels, and line segments representing the saturation curves for each channel.
#'
#' This round_digit_mapping parameter formats numeric vectors based on its parameter.
#' It rounds the numbers according to the specified digits for each notation and adds a prefix string if provided.
#' The 'notation' parameter should be an ordered factor with levels representing the notation for each number in 'numeric_vector'.
#' The 'round_digit_mapping' parameter is a named numeric vector where the names represent the notations and the values represent the number of digits to round to.
#' The default rounding is set to 2 digits for "M" notation.
#'
#' Numbers with corresponding notation greater than the name of 'round_digit_mapping' will be rounded to 0.
#' Numbers with corresponding notation equal to the name of 'round_digit_mapping' will be rounded to the value of 'round_digit_mapping'.
#' Numbers greater than or equal to 1 with corresponding notation less than the name of 'round_digit_mapping' will be rounded to the value of 'round_digit_mapping'.
#' Numbers less than 1 will be converted to a significant number of 'round_digit_mapping'.

#'
#' @import ggplot2
#' @export
saturation_curve <-
  function(x_axis,
           alpha,
           beta,
           channels,
           round_digit_mapping = setNames(2, "M"),
           y_axis_func = function(x, alpha, beta) {
             alpha * (x ^ beta)
           }) {
    # Create a data frame
    input_data <- data.frame(channels, alpha, x_axis, beta)

    # Calculate contribution
    contribution <-
      y_axis_func(input_data[["x_axis"]], input_data[["alpha"]], input_data[["beta"]])

    # Format contribution
    contribution_formatted <- format_number(
      abbreviate_number(contribution)[[1]],
      abbreviate_number(contribution)[[2]],
      round_digit_mapping = round_digit_mapping,
      prefix = ""
    )

    # Format x-axis
    x_axis_formatted <- format_number(
      abbreviate_number(input_data[["x_axis"]])[[1]],
      abbreviate_number(input_data[["x_axis"]])[[2]],
      round_digit_mapping = round_digit_mapping,
      prefix = ""
    )

    # Plot saturation curves
    ggplot(data = input_data) +
      geom_point(aes(x = .data[["x_axis"]], y = contribution, color = .data[["channels"]]),
                 "alpha" = .9,
                 size = 3) +
      geom_text(
        aes(
          x = .data[["x_axis"]],
          y = contribution,
          label =  contribution_formatted,
          color = .data[["channels"]]
        ),
        vjust = -0.5,
        hjust = 1
      ) +  # Label x values in millions
      geom_text(
        aes(
          x = .data[["x_axis"]],
          y = contribution,
          label = x_axis_formatted,
          color = .data[["channels"]]
        ),
        hjust = -.2,
        vjust = 1
      ) + # Label y values as percentage
      geom_segment(
        aes(
          x = .data[["x_axis"]],
          xend = .data[["x_axis"]],
          y = 0,
          yend = contribution,
          color = .data[["channels"]]
        ),
        linetype = "dashed",
        alpha = 0.9
      ) +
      geom_segment(
        aes(
          y = y_axis_func(.data[["x_axis"]], .data[["alpha"]], .data[["beta"]]),
          yend = contribution,
          x = 0,
          xend = .data[["x_axis"]],
          color = .data[["channels"]]
        ),
        ,
        linetype = "dashed",
        alpha = 0.9
      ) +
      lapply(seq_len(nrow(input_data)), function(i) {
        stat_function(
          fun = y_axis_func,
          args = list(alpha = input_data[["alpha"]][i], beta = input_data[["beta"]][i]),
          aes(color = .data[["channels"]][i]),
          geom = "line",
          "alpha" = .6,
          size = .6
        )
      })
  }
