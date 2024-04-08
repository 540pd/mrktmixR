#' Generate a Venn diagram plot summarizing model flags
#'
#' This function generates a Venn diagram plot summarizing the model flags provided.Each flag represent to given model.
#'
#' @param pvalue_flag A logical vector indicating the presence or absence of p-value flags for each observation.
#' @param sign_flag A logical vector indicating the presence or absence of sign flags for each observation.
#' @param vif_flag A logical vector indicating the presence or absence of VIF flags for each observation.
#'
#' @return A ggplot object representing the Venn diagram plot.
#'
#' @import ggVennDiagram
#' @importFrom ggplot2 theme element_text
#'
#' @examples
#' \dontrun{
#' # Generate sample data
#' pvalue_flag <- sample(c(T,F), 100, replace = TRUE)
#' sign_flag <- sample(c(T,F), 100, replace = TRUE)
#' vif_flag <- sample(c(T,F), 100, replace = TRUE)
#'
#' # Call the function with sample data
#' generate_model_flags_plot(pvalue_flag, sign_flag, vif_flag)
#' }
#'
#' @export
generate_model_flags_plot <- function(pvalue_flag, sign_flag, vif_flag) {
  # Identify observations belonging to each category
  p_value_obs <- which(pvalue_flag)
  sign_obs <- which(sign_flag)
  vif_obs <- which(vif_flag)
  no_flag <- which(!pvalue_flag & !sign_flag & !vif_flag)
  no_good_models <- sum(!pvalue_flag & !sign_flag & !vif_flag)

  # Modify the sets to include areas with zero count
  sets_list <- list(
    "P Value" = c(p_value_obs, no_flag),
    "Sign" = c(sign_obs, no_flag),
    "VIF" = c(vif_obs, no_flag)
  )

  # Create the Venn diagram without legend
  plot <- ggVennDiagram(sets_list) +
    scale_fill_gradient(low = "grey95", high = "darkred") +
    scale_color_brewer(palette = "Set1") +
    theme(legend.position = "none") +
    labs(title = "Summary of model flags",
         subtitle = paste0("\u2003 # of Valid Models: ", no_good_models)) +
    theme(plot.subtitle = element_text(color = "darkgreen", face = "bold"))

  return(plot)
}

#' Plot Contribution Percentage Distribution
#'
#' Plots the distribution of contribution percentages across different combinations
#' of adstock and power values.
#'
#' @param adstock A vector containing adstock values.
#' @param power A vector containing power values.
#' @param contri_perc A vector containing contribution percentages.
#'
#' @return A ggplot object displaying the distribution of contribution percentages.
#'
#' @import ggplot2
#' @importFrom tibble tibble
#'
#' @examples
#' \dontrun{
#' #Generate dummy data
#' adstock_values <- seq(0.1, 0.9, by = 0.1)
#' power_values <- seq(0.1, 0.9, by = 0.1)
#' contri_df <- expand.grid(adstock = adstock_values, power = power_values)
#' # Increase rows to 1000
#' contri_df <- bind_rows(replicate(10, contri_df, simplify = FALSE))
#' # Add contribution percentages around 100 with some variability
#' contri_df$contri_perc <- rnorm(810, mean = 100, sd = 20)
#' # Plot the contribution percentage distribution
#' plot_contri_dist(contri_df$adstock, contri_df$power, contri_df$contri_perc)
#' }
#'
#' @export
plot_contri_dist <- function(adstock, power, contri_perc) {
  contri_df <- tibble::tibble(adstock = adstock, power = power, contri_perc = contri_perc)

  contri_df %>%
    mutate(adstock = ordered(adstock), power = ordered(power)) %>%
    ggplot(aes(x = contri_perc, alpha = after_stat(count))) +
    geom_histogram(binwidth = 0.5, show.legend = F) +
    facet_grid(adstock ~ power, , labeller = label_both, scales = "free") +
    geom_vline(xintercept = 100, color = alpha("green", 0.5), linetype = "dashed") + # Add vertical line
    labs(title = "Contribution % Distribution", x = "Contribution %", y = "# of Models") +
    theme(panel.grid = element_blank())
}


