library(testthat)
library(stringr)

# Define test cases
test_that("generate_arrows returns correct arrow representations", {
  # Test case 1: Positive values
  expect_equal(generate_arrows(c(1, 2, 3)), c('<span style="color:#00FF00; font-size:40px">&#129069;</span>', '<span style="color:#00FF00; font-size:40px">&#129069;</span>', '<span style="color:#00FF00; font-size:40px">&#129069;</span>'))

  # Test case 2: Negative values
  expect_equal(generate_arrows(c(-1, -2, -3)), c('<span style="color:red; font-size:40px">&#129071;</span>', '<span style="color:red; font-size:40px">&#129071;</span>', '<span style="color:red; font-size:40px">&#129071;</span>'))

  # Test case 3: Values around the reference point
  expect_equal(generate_arrows(c(0, 0.5, -0.5)), c('', '<span style="color:#00FF00; font-size:28.2842712474619px">&#129069;</span>', '<span style="color:red; font-size:28.2842712474619px">&#129071;</span>'))

  # Test case 4: Different reference point
  expect_equal(generate_arrows(c(1, 2, 3), reference_point = 2), c('<span style="color:red; font-size:40px">&#129071;</span>', '', '<span style="color:#00FF00; font-size:40px">&#129069;</span>'))
})

# Test case 1: Generate circles with default parameters
x1 <- c(1, 2, 3, 4, 5)
circles1 <- generate_circles(x1)
print(circles1)

# Test case 2: Generate circles with custom parameters
x2 <- c(3, 6, 9, 12, 15)
circles2 <- generate_circles(x2, max_radius = 15, min_radius = 8, circle_color = "green")
print(circles2)

# Test case 3: Generate circles with negative values
x3 <- c(-2, 0, 4, 7, 10)
circles3 <- generate_circles(x3, max_radius = 12, min_radius = 6, circle_color = "orange")
print(circles3)

# Test case 4: Generate circles with empty input vector
x4 <- numeric(0)
circles4 <- generate_circles(x4)
print(circles4)


# Test case 1: Test with numeric vector ranging from 100 to 1000000
spend_vector_1 <- c(100, 1000, 10000, 100000, 1000000)
expected_result_1 <- c("$100 H", "$1.0 K", "$10 K", "$100 K", "$1.0 M")
test_that("Test case 1", {
  expect_equal(format_number(spend_vector_1), expected_result_1)
})

# Test case 2: Test with numeric vector ranging from 100000 to 1000000000
spend_vector_2 <- c(100000, 1000000, 10000000, 100000000, 1000000000)
expected_result_2 <- c("$100 K", "$1.0 M", "$10 M", "$100 M", "$1.0 B")
test_that("Test case 2", {
  expect_equal(format_number(spend_vector_2), expected_result_2)
})

# Test case 3: Test with numeric vector containing negative values
spend_vector_3 <- c(-100000, -1000000, -10000000, -100000000, -1000000000)
expected_result_3 <- c("$-100 K", "$-1.0 M", "$-10 M", "$-100 M", "$-1.0 B")
test_that("Test case 3", {
  expect_equal(format_number(spend_vector_3), expected_result_3)
})

# Test case 4: Test with a single numeric value
spend_vector_4 <- 1500000
expected_result_4 <- "$1.5 M"
test_that("Test case 4", {
  expect_equal(format_number(spend_vector_4), expected_result_4)
})

# library(testthat)

# # Test cases for positive and negative numbers with different magnitudes
# test_that("Abbreviate number function works correctly", {
#   numeric_vector <- c(-0.000012131,123, 1234, 12345, 123456, 1234567, 12345678,
#                        123456789, 1234567890, 12345678901, 123456789012, 1234567890123, 12345678901231123456789012311234567890123112345678901234567890123112345678901231123456789012311234567890)
#   expected_result <- list(
#     c(-0.00001213, 0.123, 1.234, 12.345, 123.456, 1.234567, 12.34568, 123.4568, 1234.568, 12345.68, 123456.8, 1234568, 123456800000),
#     factor(c("-", "", "K", "K", "K", "M", "M", "M", "B", "B", "B", "B", "B"), levels = c("-", "", "K", "M", "B"), ordered = TRUE)
#   )
#   expect_equal(abbreviate_number(numeric_vector), expected_result)

#   numeric_vector <- c(0.0001 , 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)
#   expected_result <- list(
#     c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000),
#     factor(c("", "", "", "", "K", "K", "K", "K", "K", "K", "M", "M"), levels = c("-", "", "K", "M", "B"), ordered = TRUE)
#   )
#   expect_equal(abbreviate_number(numeric_vector), expected_result)

#   numeric_vector <- c(0.00004999999 ,0.4999 , 0.499, 0.49, 4.9, 4, 49, 499, 4999, 49999, 499999, 4999999, 49999999)
#   expected_result <- list(
#     c(0.00005, 0.4999, 0.499, 0.49, 4.9, 4, 49, 499, 4999, 49999, 499999, 4999999, 49999999),
#     factor(c("", "", "", "", "", "", "", "K", "K", "K", "K", "K", "K"), levels = c("-", "", "K", "M", "B"), ordered = TRUE)
#   )
#   expect_equal(abbreviate_number(numeric_vector), expected_result)
# })


# # Test cases for different scenarios
# test_that("Format number function works correctly", {
#   # Test case 1: Numeric vector with notation greater than name of round_digit_mapping
#   numeric_vector <- c(1234567, 9876543)
#   notation <- ordered(c("M", "K"))
#   round_digit_mapping <- c(M = 2, K = 1)
#   prefix <- "$"
#   expected_result <- c("$1,234,568", "$9,876,543")
#   expect_equal(format_number(numeric_vector, notation, round_digit_mapping, prefix), expected_result)

#   # Test case 2: Numeric vector with notation equal to name of round_digit_mapping
#   numeric_vector <- c(1234567, 9876543)
#   notation <- ordered(c("M", "M"))
#   round_digit_mapping <- c(M = 2, K = 1)
#   prefix <- "$"
#   expected_result <- c("$1,234,567", "$9,876,543")
#   expect_equal(format_number(numeric_vector, notation, round_digit_mapping, prefix), expected_result)

#   # Test case 3: Numeric vector with notation less than name of round_digit_mapping and number >= 1
#   numeric_vector <- c(1234567, 9876543)
#   notation <- ordered(c("K", "K"))
#   round_digit_mapping <- c(M = 2, K = 1)
#   prefix <- "$"
#   expected_result <- c("$1.2M", "$9.9M")
#   expect_equal(format_number(numeric_vector, notation, round_digit_mapping, prefix), expected_result)

#   # Test case 4: Numeric vector with notation less than name of round_digit_mapping and number < 1
#   numeric_vector <- c(0.0001234, 0.0009876)
#   notation <- ordered(c("M", "M"))
#   round_digit_mapping <- c(M = 2, K = 1)
#   prefix <- "$"
#   expected_result <- c("$0.00012", "$0.00099")
#   expect_equal(format_number(numeric_vector, notation, round_digit_mapping, prefix), expected_result)
# })


# setwd("C:\\Users\\abhimma\\Desktop\\MMM_Insights")
# library(tidyverse)
#
# source("C:/Users/abhimma/Desktop/MMM/mrktmix/reporting.r")
#
#
# perf<-read_csv("Channel_Performance_apr_mod_cpm.csv", show_col_types = FALSE)
# perf$Channels <- factor(perf$Channels, levels = unique(perf$Channels))
# perf$Coefficient<-perf$Contribution/(perf$Spend^perf$Power)
# perf$Impressions<-perf$Impression
# perf$CMP<-perf$Spend/perf$Impressions*1000
#
# # Install and load required packages if not already installed
# # install.packages("ggplot2")
# # install.packages("plotly")
# library(ggplot2)
# library(plotly)
# library(gridExtra)
#
# df<-perf
# response_curve <-saturation_curve(df$Spend, df$Coefficient,df$Power, df$Channels, round_digit_mapping = setNames(1, "M"), y_axis_func = function(x, alpha, beta) {alpha * (x^beta)})
# response_curve
# response_curve<-
#   response_curve +
#   labs(
#     title = "Response Curves: Top 6 Channels by Spend",
#     x = "Spend ($ Millions)",
#     y = "Response / Contribution",
#     color = "channels"
#   ) +
#   theme_classic() +
#   theme(legend.position = "bottom", legend.title = element_blank(),
#         plot.title = element_text(size = 17, hjust = 0),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.text = element_text(size = 12)) +
#   scale_x_continuous(
#     labels = scales::dollar_format(prefix = "$", scale = 1e-6, suffix = " M", accuracy = NULL, big.mark = ","),
#     limits = range(c(0, 15e6))
#   ) +
#   scale_y_continuous(
#     labels = function(x) sprintf("%.1f", x),
#     limits = range(c(0, 6.25))
#   )
# ggsave("Spend_112.png", plot = response_curve, device = "png", width = 10, height = 8, units = "in")
#
# response_curve <-saturation_curve(df$Spend, df$Coefficient,df$Power, df$Channels, round_digit_mapping = setNames(1, "M"), y_axis_func = function(x, alpha, beta) {alpha * (x^beta)/x*1e6})
# response_curve
# response_curve<-
#   response_curve +
#   labs(
#     title = "Effectiveness Curves: Top 6 Channels by Spend",
#     x = "Spend ($ Millions)",
#     y = "Effectiveness \n (Contribution per $1M)",
#     color = "Channels"
#   ) +
#   theme_classic() +
#   theme(legend.position = "bottom", legend.title = element_blank(),
#         plot.title = element_text(size = 17, hjust = 0),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.text = element_text(size = 12)) +
#   scale_x_continuous(
#     labels = scales::dollar_format(prefix = "$", scale = 1e-6, suffix = " M", accuracy = NULL, big.mark = ","),
#     limits = range(c(0, 15e6))
#   ) +
#   scale_y_continuous(
#     labels = function(x) sprintf("%.1f", x),
#     limits = range(c(0, 1.25))
#   )
# ggsave("Effectiveness_111.png", plot = response_curve, device = "png", width = 10, height = 8, units = "in")

