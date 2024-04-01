# # Mock data for independent_var_info
# independent_var_info <- data.frame(
#   variable = c("wt", "qsec", "(Intercept)"),
#   type = c("fixed", "flexible", "intercept")
# )
#
# # Define positive and negative variables
# pos_vars <- c("var1", "var3")
# neg_vars <- c("var2")
#
# # Mock lm_model object
# lm_model <- lm(mpg ~ ., mtcars[,c("mpg" , "wt" , "qsec")])
#
# # coefficients for testing
# coefficients <- summary(lm_model)$coefficients
#
# # Mock VIF values - normally these would be calculated based on the model
# # calculate_vif <- function(model) {
# #   return(c(1.5, 2.5, 1.2))
# # }
#
# # Sample VIF threshold and p-value thresholds
# vif_threshold <- 5
# pvalue_thresholds <- c(intercept = 0.05, fixed = 0.05, flexible = 0.10)
#
# library(testthat)
# library(dplyr)
# library(stringr)
# library(tibble)
#
# # Test for determine_vif_flag function with sample data
# test_that("determine_vif_flag returns correct flags", {
#   expect_equal(determine_vif_flag("fixed", Inf, vif_threshold), FALSE)
#   expect_true(determine_vif_flag("flexible", 5.1, vif_threshold))
#   expect_false(determine_vif_flag("flexible", 4.9, vif_threshold))
# })
#
# # Test for determine_pvalue_flag function with sample data
# test_that("determine_pvalue_flag returns correct flags", {
#   # Using pvalue_thresholds defined earlier
#   expect_true(determine_pvalue_flag("intercept", 0.06, pvalue_thresholds))
#   expect_false(determine_pvalue_flag("fixed", 0.04, pvalue_thresholds))
# })
#
# # Test for determine_expected_sign function with sample data
# test_that("determine_expected_sign returns the correct expectation", {
#   expect_true(determine_expected_sign("var1", pos_vars, neg_vars, "\\|"))
#   expect_false(determine_expected_sign("var2", pos_vars, neg_vars, "\\|"))
#   expect_true(is.na(determine_expected_sign("varX", pos_vars, neg_vars, "\\|")))
# })
#
# # Test for get_base_model function with sample data
# test_that("get_base_model returns expected output structure", {
#   # Using sample_lm_model defined earlier
#   results <- suppressWarnings(get_base_model(lm_model, independent_var_info, pos_vars, neg_vars, get_model_object = TRUE))
#   expect_type(results, "list")
#   expect_length(results, 3)
#   expect_s3_class(results[[1]], "tbl_df")
#   expect_s3_class(results[[2]], "tbl_df")
#   expect_s3_class(results[[3]], "lm")
# })
#
# # Test for identify_drop_variable function with sample data
# test_that("identify_drop_variable identifies the correct variable to drop", {
#   # Create a mock coef_df based on our mock data
#   coef_df <- as.data.frame(summary(lm_model)$coefficient) %>%
#     rownames_to_column('variable')%>%
#     left_join(independent_var_info, by = "variable")
#   variable_to_drop <- identify_drop_variable(coef_df, 2, TRUE, FALSE, 10)
#   expect_equal(variable_to_drop, "qsec")
# })
#
# # Test for update_model function with sample data
# test_that("update_model updates the model formula", {
#   # Create a new variable to drop
#   variable_to_drop <- "wt"
#   # Update the model by removing 'wt' variable
#   updated_model <- update_model(lm_model, variable_to_drop)
#   expect_false(any(grepl(variable_to_drop, formula(updated_model))))
#   # Check if the model is still of class 'lm'
#   expect_s3_class(updated_model, "lm")
# })
