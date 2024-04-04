library(testthat)
library(dplyr)
library(purrr)

# Assuming 'mrktmix:::convert_to_named_seq' function is defined as earlier

# Test 1: Basic Functionality
test_that("Basic functionality works", {
  char_vector <- c("1,3,2", "4,6,2")
  result <-
    mrktmix:::convert_to_named_seq(char_vector, ",", c("start", "end", "step"))
  expect_equal(length(result), 2)
  expect_equal(names(result[[1]]), c("start", "end", "step"))
})

# Test 2: different delimiter
test_that("with different delimiter", {
  char_vector <- c("1;3;2", "4;6;2")
  result <-
    mrktmix:::convert_to_named_seq(char_vector, ";", c("start", "end", "step"))
  expect_equal(length(result), 2)
  expect_equal(names(result[[1]]), c("start", "end", "step"))
})


# Assuming 'mrktmix:::name_and_truncate_list' function is defined as above

# Test 1: Basic Functionality
test_that("Basic functionality works", {
  list_to_name <- list(1, 2)
  var_names <- c("var1", "var2")
  result <-
    mrktmix:::name_and_truncate_list(list_to_name, var_names)
  expect_equal(length(result), 2)
  expect_equal(names(result), var_names)
})

# Test 2: Longer Names Vector
test_that("Handles longer names vector correctly", {
  list_to_name <- list(1, 2)
  var_names <- c("var1", "var2", "var3")
  result <-
    mrktmix:::name_and_truncate_list(list_to_name, var_names)
  expect_equal(length(result), length(list_to_name))
  expect_equal(names(result), var_names[1:2])
})

# Test 3: Shorter Names Vector
test_that("Handles shorter names vector correctly", {
  list_to_name <- list(1, 2, 3)
  var_names <- c("var1", "var2")
  result <-
    mrktmix:::name_and_truncate_list(list_to_name, var_names)
  expect_equal(length(result), length(var_names))
  expect_equal(names(result), var_names)
})

# Test 1: Basic Functionality
test_that("Basic functionality works", {
  char_vector <- c("1,2,3", "4,5,6")
  result <- mrktmix:::split_and_convert_to_numeric(char_vector, ",")
  expect_equal(length(result), 2)
  expect_equal(result[[1]], c(1, 2, 3))
  expect_equal(result[[2]], c(4, 5, 6))
})

# Test 2: Different Delimiter
test_that("Handles different delimiter correctly", {
  char_vector <- c("1-2-3", "4-5-6")
  result <- mrktmix:::split_and_convert_to_numeric(char_vector, "-")
  expect_equal(length(result), 2)
  expect_equal(result[[1]], c(1, 2, 3))
  expect_equal(result[[2]], c(4, 5, 6))
})

# Test 3: Non-Numeric String
# test_that("Handles non-numeric strings", {
#   char_vector <- c("a,b,c", "d,e,f")
#   result <- mrktmix:::split_and_convert_to_numeric(char_vector, ",")
#   expect_true(all(sapply(result, function(x)
#     all(is.na(
#       x
#     )))))
# })

# Assuming 'mrktmix:::create_named_lists_from_vars' function is defined as above

# Test 1: Basic Functionality
test_that("Basic functionality works", {
  var_names <- c("var1", "var2")
  predefined_pairs <-
    list(setNames(1:3, c("adstock", "power", "lag")))
  result <-
    mrktmix:::create_named_lists_from_vars(var_names, predefined_pairs)
  expect_equal(length(result), 2)
  expect_equal(names(result), var_names)
  expect_true(all(sapply(result, function(x)
    identical(x, predefined_pairs[[1]]))))
})

# Test 2: Empty var_names
test_that("Handles empty var_names correctly", {
  expect_error(mrktmix:::create_named_lists_from_vars(c(), list(a = 1)))
})

# Test 3: NULL var_names
test_that("Handles NULL var_names correctly", {
  expect_error(mrktmix:::create_named_lists_from_vars(NULL, list(a = 1)))
})

# Test 4: NULL predefined_pairs
test_that("Handles NULL predefined_pairs correctly", {
  var_names <- c("var1", "var2")
  expect_error(mrktmix:::create_named_lists_from_vars(var_names, NULL))
})

# Test 5: Non-list predefined_pairs
test_that("Handles non-list predefined_pairs", {
  var_names <- c("var1" , "var2")
  expect_error(mrktmix:::create_named_lists_from_vars(var_names, "not a list"))
})

# Test case 1: Basic replacement
test_that("Basic replacement works", {
  input_vector <- c(1, 2, 3, 4, 5)
  conditions_replacements <- c(2, 4)
  replacement_values <- c(20, 40)

  result <-
    replace_values_pairwise(input_vector, conditions_replacements, replacement_values)

  expect_equal(result, c(1, 20, 3, 40, 5))
})

# Test case 2: Empty input vector
test_that("Empty input vector returns empty vector", {
  input_vector <- numeric(0)
  conditions_replacements <- c(2, 4)
  replacement_values <- c(20, 40)

  result <-
    replace_values_pairwise(input_vector, conditions_replacements, replacement_values)

  expect_equal(result, numeric(0))
})

# Test case 3: Unequal lengths of conditions and replacements
test_that("Unequal lengths of conditions and replacements throw an error", {
  input_vector <- c(1, 2, 3, 4, 5)
  conditions_replacements <- c(2, 4, 6)  # Different length
  replacement_values <- c(20, 40)

  expect_error(
    replace_values_pairwise(input_vector, conditions_replacements, replacement_values),
    "Length of conditions_replacements and replacement_values should be the same."
  )
})

# Test case 4: No replacement needed
test_that("No replacement needed if conditions are not in the input vector", {
  input_vector <- c(1, 3, 5)
  conditions_replacements <- c(2, 4)
  replacement_values <- c(20, 40)

  result <-
    replace_values_pairwise(input_vector, conditions_replacements, replacement_values)

  expect_equal(result, c(1, 3, 5))
})



# Test case 1: Basic replacement with identical variables
test_that("Basic replacement works with identical variables", {
  previous_var_apl <-
    c("var1|A_0.2_2_0.5" = 0.5,
      "var2|B_0.5_3_0.3" = 0.3)
  current_var_apl <-
    c("var1|A_0.2_2_0.5" = 0.6,
      "var2|B_0.5_3_0.3" = 0.2)

  result <-
    scope_for_dependent_variable(previous_var_apl, current_var_apl)

  expect_equal(result, current_var_apl)
})

# Test case 2: No replacement needed with non-overlapping variables; invalid case
test_that("No replacement needed with non-overlapping variables", {
  previous_var_apl <-
    c("var1|A_0.2_2_0.5" = 0.5,
      "var2|B_0.5_3_0.3" = 0.3)
  current_var_apl <-
    c("var3|C_0.4_1_0.2" = 0.2,
      "var4|D_0.6_4_0.4" = 0.4)

  result <-
    scope_for_dependent_variable(previous_var_apl, current_var_apl)

  expect_equal(result, c(
    "var1|A_0.2_2_0.5" = 0.5,
    "var2|B_0.5_3_0.3" = 0.3
  ))
})

# Test case 3: Replacement needed with one overlapping variable
test_that("Replacement needed with one overlapping variable", {
  previous_var_apl <-
    c(
      "var1_0.2_2_0.5" = 0.5,
      "var2_0.5_3_0.3" = 0.3,
      "var3_0.4_1_0.2" = 0.7
    )
  current_var_apl <-
    c("var3_0.4_1_0.2" = 0.2, "var4_0.4_1_0.2" = 0.2)

  result <-
    scope_for_dependent_variable(previous_var_apl, current_var_apl)

  expect_equal(
    result,
    c(
      "var3_0.4_1_0.2" = 0.2,
      "var4_0.4_1_0.2" = 0.2,
      "var1_0.2_2_0.5" = 0.5,
      "var2_0.5_3_0.3" = 0.3
    )
  )
})

# Test case 4: variable aggregation
test_that("Replacement needed with variable aggregation", {
  previous_var_apl <-
    c(
      "var1A_0.2_2_0.5" = 0.5,
      "var2B_0.5_3_0.3" = 0.3,
      "var3C_0.4_1_0.2" = 0.7
    )
  current_var_apl <- c("var1A|var2B_0.5_3_0.3" = 0.2)

  result <-
    scope_for_dependent_variable(previous_var_apl, current_var_apl, var_agg_delimiter = "|")

  expect_equal(result,
               c(
                 "var1A|var2B_0.5_3_0.3" = 0.2,
                 "var3C_0.4_1_0.2" = 0.7
               ))
})

# Test case 4: segregation
test_that("Replacement needed with variable aggregation", {
  previous_var_apl <-
    c("var1A|var2B_0.5_3_0.3" = 0.2,
      "var3C_0.4_1_0.2" = 0.7)
  current_var_apl <-
    c("var1A_0.2_2_0.5" = 0.5,
      "var2B_0.5_3_0.3" = 0.3)

  result <-
    scope_for_dependent_variable(previous_var_apl, current_var_apl, var_agg_delimiter = "|")

  expect_equal(result,
               c(
                 "var1A_0.2_2_0.5" = 0.5,
                 "var2B_0.5_3_0.3" = 0.3,
                 "var3C_0.4_1_0.2" = 0.7
               ))
})


paste0("c(",paste(paste(as.numeric(result), collapse=","),"),c('",paste(names(result), collapse="', '")),"')")

# Test Case 1: Both matching - aggregation & segregation
test_that("Both matching - aggregation & segregation", {
  previous_var_apl <- c("var1|A_.2_2_5"=.5, "var1|A_.2_2_9"=.5, "var2|B_.5_.4_.2" = .3,  "var2|B_.5_.1_.2" = .4, "var2|B_.5_.1_9" = .4,"afaf_2_3_4"=0)
  current_var_apl <- c("var1|A_.2_2_5"=.6, "var1|A_.2_2_9"=.6, "var2|B_.5_.4_.2" =.9, "var2|B_.5_.4_.2" =10)
  expected_output <- setNames(c(0.6,0.6,0.9,10,0), c("var1|A_.2_2_5","var1|A_.2_2_9","var2|B_.5_.4_.2","var2|B_.5_.4_.2","afaf_2_3_4"))
  result <- scope_for_dependent_variable(previous_var_apl, current_var_apl)
  expect_identical(result, expected_output)
})

test_that("Aggregation & carry forward - specific case", {
  previous_var_apl <- c("var3_.2_2_5"=.5, "B_.5_.4_.2" = .3,"d_1_2_4"=0)
  current_var_apl <- c("var3|B_2_4_9" =.9,"var3|B_2_4_2" =.9, "var3|B_2_4_2" =10, "var3|B_2_4_2" =11)
  expected_output <- setNames(c(0.9,0.9,10,11,0 ),c('var3|B_2_4_9', 'var3|B_2_4_2', 'var3|B_2_4_2', 'var3|B_2_4_2', 'd_1_2_4'))
  result <- scope_for_dependent_variable(previous_var_apl, current_var_apl)
  expect_identical(result, expected_output)
})

# Test Case 2: Aggregation & carry forward
test_that("Aggregation & carry forward", {
  previous_var_apl <- c("var3_.2_2_5"=.5, "B_.5_.4_.2" = .3,"var3_.2_9_5"=.78, "B_.9_.4_.2" = .33,  "d_1_2_4"=0)
  current_var_apl <- c("var3|B_2_4_9" =.9,"var3|B_2_4_2" =.9, "var3|B_2_4_2" =10, "var3|B_2_4_2" =11)
  expected_output <- setNames(c(0.9,0.9,10,11,0),c('var3|B_2_4_9', 'var3|B_2_4_2', 'var3|B_2_4_2', 'var3|B_2_4_2', 'd_1_2_4'))
  result <- scope_for_dependent_variable(previous_var_apl, current_var_apl)
  expect_identical(result, expected_output)
})

# Test Case 3: Segregation & carry forward
test_that("Segregation & carry forward", {
  previous_var_apl <- c("var1|c_.2_2_5"=.5,"var1|c_.2_2_1"=.5,  "d_1_2_4"=0, "d_1_2_1"=0)
  current_var_apl <- c("var1_2_4_2" =.9, "c_.5_.4_.2" =.9,"var1_2_4_2" =11, "c_.5_.4_.2" =.10,"c_.5_.4_.2" =.10, "c_.5_.4_9" =.10)
  expected_output <- setNames(c(0.9,0.9,11,0.1,0.1,0.1,0,0),c('var1_2_4_2', 'c_.5_.4_.2', 'var1_2_4_2', 'c_.5_.4_.2', 'c_.5_.4_.2', 'c_.5_.4_9', 'd_1_2_4', 'd_1_2_1'))
  result <- scope_for_dependent_variable(previous_var_apl, current_var_apl)
  expect_identical(result, expected_output)
})
