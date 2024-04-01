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
