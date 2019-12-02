library(testthat)

source("halfspacemass-sol-train.R")
source("halfspacemass-sol-evaluate.R")

context("Testing train_depth.R")

test_that("Checks for wrong args", {
  data <- c("bla bla", "or so")
  expect_error(train_depth(data, 500), regexp = "One of")
  data <- matrix(runif(300, min = -20, max = 20), ncol = 3)
  expect_error(train_depth(data, n_halfspace = "500"), regexp = "number")
  expect_error(train_depth(data, 500, subsample = "500"), regexp = "number")
  expect_error(train_depth(data, 500, scope = "500"), regexp = "number")
  expect_error(train_depth(data, 500, seed = "500"), regexp = "number")
})


context("Testing evaluate_depth.R")

test_that("Checks for wrong args", {
  data1 <- matrix(runif(300, min = -20, max = 20), ncol = 3)
  data2 <- c("bla bla", "or so")
  halfspaces <- train_depth(data1, 500)
  expect_error(evaluate_depth(data2, halfspaces), regexp = "One of")
  expect_error(evaluate_depth(data1, "halfspaces"), regexp = "from class 'halfspaces'")
})

test_that("Same result for data given in data.frame and matrix", {
  set.seed(123456789)
  data <- matrix(runif(300, min = -20, max = 20), ncol = 3)
  halfspaces <- train_depth(data, 500)
  depth_matrix <- evaluate_depth(data, halfspaces)
  data <- data.frame(data)
  halfspaces <- train_depth(data, 500)
  depth_df <- evaluate_depth(data, halfspaces)
  expect_equal(depth_df, depth_matrix)
})
