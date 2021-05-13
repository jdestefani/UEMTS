context("Input verification")

y <- AirPassengers
y_hat <- y + 1
y_bench <- y_hat + 1
expected_length_scalar <- 1

test_that("input_check_y_yhat - y not numeric", {
  y <- "1" 
  expect_error(input_check_y_yhat(y,y_hat),
               error_type_string("y"),
               fixed=T)
})

test_that("input_check_y_yhat - y_hat not numeric", {
  y_hat <- "1" 
  expect_error(input_check_y_yhat(y,y_hat),
               error_type_string("y_hat"),
               fixed=T)
})

test_that("input_check_y_yhat - y empty numeric", {
  y <- numeric() 
  expect_error(input_check_y_yhat(y,y_hat),
               error_dimension_string("y"),
               fixed=T)
})

test_that("input_check_y_yhat - y_hat empty numeric", {
  y_hat <- numeric() 
  expect_error(input_check_y_yhat(y,y_hat),
               error_dimension_string("y_hat"),
               fixed=T)
})

test_that("input_check_y_yhat - Dimension mismatch", {
  expect_error(input_check_y_yhat(c(y,1),y_hat),
               error_dimension_mismatch_string,
               fixed=T)
})

test_that("input_check_y_yhat_ybench - y not numeric", {
  y <- "1" 
  expect_error(input_check_y_yhat_ybench(y,y_hat,y_bench),
               error_type_string("y"),
               fixed=T)
})

test_that("input_check_y_yhat_ybench - y_hat not numeric", {
  y_hat <- "1" 
  expect_error(input_check_y_yhat_ybench(y,y_hat,y_bench),
               error_type_string("y_hat"),
               fixed=T)
})

test_that("input_check_y_yhat_ybench - y_bench not numeric", {
  y_bench <- "1" 
  expect_error(input_check_y_yhat_ybench(y,y_hat,y_bench),
               error_type_string("y_hat_bench"),
               fixed=T)
})

test_that("input_check_y_yhat_ybench - y empty numeric", {
  y <- numeric() 
  expect_error(input_check_y_yhat_ybench(y,y_hat,y_bench),
               error_dimension_string("y"),
               fixed=T)
})

test_that("input_check_y_yhat_ybench - y_hat empty numeric", {
  y_hat <- numeric() 
  expect_error(input_check_y_yhat_ybench(y,y_hat,y_bench),
               error_dimension_string("y_hat"),
               fixed=T)
})

test_that("input_check_y_yhat_ybench - y_bench empty numeric", {
  y_bench <- numeric()
  expect_error(input_check_y_yhat_ybench(y,y_hat,y_bench),
               error_dimension_string("y_hat_bench"),
               fixed=T)
})

test_that("input_check_y_yhat - Dimension mismatch y, y_hat", {
  expect_error(input_check_y_yhat_ybench(c(y,1),y_hat,y_bench),
               error_dimension_mismatch_string,
               fixed=T)
})

test_that("input_check_y_yhat - Dimension mismatch y, y_hat", {
  expect_error(input_check_y_yhat_ybench(c(y,1),c(y_hat,1),y_bench),
               error_dimension_mismatch_y_benchmark_string,
               fixed=T)
})

test_that("input_check_y_yhat - Dimension mismatch y, y_hat_bench", {
  expect_error(input_check_y_yhat_ybench(y,y_hat,c(y_bench,1)),
               error_dimension_mismatch_y_benchmark_string,
               fixed=T)
})


test_that("input_check_results_type - Dimension mismatch", {
  expect_error(input_check_results_type(c("measure","raw")),
               error_multiple_results_type_string,
               fixed=T)
})

test_that("input_check_results_type - Dimension mismatch", {
  expect_error(input_check_results_type(1),
               error_type_results_type_string,
               fixed=T)
})