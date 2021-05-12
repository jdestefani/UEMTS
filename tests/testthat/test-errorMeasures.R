context("Error measures - Functional tests")

y <- AirPassengers
splitting_point <- round(2*length(y)/3)
y_train <- y[1:splitting_point]
h <- 5

y_test <- y[(splitting_point+1):(splitting_point+h)]
y_hat_naive <- rep(tail(y_train,1),h)
y_hat_average <-  rep(mean(y_train),h)

expected_scalar_dimension <- 1

################### Scale independant ########################

test_that("MAPE - measure", {
  result <- MAPE(y_test,y_hat_naive)
  expect_true(is.numeric(result))
  expect_equal(length(result),expected_scalar_dimension)
})

test_that("MAPE - raw", {
  result <- MAE(y_test,y_hat_naive,result_type = "raw")
  expect_true(is.numeric(result))
  expect_equal(length(result),h)
})

test_that("RMSPE - measure", {
  result <- RMSPE(y_test,y_hat_naive)
  expect_true(is.numeric(result))
  expect_equal(length(result),expected_scalar_dimension)
})

test_that("RMSPE - raw", {
  result <- RMSPE(y_test,y_hat_naive,result_type = "raw")
  expect_true(is.numeric(result))
  expect_equal(length(result),h)
})

test_that("sMAPE - measure", {
  result <- sMAPE(y_test,y_hat_naive)
  expect_true(is.numeric(result))
  expect_equal(length(result),expected_scalar_dimension)
})

test_that("sMAPE - raw", {
  result <- sMAPE(y_test,y_hat_naive,result_type = "raw")
  expect_true(is.numeric(result))
  expect_equal(length(result),h)
})

test_that("MdAPE", {
  result <- MdAPE(y_test,y_hat_naive)
  expect_true(is.numeric(result))
  expect_equal(length(result),expected_scalar_dimension)
})

test_that("RMdSPE", {
  result <- RMdSPE(y_test,y_hat_naive)
  expect_true(is.numeric(result))
  expect_equal(length(result),expected_scalar_dimension)
})

test_that("sMdAPE", {
  result <- sMdAPE(y_test,y_hat_naive)
  expect_true(is.numeric(result))
  expect_equal(length(result),expected_scalar_dimension)
})

################### Scale dependant ########################

test_that("MSE - measure", {
  result <- MSE(y_test,y_hat_naive)
  expect_true(is.numeric(result))
  expect_equal(length(result),expected_scalar_dimension)
})

test_that("MSE - raw", {
  result <- MSE(y_test,y_hat_naive,result_type = "raw")
  expect_true(is.numeric(result))
  expect_equal(length(result),h)
})

test_that("RMSE - measure", {
  result <- RMSE(y_test,y_hat_naive, result_type = "measure")
  expect_true(is.numeric(result))
  expect_equal(length(result),expected_scalar_dimension)
})

test_that("RMSE - raw", {
  result <- RMSE(y_test,y_hat_naive,result_type = "raw")
  expect_true(is.numeric(result))
  expect_equal(length(result),h)
})

test_that("MAE - measure", {
  result <- MAE(y_test,y_hat_naive)
  expect_true(is.numeric(result))
  expect_equal(length(result),expected_scalar_dimension)
})

test_that("MAE - raw", {
  result <- MAE(y_test,y_hat_naive,result_type = "raw")
  expect_true(is.numeric(result))
  expect_equal(length(result),h)
})

test_that("MdAE", {
  result <- MdAE(y_test,y_hat_naive)
  expect_true(is.numeric(result))
  expect_equal(length(result),expected_scalar_dimension)
})


################### Relative Errors ########################

test_that("RE - measure", {
  result <- RE(y_test,y_hat_average,y_hat_naive)
  expect_true(is.numeric(result))
  expect_equal(length(result),h)
})

test_that("MRAE - measure", {
  result <- MRAE(y_test,y_hat_average,y_hat_naive)
  expect_true(is.numeric(result))
  expect_equal(length(result),expected_scalar_dimension)
})

test_that("MRAE - raw", {
  result <- MRAE(y_test,y_hat_average,y_hat_naive,result_type = "raw")
  expect_true(is.numeric(result))
  expect_equal(length(result),h)
})

test_that("GMRAE - measure", {
  result <- GMRAE(y_test,y_hat_average,y_hat_naive)
  expect_true(is.numeric(result))
  expect_equal(length(result),expected_scalar_dimension)
})

test_that("GMRAE - raw", {
  result <- GMRAE(y_test,y_hat_average,y_hat_naive,result_type = "raw")
  expect_true(is.numeric(result))
  expect_equal(length(result),h)
})

test_that("MdRAE", {
  result <- MdRAE(y_test,y_hat_average,y_hat_naive)
  expect_true(is.numeric(result))
  expect_equal(length(result),expected_scalar_dimension)
})


################### Normalized Errors ########################

test_that("MASE - measure", {
  result <- MASE(y_test,y_hat_naive)
  expect_true(is.numeric(result))
  expect_equal(length(result),expected_scalar_dimension)
})

test_that("MASE - raw", {
  result <- MASE(y_test,y_hat_naive,result_type = "raw")
  expect_true(is.numeric(result))
  expect_equal(length(result),h)
})

test_that("NMSE - measure", {
  result <- NMSE(y_test,y_hat_naive)
  expect_true(is.numeric(result))
  expect_equal(length(result),expected_scalar_dimension)
})

test_that("NMSE - raw", {
  result <- NMSE(y_test,y_hat_naive,result_type = "raw")
  expect_true(is.numeric(result))
  expect_equal(length(result),h)
})

test_that("NNMSE - measure", {
  result <- NNMSE(y_test,y_hat_average,y_hat_naive)
  expect_true(is.numeric(result))
  expect_equal(length(result),expected_scalar_dimension)
})

test_that("NNMSE - raw", {
  result <- NNMSE(y_test,y_hat_average,y_hat_naive,result_type = "raw")
  expect_true(is.numeric(result))
  expect_equal(length(result),h)
})



