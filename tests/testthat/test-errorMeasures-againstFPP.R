library(fpp)
library(forecast)

#' From https://otexts.com/fpp2/accuracy.html
beer_train <- window(ausbeer,start=1992,end=c(2007,4))
naive_mean_model <- meanf(beer_train,h=10)
naive_model <- rwf(beer_train,h=10)
seasonal_naive_model <- snaive(beer_train,h=10)

beer_test <- window(ausbeer, start=2008)
y <- beer_test
naive_mean_results <- accuracy(naive_mean_model, beer_test)
naive_results <- accuracy(naive_model, beer_test)
seasonal_naive_results <- accuracy(seasonal_naive_model, beer_test)

y_hat_naive_mean <- head(forecast(naive_mean_model,y)$mean,3)
y_hat_naive <- head(forecast(naive_model,y)$mean,3)
y_hat_seasonal_naive <- head(forecast(seasonal_naive_model,y)$mean,3)

test_that("RMSE - Compliance with fpp implementation", {
  expect_equal(RMSE(y,y_hat_naive_mean,result_type="measure"),naive_mean_results[2,"RMSE"])
  expect_equal(RMSE(y,y_hat_naive,result_type="measure"),naive_results[2,"RMSE"])
  expect_equal(RMSE(y,y_hat_seasonal_naive,result_type="measure"),seasonal_naive_results[2,"RMSE"])
})

test_that("MAE - Compliance with fpp implementation", {
  expect_equal(MAE(y,y_hat_naive_mean,result_type="measure"),naive_mean_results[2,"MAE"])
  expect_equal(MAE(y,y_hat_naive,result_type="measure"),naive_results[2,"MAE"])
  expect_equal(MAE(y,y_hat_seasonal_naive,result_type="measure"),seasonal_naive_results[2,"MAE"])
})

test_that("MAPE - Compliance with fpp implementation", {
  expect_equal(MAPE(y,y_hat_naive_mean,result_type="measure"),naive_mean_results[2,"MAPE"])
  expect_equal(MAPE(y,y_hat_naive,result_type="measure"),naive_results[2,"MAPE"])
  expect_equal(MAPE(y,y_hat_seasonal_naive,result_type="measure"),seasonal_naive_results[2,"MAPE"])
})

# From forecast documentation:
#
# By default, the MASE calculation is scaled using MAE of training set naive forecasts for non-seasonal time series, 
# training set seasonal naive forecasts for seasonal time series and training set mean forecasts for non-time series data. 
# If f is a numerical vector rather than a forecast object, the MASE will not be returned as the training data will not be available
# 
# test_that("MASE - Compliance with fpp implementation", {
#   expect_equal(MASE(y,y_hat_naive_mean,result_type="measure"),naive_mean_results[2,"MASE"])
#   expect_equal(MASE(y,y_hat_naive,result_type="measure"),naive_results[2,"MASE"])
#   expect_equal(MASE(y,y_hat_seasonal_naive,result_type="measure"),seasonal_naive_results[2,"MASE"])
# })
