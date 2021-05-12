################### Scale independant ########################

#' MAPE - Mean Absolute Percentage Error
#' 
#' MAPE = \eqn{1/n \sum_{t=0}^n  100  (y_t - y_hat_t)/y_t}
#' 
#' @references Hyndman, R. J., & Koehler, A. B. (2006). Another look at measures of forecast accuracy. International journal of forecasting, 22(4), 679-688. 
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#'
#' @return According to the value of \code{result_type}:
#'         \itemize{
#'         \item{\code{measure} -> }{MAPE computed according to the formula}
#'         \item{\code{raw} -> }{Vector containing one MAPE for each time step}
#'         }
#' 
#' @examples
#' y <- AirPassengers
#' splitting_point <- round(2*length(y)/3)
#' y_train <- y[1:splitting_point]
#' h <- 5
#' y_test <- y[(splitting_point+1):(splitting_point+h)]
#' y_hat_naive <- rep(tail(y_train,1),h)
#' result <- MAPE(y_test,y_hat_naive)
MAPE <- function(y,y_hat,result_type=c("measure","raw")){
  input_check_y_yhat(y,y_hat)
  if(missing(result_type)){
    results_type <- "measure"
  }
  else{
    input_check_results_type(result_type)
  }
  result_type <- match.arg(result_type)
  return(switch(result_type,
                measure = (1/length(y))*sum(abs(100*(y-y_hat)/y)),
                raw = (1/length(y))*abs(100*(y-y_hat)/y),
                MAPE(y,y_hat,"measure") # Default -> Recursive call with measure result type
                )
         )

}

#' MdAPE - Median Absolute Percentage Error
#' 
#' MdAPE = \eqn{median( 100  (y_t - y_hat_t)/(y_t))}
#'
#' @references Hyndman, R. J., & Koehler, A. B. (2006). Another look at measures of forecast accuracy. International journal of forecasting, 22(4), 679-688.  
#' 
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#'
#' @return MdAPE value
#' @examples
#' y <- AirPassengers
#' splitting_point <- round(2*length(y)/3)
#' y_train <- y[1:splitting_point]
#' h <- 5
#' y_test <- y[(splitting_point+1):(splitting_point+h)]
#' y_hat_naive <- rep(tail(y_train,1),h)
#' result <- MdAPE(y_test,y_hat_naive)
MdAPE <- function(y,y_hat){
  input_check_y_yhat(y,y_hat)
  return(median(100*(y-y_hat)/y))
}

#' RMSPE - Root Mean Squared Percentage Error
#' 
#' RMSPE = \eqn{\sqrt{1/n \sum_{t=0}^n (100  ((y_t - y_hat_t)/(y_t))^2}}
#' 
#' @references Hyndman, R. J., & Koehler, A. B. (2006). Another look at measures of forecast accuracy. International journal of forecasting, 22(4), 679-688.
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#'
#' @return According to the value of \code{result_type}:
#'         \itemize{
#'         \item{\code{measure} -> }{RMSPE computed according to the formula}
#'         \item{\code{raw} -> }{Vector containing one RMSPE for each time step}
#'         }
#' @examples
#' y <- AirPassengers
#' splitting_point <- round(2*length(y)/3)
#' y_train <- y[1:splitting_point]
#' h <- 5
#' y_test <- y[(splitting_point+1):(splitting_point+h)]
#' y_hat_naive <- rep(tail(y_train,1),h)
#' result <- RMSPE(y_test,y_hat_naive)
RMSPE <- function(y,y_hat,result_type=c("measure","raw")){
  input_check_y_yhat(y,y_hat)
  if(missing(result_type)){
    results_type <- "measure"
  }
  else{
    input_check_results_type(result_type)
  }
  result_type <- match.arg(result_type)
  return(switch(result_type,
                measure = sqrt((1/length(y))*sum(100*(((y-y_hat)/y)^2))),
                raw = sqrt((1/length(y))*100*(((y-y_hat)/y)^2)),
                RMSPE(y,y_hat,"measure") # Default -> Recursive call with measure result type
                )
        )

}

#' RMdSPE - Root Median Squared Percentage Error
#' 
#' RMdSPE = \eqn{\sqrt{ median(100  ((y_t - y_hat_t)/(y_t))^2)}}
#' 
#' @references Hyndman, R. J., & Koehler, A. B. (2006). Another look at measures of forecast accuracy. International journal of forecasting, 22(4), 679-688.
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#'
#' @return RMdSPE value
#' @examples
#' y <- AirPassengers
#' splitting_point <- round(2*length(y)/3)
#' y_train <- y[1:splitting_point]
#' h <- 5
#' y_test <- y[(splitting_point+1):(splitting_point+h)]
#' y_hat_naive <- rep(tail(y_train,1),h)
#' result <- RMdSPE(y_test,y_hat_naive)
RMdSPE <- function(y,y_hat){
  input_check_y_yhat(y,y_hat)
  return(sqrt(median(100*(((y-y_hat)/y)^2))))
}

#' sMAPE - Scaled Mean Absolute Percentage Error
#' 
#' sMAPE = \eqn{100/n \sum_{t=0}^n   (y_t - y_hat_t)/((y_t+y_hat_t)/(2))}
#' 
#' @references Hyndman, R. J., & Koehler, A. B. (2006). Another look at measures of forecast accuracy. International journal of forecasting, 22(4), 679-688.
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#'
#' @return According to the value of \code{result_type}:
#'         \itemize{
#'         \item{\code{measure} -> }{sMAPE computed according to the formula}
#'         \item{\code{raw} -> }{Vector containing one sMAPE for each time step}
#'         }
#' @examples
#' y <- AirPassengers
#' splitting_point <- round(2*length(y)/3)
#' y_train <- y[1:splitting_point]
#' h <- 5
#' y_test <- y[(splitting_point+1):(splitting_point+h)]
#' y_hat_naive <- rep(tail(y_train,1),h)
#' result <- sMAPE(y_test,y_hat_naive)
sMAPE <- function(y,y_hat,result_type=c("measure","raw")){
  input_check_y_yhat(y,y_hat)
  if(missing(result_type)){
    results_type <- "measure"
  }
  else{
    input_check_results_type(result_type)
  }
  result_type <- match.arg(result_type)
  return(switch(result_type,
                measure = (200/length(y))*sum((abs(y-y_hat)/(y+y_hat))),
                raw = (200/length(y))*(abs(y-y_hat)/(y+y_hat)),
                sMAPE(y,y_hat,"measure") # Default -> Recursive call with measure result type
                )
    )
}


#' sMdAPE - Scaled Median Absolute Percentage Error
#' 
#' sMdAPE = \eqn{median(200 (y_t - y_hat_t)/(y_t+y_hat_t))}
#' 
#' @references Hyndman, R. J., & Koehler, A. B. (2006). Another look at measures of forecast accuracy. International journal of forecasting, 22(4), 679-688.
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#'
#' @return sMdAPE value
#' @examples
#' y <- AirPassengers
#' splitting_point <- round(2*length(y)/3)
#' y_train <- y[1:splitting_point]
#' h <- 5
#' y_test <- y[(splitting_point+1):(splitting_point+h)]
#' y_hat_naive <- rep(tail(y_train,1),h)
#' result <- sMdAPE(y_test,y_hat_naive)
sMdAPE <- function(y,y_hat){
  input_check_y_yhat(y,y_hat)
  return(median(200*((abs(y-y_hat)/(y+y_hat)))))
}

################### Scale dependant ########################

#' MSE - Mean Squared Error
#' \textbf{MSE} : $ \frac{1}{n} \sum_{t=0}^n (y_t - \hat{y}_t)^2$
#' @references Hyndman, R. J., & Koehler, A. B. (2006). Another look at measures of forecast accuracy. International journal of forecasting, 22(4), 679-688.  
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#'
#' @return According to the value of \code{result_type}:
#'         \itemize{
#'         \item{\code{measure} -> }{MSE computed according to the formula}
#'         \item{\code{raw} -> }{Vector containing one MSE for each time step}
#'         }
#'         
#' @examples
#' y <- AirPassengers
#' splitting_point <- round(2*length(y)/3)
#' y_train <- y[1:splitting_point]
#' h <- 5
#' y_test <- y[(splitting_point+1):(splitting_point+h)]
#' y_hat_naive <- rep(tail(y_train,1),h)
#' result <- MSE(y_test,y_hat_naive)
MSE <- function(y,y_hat,result_type=c("measure","raw")){
  input_check_y_yhat(y,y_hat)
  if(missing(result_type)){
    results_type <- "measure"
  }
  else{
    input_check_results_type(result_type)
  }
  result_type <- match.arg(result_type)
  return(switch(result_type,
                measure = (1/length(y))*sum((y-y_hat)^2),
                raw = (1/length(y))*(y-y_hat)^2,
                MSE(y,y_hat,"measure") # Default -> Recursive call with measure result type
                )
  )
}

#' RMSE - Root Mean Squared Error
#' \textbf{RMSE} : $\sqrt{ \frac{1}{n} \sum_{t=0}^n (y_t - \hat{y}_t)^2}$
#' @references Hyndman, R. J., & Koehler, A. B. (2006). Another look at measures of forecast accuracy. International journal of forecasting, 22(4), 679-688.
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#'
#' @return MSE value
#' @examples
#' y <- AirPassengers
#' splitting_point <- round(2*length(y)/3)
#' y_train <- y[1:splitting_point]
#' h <- 5
#' y_test <- y[(splitting_point+1):(splitting_point+h)]
#' y_hat_naive <- rep(tail(y_train,1),h)
#' result <- RMSE(y_test,y_hat_naive)
RMSE <- function(y,y_hat,result_type=c("measure","raw")){
  result_type <- match.arg(result_type)
  return(sqrt(MSE(y,y_hat,result_type)))
}

#' MAE - Mean Absolute Error
#' \textbf{MAE} : $\frac{1}{n} \sum_{t=0}^n |y_t - \hat{y}_t|$
#' @references Hyndman, R. J., & Koehler, A. B. (2006). Another look at measures of forecast accuracy. International journal of forecasting, 22(4), 679-688. 
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#'
#' @return MAE value
#' @examples
#' y <- AirPassengers
#' splitting_point <- round(2*length(y)/3)
#' y_train <- y[1:splitting_point]
#' h <- 5
#' y_test <- y[(splitting_point+1):(splitting_point+h)]
#' y_hat_naive <- rep(tail(y_train,1),h)
#' result <- MAE(y_test,y_hat_naive)
MAE <- function(y,y_hat,result_type=c("measure","raw")){
  input_check_y_yhat(y,y_hat)
  if(missing(result_type)){
    results_type <- "measure"
  }
  else{
    input_check_results_type(result_type)
  }
  result_type <- match.arg(result_type)
  return(switch(result_type,
                measure = (1/length(y))*sum(abs(y-y_hat)),
                raw = (1/length(y))*abs(y-y_hat),
                MAE(y,y_hat,"measure") # Default -> Recursive call with measure result type
               )
  )
}

#' MdAE - Median Absolute Error
#' \textbf{MdAE} : median(|y_t - \hat{y}_t|)$
#' @references Hyndman, R. J., & Koehler, A. B. (2006). Another look at measures of forecast accuracy. International journal of forecasting, 22(4), 679-688.  
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#'
#' @return MdAE value
#' @examples
#' y <- AirPassengers
#' splitting_point <- round(2*length(y)/3)
#' y_train <- y[1:splitting_point]
#' h <- 5
#' y_test <- y[(splitting_point+1):(splitting_point+h)]
#' y_hat_naive <- rep(tail(y_train,1),h)
#' result <- MdAE(y_test,y_hat_naive)
MdAE <- function(y,y_hat){
  input_check_y_yhat(y,y_hat)
  return(median(abs(y-y_hat)))
}


################### Relative Errors ########################

#' RE - Relative Error
#' \textbf{RE} : $\frac{\mathbf{y}-\hat{\mathbf{y}}}{\mathbf{y}-\hat{\mathbf{y}_b}}$
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#' @param y_hat_bench - Forecasted values of the time series using the benchmark model
#'
#' @return RE value
#' @examples 
#' y <- AirPassengers
#' splitting_point <- round(2*length(y)/3)
#' y_train <- y[1:splitting_point]
#' h <- 5

#' y_test <- y[(splitting_point+1):(splitting_point+h)]
#' y_hat_naive <- rep(tail(y_train,1),h)
#' y_hat_average <-  rep(mean(y_train),h)
#' results <- RE(y_test,y_hat_average,y_hat_naive)
RE <- function(y,y_hat,y_hat_bench){
  input_check_y_yhat_ybench(y,y_hat,y_hat_bench)
  return((y-y_hat) / (y-y_hat_bench))
}

#' MRAE - Mean Relative Absolute Error
#' \textbf{MRAE} : $\frac{1}{n} \sum_{t=0}^n  \mid r_t \mid$
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#' @param y_hat_bench - Forecasted values of the time series using the benchmark model
#'
#' @return MRAE value
#' @examples 
#' y <- AirPassengers
#' splitting_point <- round(2*length(y)/3)
#' y_train <- y[1:splitting_point]
#' h <- 5

#' y_test <- y[(splitting_point+1):(splitting_point+h)]
#' y_hat_naive <- rep(tail(y_train,1),h)
#' y_hat_average <-  rep(mean(y_train),h)
#' results <- MRAE(y_test,y_hat_average,y_hat_naive)
MRAE <- function(y,y_hat,y_hat_bench,result_type=c("measure","raw")){
  input_check_y_yhat_ybench(y,y_hat,y_hat_bench)
  if(missing(result_type)){
    results_type <- "measure"
  }
  else{
    input_check_results_type(result_type)
  }
  result_type <- match.arg(result_type)
  return(switch(result_type,
                measure = (1/length(y))*sum(abs(RE(y,y_hat,y_hat_bench))),
                raw = (1/length(y))*abs(RE(y,y_hat,y_hat_bench)),
                MRAE(y,y_hat,"measure") # Default -> Recursive call with measure result type
               )
  )
}


#' MdRAE - Median Relative Absolute Error
#' \textbf{MdRAE} : median(\mid r_t \mid)$
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#' @param y_hat_bench - Forecasted values of the time series using the benchmark model
#'
#' @return MdRAE value
#' @examples 
#' y <- AirPassengers
#' splitting_point <- round(2*length(y)/3)
#' y_train <- y[1:splitting_point]
#' h <- 5

#' y_test <- y[(splitting_point+1):(splitting_point+h)]
#' y_hat_naive <- rep(tail(y_train,1),h)
#' y_hat_average <-  rep(mean(y_train),h)
#' results <- MdRAE(y_test,y_hat_average,y_hat_naive)
MdRAE <- function(y,y_hat,y_hat_bench){
  input_check_y_yhat_ybench(y,y_hat,y_hat_bench)
  return(median(abs(RE(y,y_hat,y_hat_bench))))
}


#' GMRAE - Geometric Mean Relative Absolute Error
#' \textbf{GMRAE} : $\sqrt[n]{\frac{1}{n} \prod{t=0}^n  \mid r_t \mid}$
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#' @param y_hat_bench - Forecasted values of the time series using the benchmark model
#'
#' @return RE value
#' @examples 
#' y <- AirPassengers
#' splitting_point <- round(2*length(y)/3)
#' y_train <- y[1:splitting_point]
#' h <- 5

#' y_test <- y[(splitting_point+1):(splitting_point+h)]
#' y_hat_naive <- rep(tail(y_train,1),h)
#' y_hat_average <-  rep(mean(y_train),h)
#' results <- GMRAE(y_test,y_hat_average,y_hat_naive)
GMRAE <- function(y,y_hat,y_hat_bench,result_type=c("measure","raw")){
  input_check_y_yhat_ybench(y,y_hat,y_hat_bench)
  if(missing(result_type)){
    results_type <- "measure"
  }
  else{
    input_check_results_type(result_type)
  }
  result_type <- match.arg(result_type)
  return(switch(result_type,
                measure = ((1/length(y))*prod(abs(RE(y,y_hat,y_hat_bench))))^(1/length(y)),
                raw = ((1/length(y))*abs(RE(y,y_hat,y_hat_bench)))^(1/length(y)),
                MRAE(y,y_hat,"measure") # Default -> Recursive call with measure result type
               )
  )
}

################### Normalized Errors ########################

#' MASE - Mean Absolute Scaled Error
#' \textbf{MASE} : $\frac{1}{T}\sum_{t=1}^T \left( \frac{\left| e_t \right|}{\frac{1}{T-1}\sum_{i=2}^T \left| Y_i-Y_{i-1} \right|}\right)$
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#'
#' @return MASE value
#' @examples 
#' y <- AirPassengers
#' splitting_point <- round(2*length(y)/3)
#' y_train <- y[1:splitting_point]
#' h <- 5

#' y_test <- y[(splitting_point+1):(splitting_point+h)]
#' y_hat_naive <- rep(tail(y_train,1),h)
#' results <- MASE(y_test,y_hat_naive)
MASE <- function(y,y_hat,result_type=c("measure","raw")){
  input_check_y_yhat(y,y_hat)
  if(missing(result_type)){
    results_type <- "measure"
  }
  else{
    input_check_results_type(result_type)
  }
  
  if( length(y) == 1 || length(y_hat) == 1 ){
    stop("NMSE undefined for horizon 1")
  }
  
  result_type <- match.arg(result_type)
  return(switch(result_type,
         measure = ((length(y)-1)/length(y))*(1/sum(abs(diff(y))))*sum(abs(y-y_hat)),
         raw = ((length(y)-1)/length(y))*(1/sum(abs(diff(y))))*abs(y-y_hat),
         MASE(y,y_hat,"measure") # Default -> Recursive call with result_type measure
         )
  )
}


#' NMSE - Normalized Mean Squared Error
#' \textbf{NMSE:} $\frac{1}{n}\frac{\sum_{t=0}^{n} (y_t-\hat{y}_t)^2}{var(y_t)} $
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#' @param normalizing_variance - Variance of the training set
#'
#' @return NMSE value
#' @examples 
#' y <- AirPassengers
#' splitting_point <- round(2*length(y)/3)
#' y_train <- y[1:splitting_point]
#' h <- 5

#' y_test <- y[(splitting_point+1):(splitting_point+h)]
#' y_hat_naive <- rep(tail(y_train,1),h)
#' results <- NMSE(y_test,y_hat_naive)
NMSE <- function(y,y_hat,normalizing_variance=NULL,result_type=c("measure","raw")){
  if(missing(result_type)){
    results_type <- "measure"
  }
  else{
    input_check_results_type(result_type)
  }
  
  if( length(y) == 1 || length(y_hat) == 1 ){
    stop("NMSE undefined for horizon 1")
  }
  
  if(is.null(normalizing_variance)){
    normalizing_variance <- var(y)
  }
  
  result_type <- match.arg(result_type)
  return(MSE(y,y_hat,result_type)/normalizing_variance)
}

#' NNMSE - Normalized Naive Mean Squared Error
#' \textbf{NMSE:} $\frac{1}{n}\frac{\sum_{t=0}^{n} (y_t-\hat{y}_t)^2}{\sum_{t=0}^{n} (y_t-\hat{y}_{Naive,t})^2} $
#' @param y - True values of the time series
#' @param y_hat - Forecasted values of the time series
#' @param y_hat_naive - Forecasted values of the time series with the naive method
#'
#' @return NNMSE value
#' @examples 
#' y <- AirPassengers
#' splitting_point <- round(2*length(y)/3)
#' y_train <- y[1:splitting_point]
#' h <- 5

#' y_test <- y[(splitting_point+1):(splitting_point+h)]
#' y_hat_naive <- rep(tail(y_train,1),h)
#' y_hat_average <-  rep(mean(y_train),h)
#' results <- NNMSE(y_test,y_hat_average,y_hat_naive)
NNMSE <- function(y,y_hat,y_hat_naive,result_type=c("measure","raw")){
  input_check_y_yhat_ybench(y,y_hat,y_hat_naive)
  if(missing(result_type)){
    results_type <- "measure"
  }
  else{
    input_check_results_type(result_type)
  }
  
  result_type <- match.arg(result_type)
  return(switch(result_type,
                measure = ,
                raw = MSE(y,y_hat,result_type)/MSE(y,y_hat_naive,result_type),
                NNMSE(y,y_hat,y_hat_naive,"measure") # Default -> Recursive call with result_type measure
                )
         )
}
