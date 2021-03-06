% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ErrorMeasures.R
\name{MdRAE}
\alias{MdRAE}
\title{MdRAE - Median Relative Absolute Error}
\usage{
MdRAE(y, y_hat, y_hat_bench)
}
\arguments{
\item{y}{\itemize{
\item True values of the time series
}}

\item{y_hat}{\itemize{
\item Forecasted values of the time series
}}

\item{y_hat_bench}{\itemize{
\item Forecasted values of the time series using the benchmark model
}}
}
\value{
MdRAE value
}
\description{
MdRAE = \eqn{median(r_t)}
}
\examples{
y <- AirPassengers
splitting_point <- round(2*length(y)/3)
y_train <- y[1:splitting_point]
h <- 5
y_test <- y[(splitting_point+1):(splitting_point+h)]
y_hat_naive <- rep(tail(y_train,1),h)
y_hat_average <-  rep(mean(y_train),h)
results <- MdRAE(y_test,y_hat_average,y_hat_naive)
}
