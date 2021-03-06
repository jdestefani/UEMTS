% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ErrorMeasures.R
\name{NNMSE}
\alias{NNMSE}
\title{NNMSE - Normalized Naive Mean Squared Error}
\usage{
NNMSE(y, y_hat, y_hat_naive, result_type = c("measure", "raw"))
}
\arguments{
\item{y}{\itemize{
\item True values of the time series
}}

\item{y_hat}{\itemize{
\item Forecasted values of the time series
}}

\item{y_hat_naive}{\itemize{
\item Forecasted values of the time series with the naive method
}}

\item{result_type}{\itemize{
\item String indicating whether the function should return the raw values ("raw"), or the computed measure ("measure")
}}
}
\value{
According to the value of \code{result_type}:
\itemize{
\item{\code{measure} -> }{NNMSE computed according to the formula}
\item{\code{raw} -> }{Vector containing one NNMSE for each time step}
}
}
\description{
NMSE = \eqn{1/n (\sum_{t=0}^{n} (y_t-y_hat_t)^2)/(\sum_{t=0}^{n} (y_t-y_hat_{Naive,t})^2)}
}
\examples{
y <- AirPassengers
splitting_point <- round(2*length(y)/3)
y_train <- y[1:splitting_point]
h <- 5

y_test <- y[(splitting_point+1):(splitting_point+h)]
y_hat_naive <- rep(tail(y_train,1),h)
y_hat_average <-  rep(mean(y_train),h)
results <- NNMSE(y_test,y_hat_average,y_hat_naive)
}
