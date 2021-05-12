# Constant string literals
error_type_string <- function(variable){return(paste("[ERROR] - Parameter", variable, "is not a numeric"))}
error_dimension_string <- function(variable){return(paste("[ERROR] - Parameter", variable, "has wrong dimension"))}
error_dimension_mismatch_string <- "[ERROR] - Dimension mismatch between y and y_hat"
error_dimension_mismatch_y_benchmark_string <- "[ERROR] - Dimension mismatch between y and y_benchmark"
error_dimension_mismatch_y_hat_benchmark_string <- "[ERROR] - Dimension mismatch between y_hat and y_benchmark"
error_multiple_results_type_string <- "[ERROR] - Only one 'result_type' allowed"
error_type_results_type_string <- "[ERROR] - Parameter result_type is not a string"


#' check_variable
#' 
#' Helper function to avoid code repetition to check type and length of inputs
#'
#' @param variable - Variable to check
#' @param variable_name - Variable name to be used for the error message
#' 
#' @return An error in case of length or type error, nothing otherwise
check_variable <- function(variable,variable_name) {
  if(!is.numeric(variable)){
    stop(error_type_string(variable_name))
  }
  
  if(length(variable) < 1){
    stop(error_dimension_string(variable_name))
  }
}

#' check_equal_length_pair
#'
#' @param y - numeric vector
#' @param y_hat - numeric vector
#'
#' @return An error in case of length mismatch, nothing otherwise
check_equal_length_pair <- function(y,y_hat){
  if(length(y) != length(y_hat)){
    stop(error_dimension_mismatch_string)
  }
}

#' check_equal_length_trio
#'
#' @param y - numeric vector
#' @param y_hat - numeric vector
#' @param y_hat_bench - numeric vector
#'
#' @return An error in case of any length mismatch among the three vectors, nothing otherwise
check_equal_length_trio <- function(y,y_hat,y_hat_bench){
  if(length(y) != length(y_hat)){
    stop(error_dimension_mismatch_string)
  }
  if(length(y) != length(y_hat_bench)){
    stop(error_dimension_mismatch_y_benchmark_string)
  }
  if(length(y_hat) != length(y_hat_bench)){
    stop(error_dimension_mismatch_y_hat_benchmark_string)
  }
}

#' input_check_y_yhat
#'
#' @param y - Value of y to check - numeric vector
#' @param y_hat - Value of y_hat to check - numeric vector
#' 
#' @return An error in case of length,type error or dimension mismatch, nothing otherwise
#' 
input_check_y_yhat <- function(y,y_hat){
  check_variable(y,"y")
  check_variable(y_hat,"y_hat")
  check_equal_length_pair(y,y_hat)
}

#' input_check_y_yhat_ybench
#'
#' @param y - Value of y to check - numeric vector
#' @param y_hat - Value of y_hat to check - numeric vector
#' @param y_hat_bench - Value of y_hat_bench to check - numeric vector
#' 
#' @return An error in case of length,type error or dimension mismatch, nothing otherwise
input_check_y_yhat_ybench <- function(y,y_hat,y_hat_bench){
  check_variable(y,"y")
  check_variable(y_hat,"y_hat")
  check_variable(y_hat_bench,"y_hat_bench")
  check_equal_length_trio(y,y_hat,y_hat_bench)
}

#' input_check_results_type
#'
#' @param result_type - Value of results_type to check - string
#'
#' @return An error in case of length or type error, nothing otherwise
input_check_results_type <- function(result_type) {
  if(length(result_type)>1){
    stop(error_multiple_results_type_string)
  }
  
  if(!is.character(result_type)){
    stop(error_type_results_type_string)
  }
}



