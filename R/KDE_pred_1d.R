#' Predictions for 1-dimensional Kernel Density Estimation (w/ Gaussian kernel only)
#'
#' @param t New point to predict
#' @param xs Vector of points on which the density was evaluated
#' @param h Bandwidth for KDE
#'
#' @return Prediction for 1D Gaussian KDE
#' @noRd
KDE_pred_1d <- function(t, xs, h){
  kernelValues <- rep(0,length(xs))
  for(i in 1:length(xs)){
    transformed = (t - xs[i]) / h
    kernelValues[i] <- stats::dnorm(transformed, mean = 0, sd = 1) / h
  }
  return(sum(kernelValues) / length(xs))
}
