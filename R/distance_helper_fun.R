#' Weighted Minkowski distance between 2 observations
#'
#' @param x Point (observation) 1
#' @param y Point (observation) 2
#' @param w Weights vector
#' @param deg Minkowski order
#'
#' @return Weighted Minkowski distance between x and y
#' @noRd
distance_helper_fun <- function(x, y, w, deg){
  sum(w*abs(x-y)^deg)^(1/deg)
}
