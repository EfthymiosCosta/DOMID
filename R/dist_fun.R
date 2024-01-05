#' Weighted Minkowski distance matrix function
#'
#' @param x Observations
#' @param coeffs Weights
#' @param degree Minkowski order
#'
#' @return Weighted Minkowski distance matrix
#'
#' @noRd
dist_fun <- function(x, coeffs, degree){
  mat <- as.matrix(x, ncol=ncol(x))
  dist_mat <- matrix(NA, nrow=nrow(x), ncol=nrow(x))
  for (i in 1:nrow(x)){
    for (j in 1:i){
      dist_mat[i,j] <- distance_helper_fun(x = mat[i,], y = mat[j,],
                                           w = coeffs, deg = degree)
      dist_mat[j,i] <- dist_mat[i,j]
    }
  }
  return(dist_mat)
}
