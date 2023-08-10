#' Quantile discretisation of continuous vector
#'
#' @param vec Vector to be discretised
#' @param class Number of levels
#'
#' @return Discretised vector (using quantile discretisation)
#'
#' @examples intv(vec = c(1:100), class = 5)
intv <- function(vec, class){
  nbase <- (1:(class-1))/class
  nq <- numeric(length(nbase))
  for (i in 1:length(nq)) {
    nq[i] <- quantile(vec, nbase[i])
  }
  res <- c(min(vec), nq, max(vec))
  res[1] <- res[1]-1
  for (i in 2:length(res)){
    if (res[i-1]==res[i]){
      res[i] <- res[i]+2e-15
    }
  }
  return(res)
}
