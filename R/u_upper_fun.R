#' Function to determine the upper value of Theil's U, u_upper
#'
#' @param x1 Vector 1, consisting of factors
#' @param x2 Vector 2, also consisting of factors
#'
#' @return Upper value of Theil's U, u_upper
#'
#' @examples u_upper_fun(as.factor(c(1,1,2)), as.factor(c(0,1,2)))
u_upper_fun <- function(x1, x2){
  n1 <- length(levels(x1))
  n2 <- length(levels(x2))
  n <- length(x1)
  sigma_mat <- matrix(c(1, 0.35,
                        0.35, 1), nrow=2)
  dt <- mvtnorm::rmvnorm(n, mean=rep(0, 2), sigma=sigma_mat)
  dt[,1] <- as.factor(cut(dt[,1], intv(dt[,1], n1), labels = (1:n1)))
  dt[,2] <- as.factor(cut(dt[,2], intv(dt[,2], n2), labels = (1:n2)))
  u_val <- max(DescTools::UncertCoef(dt[,1], dt[,2], direction='column'),
               DescTools::UncertCoef(dt[,2], dt[,1], direction='column'))
  return(u_val)
}
