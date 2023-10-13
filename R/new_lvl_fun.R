#' Generation of mild overlap between classes
#'
#' @param new_lvl New discrete level.
#' @param value Value of relationship.
#' @param quantiles_vec Vector of quantiles for given relationship.
#'
#' @return New discrete level for discrete observation.
#'
new_lvl_fun <- function(new_lvl, value, quantiles_vec){
  n_lvls <- length(quantiles_vec)-1
  if (length(new_lvl)>1){
    new_lvl <- sample(new_lvl, 1)
  }
  if (new_lvl > 1 & (value-0.5) <= quantiles_vec[new_lvl]){
    aux <- sample(c(0,1), 1)
    if (aux){
      new_lvl <- new_lvl-1
    }
  }
  return(new_lvl)
}
