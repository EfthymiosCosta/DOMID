#' Generate joint outliers based on a given association
#'
#' @param data Data frame on which joint outliers will be introduced.
#' @param level Level of discrete features.
#' @param value Value of relationship.
#' @param quantiles_vec Vector of quantiles for given relationship.
#' @param out_label Index of label column, indicating which observation is outlying.
#'
#' @return New level, not agreeing to specified pattern,
#'
joint_outs_lvls <- function(data, level, value, quantiles_vec, out_label, target){
  n_lvls <- length(quantiles_vec) - 1
  if (n_lvls == 2){
    if (level == 1 & (value+1) <= quantiles_vec[2]){
      new_lvl <- 2
    } else if (level == 2 & (value-1) >= quantiles_vec[2]){
      new_lvl <- 1
    } else {
      new_lvl <- 0
    }
  } else {
    if (level == 1 & (value+0.5) >= quantiles_vec[2]){
      new_lvl <- sample(setdiff(unique(data[which(data[, out_label]==0), target]), c(level, 2)), size=1)
    } else if (level == n_lvls & (value-0.25) <= quantiles_vec[n_lvls-1]){
      new_lvl <- sample(setdiff(unique(data[which(data[, out_label]==0), target]), c(level, (n_lvls-1))), size=1)
    } else if (level != 1 & level != n_lvls){
      if ((value+0.25) >= quantiles_vec[level+1]){
        new_lvl <- sample(setdiff(unique(data[which(data[, out_label]==0), target]), c(level, (level+1))), size=1)
      } else if ((value-0.5) <= quantiles_vec[level-1]){
        new_lvl <- sample(setdiff(unique(data[which(data[, out_label]==0), target]), c(level, (level-1))), size=1)
      } else {
        new_lvl <- 0
      }
    } else {
      new_lvl <- sample(setdiff(unique(data[which(data[, out_label]==0), target]), level), size=1)
    }
  }
  return(new_lvl)
}
