#' Detection of marginal outliers.
#'
#' @description
#' Function used to detect marginal outliers. The function only requires the column indices of the
#' discrete and the continuous variables and uses the functions disc_scores and cont_scores with their
#' default hyperparameter values to compute scores of outlyingness. The user can refer to the function
#' marg_outs_scores should they wish to detect the marginal outliers using discrete or continuous scores
#' that have been computed using different methods.
#'
#' @param data Data frame of mixed-type data. Should be of class 'data.frame'.
#' @param disc_cols Column indices of discrete variables.
#' @param cont_cols Column indices of continuous variables.
#' @return A list with 3 vectors; the vector "Discrete" includes the row indices
#' for the observations which are marginally outlying in the discrete space only (single
#' marginal outliers). The vector "Continuous" includes the row
#' indices for the observations which are marginally outlying in the continuous space only (only
#' single marginal outliers included). The vector "Combined" includes the row indices for
#' the combined marginal observations.
#' @export
#'
#' @examples dt <- gen_marg_joint_data(n_obs = 1000, n_disc = 5, n_cont = 5, n_lvls = 3, p_outs = 0.05, jp_outs = 0.2, assoc_target = c(1, 2), assoc_vars = list(c(1, 2), c(4,5)), assoc_type = c('linear', 'product'), seed_num = 1)
#' marg_outs(data = dt, disc_cols = c(1:5), cont_cols = c(6:10))
#'
marg_outs <- function(data, disc_cols, cont_cols){
  ### INPUT CHECKS ###
  if (!is.data.frame(data)){
    stop("Data set should be of class 'data.frame'.")
  }
  for (i in cont_cols){
    stopifnot("Continuous variables should be of class 'numeric'." = (is.numeric(data[, i])))
  }
  for (i in disc_cols){
    stopifnot("Discrete variables should be of class 'factor'." = (is.factor(data[, i])))
  }
  ### END OF CHECKS ###
  disc_out_scores <- disc_scores(data, disc_cols)
  outscorediscdf <- disc_out_scores[[2]]
  outscorediscdfcells <- disc_out_scores[[3]]
  outscorecontdf <- cont_scores(data, cont_cols = cont_cols,
                                sample_size = 256, ntrees = 500, ndim = 0,
                                max_depth = 100, seed_num = 1)
  marg_outs_list <- marg_outs_scores(data = data, disc_cols = disc_cols,
                                     outscorediscdf,
                                     outscorecontdf,
                                     outscorediscdfcells)
  return(marg_outs_list)
}
