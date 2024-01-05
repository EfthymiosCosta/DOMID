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
#' @param alpha Significance level for the simultaneous Multinomial confidence intervals constructed for calculating discrete scores,
#' determining what the frequency thresholds should be for itemsets of different length. Must be a positive real,
#' at most equal to 0.20. A greater value leads to a much more conservative algorithm that also penalises less
#' infrequent itemsets. Default value is 0.01.
#' @param MAXLEN Maximum itemset sequence length to be considered for discrete scores. Default value is 0 which calculates MAXLEN according to a criterion
#' on the sparsity caused by the total combinations that can be encountered as sequences of greater length are taken into account.
#' Otherwise, MAXLEN can take any value from 1 up to the total number of discrete variables included in the data set.
#' @param rho Maximum proportion of outliers believed to be in the data set. Used together with epsilon
#' to determine a stopping criterion for the search for marginal outliers based on scores of outlyingness.
#' Must be a real number in range (0, 0.5) with rho + epsilon \eqn{\leq 0.50} (\eqn{\rho + \epsilon \leq 0.5}). A smaller rho assumes less outliers.
#' Defaults to 0.20 (20%). See marg_outs_scores for more information.
#' @param epsilon Additional proportion of outliers that we are willing to tolerate. Must be a number
#' in the range (0, 0.25) with rho + epsilon \eqn{\leq 0.50} (\eqn{\rho + \epsilon \leq 0.5}). Must also be smaller than rho, as it only represents
#' the additional error. Defaults to 0.02 (2%). See marg_outs_scores for more information.
#' @return A list with 3 vectors; the vector "Discrete" includes the row indices
#' for the observations which are marginally outlying in the discrete space only (single
#' marginal outliers). The vector "Continuous" includes the row
#' indices for the observations which are marginally outlying in the continuous space only (only
#' single marginal outliers included). The vector "Combined" includes the row indices for
#' the combined marginal observations.
#' @export
#'
#' @examples
#' \dontrun{
#' dt <- gen_marg_joint_data(n_obs = 1000,
#'                           n_disc = 5,
#'                           n_cont = 5,
#'                           n_lvls = 3,
#'                           p_outs = 0.05,
#'                           jp_outs = 0.2,
#'                           assoc_target = c(1, 2),
#'                           assoc_vars = list(c(1, 2), c(4,5)),
#'                           assoc_type = c('linear', 'product'),
#'                           seed_num = 1)
#' marg_outs(data = dt,
#'           disc_cols = c(1:5),
#'           cont_cols = c(6:10),
#'           alpha=0.01,
#'           MAXLEN = 0,
#'           rho = 0.20,
#'           epsilon = 0.02)
#' }
marg_outs <- function(data, disc_cols, cont_cols, alpha = 0.01, MAXLEN = 0, rho = 0.20, epsilon = 0.02){
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
  stopifnot("rho must be a number between 0 and 0.50." = is.numeric(rho))
  stopifnot("epsilon must be a number between 0 and 0.50." = is.numeric(epsilon))
  if (rho <= 0 | rho > 0.50){
    stop("Incorrect value for rho - must be between 0 and 0.50.")
  }
  if (epsilon < 0 | epsilon > 0.25){
    stop("Incorrest value for epsilon - must be between 0 and 0.25.")
  }
  if ((rho + epsilon) > 0.50 | (rho <= epsilon)){
    stop("rho must be greater than epsilon and their sum should be at most 0.50.")
  }
  stopifnot("alpha should be of class 'numeric'." = is.numeric(alpha))
  if (length(alpha) > 1){
    stop("alpha should be of unit length.")
  }
  if (alpha <= 0 | alpha > 0.20){
    stop("alpha should be positive and at most equal to 0.20.")
  }
  if (length(MAXLEN) > 1){
    stop("MAXLEN should be an integer at most equal to the number of discrete variables.")
  }
  if (MAXLEN %% 1 !=0){
    stop("MAXLEN should be an integer at most equal to the number of discrete variables.")
  }
  if (MAXLEN < 0 | MAXLEN > length(disc_cols)){
    stop("MAXLEN should be an integer at most equal to the number of discrete variables.")
  }
  ### END OF CHECKS ###
  disc_out_scores <- disc_scores(data, disc_cols, alpha, MAXLEN)
  outscorediscdf <- disc_out_scores[[2]]
  outscorediscdfcells <- disc_out_scores[[3]]
  outscorecontdf <- cont_scores(data, cont_cols = cont_cols,
                                sample_size = 256, ntrees = 500, ndim = 0,
                                max_depth = 100, seed_num = 1)
  marg_outs_list <- marg_outs_scores(data = data, disc_cols = disc_cols,
                                     outscorediscdf,
                                     outscorecontdf,
                                     outscorediscdfcells,
                                     rho = rho, epsilon = epsilon)
  return(marg_outs_list)
}
