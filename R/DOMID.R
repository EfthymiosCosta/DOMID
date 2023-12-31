#' DOMID: Detecting Outliers in MIxed-type Data (marginal & joint)
#'
#' @param data Data set of mixed-type data. Should be of class 'data.frame'.
#' @param disc_cols Indices of discrete variable columns. Should be of class 'factor'.
#' @param cont_cols Indices of discrete variable columns. Should be of class 'numeric'.
#' @param alpha Significance level for the simultaneous Multinomial confidence intervals constructed, determining what the
#' frequency thresholds should be for itemsets of different length, used for outlier detection for discrete features. Must be a positive real, at most equal to 0.20. A
#' greater value leads to a much more conservative algorithm that also penalises less infrequent itemsets. Default value is 0.01.
#' @param MAXLEN Maximum itemset sequence length to be considered. Default value is 0 which calculates MAXLEN according to a criterion
#' on the sparsity caused by the total combinations that can be encountered as sequences of greater length are taken into account.
#' Otherwise, MAXLEN can take any value from 1 up to the total number of discrete variables included in the data set.
#' @param rho Maximum proportion of outliers believed to be in the data set. Used together with epsilon
#' to determine a stopping criterion for the search for marginal outliers based on scores of outlyingness.
#' Must be a real number in range (0, 0.5) with rho + epsilon \eqn{\leq 0.50} (\eqn{\rho + \epsilon \leq 0.5}). A smaller rho assumes less outliers.
#' Defaults to 0.20 (20%). See marg_outs_scores for more information.
#' @param epsilon Additional proportion of marginal outliers that we are willing to tolerate. Must be a number
#' in the range (0, 0.25) with rho + epsilon \eqn{\leq 0.50} (\eqn{\rho + \epsilon \leq 0.5}). Must also be smaller than rho, as it only represents
#' the additional error. Defaults to 0.02 (2%). See marg_outs_scores for more information.
#' @param sample_size Sample size for Extended Isolation Forest algorithm, default choice is 256.
#' If the data set includes less than sample_size observations, then the number of observations is used as sample_size instead.
#' @param ntrees Number of binary trees used in Extended Isolation Forest algorithm, default choice is 500.
#' @param ndim Number of dimensions on which the splits are made in Extended Isolation Forest algorithm,
#' default choice of 0 corresponds to the number of continuous variables in the data.
#' (Note: Setting ndim = 1 yields the original Isolation Forest algorithm.)
#' @param max_depth Maximum depth of binary trees used in Extended Isolation Forest algorithm, default choice is 100.
#' @param seed_num Seed number for reproducibility of Extended Isolation Forest results, default choice is 1.
#' @param delta Proportion of nearest neighbours considered for identifying associations. Should be a number in the range (0, 0.5],
#' default value is 0.50.
#' @param mink_order Order of Minkowski distance used for nearest neighbour search in identification of associations.
#' The default value of 1 returns the L1 norm (Manhattan distance).
#' @param alpha1 Significance level of Kruskal Wallis H test used; default value is 1e-3.
#' @param alpha2 Significance level for the chi-square goodness of fit test used for each class; default value is 1e-1.
#' @param method Method used for detecting an optimal \eqn{\Lambda^*_i} value (see documentation of kde_classif & consec_angles functions);
#' only "consec_angles", "conservative" and "bin" are supported, corresponding to determining the optimal value using the method of consecutive angles,
#' setting it equal to 3 or equal to 2, respectively.
#' If multiple associations are present, a vector of methods to be used should be provided for each association, or one method for all associations.
#' Default is "consec_angles", with candidate values for \eqn{\Lambda^*_i} ranging from 1 up to 20 with increments of 0.5.
#' @param drop_tol Tolerated level of drop in consecutive vec values for which
#' a drop is not considered significant. The default value is 3 assuming integer values in vec
#' but this can be modified (see documentation of consec_angles function).
#' Argument is ignored if method is not "consec_angles".
#' @param range_tol Parameter used to restrict the search, so that if the index of the value where
#' a drop is found exceeds range_tol, the kneedle algorithm is used and the elbow point is
#' returned instead. Default is 21, corresponding to \eqn{\Lambda^*_i = 11} as maximum possible value for the
#' threshold \eqn{\Lambda_i}. Set this to the maximum value of 39 if you don't want such a restriction
#' (see documentation of consec_angles function). Argument is ignored if method is not "consec_angles".
#'
#' @return A list with 8 elements. The first 3 elements are the Discrete, Continuous and Combined marginal outliers.
#' The 4th element is a vector with the row indices of the joint outliers (if any exist).
#' Then, the discrete scores, the contributions matrix, the continuous scores
#' and the value of MAXLEN are the last 4 elements.
#' @export
#'
#' @examples
#' \dontrun{
#' dt <- DOMID::gen_marg_joint_data(n_obs = 500, n_disc = 2, n_cont = 3, n_lvls = 3,
#'                                  p_outs = 0.15, jp_outs = 0.5, assoc_target = 1,
#'                                  assoc_vars = c(1,3), assoc_type = "product")
#' DOMID(data = dt, disc_cols = c(1:2), cont_cols = c(3:5))
#' }
DOMID <- function(data, disc_cols, cont_cols, alpha = 0.01,
                  MAXLEN = 0, rho = 0.20, epsilon = 0.02,
                  sample_size = 256, ntrees = 500, ndim = 0,
                  max_depth = 100, seed_num = 1,
                  delta = 0.50, mink_order = 1,
                  alpha1 = 1e-3, alpha2 = 1e-1,
                  method = "consec_angles",
                  drop_tol = 3, range_tol = 21){
  # Compute discrete scores
  discrete_scores <- DOMID::disc_scores(data, disc_cols, alpha, MAXLEN)
  # Compute continuous scores
  continuous_scores <- DOMID::cont_scores(data, cont_cols, sample_size,
                                          ntrees, ndim, max_depth, seed_num)
  # Obtain marginal outliers based on scores
  marginals <- DOMID::marg_outs_scores(data, disc_cols,
                                       outscorediscdf = discrete_scores[[2]],
                                       outscorecontdf = continuous_scores,
                                       outscorediscdfcells = discrete_scores[[3]],
                                       alpha, rho, epsilon)
  # Find associations
  assocs <- list()
  for (i in 1:length(disc_cols)){
    assocs[[length(assocs)+1]] <- DOMID::assoc_detect(data, marginals = unique(unlist(marginals)),
                                                      target_inx = disc_cols[i],
                                                      pred_inx = cont_cols,
                                                      delta, mink_order, alpha1, alpha2)
  }
  outliers_detected <- list()
  outliers_detected$Discrete <- marginals[[1]]
  outliers_detected$Continuous <- marginals[[2]]
  outliers_detected$Combined <- marginals[[3]]
  # Use associations found (if any) to detect joint outliers
  num_assocs <- length(which(!is.na(assocs)))
  if (num_assocs > 0){
    joint_outliers <- DOMID::joint_outs(data, marg_outs = unique(unlist(marginals)),
                                        assoc_target = disc_cols[which(!is.na(assocs))],
                                        assoc_vars = assocs[which(!is.na(assocs))],
                                        method, drop_tol, range_tol)
    outliers_detected$Joint <- joint_outliers
  } else {
    outliers_detected[length(outliers_detected)+1] <- list(c())
    names(outliers_detected)[length(outliers_detected)] <- "Joint"
  }
  cat('Outliers detected.\n')
  outliers_detected$DiscreteScores <- discrete_scores[[2]]
  outliers_detected$Contributions <- discrete_scores[[3]]
  outliers_detected$ContinuousScores <- continuous_scores
  outliers_detected$MAXLEN <- discrete_scores[[1]]
  return(outliers_detected)
}
