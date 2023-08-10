#' Detect joint outliers given a set of associated features.
#'
#' @description
#' This function detects the joint outliers in a data set based on a set of given associations.
#' The association needs to have been detected prior to using this function, so that the output
#' is meaningful. Associations are defined between a discrete and a set of continuous variables.
#' Currently 3 methods are supported, namely the method of consecutive angles, the conservative
#' method that takes Lambda*_i = 3 and the binary method (used for binary target discrete variables,
#' upon removal of marginal outliers) which uses Lambda*_i = 2.
#'
#'
#' @param data Data set; should be of class 'data.frame'.
#' @param marg_outs Vector of row indices for marginal outliers in the data set. These will be discarded from the data set for detecting the joint outliers. One can set this to be an empty vector in case they provide a data set with no marginal outliers.
#' @param assoc_target Indices of target discrete variables associated with sets of continuous features, on which joint outliers will be detected. This can be a single number if only one association exists in the data set, otherwise a vector indicating which discrete variables are associated with some continuous features.
#' @param assoc_vars Indices of sets of continuous variables associated with assoc_target. If there only exists one association in the data, this is a vector, otherwise it should be a list of vectors, the list having length equal to length(assoc_target).
#' @param method Method used for detecting an optimal Lambda*_i value (see documentation of kde_classif & consec_angles functions); only "consec_angles", "conservative" and "bin" are supported,
#' corresponding to determining the optimal value using the method of consecutive angles, setting it equal to 3 or equal to 2, respectively.
#' If multiple associations are present, a vector of methods to be used should be provided for each association, or one method for all associations.
#' Default is "consec_angles", with candidate values for Lambda*_i ranging from 1 up to 20 with increments of 0.5.
#' @param drop_tol Tolerated level of drop in consecutive vec values for which
#' a drop is not considered significant. The default value is 3 assuming integer values in vec
#' but this can be modified (see documentation of consec_angles function).
#' Argument is ignored if method is not "consec_angles".
#' @param range_tol Parameter used to restrict the search, so that if the index of the value where
#' a drop is found exceeds range_tol, the kneedle algorithm is used and the elbow point is
#' returned instead. Default is 21, corresponding to Lambda*_i = 11 as maximum possible value for the
#' threshold Lambda_i. Set this to the maximum value of 39 if you don't want such a restriction (see documentation of consec_angles function).
#' Argument is ignored if method is not "consec_angles".
#'
#' @return Row indices of joint outliers in the provided data set.
#' @export
#'
#' @examples dt <- gen_marg_joint_data(n_obs = 1000, n_disc = 5, n_cont = 5, n_lvls = 3, p_outs = 0.05, jp_outs = 0.2, assoc_target = c(1,2), assoc_vars = list(c(1, 2),c(1,2)), assoc_type = 'linear', seed_num = 1)
#' discrete_scores <- disc_scores(dt, c(1:5))
#' continuous_scores <- cont_scores(dt, c(6:10))
#' marginal_outs <- unique(unlist(marg_outs_scores(data = dt, disc_cols = c(1:5), outscorediscdf = discrete_scores[[2]], outscorecontdf = continuous_scores, outscorediscdfcells = discrete_scores[[3]])))
#' joint_outs(data = dt, marg_outs = marginal_outs, assoc_target = c(1,2), assoc_vars = list(c(6, 7), c(6,7)), method = 'consec_angles', drop_tol = 3, range_tol = 11)
#' joint_outs(data = dt, marg_outs = marginal_outs, assoc_target = 1, assoc_vars = c(6, 7), method = 'conservative', drop_tol = 3, range_tol = 11)
joint_outs <- function(data, marg_outs, assoc_target, assoc_vars, method = "consec_angles",
                       drop_tol = 3, range_tol = 21){
  ### INPUT CHECKS ###
  stopifnot("Data set should be of class 'data.frame'." = is.data.frame(data) == TRUE)
  stopifnot("Indices of marginal outliers should be unique." = length(unique(marg_outs)) == length(marg_outs))
  stopifnot("Indices of marginal outliers should be unique integer values from 1 up to the number of observations in the data." = sum(marg_outs %in% c(1:nrow(data))) == length(marg_outs))
  if (length(method) == 1 & length(assoc_target) > 1){
    method <- rep(method, length(assoc_target))
  }
  for (i in 1:length(method)){
    stopifnot("Method not supported - see documentation for supported methods." = method[i] %in% c("consec_angles", "conservative", "bin"))
  }
  stopifnot("Argument drop_tol should be a positive number." = is.numeric(drop_tol))
  stopifnot("Argument drop_tol should be a positive number." = (length(drop_tol) == 1))
  stopifnot("Argument drop_tol should be a positive number." = (drop_tol > 0))
  stopifnot("Argument range_tol should be a positive integer." = is.numeric(range_tol))
  stopifnot("Argument range_tol should be a positive integer." = (range_tol %% 1 == 0))
  if (range_tol < 1 | range_tol > 39){
    stop("Argument range_tol should be a positive integer from 1 up to 39.")
  }
  if (!is.list(assoc_vars)){
    assoc_vars <- list(assoc_vars)
  }
  if (length(assoc_vars) != length(assoc_target)){
    stop("Length of association targets should be equal to length of list of association variables.")
  }
  stopifnot("Association targets should be unique." = length(unique(assoc_target)) == length(assoc_target))
  if (any(assoc_target > ncol(data))){
    stop("Association target indices should be unique positive integers, at most equal to the number of variables in the data set.")
  }
  for (i in 1:length(assoc_vars)){
    if (sum(assoc_vars[[i]] %% 1 == 0) != length(assoc_vars[[i]]) | sum(assoc_vars[[i]] > 0) != length(assoc_vars[[i]]) | sum(assoc_vars[[i]] <= ncol(data)) != length(assoc_vars[[i]]) | length(unique(assoc_vars[[i]])) != length(assoc_vars[[i]])){
      stop("Association variable indices should be unique positive integers, at most equal to the number of variables in the data set.")
    }
  }
  for (i in 1:length(assoc_target)){
    stopifnot("Association targets should be of class 'factor'." = is.factor(data[, assoc_target[i]]))
  }
  for (i in 1:length(assoc_vars)){
    for (j in 1:length(assoc_vars[[i]])){
      stopifnot("Association variables should be of class 'numeric'." = is.numeric(data[, assoc_vars[[i]][j]]))
    }
  }
  ### END OF CHECKS ###
  joint_outs_vec <- c()
  for (target in c(1:length(assoc_target))){
    kde <- kde_classif(data = data,
                       target_inx = assoc_target[target],
                       pred_inx = assoc_vars[[target]],
                       marg_outs = marg_outs,
                       Lambda_i = 0,
                       kernel = 'gauss',
                       alpha_val = 0.3)
    j_det <- kde[[1]]
    if (method[target] == 'consec_angles'){
      Lambda_i_star <- consec_angles(vec = j_det,
                                     range = seq(1, 20, by=.5),
                                     drop_tol = drop_tol,
                                     range_tol = range_tol)
      Lambda_i_star_inx <- match_numeric(Lambda_i_star, seq(1, 20, by=.5))
      joint_outs_vec <- c(joint_outs_vec, unlist(kde[[2]][Lambda_i_star_inx]))
    } else if (method[target] == "bin"){
      # For binary variables, Lambda_i_star = 2 corresponds to 3rd element
      joint_outs_vec <- c(joint_outs_vec, unlist(kde[[2]][3]))
    } else if (method[target] == "conservative"){
      # For conservative method, Lambda_i_star = 3, corresponding to 5th element
      joint_outs_vec <- c(joint_outs_vec, unlist(kde[[2]][5]))
    }
  }
  # Any repeating joint outliers violating multiple associations should only be reported once
  joint_outs_vec <- unique(joint_outs_vec)
  return(joint_outs_vec)
}
