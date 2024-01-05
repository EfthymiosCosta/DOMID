#' Perform KDE classification using locfit.
#'
#' @description
#' This function performs classification of a discrete feature using a set
#' of continuous variables by using Kernel Density Estimation (KDE). The data set should be provided
#' together with a set of indices of marginally outlying observations, since these observations will
#' be discarded - one can also provide the data set without any marginal outliers and simply provide
#' an empty vector for the marginal outliers parameter. A Kernel Density Estimator is built for each
#' of the levels of the target feature. The value of \eqn{\Lambda_i} is there to account for
#' the number of misclassifications for which the KDE ratio (max KDE value over all levels divided
#' by the KDE value for the true level for each observation) exceeds \eqn{\Lambda_i}. Setting this equal to
#' 0 (default choice) will return results for all \eqn{\Lambda_i} values from 1 up to 20 with a step size of 0.5.
#' The locfit package is used when there are more than 1 predictor, otherwise the density function from
#' the stats package is used for targeting univariate densities.
#'
#' @param data Data set; should be of class 'data.frame'.
#' @param target_inx Column index for target discrete variable. This variable should be of unit length and the target variable should be of class 'factor'.
#' @param pred_inx Column index for predictor variables. The predictor variables can only be of class 'numeric'.
#' @param marg_outs Vector of row indices for marginal outliers in the data set. These will be discarded from the KDE classification. One can set this to be an empty vector in case they provide a data set with no marginal outliers.
#' @param Lambda_i Vector of \eqn{\Lambda_i} values, such that the function returns the misclassified observations for which the KDE ratio exceeds the threshold value of \eqn{\Lambda_i}. This can be any vector of values greater than 1, the default choice being 0 which corresponds to a vector of values from 1 to 20 in step sizes of 0.5.
#' @param kernel Kernel chosen for KDE. Default choice is 'gauss' for Gaussian kernel. This is also the only kernel used if there is just 1 predictor and this argument is ignored. Other options are 'rect', 'trwt', 'tria', 'epan' or 'bisq' for Rectangular, Triweight, Triangle, Epanechnikov and Bisquare kernels - see the documentation of locfit for more details.
#' @param alpha_val The value of \eqn{\alpha} that determines the kernel bandwidth. This argument is ignored if there is only 1 predictor. The KDE estimator uses an adaptive nearest-neighbour bandwidth to overcome sparsity issues; this uses a bandwidth equal to the kth smallest distance between each point and its neighbours, where \eqn{k = \lfloor n* \alpha \rfloor} and n is
#' the number of observations possessing each target index level of interest. Default value is 0.3 and the value can be between 0 and 1 - see the documentation of locfit for more details.
#' @param maxk Controls space assignment for evaluation structures for the locfit evaluation. This argument is ignored if there is only 1 predictor. See locfit documentation
#' for more details. If you get errors or warnings about `Insufficient vertex space',
#' locfit's default assigment can be increased by increasing 'maxk'. Default value is 1000.
#'
#' @return A list with 2 elements. The first element is a vector of length
#' equal to length(Lambda_i), including the number of misclassifications for which the KDE ratio exceeds
#' the elements of Lambda_i. The second element is a list of length equal to length(Lambda_i), with the
#' indices of the misclassified observation for which the KDE ratio exceeds the elements of Lambda_i.
#' Setting Lambda_i equal to its default value of 0 will set the argument equal to a vector of
#' values from 1 up to 20, with step size of 0.5.
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
#'                           assoc_target = 1,
#'                           assoc_vars = c(1, 2),
#'                           assoc_type = 'linear',
#'                           seed_num = 1)
#' discrete_scores <- disc_scores(dt, c(1:5))
#' continuous_scores <- cont_scores(dt, c(6:10))
#' marginal_outs <- unique(unlist(marg_outs_scores(data = dt,
#'                                                 disc_cols = c(1:5),
#'                                                 outscorediscdf = discrete_scores[[2]],
#'                                                 outscorecontdf = continuous_scores,
#'                                                 outscorediscdfcells = discrete_scores[[3]])))
#' kde_classification <- kde_classif(data = dt,
#'                                   target_inx = c(1),
#'                                   pred_inx = c(6, 7),
#'                                   marg_outs = marginal_outs,
#'                                   Lambda_i = 0,
#'                                   kernel = 'gauss',
#'                                   alpha_val = 0.3)
#' kde_classification2 <- kde_classif(data = dt,
#'                                    target_inx = c(1),
#'                                    pred_inx = c(6, 7),
#'                                    marg_outs = marginal_outs,
#'                                    Lambda_i = c(1.5, 5, 7.3, 21.1),
#'                                    kernel = 'epan',
#'                                    alpha_val = 0.5)
#' kde_classification3 <- kde_classif(data = dt,
#'                                    target_inx = c(1),
#'                                    pred_inx = c(6, 7),
#'                                    marg_outs = marginal_outs,
#'                                    Lambda_i = 8,
#'                                    kernel = 'rect',
#'                                    alpha_val = 0.9,
#'                                    maxk = 2000)
#' }
kde_classif <- function(data, target_inx, pred_inx, marg_outs,
                        Lambda_i = 0, kernel = "gauss", alpha_val = 0.3, maxk = 1000){
  ### INPUT CHECKS ###
  stopifnot("Data set should be of class 'data.frame'." = is.data.frame(data) == TRUE)
  stopifnot("Lambda_i should be of class 'numeric'." = is.numeric(Lambda_i))
  if (length(Lambda_i) > 1){
    stopifnot("All values of Lambda_i should be at least equal to 1." = sum(Lambda_i >= 1) == length(Lambda_i))
  } else if (length(Lambda_i) == 1 & Lambda_i < 1 & Lambda_i != 0){
    stop("Lambda_i should be at least equal to 1 or equal to 0 (see documentation).")
  }
  stopifnot("Target (discrete) variable should be of class 'factor'." = (is.factor(data[, target_inx])))
  for (i in pred_inx){
    stopifnot("Predictor (continuous) variables should be of class 'numeric'." = (is.numeric(data[, i])))
  }
  if (length(alpha_val) != 1 | !is.numeric(alpha_val)){
    stop("alpha_val should be a number between 0 and 1.")
  }
  if (alpha_val < 0 | alpha_val > 1){
    stop("alpha_val should be a number between 0 and 1.")
  }
  stopifnot("Indices of marginal outliers should be unique." = length(unique(marg_outs)) == length(marg_outs))
  stopifnot("Indices of marginal outliers should be unique integer values from 1 up to the number of observations in the data." = sum(marg_outs %in% c(1:nrow(data))) == length(marg_outs))
  stopifnot("Kernel choice not supported - see documentation for supported kernels." = kernel %in% c("gauss", "rect", "trwt", "tria", "epan", "bisq"))
  if (length(maxk) > 1){
    stop("The 'maxk' parameter must be a single positive integer.")
  }
  if (maxk %% 1 != 0 | maxk <= 0){
    stop("The 'maxk' parameter must be a positive integer.")
  }
  ### END OF CHECKS ###

  # Filter out marginal outliers
  if (length(marg_outs) > 0){
    data_no_marg <- data[-marg_outs, ]
  } else {
    data_no_marg <- data
  }
  original_row_names <- row.names(data_no_marg)
  row.names(data_no_marg) <- c(1:nrow(data_no_marg))
  # Store observations corresponding to each level of target variable
  data_no_marg_lvl <- list()
  for (i in sort(unique(data_no_marg[, target_inx]))){
    data_no_marg_lvl[[length(data_no_marg_lvl)+1]] <- data_no_marg[which(data_no_marg[, target_inx]==i), pred_inx]
  }
  # Perform KDE and store the fits
  fits <- list()
  # Check if we are targetting a univariate distribution
  # If so, locfit shall not be used and density is called instead
  aux <- TRUE
  if (length(pred_inx) == 1){
    aux <- FALSE
  }
  for (i in 1:length(unique(data_no_marg[, target_inx]))){
    if (aux){
      fits[[i]] <- locfit::locfit(~.,
                                  data = data_no_marg_lvl[[i]],
                                  kern = kernel, alpha = alpha_val,
                                  maxk = maxk, family = 'dens',
                                  maxit = 1000)
    } else {
      fits[[i]] <- stats::density(x = data_no_marg_lvl[[i]],
                                  kernel = "gaussian",
                                  n = 2^12)
    }
  }
  # Store prediction for each class in matrix
  pred_dens_mat <- matrix(NA, nrow = nrow(data_no_marg), ncol = length(unique(data_no_marg[, target_inx])))
  for (i in 1:length(unique(data_no_marg[, target_inx]))){
    if (aux){
      pred_dens_mat[,i] <- sapply(1:nrow(data_no_marg), FUN = function(j) stats::predict(fits[[i]], data_no_marg[j, pred_inx]))
    } else {
      pred_dens_mat[,i] <- sapply(1:nrow(data_no_marg), FUN = function(j) KDE_pred_1d(data_no_marg[j, pred_inx], data_no_marg_lvl[[i]], h = fits[[i]]$bw))
    }
  }
  # Get classifications
  pred_dens <- sapply(1:nrow(data_no_marg), FUN = function(j) which.max(pred_dens_mat[j,]))
  # Misclassified observations
  dens_missed <- as.character(which(pred_dens != data_no_marg[, target_inx]))
  Lambda_val_vec <- c(Lambda_i)
  if (length(Lambda_i) == 1){
    if (Lambda_i == 0){
      Lambda_val_vec <- seq(1, 20, by=.5)
    }
  }
  # Empty vectors for storing boundary points & number of joint outliers found
  j_det <- c()
  bounds_det <- c()
  # Empty list for storing misclassified observations for each Lambda_i
  missed_list <- list()
  for (Lambda_val in Lambda_val_vec){
    boundary_pts <- rep(FALSE, length(dens_missed))
    boundary_pts[bounds_det] <- TRUE
    for (i in 1:length(dens_missed)){
      if (!boundary_pts[i]){
        # Extract misclassified observations and their true levels
        obs <- data_no_marg[which(row.names(data_no_marg)==dens_missed[i]), pred_inx]
        true_class <- data_no_marg[which(row.names(data_no_marg)==dens_missed[i]), target_inx]
        # Look at local densities
        if (aux){
          true_dens <- stats::predict(fits[[true_class]], obs)
        } else {
          true_dens <- KDE_pred_1d(t = obs, xs = data_no_marg_lvl[[true_class]], h = fits[[true_class]]$bw)
        }
        dens_preds <- c()
        for (j in 1:length(fits)){
          if (aux){
            dens_preds <- c(dens_preds, stats::predict(fits[[j]], obs))
          } else {
            dens_preds <- c(dens_preds, KDE_pred_1d(t = obs, xs = data_no_marg_lvl[[j]], h=fits[[j]]$bw))
          }
        }
        # Look at KDE ratios
        dens_ratios <- setdiff(dens_preds, true_dens)/true_dens
        if ((sum(dens_ratios <= Lambda_val) == length(dens_ratios))){
          boundary_pts[i] <- TRUE
          bounds_det <- c(bounds_det, i)
        }
      }
    }
    # Number of joint outliers
    j_det <- c(j_det, nrow(data_no_marg[which(row.names(data_no_marg) %in% dens_missed[!boundary_pts]),]))
    # Joint outliers
    missed_list[[length(missed_list) + 1]] <- as.numeric(original_row_names[which(row.names(data_no_marg) %in% dens_missed[!boundary_pts])])
  }
  return(list("Total_misclassifications" = j_det, "Misclassified_observations" = missed_list))
}
