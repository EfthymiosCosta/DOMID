#' Detection of associations between continuous & discrete features using XGBoost with Stratified K-Fold CV
#'
#' @description
#' This function uses the xgboost classifier by the homonymous R package for predicting the levels of
#' a target discrete feature using all continuous variables of a data set as predictors. Stratified K-Fold
#' cross validation is used and predictive accuracy results are returned. These can then be used by the
#' user for determining if an association between the target variable and a set of predictors is present,
#' while variable importances are also returned so that the user can see the continuous variables that
#' contribute to this association.
#'
#' @param data Data set of mixed-type data on which an XGBoost classifier will be implemented. Should be of class 'data.frame'.
#' @param K Number of folds to be used. Default value is 5.
#' @param pred_inx Indices of predictors. This will typically be a vector with the column indices of all continuous columns that will be used as predictors, although one can use less continuous columns as well.
#' @param target_inx Indices of target columns; should be discrete variables. A small manipulation of the factor levels is done as pre-processing step, as the xgboost function allows levels starting from 0 instead of 1.
#' @param cont_scores Continuous scores of outlyingness. These should be provided as a vector of length equal to the number of observations. The scores are used for appropriately weighing the cross entropy loss function and they should be calculated for the continuous variables that are used as predictors.
#' @param contribs Contributions of target discrete variable to the discrete scores of outlyingness of each observation. These should be provided as a matrix of dimensions (nrow(data) x length(target_inx)) of class 'data.frame'. These scores are used for appropriately weighing the cross entropy loss function.
#' @param marg_outs Vector of marginal outlier row indices. These will be used for appropriately weighing the observations. If empty, every observation will be given equal weight.
#'
#' @return A list of 3 elements. The first elements is a list with length(target_inx) elements, named 'Contributions'. This includes summary statistics for the contribution (importance) of each continuous variable
#' to the predictive accuracy of the target discrete variables for each of the K folds. The included summary statistics are the Average, Median, Maximum, Minimum and Standard Deviation of the Contributions across all K folds.
#' The second element is a data frame named 'Misclassifications' which summarises the number of misclassified observations for each target discrete variable, as well as the misclassification rate.
#' The third element is a list of vectors including the weight assigned to each observation for each target variable.
#' @export
#'
#' @examples dt <- gen_marg_joint_data(n_obs = 1000, n_disc = 5, n_cont = 5, n_lvls = 3, p_outs = 0.05, jp_outs = 0.2, assoc_target = 1, assoc_vars = c(1, 2), assoc_type = 'linear', seed_num = 1)
#' discrete_scores <- disc_scores(dt, c(1:5))
#' continuous_scores <- cont_scores(dt, c(6:10))
#' marginal_outs_ <- unique(unlist(marg_outs_scores(data = dt, disc_cols = c(1:5), outscorediscdf = discrete_scores[[2]], outscorecontdf = continuous_scores, outscorediscdfcells = discrete_scores[[3]])))
#' assoc_detect_xgb(dt, K = 5, pred_inx = c(6:10), target_inx = c(1:5), cont_scores = continuous_scores[,2], contribs = discrete_scores$Contributions, marg_outs = marginal_outs)
#' assoc_detect_xgb(dt, K = 5, pred_inx = c(6:10), target_inx = c(1), cont_scores = continuous_scores[,2], contribs = as.data.frame(discrete_scores$Contributions[,1]), marg_outs = marginal_outs)
#' assoc_detect_xgb(dt, K = 3, pred_inx = c(6:10), target_inx = c(1, 3), cont_scores = continuous_scores[,2], contribs = discrete_scores$Contributions[, c(1, 3)], marg_outs = marginal_outs)
assoc_detect_xgb <- function(data, K = 5, pred_inx, target_inx,
                             cont_scores, contribs, marg_outs){
  ### INPUT CHECKS ###
  stopifnot("Data set should be of class 'data.frame'." = is.data.frame(data) == TRUE)
  if (K < 1 | K%%1 != 0 | K > nrow(data)){
    stop("K should be a positive integer at most equal to the number of observations.")
  }
  for (i in target_inx){
    stopifnot("Target (discrete) variables should be of class 'factor'." = (is.factor(data[, i])))
  }
  for (i in pred_inx){
    stopifnot("Predictor (continuous) variables should be of class 'numeric'." = (is.numeric(data[, i])))
  }
  stopifnot("Continuous scores should be a vector of length equal to the number of observations." = length(cont_scores) == nrow(data))
  stopifnot("Data set of contributions should be of class 'data.frame'." = is.data.frame(contribs) == TRUE)
  stopifnot("Data set of contributions should have as many rows as the number of observations." = nrow(contribs) == nrow(data))
  stopifnot("Data set of contributions should have as many columns as the number of target (discrete) variables." = ncol(contribs) == length(target_inx))
  stopifnot("Indices of marginal outliers should be unique." = length(unique(marg_outs)) == length(marg_outs))
  stopifnot("Indices of marginal outliers should be unique integer values from 1 up to the number of observations in the data." = sum(marg_outs %in% c(1:nrow(data))) == length(marg_outs))
  ### END OF CHECKS ###

  n <- nrow(data)
  contribs_list <- list()
  weights_list <- list()
  misclassifs_df <- data.frame('Target index' = numeric(),
                               'Misclassifications' = numeric(),
                               'Misclassification Rate' = numeric(),
                               stringsAsFactors = FALSE)
  # Set continuous scores of non-marginal observations to their minimum value
  cont_scores[-marg_outs] <- min(cont_scores)
  for (t in 1:length(target_inx)){
    # Pre-processing step required for XGBoost
    data[, target_inx[t]] <- factor(data[, target_inx[t]], levels = sort(unique(data[, target_inx[t]])),
                                    labels = c('0':as.character(length(unique(data[, target_inx[t]]))-1)))
    # Number of target classes
    nclass <- length(unique(data[, target_inx[t]]))
    # Matrix storing prediction vector
    preds_mat <- matrix(rep(0, (nrow(data)*(nclass))), nrow = n)
    # Normalise contributions
    if (max(contribs[, t])>0){
      contribs[, t] <- contribs[, t]/max(contribs[, t])
    }
    # Set contributions of non-marginal observations to their minimum value
    contribs[-marg_outs, t] <- min(contribs[-marg_outs, t])
    # Create folds
    folds_list <- caret::createFolds(data[, target_inx[t]], k = K, list = TRUE)
    # List to store importance data frames
    imp_list <- list()
    # Empty vector of weights
    weights_vec <- rep(NA, n)
    # loop over 5 folds
    for (i in 1:K){
      # Get test set
      Test_set <- data[folds_list[[i]], ]
      # Get training set
      Train_set <- data[-folds_list[[i]], ]
      # Once parameters are obtained, obtain mean error on test set
      xgbdata <- xgboost::xgb.DMatrix(data = as.matrix(Train_set[, pred_inx]),
                                      label = as.numeric(Train_set[, target_inx[t]])-1)
      # Now use multi:softprob to get vector of probabilities
      xgb <- xgboost::xgboost(data = xgbdata,
                              max_depth = 10,
                              nrounds = 50,
                              eval_metric = "merror",
                              objective = "multi:softprob",
                              num_class = nclass,
                              subsample = 1,
                              weight = 2 - (cont_scores[-folds_list[[i]]] + contribs[-folds_list[[i]], t]),
                              verbose = 0)
      weights_vec[-folds_list[[i]]] <- 2 - (cont_scores[-folds_list[[i]]] + contribs[-folds_list[[i]], t])
      importance <- xgboost::xgb.importance(colnames(Train_set[, c(pred_inx)]), model = xgb)
      imp_list[[length(imp_list) + 1]] <- importance
      # get predictions on test set
      preds <- predict(xgb, as.matrix(Test_set[, pred_inx]))
      # Save prediction vectors
      preds_mat[folds_list[[i]], ] <- matrix(preds, nrow=nrow(Test_set), byrow = TRUE)
      cat('Fold',i,'/',K, 'for target variable', target_inx[t], 'complete. \n')
    }
    res_list <- list(preds_mat, imp_list)
    # Get predicted class labels
    xgboostpredictions <- sapply(1:nrow(res_list[[1]]), FUN = function (i) which.max(res_list[[1]][i,])-1)
    # Contributions
    # Loop over K-1 folds
    # Output list includes a list of single-element lists
    # The elements are the data frames of interest
    # We only care about the first 2 columns and merge by Feature
    contribs_df <- res_list[[2]][1][[1]][, c(1:2)]
    for (i in 2:K){
      # This is needed to remove column name duplicate issues
      colnames(res_list[[2]][i][[1]])[2] <- as.character(i)
      contribs_df <- merge(contribs_df, res_list[[2]][i][[1]][,c(1:2)], by="Feature", all = TRUE)
    }
    contribs_df <- data.frame("Feature"=contribs_df$Feature,
                              "Avg_Contrib" = rowMeans(contribs_df[,-1], na.rm = TRUE),
                              "Median_Contrib" = matrixStats::rowMedians(as.matrix(contribs_df[,-1]), na.rm=TRUE),
                              "Max_Contrib" = matrixStats::rowMaxs(as.matrix(contribs_df[,-1]), na.rm=TRUE),
                              "Min_Contrib" = matrixStats::rowMins(as.matrix(contribs_df[,-1]), na.rm=TRUE),
                              "SD_Contrib" = matrixStats::rowSds(as.matrix(contribs_df[,-1]), na.rm=TRUE))
    # Find deatures whose names appear in importances
    features <- c(as.vector(unique(contribs_df$Feature)))
    # Find continuous features with no importance - fixes a bug that may occur
    features_not_imp <- colnames(data[, pred_inx])[!(colnames(data[, pred_inx]) %in% features)]
    if (length(features_not_imp) > 0){
      contribs_df <- rbind(contribs_df, data.frame('Feature'=features_not_imp,
                                                   "Avg_Contrib" = 0,
                                                   "Median_Contrib" = 0,
                                                   "Max_Contrib" = 0,
                                                   "Min_Contrib" = 0,
                                                   "SD_Contrib" = 0))
    }
    # Save contributions, weights and misclassification results
    contribs_list[[length(contribs_list)+1]] <- contribs_df
    misclassifs <- n - sum(xgboostpredictions == data[, target_inx[t]])
    misclassifs_df <- rbind(misclassifs_df,
                            data.frame('Target index' = target_inx[t],
                                       'Misclassifications' = misclassifs,
                                       'Misclassification Rate' = misclassifs/n))
    weights_list[[length(weights_list)+1]] <- weights_vec
  }
  return(list("Contributions" = contribs_list, "Misclassifications" = misclassifs_df,
              "Weights" = weights_list))
}
