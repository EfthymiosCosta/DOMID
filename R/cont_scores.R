#' Continuous scores of outlyingness
#'
#' @description
#' Function used to calculate the continuous scores of outlyingness, based on the Extended Isolation Forest
#' algorithm of Hariri et al. (2019).
#'
#' @param data Data frame including continuous variables on which outlier detection will be implemented. Should be of class 'data.frame'.
#' @param cont_cols Column indices for discrete variables, these columns should be of class 'numeric'.
#' @param sample_size Sample size for Extended Isolation Forest algorithm, default choice is 256. If the data set includes less than sample_size observations, then the number of observations is used as sample_size instead.
#' @param ntrees Number of binary trees used in Extended Isolation Forest algorithm, default choice is 500.
#' @param ndim Number of dimensions on which the splits are made in Extended Isolation Forest algorithm, default choice of 0 corresponds to the number of continuous variables in the data. (Note: Setting ndim = 1 yields the original Isolation Forest algorithm.)
#' @param max_depth Maximum depth of binary trees used in Extended Isolation Forest algorithm, default choice is 100.
#' @param seed_num Seed number for reproducibility, default choice is 0.
#'
#' @return A data frame with 2 columns, the first corresponding to the observations and the second corresponding to their continuous scores of outlyingness.
#' @export
#'
#' @examples cont_scores(data = iris, cont_cols = c(1:4))
#' @examples cont_scores(data = iris, cont_cols = c(1:4), sample_size = 256, ntrees = 10, ndim = 1, max_depth = 10, seed_num = 100)
cont_scores <- function(data, cont_cols, sample_size = 256,
                        ntrees = 500, ndim = 0, max_depth = 100, seed_num = 1){
  ### INPUT CHECKS ###
  if (!is.data.frame(data)){
    stop("Data set should be of class 'data.frame'.")
  }
  for (i in cont_cols){
    stopifnot("Continuous variables should be of class 'numeric'." = (is.numeric(data[, i])))
  }
  if (sample_size < 1 | sample_size %% 1 != 0){
    stop("Sample size should be a positive integer.")
  }
  if (ntrees < 1 | ntrees %% 1 != 0){
    stop("Number of trees should be a positive integer.")
  }
  if (ndim %% 1 != 0 | ndim < 0 | ndim > length(cont_cols)){
    stop("Number of dimensions should be a positive integer, at most equal to the number of continuous columns.")
  }
  if (max_depth < 1 | max_depth %% 1 != 0){
    stop("Maximum depth should be a positive integer.")
  }
  if (seed_num < 1 | seed_num %% 1 != 0){
    stop("Seed number should be a positive integer.")
  }
  ### END OF CHECKS ###
  # Empty data frame to store outlyingness scores based on continuous variables
  outscorecontdf <- data.frame('Observation' = c(1:nrow(data)),
                               'Score' = rep(0, nrow(data)),
                               stringsAsFactors = FALSE)
  # EXTENDED ISOLATION FOREST
  # Function taken from isoforest package
  if (ndim == 0){
    ndim <- length(cont_cols)
  }
  isoforest <- isotree::isolation.forest(scale(data[, cont_cols]),
                                         sample_size = min(sample_size, nrow(data)),
                                         ntrees = ntrees,
                                         ndim = ndim,
                                         max_depth = max_depth,
                                         seed = seed_num)
  outscorecontdf[, 2] <- predict(isoforest, scale(data[, cont_cols]), type="score")
  cat('Outlyingness scores for continuous variables calculated.\n')
  return(outscorecontdf)
}
