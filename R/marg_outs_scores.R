#' Detection of marginal outliers with given scores of outlyingness.
#'
#' @description
#' Function used to detect marginal outliers given scores of outlyingness. The function requires
#' the discrete and continuous scores provided to be data frames with 2 columns, the first column
#' being the observation number/index and the second column including the scores of outlyingness.
#' The matrix of contributions of the discrete variables to the discrete scores of outlyingness also
#' needs to be provided. This function can be used if the user wishes to use a different method for
#' calculating the scores of outlyingness for either the discrete or the continuous features.
#'
#' @param data Data frame of mixed-type data. Should be of class 'data.frame'.
#' @param disc_cols Column indices of discrete variables.
#' @param outscorediscdf Data frame with discrete scores of outlyingness.
#' Should be of dimension (nrow(data) x 2) and of class 'data.frame'. First column should
#' include the row/observation number and second column should include the
#' discrete scores of outlyingness.
#' @param outscorecontdf Data frame with continuous scores of outlyingness.
#' Should be of dimension (nrow(data) x 2) and of class 'data.frame'.
#' First column should include the row/observation number and second column should include
#' the continuous scores of outlyingness.
#' @param outscorediscdfcells Matrix of contributions of discrete variables to the discrete scores of
#' outlyingness. Should be of dimension (nrow(data) x length(disc_cols)) and of class 'data.frame'.
#' @return A list with 3 vectors; the vector "Discrete" includes the row indices
#' for the observations which are marginally outlying in the discrete space only (single
#' marginal outliers). The vector "Continuous" includes the row
#' indices for the observations which are marginally outlying in the continuous space only (only
#' single marginal outliers included). The vector "Combined" includes the row indices for
#' the combined marginal observations.
#' @export
#'
#' @examples dt <- gen_marg_joint_data(n_obs = 1000, n_disc = 5, n_cont = 5, n_lvls = 3, p_outs = 0.05, jp_outs = 0.2, assoc_target = c(1, 2), assoc_vars = list(c(1, 2), c(4,5)), assoc_type = c('linear', 'product'), seed_num = 1)
#' discrete_scores <- disc_scores(data = dt, disc_cols = c(1:5))
#' continuous_scores <- cont_scores(data = dt, cont_cols = c(6:10))
#' marg_outs_scores(data = dt, disc_cols = c(1:5), outscorediscdf = discrete_scores[[2]], outscorecontdf = continuous_scores, outscorediscdfcells = discrete_scores[[3]])
#'
marg_outs_scores <- function(data, disc_cols, outscorediscdf, outscorecontdf, outscorediscdfcells){
  ### INPUT CHECKS ###
  if (!is.data.frame(data)){
    stop("Data set should be of class 'data.frame'.")
  }
  for (i in disc_cols){
    stopifnot("Discrete variables should be of class 'factor'." = (is.factor(data[, i])))
  }
  stopifnot("Data set of discrete scores should be of class 'data.frame'." = is.data.frame(outscorediscdf)==TRUE)
  stopifnot("Data set of continuous scores should be of class 'data.frame'." = is.data.frame(outscorecontdf)==TRUE)
  stopifnot("Matrix of contributions should be of class 'data.frame'." = is.data.frame(outscorediscdfcells)==TRUE)
  stopifnot("Incorrect number of rows for data frame of discrete scores." = nrow(outscorediscdf) == nrow(data))
  stopifnot("Incorrect number of rows for data frame of continuous scores." = nrow(outscorecontdf) == nrow(data))
  stopifnot("Incorrect number of rows for matrix of contributions." = nrow(outscorediscdfcells) == nrow(data))
  if (ncol(outscorediscdf) != 2){
    stop("Incorrect number of columns for data frame of discrete scores (should be 2).")
  }
  if (ncol(outscorecontdf) != 2){
    stop("Incorrect number of columns for data frame of continuous scores (should be 2).")
  }
  if (ncol(outscorediscdfcells) != length(disc_cols)){
    stop("Incorrect number of columns for matrix of contributions.")
  }
  ### END OF CHECKS ###
  # First construct the thresholds data frame for unlikely itemsets of length 1
  thresh_1lvl <- data.frame('CatVariable'=numeric(),
                            'Thresh'=numeric(),
                            stringsAsFactors = FALSE)
  for (cat_col in disc_cols){
    combs <- length(unique(data[,cat_col]))
    s <- floor(as.numeric(nrow(data) * DescTools::MultinomCI(rep(nrow(data)/combs, combs), conf.level=0.99)[1,2]))
    thresh_1lvl <- rbind(thresh_1lvl, data.frame('CatVariable' = cat_col, 'Thresh' = s))
  }
  # K-Means approach on our data frame
  x <- outscorediscdf[,2]
  K_max <- length(unique(x))
  if (K_max >1){
    C_0_size <- c()
    for (K in c(1:K_max)){
      kmeans_outscore <- Ckmeans.1d.dp::Ckmeans.1d.dp(x, k=K)
      cluster_alloc <- kmeans_outscore$cluster
      C_0_inx <- unique(cluster_alloc[which(x==0)])
      C_0_size <- c(C_0_size, sum(cluster_alloc==C_0_inx))
    }
    # Find optimal K
    tab <- as.data.frame(table(C_0_size))
    tab[,1] <- as.numeric(as.character(tab[,1]))
    ifelse(length(tab[which(tab[,2]==max(tab[,2])),1]) == 1,
           K_final <- which(C_0_size==max(tab[which(tab[,2]==max(tab[,2])),1]))[1],
           K_final <- which(C_0_size==min(tab[which(tab[,2]==max(tab[,2])),1]))[1])

    # Get clusters & discard points not in cluster with 0 discrete scores
    # Further discard observations with infrequent itemsets of length 1
    # Make sure condition of size is checked
    K_val <- K_final
    if (K_final == 1){
      K_val <- K_max
      repeat{
        kmeans_outscore <- Ckmeans.1d.dp::Ckmeans.1d.dp(x, k=K_val)
        cluster_alloc <- kmeans_outscore$cluster
        C_0_inx <- unique(cluster_alloc[which(x==0)])
        disc_pts_cat <- which(cluster_alloc!=C_0_inx)
        disc_pts_cat <- unique(disc_pts_cat)
        # Use Chebyshev with k=2
        cheb <- abs(sort(unique(scale(outscorediscdf[,2]))))
        cheb_thresh <- sort(unique(outscorediscdf[,2]))[min(which(diff(cheb)>1))+1]
        disc_pts_cat <- setdiff(disc_pts_cat, outscorediscdf[which(outscorediscdf[,2] < cheb_thresh),1])
        disc_pts_cat <- c(disc_pts_cat,
                          which(apply(outscorediscdfcells, FUN=function(c)sum(c!=0)==1, 1)))
        # Remove itemsets of length 1 that may not be very infrequent
        disc_pts_cat <- setdiff(disc_pts_cat, rm_likely_1lvl(disc_pts_cat, outscorediscdfcells, thresh_1lvl))
        disc_pts_cat <- unique(disc_pts_cat)
        K_val <- K_val - 1
        # Check condition of size
        if (length(disc_pts_cat) < floor(0.22*nrow(outscorediscdf))) break
      }
    } else if (K_final==2 & nrow(tab)==2){
      kmeans_outscore <- Ckmeans.1d.dp::Ckmeans.1d.dp(x, k=K_val)
      cluster_alloc <- kmeans_outscore$cluster
      C_0_inx <- unique(cluster_alloc[which(x==0)])
      disc_pts_cat <- which(cluster_alloc!=C_0_inx)
      disc_pts_cat <- c(disc_pts_cat,
                        which(apply(outscorediscdfcells, FUN=function(c)sum(c!=0)==1, 1)))
      disc_pts_cat <- unique(disc_pts_cat)
    } else {
      repeat{
        kmeans_outscore <- Ckmeans.1d.dp::Ckmeans.1d.dp(x, k=K_val)
        cluster_alloc <- kmeans_outscore$cluster
        C_0_inx <- unique(cluster_alloc[which(x==0)])
        disc_pts_cat <- which(cluster_alloc!=C_0_inx)
        disc_pts_cat <- unique(disc_pts_cat)
        # Use Chebyshev with k=2
        cheb <- abs(sort(unique(scale(outscorediscdf[,2]))))
        cheb_thresh <- sort(unique(outscorediscdf[,2]))[min(which(diff(cheb)>1))+1]
        disc_pts_cat <- setdiff(disc_pts_cat, outscorediscdf[which(outscorediscdf[,2] < cheb_thresh),1])
        disc_pts_cat <- c(disc_pts_cat,
                          which(apply(outscorediscdfcells, FUN=function(c)sum(c!=0)==1, 1)))
        # Remove itemsets of length 1 that may not be very infrequent
        disc_pts_cat <- setdiff(disc_pts_cat, rm_likely_1lvl(disc_pts_cat, outscorediscdfcells, thresh_1lvl))
        disc_pts_cat <- unique(disc_pts_cat)
        # Look at case of having scores over 1
        K_val <- K_val - 1
        # Check condition of size
        if (length(disc_pts_cat) < floor(0.22*nrow(outscorediscdf))) break
        if (K_val == 0){
          kmeans_outscore <- Ckmeans.1d.dp::Ckmeans.1d.dp(x, k=2)
          cluster_alloc <- kmeans_outscore$cluster
          C_0_inx <- unique(cluster_alloc[which(x==0)])
          disc_pts_cat <- which(cluster_alloc!=C_0_inx)
          disc_pts_cat <- unique(disc_pts_cat)
          if (length(disc_pts_cat) > floor(0.22*nrow(outscorediscdf))){
            disc_pts_cat <- c()
          }
          break
        }
      }
    }
  } else {
    disc_pts_cat <- c()
  }
  # Continuous scores
  # Start by discarding points in C_0
  sorted_diffs_cont <- diff(sort(outscorecontdf[-disc_pts_cat, 2]))
  # If no outliers, most scores should be below 0.65 (if not all)
  if (all(outscorecontdf[-disc_pts_cat, 2] < 0.65)){
    disc_pts_cont <- c()
  } else {
    # Use Chebyshev's Inequality for outlier detection in differences of sorted scores
    # Start with 1:10, then expand up to 20 if necessary
    large_gaps <- c()
    for (k in c(2:20)){
      large_gaps <- c(large_gaps,
                      length(sorted_diffs_cont[which((sorted_diffs_cont-mean(sorted_diffs_cont))>=k*sd(sorted_diffs_cont))]))
    }
    gaps <- as.data.frame(table(large_gaps))
    gaps[,1] <- as.numeric(as.character(gaps[,1]))
    max_freq <- gaps[which(gaps[,2]==max(gaps[,2])),2][1]
    ifelse(max(gaps[which(gaps[,2]==max(gaps[,2])),1]) == large_gaps[1]  & length(gaps[which(gaps[,2]==max(gaps[,2])),1])>1,
           n_gaps <- max(setdiff(gaps[which(gaps[,2]==max(gaps[,2])),1], large_gaps[1])),
           n_gaps <- max(gaps[which(gaps[,2]==max(gaps[,2])),1]))
    if (max_freq>=3 & n_gaps > 0){
      k_chebyshev <- min(which(large_gaps==n_gaps))+1
    } else if (n_gaps==0){
      ifelse(sum(large_gaps!=0)!=0,
             k_chebyshev <- max(which(large_gaps!=0)) + 1,
             k_chebyshev <- 20)
    } else {
      k_chebyshev <- 20
    }

    outlying_diffs <- sorted_diffs_cont[which((sorted_diffs_cont-mean(sorted_diffs_cont))>=k_chebyshev*sd(sorted_diffs_cont))]
    inxs <- which(sorted_diffs_cont %in% outlying_diffs)
    cont_thresh_vec <- sort(outscorecontdf[-disc_pts_cat, 2])[c(inxs)]
    # Filter out scores below 0.4
    cont_thresh_vec <- cont_thresh_vec[which(cont_thresh_vec>0.4)]
    for (i in 1:length(cont_thresh_vec)){
      disc_pts_cont <- which(outscorecontdf[-disc_pts_cat, 2]>cont_thresh_vec[i])
      if (length(disc_pts_cat)+length(disc_pts_cont) <= floor(0.22*nrow(outscorediscdf))) break
    }
    disc_pts_cont <- outscorecontdf[which(outscorecontdf[,2] > cont_thresh_vec[i]),1]
  }
  disc_pts <- c(disc_pts_cat, disc_pts_cont)
  disc_pts <- unique(disc_pts)
  cat('Marginal outliers detected.\n')
  return(list("Discrete" = setdiff(disc_pts_cat, disc_pts_cont),
              "Continuous" = setdiff(disc_pts_cont, disc_pts_cat),
              "Combined" = intersect(disc_pts_cat, disc_pts_cont)))
}
