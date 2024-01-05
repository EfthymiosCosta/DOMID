#' Detecting associations between a discrete and a set of continuous features.
#'
#' @param data Data set of observations, should be of class 'data.frame' with mixed-type data.
#' @param marginals Vector of row indices for marginal outliers detected. These are excluded from the analysis.
#' @param target_inx Index of target discrete variable, should be of class 'factor'.
#' @param pred_inx Indices of predictor continuous variables, should be of class 'numeric'.
#' @param delta Proportion of nearest neighbours considered. Should be a number in the range (0, 0.5], default value is 0.50.
#' @param mink_order Order of Minkowski distance. The default value of 1 returns the L1 norm (Manhattan distance).
#' @param alpha1 Significance level of Kruskal Wallis H test used; default value is 1e-3.
#' @param alpha2 Significance level for the chi-square goodness of fit test used for each class; default value is 1e-1.
#'
#' @return A vector of the predictor indices associated with the target discrete feature of interest. If NULL,
#' no association has been detected.
#' @export
#' @examples
#' \dontrun{dt <- DOMID::gen_marg_joint_data(n_obs = 1000,
#'                                            n_disc = 5,
#'                                            n_cont = 10,
#'                                            n_lvls = 4,
#'                                            p_outs = 0.20,
#'                                            jp_outs = 0.80,
#'                                            assoc_target = 1,
#'                                            assoc_var = c(3, 4, 10),
#'                                            assoc_type = "quotient",
#'                                            seed_num = 1)
#' disc_scores <- DOMID::disc_scores(data = dt, disc_cols = c(1:5), alpha = 0.01, MAXLEN = 0)
#' cont_scores <- DOMID::cont_scores(data = dt, cont_cols = c(6:15), sample_size = 256,
#'                                   ntrees = 500, ndim = 0, max_depth = 100, seed_num = 1)
#' marginals <- DOMID::marg_outs_scores(data = dt,
#'                                      disc_cols = c(1:5),
#'                                      outscorediscdf = disc_scores[[2]],
#'                                      outscorecontdf = cont_scores,
#'                                      outscorediscdfcells = disc_scores[[3]],
#'                                      alpha = 0.01,
#'                                      rho = 0.2,
#'                                      epsilon = 0.02)
#' marginals <- unique(unlist(marginals))
#' DOMID::assoc_detect(data = dt,
#'                     marginals = marginals,
#'                     target_inx = 1,
#'                     pred_inx = c(6:15),
#'                     delta = 0.10)
#' }
#'
assoc_detect <- function(data, marginals, target_inx, pred_inx, delta = 0.50, mink_order = 1, alpha1 = 1e-3, alpha2 = 1e-1){
  ### INPUT CHECKS ###
  if (!is.data.frame(data)){
    stop("Data set should be of class 'data.frame'.")
  }
  if (length(target_inx) > 1){
    stop("Only one target (discrete) variable is allowed.")
  }
  stopifnot("Target (discrete) variable should be of class 'factor'." = (is.factor(data[, target_inx])))
  for (i in pred_inx){
    stopifnot("Predictor variables should be of class 'numeric'." = (is.numeric(data[, i])))
  }
  stopifnot("Indices of marginal outliers should be unique." = length(unique(marginals)) == length(marginals))
  stopifnot("Indices of marginal outliers should be unique integer values from 1 up to the number of observations in the data." = sum(marginals %in% c(1:nrow(data))) == length(marginals))
  stopifnot("alpha1 should be of class 'numeric'." = is.numeric(alpha1))
  stopifnot("alpha2 should be of class 'numeric'." = is.numeric(alpha2))
  stopifnot("delta should be of class 'numeric'." = is.numeric(delta))
  stopifnot("Minkowski order should be of class 'numeric'." = is.numeric(mink_order))
  if (length(alpha1) > 1){
    stop("alpha1 should be of unit length.")
  }
  if (length(alpha2) > 1){
    stop("alpha2 should be of unit length.")
  }
  if (length(delta) > 1){
    stop("delta should be of unit length.")
  }
  if (length(mink_order) > 1){
    stop("Minkowski order should be of unit length.")
  }
  if (alpha1 <= 0 | alpha1 > 1){
    stop("alpha1 should be positive.")
  }
  if (alpha2 <= 0 | alpha2 > 1){
    stop("alpha2 should be positive.")
  }
  if (delta <= 0 | delta > 0.5){
    stop("delta should be positive and at most equal to 0.5.")
  }
  if (mink_order <= 0){
    stop("Minkowski order should be positive.")
  }
  ### END OF CHECKS ###
  cat('Checking discrete variable', target_inx, '\n')
  if (length(marginals) > 0){
    dt_cleaned <- data[-marginals, ]
  } else {
    dt_cleaned <- data
  }
  # Kruskal Wallis test
  vars <- c()
  for (var in pred_inx){
    if (stats::kruskal.test(dt_cleaned[, var] ~ dt_cleaned[, target_inx], data = dt_cleaned)$p.value < alpha1){
      vars <- c(vars, var)
    }
  }
  if (length(vars) == 0){
    cat('No association found between discrete variable', target_inx, 'and continuous variables.\n')
    return(c())
  } else {
    aux_back <- FALSE
    # Start with 1 dimension using Multinomial testing
    classes_prob <- as.vector(table(dt_cleaned[, target_inx])/sum(table(dt_cleaned[, target_inx])))
    levels_num <- length(which(classes_prob > 0))
    # Remove classes with 0 probability caused by factors
    zero_classes <- which(classes_prob == 0)
    if (length(zero_classes) > 0){
      classes_prob <- classes_prob[-zero_classes]
    }
    best_subset <- c()
    best_subset_list <- list()
    best_p_prod <- c()
    prod_p_vals <- c()
    for (subset in vars){
      p_vals <- c()
      for (class in c(1:levels_num)){
        dt_cleaned_1 <- dt_cleaned[which(dt_cleaned[, target_inx]==class), subset]
        test_dist_mat <- as.matrix(stats::dist(dt_cleaned_1, method='minkowski', p=mink_order))
        centre_pt <- which.min(Biobase::rowMedians(test_dist_mat))
        # We get index from original data set
        labels <- which(dt_cleaned[, target_inx]==class)
        inx <- labels[centre_pt]
        # Obtain distances using relevant distance metric
        dist_mat_full <- as.matrix(stats::dist(dt_cleaned[, subset], method='manhattan'))
        # Define number of neighbours
        num_neighbours <- ceiling(delta*nrow(dt_cleaned[which(dt_cleaned[, target_inx]==class), ]))
        closest_pts <- order(dist_mat_full[inx, ], decreasing = FALSE)[2:(num_neighbours+1)]
        classes_freq <- as.vector(table(dt_cleaned[closest_pts, target_inx]))
        if (length(zero_classes) > 0){
          classes_freq <- classes_freq[-zero_classes]
        }
        p_vals <- c(p_vals, stats::chisq.test(classes_freq, p = classes_prob, simulate.p.value = FALSE)$p.value)
      }
      if (all(sort(p_vals) < alpha2/(levels_num-order(p_vals)+1))){
        prod_p_vals <- c(prod_p_vals, sum(log(p_vals)))
        best_subset <- c(best_subset, subset)
      }
    }
    # Store best 1-dimensional subset (if any)
    if (length(prod_p_vals) > 0){
      best_p_prod <- c(best_p_prod, min(prod_p_vals))
      best_subset <- best_subset[which.min(prod_p_vals)]
      best_subset_list[[length(best_subset_list)+1]] <- best_subset
    }
    # This is to avoid errors later
    best_superset <- c()
    best_superset_list <- list()
    # Proceed with backward elimination if more than 2 variables detected
    if (length(vars) > 1){
      lengths <- length(vars)
      # Power set of continuous columns
      power_set <- rje::powerSet(x = vars)
      power_set <- power_set[sapply(power_set, length) >= 2]
      # Products of p-values & best subsets
      best_subset <- c()
      while (lengths >= 2){
        # Store product of p-values
        prod_p_vals <- c()
        association_subs <- list()
        if (lengths == length(vars)){
          sub_list <- power_set[sapply(power_set, length) == lengths]
        } else {
          sub_list <- power_set[sapply(power_set, length) == lengths & sapply(power_set, FUN = function(x) all(x %in% best_subset))]
        }
        for (subset in sub_list){
          # Rates
          p_vals <- c()
          for (class in c(1:levels_num)){
            dt_cleaned_1 <- dt_cleaned[which(dt_cleaned[, target_inx]==class), subset]
            robpca_test <- rospca::robpca(x = dt_cleaned_1, k = ncol(dt_cleaned_1), alpha = 0.5, ndir=5000)
            robpca_rotated_dt <- as.matrix(dt_cleaned[, subset]) %*% robpca_test$loadings
            test_dist_mat <- dist_fun(robpca_rotated_dt, 1/robpca_test$eigenvalues, mink_order)
            centre_pt <- which.min(Biobase::rowMedians(test_dist_mat[which(dt_cleaned[, target_inx] == class),
                                                                     which(dt_cleaned[, target_inx] == class)]))
            # We get index from original data set
            inx <- row.names(dt_cleaned_1)[centre_pt]
            inx_position <- match(inx, row.names(dt_cleaned))
            # Define number of neighbours
            num_neighbours <- ceiling(delta*nrow(dt_cleaned[which(dt_cleaned[, target_inx]==class), ]))
            closest_pts <- order(test_dist_mat[inx_position, ], decreasing = FALSE)[2:(num_neighbours+1)]
            classes_freq <- as.vector(table(dt_cleaned[closest_pts, target_inx]))
            if (length(zero_classes) > 0){
              classes_freq <- classes_freq[-zero_classes]
            }
            p_vals <- c(p_vals, stats::chisq.test(classes_freq, p = classes_prob, simulate.p.value = FALSE)$p.value)
          }
          if (all(p_vals < alpha2/(levels_num-rank(p_vals)+1))){
            prod_p_vals <- c(prod_p_vals, sum(log(p_vals)))
            association_subs[[length(association_subs)+1]] <- subset
          }
        }
        if (length(prod_p_vals)==0){
          if (lengths == length(vars) & lengths > 2){
            aux_back <- TRUE
          }
          break
        } else {
          best_subset <- association_subs[[which.min(prod_p_vals)]]
          best_subset_list[[length(best_subset_list)+1]] <- best_subset
          best_p_prod <- c(best_p_prod, min(prod_p_vals))
          lengths <- lengths - 1
        }
      }
      # Start with 2 variables if multiple ones failed
      if (aux_back){
        lengths <- 2
        # Power set of continuous columns
        power_set <- rje::powerSet(x = vars)
        power_set <- power_set[sapply(power_set, length) >= 2]
        # Product of p-values & best supersets
        if (length(best_subset_list) > 0){
          best_superset_list[[1]] <- best_subset_list[[1]]
        }
        best_superset <- c()
        while (lengths <= length(vars)){
          # Store prod p_vals
          prod_p_vals <- c()
          association_subs <- list()
          if (lengths == 2){
            sub_list <- power_set[sapply(power_set, length) == lengths]
          } else {
            sub_list <- power_set[sapply(power_set, length) == lengths & sapply(power_set, FUN = function(x) all(best_superset %in% x))]
          }
          for (subset in sub_list){
            # Rates
            p_vals <- c()
            for (class in c(1:levels_num)){
              dt_cleaned_1 <- dt_cleaned[which(dt_cleaned[, target_inx]==class), subset]
              robpca_test <- rospca::robpca(x = dt_cleaned_1, k = ncol(dt_cleaned_1), alpha = 0.5, ndir=5000)
              robpca_rotated_dt <- as.matrix(dt_cleaned[, subset]) %*% robpca_test$loadings
              test_dist_mat <- dist_fun(robpca_rotated_dt, 1/robpca_test$eigenvalues, mink_order)
              centre_pt <- which.min(Biobase::rowMedians(test_dist_mat[which(dt_cleaned[, target_inx] == class),
                                                                       which(dt_cleaned[, target_inx] == class)]))
              # We get index from original data set
              inx <- row.names(dt_cleaned_1)[centre_pt]
              inx_position <- match(inx, row.names(dt_cleaned))
              # Define number of neighbours
              num_neighbours <- ceiling(delta*nrow(dt_cleaned[which(dt_cleaned[, target_inx]==class), ]))
              closest_pts <- order(test_dist_mat[inx_position, ], decreasing = FALSE)[2:(num_neighbours+1)]
              classes_freq <- as.vector(table(dt_cleaned[closest_pts, target_inx]))
              if (length(zero_classes) > 0){
                classes_freq <- classes_freq[-zero_classes]
              }
              p_vals <- c(p_vals, stats::chisq.test(classes_freq, p = classes_prob, simulate.p.value = FALSE)$p.value)
            }
            if (all(p_vals < (1e-1)/(levels_num-rank(p_vals)+1))){
              prod_p_vals <- c(prod_p_vals, sum(log(p_vals)))
              association_subs[[length(association_subs)+1]] <- subset
            }
          }
          if (length(prod_p_vals)==0){
            break
          } else {
            best_superset <- association_subs[[which.min(prod_p_vals)]]
            best_superset_list[[length(best_superset_list)+1]] <- best_superset
            best_p_prod <- c(best_p_prod, min(prod_p_vals))
            lengths <- lengths + 1
          }
        }
      }
    }
    if ((length(best_subset_list) > 0 | length(best_superset_list) > 0)){
      if (length(best_superset_list) > 0){
        association_subset <- best_superset_list[[which.min(best_p_prod)]]
      } else {
        association_subset <- best_subset_list[[which.min(best_p_prod)]]
      }
      cat('Association detected between discrete variable', target_inx,
          'with continuous variables:', association_subset, '\n')
      return(association_subset)
    } else {
      cat('No association found between discrete variable', target_inx, 'and continuous variables.\n')
      return(NA)
    }
  }
}
