#' Generate mixed data with marginal & joint outliers
#'
#' @description Function used to generate datasets with mixed-type variables and marginal & joint
#' outliers. The function returns a dataset of the dimensions provided with an additional column at the end,
#' stating whether each observation is an inlier, a single marginal, a combined marginal or a
#' joint outlier (corresponding to numbers 0, 1, 2 and 3, respectively).
#' Discrete variables are generated using quantile discretisation. The joint outliers are defined
#' by imposing an association between some of the discrete features and sets of continuous variables.
#' Only the linear, product and quotient designs are supported for now.
#'
#' @param n_obs Number of observations.
#' @param n_disc Number of discrete variables.
#' @param n_cont Number of continuous variables.
#' @param n_lvls Number of levels for discrete variables. If this is equal to an integer, all discrete variables will have the same number of levels. If some discrete variables have a different number of levels, a vector of length n_disc should be provided for the number of levels of each discrete level separately.
#' @param p_outs Proportion of observations in the data which are outliers (marginal or joint). This cannot exceed 0.2.
#' @param jp_outs Proportion of outliers which are joint. A value of 0 will return a data set with only marginal outliers, while a value of 1 will return a data set with only joint outliers.
#' @param assoc_target Indices of target discrete variables associated with sets of continuous features, on which joint outliers will be defined. This can be a single number if only one association exists in the data set, otherwise a vector indicating which discrete variables are associated with some continuous features.
#' @param assoc_vars Indices of sets of continuous variables associated with assoc_target. If there only exists one association in the data, this is a vector, otherwise it should be a list of vectors, the list having length equal to length(assoc_target).
#' @param assoc_type Type of association between discrete feature and a set of continuous variables; only "linear", "product" or "quotient" are supported. If assoc_vars is a vector of 1 element, argument is ignored. If multiple associations are present, a vector of association types should be provided for each association, or one type for all associations. The "linear" type cannot be given as input together with "product" or "quotient" due to a transformation that is applied on assoc_vars (except if the sets of assoc_vars are disjoint). Default is "linear".
#' @param seed_num Seed number (default value is 1).
#'
#' @return Data frame with mixed-type data and the specified number of observations and features of each type.
#' An additional column is added as an extra feature in the data set, indicating if an observation
#' is an inlier, a single marginal, a combined marginal or a joint outlier
#' (corresponding to values 0, 1, 2 and 3, respectively). Discrete variables are returned as factor class.
#' @export
#'
#' @examples data <- gen_marg_joint_data(n_obs = 1000, n_disc = 5, n_cont = 5, n_lvls = 3, p_outs = 0.05, jp_outs = 0.2, assoc_target = 1, assoc_vars = c(1, 2), assoc_type = "linear", seed_num = 1)
#' @examples data <- gen_marg_joint_data(n_obs = 3000, n_disc = 4, n_cont = 8, n_lvls = c(3, 4, 3, 4), p_outs = 0.10, jp_outs = 0.6, assoc_target = 2, assoc_vars = c(1, 2, 7), assoc_type = "product", seed_num = 1)
#' @examples data <- gen_marg_joint_data(n_obs = 5000, n_disc = 5, n_cont = 3, n_lvls = 4, p_outs = 0.20, jp_outs = 0.1, assoc_target = c(2, 5), assoc_vars = list(c(1, 2), c(2,3)), assoc_type = c("product", "quotient"), seed_num = 1)
gen_marg_joint_data <- function(n_obs, n_disc, n_cont, n_lvls,
                                p_outs, jp_outs, assoc_target, assoc_vars,
                                assoc_type = "linear", seed_num = 1){
  ### INPUT CHECKS ###
  if (seed_num < 1 | seed_num %% 1 != 0){
    stop("Seed number should be a positive integer.")
  }
  if (n_disc <= 0 | n_disc %% 1 != 0 | n_cont <= 0 | n_cont %% 1 != 0){
    stop("Number of discrete/continuous variables should be a positive integer.")
  }
  if (!is.numeric(p_outs) | !is.numeric(jp_outs)){
    stop("Proportion of (joint) outliers should be a real number.")
  }
  if (p_outs > 0.20 | p_outs < 0){
    stop("Proportion of outliers should be at most 0.2 and at least 0.")
  }
  if (jp_outs < 0 | jp_outs > 1){
    stop("Proportion of joint outliers should be between 0 and 1.")
  }
  if (n_obs < 1 | n_obs %% 1 != 0){
    stop("Number of observations should be a positive integer.")
  }
  if (sum(n_lvls > 1) != length(n_lvls) | sum(n_lvls %% 1 == 0) != length(n_lvls) | sum(n_lvls <= n_obs) != length(n_lvls)){
    stop("Number of levels should be an integer at least equal to 2 and at most equal to n_obs.")
  }
  if (length(n_lvls) > 1 & length(n_lvls) != n_disc){
    stop("Vector of number of levels should either be of unit length or of length equal to n_disc.")
  }
  if (sum(assoc_target <= n_disc) != length(assoc_target)){
    stop("Incorrect column index for association target variable.")
  }
  if (length(unique(assoc_target)) != length(assoc_target)){
    stop("Cannot define 2 associations with the same target.")
  }
  if (sum(assoc_type %in% c('linear', 'product', 'quotient')) != length(assoc_type)){
    stop("Only association types 'linear', 'product' and 'quotient' are supported.")
  }
  if (length(assoc_type) != 1 & length(assoc_type) != length(assoc_target)){
    stop("Length of association type should be either 1 or equal to the length of association targets.")
  }
  if (!is.list(assoc_vars)){
    assoc_vars <- list(assoc_vars)
  }
  if (length(assoc_target) > 1 & length(assoc_type)==1){
    assoc_type <- rep(assoc_type, length(assoc_target))
  }
  if (length(assoc_vars) != length(assoc_target)){
    stop("Length of association targets should be equal to length of list of association variables.")
  }
  for (i in 1:length(assoc_vars)){
    if (sum(assoc_vars[[i]] %% 1 == 0) != length(assoc_vars[[i]]) | sum(assoc_vars[[i]] > 0) != length(assoc_vars[[i]]) | sum(assoc_vars[[i]] <= n_cont) != length(assoc_vars[[i]]) | length(unique(assoc_vars[[i]])) != length(assoc_vars[[i]])){
      stop("Association variables should be unique positive integers, at most equal to n_cont.")
    }
  }
  # Check if overlapping association variables have linear & product/quotient association types.
  lin_type <- which(assoc_type == "linear")
  prod_quo_type <- setdiff(c(1:length(assoc_type)), lin_type)
  if (length(lin_type) > 0 & length(prod_quo_type) > 0){
    for (i in lin_type){
      for (j in prod_quo_type){
        if (length(intersect(assoc_vars[[i]], assoc_vars[[j]])) > 0){
          stop("Cannot have linear and product/quotient associations for sets of variables with common elements.")
        }
      }
    }
  }
  ### END OF CHECKS ###
  set.seed(seed_num)
  # Total number of variables
  n_var <- n_disc + n_cont
  # Generate covariance matrix
  sigma_mat <- clusterGeneration::genPositiveDefMat(dim = n_var,
                                                    covMethod = "unifcorrmat",
                                                    alphad = 5,
                                                    rangeVar = c(0.1, 5))
  dt <- mvtnorm::rmvnorm(n_obs, mean=rep(0, n_var), sigma = sigma_mat$Sigma)

  # Discretise variables
  for (i in 1:n_disc){
    ifelse(length(n_lvls) == 1,
           dt[, i] <- as.factor(cut(dt[, i], intv(dt[, i], n_lvls), labels = (1:n_lvls))),
           dt[, i] <- as.factor(cut(dt[, i], intv(dt[, i], n_lvls[i]), labels = (1:n_lvls[i]))))
  }
  # Add a column to the data frame indicating outlyingness
  dt <- as.data.frame(dt)
  dt[, (n_var+1)] <- 0
  # Time to define outliers
  n_outs <- floor(p_outs * nrow(dt))
  # Set number of marginal outliers
  n_marg <- floor(n_outs * (1-jp_outs))
  n_joint <- n_outs-n_marg
  # Randomly pick n_marg observations
  outs_marg <- sample(nrow(dt), n_marg)
  if (n_marg > 0){
    # For half of them, change their continuous values
    # We add or subtract 10 units from either 1 or more continuous variable
    # Then for the other half, we set 1 categorical variable value to a new level "5"
    for (i in 1:floor(n_marg/2)){
      # Randomly choose how many variables are affected
      j <- sample(c(1:(n_cont-1)), 1)
      aff_cols <- sample(seq(1 + n_disc, n_var), j)
      # Generate marginal outliers
      dt[outs_marg[i], aff_cols] <- dt[outs_marg[i], aff_cols]+15*(-1)^outs_marg[i]
      dt[outs_marg[i], (n_var+1)] <- 1
    }
    for (i in ((floor(n_marg/2)+1):n_marg)){
      # Randomly pick 1 categorical variable
      j <- sample(c(1:n_disc), 1)
      if (length(n_lvls) == 1){
        levels(dt[, j]) <- c(1:(n_lvls+1))
        dt[outs_marg[i], j] <- n_lvls + 1
        dt[outs_marg[i], (n_var+1)] <- 1
      } else {
        levels(dt[, j]) <- c(1:(n_lvls[j]+1))
        dt[outs_marg[i], j] <- n_lvls[j] + 1
        dt[outs_marg[i], (n_var+1)] <- 1
      }
    }
    # Combined marginal outliers
    both_marg_outs <- sample(x = outs_marg[1:floor(n_marg/2)],
                             size = sample.int(n = floor(n_marg/2), size = 1, prob=NULL),
                             prob = NULL)
    for (i in both_marg_outs){
      # Randomly pick 1 categorical variable
      j <- sample(c(1:n_disc), 1)
      if (length(n_lvls) == 1){
        levels(dt[i, j]) <- c(1:(n_lvls+1))
        dt[i, j] <- n_lvls + 1
        dt[i, (n_var+1)] <- 2
      } else {
        levels(dt[i, j]) <- c(1:(n_lvls[j]+1))
        dt[i, j] <- n_lvls[j] + 1
        dt[i, (n_var+1)] <- 2
      }
    }
  }
  cat('Marginal outliers generated.\n')
  # Then we define joint outliers
  # First we generate a data set with associations
  # Add the number of discrete features to get right indices for continuous vars associated
  for (i in 1:length(assoc_vars)){
    assoc_vars[[i]] <- assoc_vars[[i]] + n_disc
  }
  # Choose some values from the target discrete variable and change them
  outs_joint <- sample(setdiff(c(as.integer(row.names(dt))), outs_marg), n_joint)
  if (length(assoc_target)==1){
    outs_joint <- list(outs_joint)
  } else {
    outs_joint <- split(outs_joint, cut(seq_along(outs_joint), length(assoc_target), labels = FALSE))
  }
  for (k in 1:length(assoc_target)){
    gen_association <- gen_assoc(data = dt, n_var, assoc_target[k], assoc_vars[[k]], assoc_type[k])
    dt <- gen_association[[1]]
    qs <- gen_association[[2]]
    # Define joint outliers
    # We need to make sure none of these are on the boundary
    for (i in outs_joint[[k]]){
      row_inx <- i
      lvl <- dt[row_inx, assoc_target[k]]
      if (length(assoc_vars[[k]]) == 1){
        val <- dt[which(row.names(dt)==row_inx), assoc_vars[[k]]]
      } else if ((length(assoc_vars[[k]]) > 1) & (assoc_type[k] == "linear")){
        val <- dt[which(row.names(dt)==row_inx), assoc_vars[[k]][1]]
        for (j in 2:length(assoc_vars[[k]])){
          val <- val - dt[which(row.names(dt)==row_inx), assoc_vars[[k]][j]]
        }
      } else if ((length(assoc_vars[[k]]) > 1) & (assoc_type[k] == "product")){
        val <- dt[which(row.names(dt)==row_inx), assoc_vars[[k]][1]]
        for (j in 2:length(assoc_vars[k])){
          val <- val * dt[which(row.names(dt)==row_inx), assoc_vars[[k]][j]]
        }
      } else if ((length(assoc_vars[[k]]) > 1) & (assoc_type[k] == "quotient")){
        val <- dt[which(row.names(dt)==row_inx), assoc_vars[[k]][1]]
        for (j in 2:length(assoc_vars[[k]])){
          val <- val/dt[which(row.names(dt)==row_inx), assoc_vars[[k]][j]]
        }
      }
      new_lvl <- joint_outs_lvls(dt, lvl, val, qs, (n_var+1), assoc_target[k])[1]
      # Removes a small bug
      while (new_lvl==0){
        new_out <- sample(setdiff(c(as.integer(row.names(dt))), c(outs_marg, unname(unlist(outs_joint)))), 1)
        lvl <- dt[new_out, assoc_target[k]]
        if (length(assoc_vars[[k]]) == 1){
          val <- dt[which(row.names(dt)==new_out), assoc_vars[[k]]]
        } else if ((length(assoc_vars[[k]]) > 1) & (assoc_type[k] == "linear")){
          val <- dt[which(row.names(dt)==new_out), assoc_vars[[k]][1]]
          for (j in 2:length(assoc_vars[[k]])){
            val <- val - dt[which(row.names(dt)==new_out), assoc_vars[[k]][j]]
          }
        } else if ((length(assoc_vars[[k]]) > 1) & (assoc_type[k] == "product")){
          val <- dt[which(row.names(dt)==new_out), assoc_vars[[k]][1]]
          for (j in 2:length(assoc_vars[[k]])){
            val <- val * dt[which(row.names(dt)==new_out), assoc_vars[[k]][j]]
          }
        } else if ((length(assoc_vars[[k]]) > 1) & (assoc_type[k] == "quotient")){
          val <- dt[which(row.names(dt)==row_inx), assoc_vars[[k]][1]]
          for (j in 2:length(assoc_vars[[k]])){
            val <- val/dt[which(row.names(dt)==row_inx), assoc_vars[[k]][j]]
          }
        }
        new_lvl <- joint_outs_lvls(dt, lvl, val, qs, (n_var+1), assoc_target[k])[1]
        row_inx <- new_out
      }
      dt[row_inx, assoc_target[k]] <- new_lvl
      dt[row_inx, (n_var+1)] <- 3
    }
  }
  cat('Joint outliers generated.\n')
  dtframe <- dt
  cat_cols <- c(1:n_disc)
  for (i in cat_cols){
    dtframe[, i] <- as.factor(dtframe[, i])
  }
  cat('Data frame generated.\n')
  return(dtframe)
}
