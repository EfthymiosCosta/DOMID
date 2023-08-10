gen_assoc <- function(data, n_var, assoc_target, assoc_vars, assoc_type){
  n_lvls <- length(unique(data[which(data[, (n_var+1)]==0), assoc_target]))
  if (length(assoc_vars) == 1){
    qs <- c(min(data[which(data[, (n_var+1)]==0), assoc_vars]))
    for (i in 1:n_lvls){
      qs <- c(qs, quantile(data[which(!data[, (n_var+1)] %in% c(1,2)), assoc_vars], i/n_lvls))
    }
    for (i in row.names(data[which(data[, (n_var+1)]==0),])){
      # Set target discrete variable value based on the "rule"
      val <- data[which(row.names(data)==i), assoc_vars]
      # Find position where val lies
      new_lvl <- which(diff(sign(qs-val))!=0)
      data[which(row.names(data)==i), assoc_target] <- new_lvl_fun(new_lvl, val, qs)
    }
  } else if ((length(assoc_vars) > 1) & (assoc_type == "linear")){
    aux_vec <- data[which(data[, (n_var+1)]==0), assoc_vars[1]]
    for (i in 2:length(assoc_vars)){
      aux_vec <- aux_vec - data[which(data[, (n_var+1)]==0), assoc_vars[i]]
    }
    qs <- c(min(aux_vec))
    aux_vec <- data[which(!data[, (n_var+1)] %in% c(1,2)), assoc_vars[1]]
    for (i in 2:length(assoc_vars)){
      aux_vec <- aux_vec - data[which(!data[, (n_var+1)] %in% c(1,2)), assoc_vars[i]]
    }
    for (i in 1:n_lvls){
      qs <- c(qs, quantile(aux_vec, i/n_lvls))
    }
    for (i in row.names(data[which(data[, (n_var+1)]==0),])){
      # Set target discrete variable value based on the "rule"
      val <- data[which(row.names(data)==i), assoc_vars[1]]
      for (j in 2:length(assoc_vars)){
        val <- val - data[which(row.names(data)==i), assoc_vars[j]]
      }
      # Find position where val lies
      new_lvl <- which(diff(sign(qs-val))!=0)
      data[which(row.names(data)==i), assoc_target] <- new_lvl_fun(new_lvl, val, qs)
    }
  } else if ((length(assoc_vars) > 1) & (assoc_type == "product")){
    for (i in 1:length(assoc_vars)){
      data[which(data[, (n_var+1)]==0), assoc_vars[i]] <- abs(data[which(data[, (n_var+1)]==0), assoc_vars[i]])
    }
    aux_vec <- data[which(data[, (n_var+1)]==0), assoc_vars[1]]
    for (i in 2:length(assoc_vars)){
      aux_vec <- aux_vec * data[which(data[, (n_var+1)]==0), assoc_vars[i]]
    }
    qs <- c(min(aux_vec))
    aux_vec <- data[which(!data[, (n_var+1)] %in% c(1,2)), assoc_vars[1]]
    for (i in 2:length(assoc_vars)){
      aux_vec <- aux_vec * data[which(!data[, (n_var+1)] %in% c(1,2)), assoc_vars[i]]
    }
    for (i in 1:n_lvls){
      qs <- c(qs, quantile(aux_vec, i/n_lvls))
    }
    for (i in row.names(data[which(data[, (n_var+1)]==0),])){
      # Set target discrete variable value based on the "rule"
      val <- data[which(row.names(data)==i), assoc_vars[1]]
      for (j in 2:length(assoc_vars)){
        val <- val * data[which(row.names(data)==i), assoc_vars[j]]
      }
      # Find position where val lies
      new_lvl <- which(diff(sign(qs-val))!=0)
      data[which(row.names(data)==i), assoc_target] <- new_lvl_fun(new_lvl, val, qs)
    }
  } else if ((length(assoc_vars) > 1) & (assoc_type == "quotient")){
    for (i in 1:length(assoc_vars)){
      data[which(data[, (n_var+1)]==0), assoc_vars[i]] <- abs(data[which(data[, (n_var+1)]==0), assoc_vars[i]])
    }
    aux_vec <- data[which(data[, (n_var+1)]==0), assoc_vars[1]]
    for (i in 2:length(assoc_vars)){
      aux_vec <- aux_vec/data[which(data[, (n_var+1)]==0), assoc_vars[i]]
    }
    qs <- c(min(aux_vec))
    aux_vec <- data[which(!data[, (n_var+1)] %in% c(1,2)), assoc_vars[1]]
    for (i in 2:length(assoc_vars)){
      aux_vec <- aux_vec/data[which(!data[, (n_var+1)] %in% c(1,2)), assoc_vars[i]]
    }
    for (i in 1:n_lvls){
      qs <- c(qs, quantile(aux_vec, i/n_lvls))
    }
    for (i in row.names(data[which(data[, (n_var+1)]==0),])){
      # Set target discrete variable value based on the "rule"
      val <- data[which(row.names(data)==i), assoc_vars[1]]
      for (j in 2:length(assoc_vars)){
        val <- val/data[which(row.names(data)==i), assoc_vars[j]]
      }
      # Find position where val lies
      new_lvl <- which(diff(sign(qs-val))!=0)
      data[which(row.names(data)==i), assoc_target] <- new_lvl_fun(new_lvl, val, qs)
    }
  }
  return(list(data, qs))
}
