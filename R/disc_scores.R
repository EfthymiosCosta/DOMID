#' Discrete scores of outlyingness
#'
#' @description
#' Function used to calculate the discrete scores of outlyingness. The method is an extension of the
#' ODMAD algorithm of Koufakou & Georgiopoulos (2010).
#' @param data Data frame including discrete variables on which outlier detection will be implemented. Should be of class 'data.frame'.
#' @param disc_cols Column indices for discrete variables, these columns should be of factor class.
#'
#' @return A list with 3 elements, the first being MAXLEN, the 2nd being the discrete scores of outlyingness and the 3rd being the matrix of contributions of each discrete variable to the discrete score of each observation.
#' @export
#'
#' @examples dt <- gen_marg_joint_data(n_obs = 1000, n_disc = 5, n_cont = 5, n_lvls = 3, p_outs = 0.05, jp_outs = 0.2, assoc_target = c(1, 2), assoc_vars = list(c(1, 2), c(4,5)), assoc_type = c('linear', 'product'), seed_num = 1)
#' disc_scores(data = dt, disc_cols = c(1:5))
disc_scores <- function(data, disc_cols){
  ### INPUT CHECKS ###
  if (!is.data.frame(data)){
    stop("Data set should be of class 'data.frame'.")
  }
  for (i in disc_cols){
    stopifnot("Discrete variables should be of class 'factor'." = (is.factor(data[, i])))
  }
  if (length(disc_cols)==1){
    data[, disc_cols] <- as.data.frame(data[, disc_cols])
  }
  ### END OF CHECKS ###
  # Get all power sets up to length MAXLEN
  # For MAXLEN we need to make sure that the threshold value s >= 2
  # In order to do that, we take combinations of variables and calculate s
  # Until we achieve s < 2
  nlevelsvec <- c()
  for (i in disc_cols){
    nlevelsvec <- c(nlevelsvec, length(levels(data[,i])))
  }
  if (length(disc_cols)==1){
    MAXLEN <- 1
  } else {
    for (i in 1:length(disc_cols)){
      combs <- prod(sort(nlevelsvec, decreasing = TRUE)[1:i])
      s <- floor(as.numeric(nrow(data) * DescTools::MultinomCI(rep(nrow(data)/combs, combs), conf.level=0.99)[1,2]))
      if (s < 2){
        MAXLEN <- i-1
        break
      } else {
        MAXLEN <- i
      }
    }
  }
  # Get power set
  powerset_test <- rje::powerSet(disc_cols, MAXLEN)
  # Find U values
  corr_mat_cat <- matrix(rep(0,length(disc_cols)^2), nrow=length(disc_cols))
  for (i in c(1:length(disc_cols))){
    for (j in c(1:length(disc_cols))){
      corr_mat_cat[i,j] <- DescTools::UncertCoef(data[, disc_cols[i]], data[, disc_cols[j]], direction='column')
    }
  }
  diag(corr_mat_cat) <- rep(0, length(disc_cols))

  # We then find the values exceeding u_upper
  corr_cat_vars <- matrix(data=c(NA,NA), ncol=2)
  for (i in 1:length(disc_cols)){
    for (j in setdiff(c(1:length(disc_cols)), i)){
      u_upper <- mean(replicate(50, u_upper_fun(data[, disc_cols[i]], data[, disc_cols[j]])))
      if (corr_mat_cat[i,j] > u_upper){
        corr_cat_vars <- rbind(corr_cat_vars, c(i,j))
      }
    }
  }
  corr_cat_vars <- matrix(corr_cat_vars[-1,], ncol=2)

  # Find elements of power set to be removed
  powerset_rm_inx <- c()
  # Check if we have any significant correlations
  if (nrow(corr_cat_vars) > 0){
    for (i in 1:nrow(corr_cat_vars)){
      powerset_rm_inx <- c(powerset_rm_inx, which(sapply(powerset_test, FUN=function(X) all(as.numeric(corr_cat_vars[i,]) %in% X))))
    }
    # The above gives duplicates which we don't need
    powerset_rm_inx <- unique(powerset_rm_inx)
    # Remove combinations of variables highly correlated
    powerset_test <- powerset_test[-powerset_rm_inx]
  }
  cat('Power set object created. \n')

  # Empty list to store outlier scores data frames
  outlierdfs_list <- list()

  # create corresponding list of data frames with sequences for each data point
  for (i in disc_cols){
    data[,i] <- as.numeric(data[,i])
  }

  cat('Pre-processing done. \n')

  # create corresponding list of data frames with sequences for each data point
  seqs <- list()
  for (i in 1:MAXLEN){
    inxs <- which(sapply(X=powerset_test, FUN=length)==i)
    for (j in inxs){
      nam <- "vars"
      for (k in 1:length(powerset_test[[j]])){
        nam <- paste(nam, powerset_test[[j]][k], sep = "_")
      }
      dfseq <- data.frame('Observation'=character(),
                          'Sequence'=character(),
                          stringsAsFactors = FALSE)
      for (k in 1:nrow(data)){
        row <- data[k, powerset_test[[j]]]
        rownam <- character()
        for (l in 1:length(row)){
          ifelse(l==1, rownam <- paste0(rownam, row[l]), rownam <- paste(rownam, row[l], sep="_"))
        }
        dfseq <- rbind(dfseq, data.frame('Observation'=k,
                                         'Sequence'=rownam))
      }
      seqs[[nam]] <- assign(nam, dfseq)
    }
  }
  ### GOOD UP TO HERE!
  cat('Sequences list created. \n')

  dfs <- list()
  # List with infrequent items and powersets
  infreq_list <- list()
  for (i in 1:MAXLEN){
    inxs <- which(sapply(X=powerset_test, FUN=length)==i)
    for (j in inxs){
      nam <- "df"
      # Setting threshold value s
      combs <- 1
      for (k in 1:length(powerset_test[[j]])){
        nam <- paste(nam, powerset_test[[j]][k], sep="_")
        combs <- combs*length(unique(data[,powerset_test[[j]][k]]))
      }
      s <- floor(as.numeric(nrow(data) * DescTools::MultinomCI(rep(nrow(data)/combs, combs), conf.level=0.99)[1,2]))
      #if (s < 2){
      #  s <- 2
      #}
      df <- data.frame('Sequence'=character(),
                       'Count'=integer(),
                       'Frequent'=logical(),
                       stringsAsFactors = FALSE)
      if (i==1){
        dt <- data[, powerset_test[[j]]]
        tab <- table(dt)
        for (k in 1:length(tab)){
          df <- rbind(df, data.frame('Sequence'=names(tab)[k],
                                     'Count'=as.numeric(tab)[k],
                                     'Frequent'=as.numeric(tab)[k]>=s))
          if (as.numeric(tab)[k]<s){
            infreq_list[[length(infreq_list)+1]] <- list("Variables"=powerset_test[[j]],
                                                         "Sequence"=names(tab)[k])
          }
        }
      } else {
        rows <- c()
        if (length(infreq_list)>0){
          for (k in 1:length(infreq_list)){
            if (rje::is.subset(infreq_list[[k]]$Variables, powerset_test[[j]])){
              ifelse(length(infreq_list[[k]]$Variables)>1, {
                rows <- c(rows,
                          which(sapply(1:nrow(data),
                                       FUN = function(i) check_vecs_equal(data[i, infreq_list[[k]]$Variables],
                                                                          vec2=as.numeric(strsplit(infreq_list[[k]]$Sequence, split="_")[[1]])))==length(infreq_list[[k]]$Variables)))
              }, {
                rows <- c(rows,
                          which(data[, infreq_list[[k]]$Variables]==infreq_list[[k]]$Sequence))
              })
            }
          }
        }
        ifelse(length(rows)>0, dt <- data[-rows,powerset_test[[j]]], dt <- data[,powerset_test[[j]]])
        for (k in 1:nrow(dt)){
          row <- dt[k,]
          rownam <- character()
          for (l in 1:length(row)){
            ifelse(l==1, rownam <- paste0(rownam, row[l]), rownam <- paste(rownam, row[l], sep="_"))
          }
          # Save sequences based on power set, with their support
          ifelse(rownam %in% df$Sequence,
                 {row_inx <- which(df$Sequence==rownam, arr.ind=TRUE)
                 df[row_inx, 2] <- df[row_inx, 2]+1},
                 {newrow <- data.frame('Sequence'=rownam,
                                       'Count'=1,
                                       'Frequent'=FALSE)
                 df <- rbind(df, newrow)})
        }
        for (k in 1:nrow(df)){
          ifelse(df[k,2]>=s, df[k,3] <- TRUE, infreq_list[[length(infreq_list)+1]] <- list("Variables"=powerset_test[[j]],
                                                                                           "Sequence"=df[k,1]))
        }
      }
      dfs[[nam]] <- assign(nam, df)
    }
  }

  cat('List with counts and frequencies created successfully. \n')

  # Save outlyingness scores based on categorical variables
  outscoredf <- data.frame('Observation'=integer(),
                           'Score'=double(),
                           stringsAsFactors = FALSE)

  # Save cell-wise outlyingness scores
  outscoredfcells <- as.data.frame(matrix(rep(0, nrow(data)*length(disc_cols)),
                                          nrow = nrow(data)))
  colnames(outscoredfcells) <- colnames(data[, disc_cols])
  for (i in 1:nrow(data)){
    score <- 0
    count <- 1
    for (j in 1:MAXLEN){
      inxs <- which(sapply(X=powerset_test, FUN=length)==j)
      for (k in inxs){
        row <- data[i,powerset_test[[k]]]
        rownam <- character()
        for (l in 1:length(row)){
          ifelse(l==1, rownam <- paste0(rownam, row[l]), rownam <- paste(rownam, row[l], sep="_"))
        }
        row_inx <- which(dfs[[count]]$Sequence==rownam, arr.ind=TRUE)
        if (length(row_inx)>0){
          add_score <- (!dfs[[count]][row_inx, 3])/(length(row)^2*dfs[[count]][row_inx, 2])
          score <- score + add_score
          if (length(disc_cols) > 1){
            for (l in powerset_test[[k]]){
              outscoredfcells[i, match_numeric(l, disc_cols)] <- outscoredfcells[i, match_numeric(l, disc_cols)] + add_score/length(powerset_test[[k]])
            }
          }
        }
        count <- count+1
      }
    }
    score_row <- data.frame('Observation'=i,
                            'Score'=score)
    outscoredf <- rbind(outscoredf, score_row)
  }
  if (length(disc_cols) == 1){
    outscoredfcells <- as.matrix(outscoredf[, 2], ncol = 1)
    colnames(outscoredfcells) <- colnames(data[, disc_cols])
    outscoredfcells <- as.data.frame(outscoredfcells)
  }

  cat('Outlyingness scores for discrete variables calculated.\n')
  return(list('MAXLEN'=MAXLEN, 'Discrete Scores'=outscoredf, 'Contributions'=outscoredfcells))
}
