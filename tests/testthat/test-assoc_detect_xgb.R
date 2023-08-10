rows <- sample(seq(1000, 1500), 1)
disc_vars <- sample.int(5, 1)
cont_vars <- sample(seq(2, 8), 1)

dt <- gen_marg_joint_data(n_obs = rows,
                          n_disc = disc_vars,
                          n_cont = cont_vars,
                          n_lvls = 3,
                          p_outs = 0.05,
                          jp_outs = 0.2,
                          assoc_target = 1,
                          assoc_vars = c(1,2),
                          assoc_type="linear",
                          seed_num = 1)

discrete_scores <- disc_scores(data = dt, disc_cols = c(1:disc_vars))
continuous_scores <- cont_scores(data = dt, cont_cols = c((disc_vars+1):(disc_vars+cont_vars)))
marginal_outliers <- marg_outs_scores(data = dt,
                                      disc_cols = c(1:disc_vars),
                                      outscorediscdf = discrete_scores[[2]],
                                      outscorecontdf = continuous_scores,
                                      outscorediscdfcells = discrete_scores[[3]])
marginal_outliers <- unique(unlist(marginal_outliers))
assoc_obj <- assoc_detect_xgb(dt, K = 5, pred_inx = c((disc_vars+1):(disc_vars+cont_vars)),
                              target_inx = c(1:disc_vars),
                              cont_scores = continuous_scores[,2],
                              contribs = discrete_scores[[3]],
                              marg_outs = marginal_outliers)

test_that("Correct output object.", {
  expect_equal(is.list(assoc_obj), TRUE)
  expect_equal(length(assoc_obj), 3)
  expect_equal(is.list(assoc_obj[[1]]), TRUE)
  expect_equal(length(assoc_obj[[1]]), disc_vars)
  expect_equal(is.data.frame(assoc_obj[[2]]), TRUE)
  expect_equal(dim(assoc_obj[[2]]), c(disc_vars, 3))
  expect_equal(is.list(assoc_obj[[3]]), TRUE)
  expect_equal(length(assoc_obj[[3]]), disc_vars)
  expect_equal(all(lengths(assoc_obj[[3]]) == rows), TRUE)
})

test_that("Incorrect input argument values.", {
  expect_error(assoc_detect_xgb(as.matrix(dt, nrow = rows), K = 5,
                                pred_inx = c((disc_vars+1):(disc_vars+cont_vars)),
                                target_inx = c(1:disc_vars),
                                cont_scores = continuous_scores[,2],
                                contribs = discrete_scores[[3]],
                                marg_outs = marginal_outliers),
               "Data set should be of class 'data.frame'.")
  expect_error(assoc_detect_xgb(dt, K = 1.5,
                                pred_inx = c((disc_vars+1):(disc_vars+cont_vars)),
                                target_inx = c(1:disc_vars),
                                cont_scores = continuous_scores[,2],
                                contribs = discrete_scores[[3]],
                                marg_outs = marginal_outliers),
               "K should be a positive integer at most equal to the number of observations.")
  expect_error(assoc_detect_xgb(dt, K = 0,
                                pred_inx = c((disc_vars+1):(disc_vars+cont_vars)),
                                target_inx = c(1:disc_vars),
                                cont_scores = continuous_scores[,2],
                                contribs = discrete_scores[[3]],
                                marg_outs = marginal_outliers),
               "K should be a positive integer at most equal to the number of observations.")
  expect_error(assoc_detect_xgb(dt, K = (rows + 1),
                                pred_inx = c((disc_vars+1):(disc_vars+cont_vars)),
                                target_inx = c(1:disc_vars),
                                cont_scores = continuous_scores[,2],
                                contribs = discrete_scores[[3]],
                                marg_outs = marginal_outliers),
               "K should be a positive integer at most equal to the number of observations.")
  expect_error(assoc_detect_xgb(dt, K = 5,
                                pred_inx = c((disc_vars+1):(disc_vars+cont_vars)),
                                target_inx = c(1:(disc_vars+1)),
                                cont_scores = continuous_scores[,2],
                                contribs = discrete_scores[[3]],
                                marg_outs = marginal_outliers),
               "Target (discrete) variables should be of class 'factor'.", fixed = TRUE)
  expect_error(assoc_detect_xgb(dt, K = 5,
                                pred_inx = c((disc_vars):(disc_vars+cont_vars)),
                                target_inx = c(1:disc_vars),
                                cont_scores = continuous_scores[,2],
                                contribs = discrete_scores[[3]],
                                marg_outs = marginal_outliers),
               "Predictor (continuous) variables should be of class 'numeric'.", fixed = TRUE)
  expect_error(assoc_detect_xgb(dt, K = 5,
                                pred_inx = c((disc_vars+1):(disc_vars+cont_vars)),
                                target_inx = c(1:disc_vars),
                                cont_scores = continuous_scores[c(1:(rows-1)),2],
                                contribs = discrete_scores[[3]],
                                marg_outs = marginal_outliers),
               "Continuous scores should be a vector of length equal to the number of observations.", fixed = TRUE)
  expect_error(assoc_detect_xgb(dt, K = 5,
                                pred_inx = c((disc_vars+1):(disc_vars+cont_vars)),
                                target_inx = c(1:disc_vars),
                                cont_scores = continuous_scores[,2],
                                contribs = as.matrix(discrete_scores[[3]]),
                                marg_outs = marginal_outliers),
               "Data set of contributions should be of class 'data.frame'.", fixed = TRUE)
  expect_error(assoc_detect_xgb(dt, K = 5,
                                pred_inx = c((disc_vars+1):(disc_vars+cont_vars)),
                                target_inx = c(1:disc_vars),
                                cont_scores = continuous_scores[,2],
                                contribs = discrete_scores[[3]][c(1:(rows-1)), c(1:disc_vars)],
                                marg_outs = marginal_outliers),
               "Data set of contributions should have as many rows as the number of observations.",
               fixed = TRUE)
  expect_error(assoc_detect_xgb(dt, K = 5,
                                pred_inx = c((disc_vars+1):(disc_vars+cont_vars)),
                                target_inx = c(1:disc_vars),
                                cont_scores = continuous_scores[,2],
                                contribs = data.frame(discrete_scores[[3]][c(1:rows), c(1:(disc_vars-1))]),
                                marg_outs = marginal_outliers),
               "Data set of contributions should have as many columns as the number of target (discrete) variables.",
               fixed = TRUE)
  expect_error(assoc_detect_xgb(dt, K = 5,
                                pred_inx = c((disc_vars+1):(disc_vars+cont_vars)),
                                target_inx = c(1:disc_vars),
                                cont_scores = continuous_scores[,2],
                                contribs = discrete_scores[[3]],
                                marg_outs = c(marginal_outliers, marginal_outliers[1])),
                                "Indices of marginal outliers should be unique.", fixed = TRUE)
  expect_error(assoc_detect_xgb(dt, K = 5,
                                pred_inx = c((disc_vars+1):(disc_vars+cont_vars)),
                                target_inx = c(1:disc_vars),
                                cont_scores = continuous_scores[,2],
                                contribs = discrete_scores[[3]],
                                marg_outs = c(marginal_outliers, 0)),
               "Indices of marginal outliers should be unique integer values from 1 up to the number of observations in the data.",
               fixed = TRUE)
  expect_error(assoc_detect_xgb(dt, K = 5,
                                pred_inx = c((disc_vars+1):(disc_vars+cont_vars)),
                                target_inx = c(1:disc_vars),
                                cont_scores = continuous_scores[,2],
                                contribs = discrete_scores[[3]],
                                marg_outs = c(marginal_outliers, 9.2)),
               "Indices of marginal outliers should be unique integer values from 1 up to the number of observations in the data.",
               fixed = TRUE)
  expect_error(assoc_detect_xgb(dt, K = 5,
                                pred_inx = c((disc_vars+1):(disc_vars+cont_vars)),
                                target_inx = c(1:disc_vars),
                                cont_scores = continuous_scores[,2],
                                contribs = discrete_scores[[3]],
                                marg_outs = c(marginal_outliers, -10)),
               "Indices of marginal outliers should be unique integer values from 1 up to the number of observations in the data.",
               fixed = TRUE)
  expect_error(assoc_detect_xgb(dt, K = 5,
                                pred_inx = c((disc_vars+1):(disc_vars+cont_vars)),
                                target_inx = c(1:disc_vars),
                                cont_scores = continuous_scores[,2],
                                contribs = discrete_scores[[3]],
                                marg_outs = c(marginal_outliers, 2000)),
               "Indices of marginal outliers should be unique integer values from 1 up to the number of observations in the data.",
               fixed = TRUE)
})
