rows <- 800
disc_vars <- sample.int(3, 1)
cont_vars <- sample(seq(2, 5), 1)

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
kde_classification0 <- kde_classif(data = dt,
                                   target_inx = c(1),
                                   pred_inx = c((disc_vars+1), (disc_vars+2)),
                                   marg_outs = marginal_outliers,
                                   Lambda_i = 0,
                                   kernel = 'gauss',
                                   alpha_val = 0.3)
kde_classification1 <- kde_classif(data = dt,
                                   target_inx = c(1),
                                   pred_inx = c((disc_vars+1), (disc_vars+2)),
                                   marg_outs = marginal_outliers,
                                   Lambda_i = 13,
                                   kernel = 'gauss',
                                   alpha_val = 0.3)
random_vector_size <- sample.int(20, 1)
random_vector <- seq(1, 20, length.out = random_vector_size)
kde_classification2 <- kde_classif(data = dt,
                                   target_inx = c(1),
                                   pred_inx = c((disc_vars+1), (disc_vars+2)),
                                   marg_outs = marginal_outliers,
                                   Lambda_i = random_vector,
                                   kernel = 'gauss',
                                   alpha_val = 0.3)

testthat::test_that("Correct output object.", {
  testthat::expect_equal(is.list(kde_classification0), TRUE)
  testthat::expect_equal(is.list(kde_classification1), TRUE)
  testthat::expect_equal(is.list(kde_classification2), TRUE)
  testthat::expect_equal(length(kde_classification0), 2)
  testthat::expect_equal(length(kde_classification1), 2)
  testthat::expect_equal(length(kde_classification2), 2)
  testthat::expect_equal(is.list(kde_classification0[[2]]), TRUE)
  testthat::expect_equal(is.list(kde_classification1[[2]]), TRUE)
  testthat::expect_equal(is.list(kde_classification2[[2]]), TRUE)
  testthat::expect_equal(length(kde_classification0[[1]]), 39)
  testthat::expect_equal(length(kde_classification0[[2]]), 39)
  testthat::expect_equal(length(kde_classification1[[1]]), 1)
  testthat::expect_equal(length(kde_classification1[[2]]), 1)
  testthat::expect_equal(length(kde_classification2[[1]]), random_vector_size)
  testthat::expect_equal(length(kde_classification2[[2]]), random_vector_size)
})

testthat::test_that("Incorrect input argument values.", {
  testthat::expect_error(kde_classif(data = matrix(dt),
                           target_inx = c(1),
                           pred_inx = c((disc_vars+1), (disc_vars+2)),
                           marg_outs = marginal_outliers,
                           Lambda_i = 2,
                           kernel = 'gauss',
                           alpha_val = 0.3),
               "Data set should be of class 'data.frame'.", fixed = TRUE)
  testthat::expect_error(kde_classif(data = dt,
                           target_inx = c(1),
                           pred_inx = c((disc_vars+1), (disc_vars+2)),
                           marg_outs = marginal_outliers,
                           Lambda_i = "hi",
                           kernel = 'gauss',
                           alpha_val = 0.3),
               "Lambda_i should be of class 'numeric'.", fixed = TRUE)
  testthat::expect_error(kde_classif(data = dt,
                           target_inx = c(1),
                           pred_inx = c((disc_vars+1), (disc_vars+2)),
                           marg_outs = marginal_outliers,
                           Lambda_i = c(0.99, 1, 1.5),
                           kernel = 'gauss',
                           alpha_val = 0.3),
               "All values of Lambda_i should be at least equal to 1.", fixed = TRUE)
  testthat::expect_error(kde_classif(data = dt,
                           target_inx = c(1),
                           pred_inx = c((disc_vars+1), (disc_vars+2)),
                           marg_outs = marginal_outliers,
                           Lambda_i = 0.5,
                           kernel = 'gauss',
                           alpha_val = 0.3),
               "Lambda_i should be at least equal to 1 or equal to 0 (see documentation).", fixed = TRUE)
  testthat::expect_error(kde_classif(data = dt,
                           target_inx = c(disc_vars+1),
                           pred_inx = c((disc_vars+1), (disc_vars+2)),
                           marg_outs = marginal_outliers,
                           Lambda_i = 1,
                           kernel = 'gauss',
                           alpha_val = 0.3),
               "Target (discrete) variable should be of class 'factor'.", fixed = TRUE)
  testthat::expect_error(kde_classif(data = dt,
                           target_inx = c(1),
                           pred_inx = c((disc_vars), (disc_vars+2)),
                           marg_outs = marginal_outliers,
                           Lambda_i = 1,
                           kernel = 'gauss',
                           alpha_val = 0.3),
               "Predictor (continuous) variables should be of class 'numeric'.", fixed = TRUE)
  testthat::expect_error(kde_classif(data = dt,
                           target_inx = c(1),
                           pred_inx = c((disc_vars+1), (disc_vars+2)),
                           marg_outs = marginal_outliers,
                           Lambda_i = 1,
                           kernel = 'gauss',
                           alpha_val = 1.1),
               "alpha_val should be a number between 0 and 1.", fixed = TRUE)
  testthat::expect_error(kde_classif(data = dt,
                           target_inx = c(1),
                           pred_inx = c((disc_vars+1), (disc_vars+2)),
                           marg_outs = marginal_outliers,
                           Lambda_i = 1,
                           kernel = 'gauss',
                           alpha_val = -0.1),
               "alpha_val should be a number between 0 and 1.", fixed = TRUE)
  testthat::expect_error(kde_classif(data = dt,
                           target_inx = c(1),
                           pred_inx = c((disc_vars+1), (disc_vars+2)),
                           marg_outs = marginal_outliers,
                           Lambda_i = 1,
                           kernel = 'gauss',
                           alpha_val = "hi"),
               "alpha_val should be a number between 0 and 1.", fixed = TRUE)
  testthat::expect_error(kde_classif(data = dt,
                           target_inx = c(1),
                           pred_inx = c((disc_vars+1), (disc_vars+2)),
                           marg_outs = marginal_outliers,
                           Lambda_i = 1,
                           kernel = 'gauss',
                           alpha_val = c(0.5, 0.5)),
               "alpha_val should be a number between 0 and 1.", fixed = TRUE)
  testthat::expect_error(kde_classif(data = dt,
                           target_inx = c(1),
                           pred_inx = c((disc_vars+1), (disc_vars+2)),
                           marg_outs = marginal_outliers,
                           Lambda_i = 1,
                           kernel = 'some_random_kernel',
                           alpha_val = 0.3),
               "Kernel choice not supported - see documentation for supported kernels.", fixed = TRUE)
  testthat::expect_error(kde_classif(data = dt,
                           target_inx = c(1),
                           pred_inx = c((disc_vars+1), (disc_vars+2)),
                           marg_outs = c(marginal_outliers, marginal_outliers[1]),
                           Lambda_i = 1,
                           kernel = 'gauss',
                           alpha_val = 0.3),
               "Indices of marginal outliers should be unique.", fixed = TRUE)
  testthat::expect_error(kde_classif(data = dt,
                           target_inx = c(1),
                           pred_inx = c((disc_vars+1), (disc_vars+2)),
                           marg_outs = c(marginal_outliers, 0),
                           Lambda_i = 1,
                           kernel = 'gauss',
                           alpha_val = 0.3),
               "Indices of marginal outliers should be unique integer values from 1 up to the number of observations in the data.",
               fixed = TRUE)
  testthat::expect_error(kde_classif(data = dt,
                           target_inx = c(1),
                           pred_inx = c((disc_vars+1), (disc_vars+2)),
                           marg_outs = c(marginal_outliers, 2000),
                           Lambda_i = 1,
                           kernel = 'gauss',
                           alpha_val = 0.3),
               "Indices of marginal outliers should be unique integer values from 1 up to the number of observations in the data.",
               fixed = TRUE)
  testthat::expect_error(kde_classif(data = dt,
                           target_inx = c(1),
                           pred_inx = c((disc_vars+1), (disc_vars+2)),
                           marg_outs = c(marginal_outliers, -10),
                           Lambda_i = 1,
                           kernel = 'gauss',
                           alpha_val = 0.3),
               "Indices of marginal outliers should be unique integer values from 1 up to the number of observations in the data.",
               fixed = TRUE)
  testthat::expect_error(kde_classif(data = dt,
                           target_inx = c(1),
                           pred_inx = c((disc_vars+1), (disc_vars+2)),
                           marg_outs = c(marginal_outliers, 38.9),
                           Lambda_i = 1,
                           kernel = 'gauss',
                           alpha_val = 0.3),
               "Indices of marginal outliers should be unique integer values from 1 up to the number of observations in the data.",
               fixed = TRUE)
  testthat::expect_error(kde_classif(data = dt,
                           target_inx = c(1),
                           pred_inx = c((disc_vars+1), (disc_vars+2)),
                           marg_outs = marginal_outliers,
                           Lambda_i = 1,
                           kernel = 'gauss',
                           alpha_val = 0.3,
                           maxk = c(1, 3)),
               "The 'maxk' parameter must be a single positive integer.",
               fixed = TRUE)
  testthat::expect_error(kde_classif(data = dt,
                           target_inx = c(1),
                           pred_inx = c((disc_vars+1), (disc_vars+2)),
                           marg_outs = marginal_outliers,
                           Lambda_i = 1,
                           kernel = 'gauss',
                           alpha_val = 0.3,
                           maxk = -10),
               "The 'maxk' parameter must be a positive integer.",
               fixed = TRUE)
  testthat::expect_error(kde_classif(data = dt,
                           target_inx = c(1),
                           pred_inx = c((disc_vars+1), (disc_vars+2)),
                           marg_outs = marginal_outliers,
                           Lambda_i = 1,
                           kernel = 'gauss',
                           alpha_val = 0.3,
                           maxk = 1000.1),
               "The 'maxk' parameter must be a positive integer.",
               fixed = TRUE)
})
