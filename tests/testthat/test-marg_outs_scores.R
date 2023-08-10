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
test_that("Correct output object.", {
  expect_equal(is.list(marginal_outliers), TRUE)
  expect_equal(length(marginal_outliers), 3)
})

test_that("Incorrect input argument values.", {
  expect_error(marg_outs_scores(data = dt,
                                disc_cols = c(1:(disc_vars+1)),
                                outscorediscdf = discrete_scores[[2]],
                                outscorecontdf = continuous_scores,
                                outscorediscdfcells = discrete_scores[[3]]),
               "Discrete variables should be of class 'factor'.")
  expect_error(marg_outs_scores(data = as.matrix(dt, nrow = rows),
                                disc_cols = c(1:(disc_vars)),
                                outscorediscdf = discrete_scores[[2]],
                                outscorecontdf = continuous_scores,
                                outscorediscdfcells = discrete_scores[[3]]),
               "Data set should be of class 'data.frame'.")
  expect_error(marg_outs_scores(data = dt,
                                disc_cols = c(1:(disc_vars)),
                                outscorediscdf = as.matrix(discrete_scores[[2]]),
                                outscorecontdf = continuous_scores,
                                outscorediscdfcells = discrete_scores[[3]]),
               "Data set of discrete scores should be of class 'data.frame'.")
  expect_error(marg_outs_scores(data = dt,
                                disc_cols = c(1:(disc_vars)),
                                outscorediscdf = discrete_scores[[2]],
                                outscorecontdf = as.matrix(continuous_scores),
                                outscorediscdfcells = discrete_scores[[3]]),
               "Data set of continuous scores should be of class 'data.frame'.")
  expect_error(marg_outs_scores(data = dt,
                                disc_cols = c(1:(disc_vars)),
                                outscorediscdf = discrete_scores[[2]],
                                outscorecontdf = continuous_scores,
                                outscorediscdfcells = as.matrix(discrete_scores[[3]])),
               "Matrix of contributions should be of class 'data.frame'.")
  expect_error(marg_outs_scores(data = dt,
                                disc_cols = c(1:(disc_vars)),
                                outscorediscdf = discrete_scores[[2]][c(1:(rows-1)),],
                                outscorecontdf = continuous_scores,
                                outscorediscdfcells = discrete_scores[[3]]),
               "Incorrect number of rows for data frame of discrete scores.")
  expect_error(marg_outs_scores(data = dt,
                                disc_cols = c(1:(disc_vars)),
                                outscorediscdf = discrete_scores[[2]],
                                outscorecontdf = continuous_scores[c(1:(rows-1)),],
                                outscorediscdfcells = discrete_scores[[3]]),
               "Incorrect number of rows for data frame of continuous scores.")
  expect_error(marg_outs_scores(data = dt,
                                disc_cols = c(1:(disc_vars)),
                                outscorediscdf = discrete_scores[[2]],
                                outscorecontdf = continuous_scores,
                                outscorediscdfcells = data.frame(discrete_scores[[3]][c(1:(rows-1)), c(1:disc_vars)])),
               "Incorrect number of rows for matrix of contributions.")
  expect_error(marg_outs_scores(data = dt,
                                disc_cols = c(1:(disc_vars)),
                                outscorediscdf = discrete_scores[[2]],
                                outscorecontdf = continuous_scores,
                                outscorediscdfcells = data.frame(discrete_scores[[3]][c(1:rows), c(1:(disc_vars-1))])),
               "Incorrect number of columns for matrix of contributions.")
  expect_error(marg_outs_scores(data = dt,
                                disc_cols = c(1:(disc_vars)),
                                outscorediscdf = data.frame(discrete_scores[[2]][c(1:rows), 1]),
                                outscorecontdf = continuous_scores,
                                outscorediscdfcells = discrete_scores[[3]]),
               "Incorrect number of columns for data frame of discrete scores (should be 2).", fixed = TRUE)
  expect_error(marg_outs_scores(data = dt,
                                disc_cols = c(1:(disc_vars)),
                                outscorediscdf = discrete_scores[[2]],
                                outscorecontdf = data.frame(continuous_scores[c(1:rows), 1]),
                                outscorediscdfcells = discrete_scores[[3]]),
               "Incorrect number of columns for data frame of continuous scores (should be 2).", fixed = TRUE)
  expect_error(marg_outs_scores(data = dt,
                                disc_cols = c(1:(disc_vars)),
                                outscorediscdf = cbind(discrete_scores[[2]], rnorm(rows)),
                                outscorecontdf = continuous_scores,
                                outscorediscdfcells = discrete_scores[[3]]),
               "Incorrect number of columns for data frame of discrete scores (should be 2).", fixed = TRUE)
  expect_error(marg_outs_scores(data = dt,
                                disc_cols = c(1:(disc_vars)),
                                outscorediscdf = discrete_scores[[2]],
                                outscorecontdf = cbind(continuous_scores, rnorm(rows)),
                                outscorediscdfcells = discrete_scores[[3]]),
               "Incorrect number of columns for data frame of continuous scores (should be 2).", fixed = TRUE)
})
