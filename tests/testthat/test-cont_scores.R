rows <- sample(seq(1000, 2000), 1)
disc_vars <- sample.int(8, 1)
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

test_that("Correct output dimensions.", {
  expect_equal(dim(cont_scores(data = dt,
                               cont_cols = c((disc_vars+1):(disc_vars+cont_vars)))),
               c(rows, 2))
})
test_that("Incorrect labels for continuous variables.", {
  expect_error(cont_scores(data = dt,
                           cont_cols = c(1:disc_vars)),
               "Continuous variables should be of class 'numeric'.")
  expect_error(cont_scores(data = dt,
                           cont_cols = c(1:(disc_vars+1))),
               "Continuous variables should be of class 'numeric'.")
})
test_that("Incorrect input variable values for EIF algorithm.", {
  expect_error(cont_scores(data = dt,
                           cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                           sample_size = 0.1),
               "Sample size should be a positive integer.")
  expect_error(cont_scores(data = dt,
                           cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                           sample_size = -10),
               "Sample size should be a positive integer.")
  expect_error(cont_scores(data = dt,
                           cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                           sample_size = 0),
               "Sample size should be a positive integer.")
  expect_error(cont_scores(data = dt,
                           cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                           ntrees = 0),
               "Number of trees should be a positive integer.")
  expect_error(cont_scores(data = dt,
                           cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                           ntrees = -10),
               "Number of trees should be a positive integer.")
  expect_error(cont_scores(data = dt,
                           cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                           ntrees = 1.3),
               "Number of trees should be a positive integer.")
  expect_error(cont_scores(data = dt,
                           cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                           ndim = 1.1),
               "Number of dimensions should be a positive integer, at most equal to the number of continuous columns.")
  expect_error(cont_scores(data = dt,
                           cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                           ndim = -2),
               "Number of dimensions should be a positive integer, at most equal to the number of continuous columns.")
  expect_error(cont_scores(data = dt,
                           cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                           ndim = (cont_vars+1)),
               "Number of dimensions should be a positive integer, at most equal to the number of continuous columns.")
  expect_error(cont_scores(data = dt,
                           cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                           max_depth = 0.1),
               "Maximum depth should be a positive integer.")
  expect_error(cont_scores(data = dt,
                           cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                           max_depth = 1.4),
               "Maximum depth should be a positive integer.")
  expect_error(cont_scores(data = dt,
                           cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                           max_depth = 0),
               "Maximum depth should be a positive integer.")
  expect_error(cont_scores(data = dt,
                           cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                           max_depth = -2),
               "Maximum depth should be a positive integer.")
  expect_error(cont_scores(data = dt,
                           cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                           seed_num = 0.1),
               "Seed number should be a positive integer.")
  expect_error(cont_scores(data = dt,
                           cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                           seed_num = 0),
               "Seed number should be a positive integer.")
  expect_error(cont_scores(data = dt,
                           cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                           seed_num = 9.1),
               "Seed number should be a positive integer.")
  expect_error(cont_scores(data = dt,
                           cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                           seed_num = -1),
               "Seed number should be a positive integer.")
})
