rows <- 1000
disc_vars <- sample(seq(2, 3), 1)
cont_vars <- sample(seq(3, 8), 1)

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
discrete_scores <- disc_scores(data = dt,
                               disc_cols = c(1:disc_vars),
                               alpha = 0.01,
                               MAXLEN = 0)
testthat::test_that("Correct number of output dimensions & correct output type.", {
  testthat::expect_equal(is.list(discrete_scores), TRUE)
  testthat::expect_equal(length(discrete_scores), 3)
  testthat::expect_equal(length(discrete_scores[[1]]), 1)
  testthat::expect_equal(dim(discrete_scores[[2]]), c(rows, 2))
  testthat::expect_equal(dim(discrete_scores[[3]]), c(rows, disc_vars))
  testthat::expect_equal(all(rowSums(discrete_scores[[3]]) == discrete_scores[[2]][, 2]), TRUE)
})

testthat::test_that("Incorrect input variable values error messages.", {
  testthat::expect_error(disc_scores(data = dt, disc_cols = c((disc_vars+1):(disc_vars + cont_vars))),
               "Discrete variables should be of class 'factor'.")
  testthat::expect_error(disc_scores(data = dt, disc_cols = c(1:(disc_vars + cont_vars))),
               "Discrete variables should be of class 'factor'.")
  testthat::expect_error(disc_scores(data = dt, disc_cols = c((disc_vars - 1):(disc_vars + 2))),
               "Discrete variables should be of class 'factor'.")
  testthat::expect_error(disc_scores(data = dt, disc_cols = c(1:disc_vars), alpha = 0),
               "alpha should be positive and at most equal to 0.20.")
  testthat::expect_error(disc_scores(data = dt, disc_cols = c(1:disc_vars), alpha = -0.1),
               "alpha should be positive and at most equal to 0.20.")
  testthat::expect_error(disc_scores(data = dt, disc_cols = c(1:disc_vars), alpha = 0.21),
               "alpha should be positive and at most equal to 0.20.")
  testthat::expect_error(disc_scores(data = dt, disc_cols = c(1:disc_vars), alpha = '0.01'),
               "alpha should be of class 'numeric'.")
  testthat::expect_error(disc_scores(data = dt, disc_cols = c(1:disc_vars), alpha = c(0.01, 0.1)),
               "alpha should be of unit length.")
  testthat::expect_error(disc_scores(data = dt, disc_cols = c(1:disc_vars), alpha = 0.01,
               MAXLEN = c(0, 0.2)),
               "MAXLEN should be an integer at most equal to the number of discrete variables.")
  testthat::expect_error(disc_scores(data = dt, disc_cols = c(1:disc_vars), alpha = 0.01,
               MAXLEN = 0.8),
               "MAXLEN should be an integer at most equal to the number of discrete variables.")
  testthat::expect_error(disc_scores(data = dt, disc_cols = c(1:disc_vars), alpha = 0.01,
               MAXLEN = -1),
               "MAXLEN should be an integer at most equal to the number of discrete variables.")
  testthat::expect_error(disc_scores(data = dt, disc_cols = c(1:disc_vars), alpha = 0.01,
               MAXLEN = (disc_vars + 1)),
               "MAXLEN should be an integer at most equal to the number of discrete variables.")
})
