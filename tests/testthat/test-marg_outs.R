rows <- 1000
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
marginal_outliers <- marg_outs(data = dt,
                               disc_cols = c(1:disc_vars),
                               cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                               alpha = 0.01, MAXLEN = 0,
                               rho = 0.20, epsilon = 0.02)
testthat::test_that("Correct output object.", {
  testthat::expect_equal(is.list(marginal_outliers), TRUE)
  testthat::expect_equal(length(marginal_outliers), 3)
})
testthat::test_that("Incorrect input argument values.", {
  testthat::expect_error(marg_outs(data = dt,
                         disc_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                         cont_cols = c((disc_vars+1):(disc_vars+cont_vars))),
               "Discrete variables should be of class 'factor'.")
  testthat::expect_error(marg_outs(data = dt,
                         disc_cols = c(1:disc_vars),
                         cont_cols = c((disc_vars):(disc_vars+cont_vars))),
               "Continuous variables should be of class 'numeric'.")
  testthat::expect_error(marg_outs(data = as.matrix(dt, nrow = rows),
                         disc_cols = c(1:disc_vars),
                         cont_cols = c((disc_vars+1):(disc_vars+cont_vars))),
               "Data set should be of class 'data.frame'.")
  testthat::expect_error(marg_outs(data = dt,
                         disc_cols = c(1:disc_vars),
                         cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                         rho = '0.20', epsilon = 0.02),
               "rho must be a number between 0 and 0.50.", fixed = TRUE)
  testthat::expect_error(marg_outs(data = dt,
                        disc_cols = c(1:disc_vars),
                        cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                        rho = 0.20, epsilon = '0.02'),
               "epsilon must be a number between 0 and 0.50.", fixed = TRUE)
  testthat::expect_error(marg_outs(data = dt,
                         disc_cols = c(1:disc_vars),
                         cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                         rho = 0.60, epsilon = 0),
               "Incorrect value for rho - must be between 0 and 0.50.", fixed = TRUE)
  testthat::expect_error(marg_outs(data = dt,
                         disc_cols = c(1:disc_vars),
                         cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                         rho = 0, epsilon = 0),
               "Incorrect value for rho - must be between 0 and 0.50.", fixed = TRUE)
  testthat::expect_error(marg_outs(data = dt,
                         disc_cols = c(1:disc_vars),
                         cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                         rho = -0.1, epsilon = 0),
               "Incorrect value for rho - must be between 0 and 0.50.", fixed = TRUE)
  testthat::expect_error(marg_outs(data = dt,
                         disc_cols = c(1:disc_vars),
                         cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                         rho = 0.20, epsilon = -0.25),
               "Incorrest value for epsilon - must be between 0 and 0.25.", fixed = TRUE)
  testthat::expect_error(marg_outs(data = dt,
                         disc_cols = c(1:disc_vars),
                         cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                         rho = 0.20, epsilon = 0.30),
               "Incorrest value for epsilon - must be between 0 and 0.25.", fixed = TRUE)
  testthat::expect_error(marg_outs(data = dt,
                         disc_cols = c(1:disc_vars),
                         cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                         rho = 0.20, epsilon = 0.20),
               "rho must be greater than epsilon and their sum should be at most 0.50.", fixed = TRUE)
  testthat::expect_error(marg_outs(data = dt,
                         disc_cols = c(1:disc_vars),
                         cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                         rho = 0.20, epsilon = 0.21),
               "rho must be greater than epsilon and their sum should be at most 0.50.", fixed = TRUE)
  testthat::expect_error(marg_outs(data = dt,
                         disc_cols = c(1:disc_vars),
                         cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                         rho = 0.40, epsilon = 0.20),
               "rho must be greater than epsilon and their sum should be at most 0.50.", fixed = TRUE)
  testthat::expect_error(marg_outs(data = dt,
                         disc_cols = c(1:disc_vars),
                         cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                         alpha = 0,
                         rho = 0.20, epsilon = 0.02),
               "alpha should be positive and at most equal to 0.20.", fixed = TRUE)
  testthat::expect_error(marg_outs(data = dt,
                         disc_cols = c(1:disc_vars),
                         cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                         alpha = -0.1,
                         rho = 0.20, epsilon = 0.02),
               "alpha should be positive and at most equal to 0.20.", fixed = TRUE)
  testthat::expect_error(marg_outs(data = dt,
                         disc_cols = c(1:disc_vars),
                         cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                         alpha = 0.21,
                         rho = 0.20, epsilon = 0.02),
               "alpha should be positive and at most equal to 0.20.", fixed = TRUE)
  testthat::expect_error(marg_outs(data = dt,
                         disc_cols = c(1:disc_vars),
                         cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                         alpha = '0.01',
                         rho = 0.20, epsilon = 0.02),
               "alpha should be of class 'numeric'.", fixed = TRUE)
  testthat::expect_error(marg_outs(data = dt,
                         disc_cols = c(1:disc_vars),
                         cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                         alpha = c(0.01, 0.1),
                         rho = 0.20, epsilon = 0.02),
               "alpha should be of unit length.", fixed = TRUE)
  testthat::expect_error(marg_outs(data = dt,
                         disc_cols = c(1:disc_vars),
                         cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                         alpha = 0.01, MAXLEN = c(0, 0.2),
                         rho = 0.20, epsilon = 0.02),
               "MAXLEN should be an integer at most equal to the number of discrete variables.", fixed = TRUE)
  testthat::expect_error(marg_outs(data = dt,
                         disc_cols = c(1:disc_vars),
                         cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                         alpha = 0.01, MAXLEN = 0.8,
                         rho = 0.20, epsilon = 0.02),
               "MAXLEN should be an integer at most equal to the number of discrete variables.", fixed = TRUE)
  testthat::expect_error(marg_outs(data = dt,
                         disc_cols = c(1:disc_vars),
                         cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                         alpha = 0.01, MAXLEN = -1,
                         rho = 0.20, epsilon = 0.02),
               "MAXLEN should be an integer at most equal to the number of discrete variables.", fixed = TRUE)
  testthat::expect_error(marg_outs(data = dt,
                         disc_cols = c(1:disc_vars),
                         cont_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                         alpha = 0.01, MAXLEN = (disc_vars + 1),
                         rho = 0.20, epsilon = 0.02),
               "MAXLEN should be an integer at most equal to the number of discrete variables.", fixed = TRUE)
})
