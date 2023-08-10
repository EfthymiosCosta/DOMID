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
marginal_outliers <- marg_outs(data = dt,
                               disc_cols = c(1:disc_vars),
                               cont_cols = c((disc_vars+1):(disc_vars+cont_vars)))
test_that("Correct output object.", {
  expect_equal(is.list(marginal_outliers), TRUE)
  expect_equal(length(marginal_outliers), 3)
})
test_that("Incorrect input argument values.", {
  expect_error(marg_outs(data = dt,
                         disc_cols = c((disc_vars+1):(disc_vars+cont_vars)),
                         cont_cols = c((disc_vars+1):(disc_vars+cont_vars))),
               "Discrete variables should be of class 'factor'.")
  expect_error(marg_outs(data = dt,
                         disc_cols = c(1:disc_vars),
                         cont_cols = c((disc_vars):(disc_vars+cont_vars))),
               "Continuous variables should be of class 'numeric'.")
  expect_error(marg_outs(data = as.matrix(dt, nrow = rows),
                         disc_cols = c(1:disc_vars),
                         cont_cols = c((disc_vars+1):(disc_vars+cont_vars))),
               "Data set should be of class 'data.frame'.")
})
