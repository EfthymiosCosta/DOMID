rows <- sample(seq(1000, 1500), 1)
disc_vars <- sample(seq(2, 8), 1)
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
                               disc_cols = c(1:disc_vars))
test_that("Correct number of output dimensions & correct output type.", {
  expect_equal(is.list(discrete_scores), TRUE)
  expect_equal(length(discrete_scores), 3)
  expect_equal(length(discrete_scores[[1]]), 1)
  expect_equal(dim(discrete_scores[[2]]), c(rows, 2))
  expect_equal(dim(discrete_scores[[3]]), c(rows, disc_vars))
  expect_equal(all(rowSums(discrete_scores[[3]]) == discrete_scores[[2]][, 2]), TRUE)
})

test_that("Incorrect input variable values error messages.", {
  expect_error(disc_scores(data = dt, disc_cols = c((disc_vars+1):(disc_vars + cont_vars))),
               "Discrete variables should be of class 'factor'.")
  expect_error(disc_scores(data = dt, disc_cols = c(1:(disc_vars + cont_vars))),
               "Discrete variables should be of class 'factor'.")
  expect_error(disc_scores(data = dt, disc_cols = c((disc_vars - 1):(disc_vars + 2))),
               "Discrete variables should be of class 'factor'.")
})
