## ---- include = FALSE-----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----gen_marg_joint_data--------------------------------------------------------------------------
library(DOMID)
dt <- gen_marg_joint_data(n_obs = 1000, n_disc = 5, n_cont = 5,
                          n_lvls = c(3, 4, 3, 3, 3), p_outs = 0.20, jp_outs = 0.80,
                          assoc_target = c(1,2), assoc_vars = list(c(1, 2), c(1, 2)),
                          assoc_type = 'quotient', seed_num = 1)
str(dt)

## ----data_gen_plots-------------------------------------------------------------------------------
plot(dt[which(dt$V11 %in% c(0,3)), c(6, 7)],
     col = dt[which(dt$V11 %in% c(0,3)), 1],
     pch = 16,
     xlab = 'Continuous Variable 1',
     ylab = 'Continuous Variable 2')
legend('topright',
       legend = c('Level 1', 'Level 2', 'Level 3'),
       col = c(1, 2, 3),
       pch = 16,
       title = 'Discrete Variable 1',
       cex = 0.9)
plot(dt[which(dt$V11 %in% c(0,3)), c(6, 7)],
     col = dt[which(dt$V11 %in% c(0,3)), 2],
     pch = 16,
     xlab = 'Continuous Variable 1',
     ylab = 'Continuous Variable 2')
legend('topright',
       legend = c('Level 1', 'Level 2', 'Level 3', 'Level 4'),
       col = c(1, 2, 3, 4),
       pch = 16,
       title = 'Discrete Variable 2',
       cex = 0.9)

## ----disc_scores----------------------------------------------------------------------------------
discrete_scores <- disc_scores(data = dt, disc_cols = c(1:5), alpha = 0.01, MAXLEN = 0)
# MAXLEN
discrete_scores[[1]]
# Discrete scores for observations 61-70
discrete_scores[[2]][c(61:70), ]
# Contributions of discrete features for observations 61-70
discrete_scores[[3]][c(61:70), ]

## ----cont_scores----------------------------------------------------------------------------------
continuous_scores <- cont_scores(data = dt, cont_cols = c(6:10), sample_size = 256,
                                 ntrees = 500, ndim = 0, max_depth = 100, seed_num = 1)
# Continuous scores for first 10 observations
continuous_scores[c(1:10), ]

## ----score_profile--------------------------------------------------------------------------------
# Add everything in a data frame
score_profile_dt <- data.frame('Type' = as.factor(dt[, 11]),
                               'Discrete_Score' = discrete_scores[[2]][, 2],
                               'Continuous_Score' = continuous_scores[, 2])
# Score profile plot
score_profile_dt <- score_profile_dt[order(score_profile_dt$Type), ]
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(score_profile_dt[, c(2 ,3)],
     col = score_profile_dt[, 1],
     pch = 16,
     xlab = 'Discrete Score',
     ylab = 'Continuous Score',
     main = "Score profile plot for generated data set")
legend("topright",
       inset=c(-0.325,0),
       legend=c("Inlier",
                "Single Marginal",
                "Combined Marginal",
                "Joint"),
       col = c(1, 2, 3, 4),
       pch=16,
       title="Outlier Type",
       cex = 0.85)

## ----marg_outs_scores-----------------------------------------------------------------------------
marginal_outliers <- marg_outs_scores(data = dt,
                                      disc_cols = c(1:5),
                                      outscorediscdf = discrete_scores[[2]],
                                      outscorecontdf = continuous_scores,
                                      outscorediscdfcells = discrete_scores[[3]],
                                      alpha = 0.01,
                                      rho = 0.20, epsilon = 0.02)
table(dt[unique(unlist(marginal_outliers)), 11])

## ----marg_outs------------------------------------------------------------------------------------
marginal_outliers_2 <- marg_outs(data = dt,
                                 disc_cols = c(1:5),
                                 cont_cols = c(6:10),
                                 alpha = 0.01, MAXLEN = 0,
                                 rho = 0.20, epsilon = 0.02)
all(unique(unlist(marginal_outliers)) == unique(unlist(marginal_outliers_2)))

## ----assoc_detect---------------------------------------------------------------------------------
assoctns1 <- assoc_detect(data = dt,
                          marginals = unique(unlist(marginal_outliers)),
                          target_inx = 1,
                          pred_inx = c(6:10),
                          delta = 0.50,
                          mink_order = 1,
                          alpha1 = 1e-3,
                          alpha2 = 1e-2)
assoctns2 <- assoc_detect(data = dt,
                          marginals = unique(unlist(marginal_outliers)),
                          target_inx = 2,
                          pred_inx = c(6:10),
                          delta = 0.50,
                          mink_order = 1,
                          alpha1 = 1e-3,
                          alpha2 = 1e-2)
assoctns3 <- assoc_detect(data = dt,
                          marginals = unique(unlist(marginal_outliers)),
                          target_inx = 3,
                          pred_inx = c(6:10),
                          delta = 0.50,
                          mink_order = 1,
                          alpha1 = 1e-3,
                          alpha2 = 1e-2)
assoctns4 <- assoc_detect(data = dt,
                          marginals = unique(unlist(marginal_outliers)),
                          target_inx = 4,
                          pred_inx = c(6:10),
                          delta = 0.50,
                          mink_order = 1,
                          alpha1 = 1e-3,
                          alpha2 = 1e-2)
assoctns5 <- assoc_detect(data = dt,
                          marginals = unique(unlist(marginal_outliers)),
                          target_inx = 5,
                          pred_inx = c(6:10),
                          delta = 0.50,
                          mink_order = 1,
                          alpha1 = 1e-3,
                          alpha2 = 1e-2)

## ----kde_classif----------------------------------------------------------------------------------
kde_classifications <- kde_classif(data = dt,
                                   target_inx = 1,
                                   pred_inx = c(6, 7),
                                   marg_outs = unique(unlist(marginal_outliers)),
                                   Lambda_i = 0,
                                   kernel = "gauss",
                                   alpha_val = 0.3)
plot(x = seq(1, 20, by =.5), y = kde_classifications[[1]],
     type = 'l', lwd = 2, col = 'navy',
     xlab = expression(Lambda[i]~"*"),
     ylab = "Misclassified Observations",
     main = "Misclassified observations for varying threshold values")

## ----consec_angles--------------------------------------------------------------------------------
Lambda_star <- consec_angles(vec = kde_classifications[[1]],
                             range = seq(1, 20, by = .5),
                             drop_tol = 3,
                             range_tol = 21)
print(Lambda_star)

# Use Lambda_star
Lambda_star_inx <- match(Lambda_star, seq(1, 20, by = .5))
# Joint outliers detected
joint_outs_det <- kde_classifications[[2]][[Lambda_star_inx]]
print(length(joint_outs_det))
summary(dt[joint_outs_det, ])

## ----elbow_angle----------------------------------------------------------------------------------
angle <- elbow_angle(vec = kde_classifications[[1]],
                     range = seq(1, 20, by = .5))
print(angle)

## ----joint_outs-----------------------------------------------------------------------------------
joint_outliers <- joint_outs(data = dt,
                             marg_outs = unique(unlist(marginal_outliers)),
                             assoc_target = c(1, 2),
                             assoc_vars = list(c(6, 7), c(6, 7)),
                             method = "consec_angles",
                             drop_tol = 3,
                             range_tol = 21)
print(length(joint_outliers))
table(dt[joint_outliers, 11])

## ----DOMID_test-----------------------------------------------------------------------------------
outliers_detected <- DOMID(data = dt, disc_cols = c(1:5), cont_cols = c(6:10),
                           alpha = 0.01, MAXLEN = 0, rho = 0.2, epsilon = 0.02,
                           sample_size = 256, ntrees = 500, ndim = 0,
                           max_depth = 100, seed_num = 1, delta = 0.5,
                           mink_order = 1, alpha1 = 1e-3, alpha2 = 1e-2,
                           method = "consec_angles", drop_tol = 3,
                           range_tol = 21)
all(c(unique(unlist(marginal_outliers)), joint_outliers) %in% unique(unlist(outliers_detected[1:4])))
all(unique(unlist(outliers_detected[1:4])) %in% c(unique(unlist(marginal_outliers)),
                                                  joint_outliers))

