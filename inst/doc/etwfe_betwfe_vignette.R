## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(fetwfe)

## -----------------------------------------------------------------------------
sim_coefs <- genCoefs(
  R         = 3,
  T         = 6,
  d         = 2,
  density   = 0.5,
  eff_size  = 2,
  seed      = 20260510
)

sim_data <- simulateData(
  sim_coefs,
  N            = 120,
  sig_eps_sq   = 1,
  sig_eps_c_sq = 1
)

# True treatment effects (we'll compare estimator output to these):
true_tes <- getTes(sim_coefs)
cat("True overall ATT:", true_tes$att_true, "\n")
print(true_tes$actual_cohort_tes)

## -----------------------------------------------------------------------------
res_etwfe <- etwfeWithSimulatedData(sim_data)

summary(res_etwfe)

## -----------------------------------------------------------------------------
cat("True ATT:     ", true_tes$att_true, "\n")
cat("Estimated ATT:", res_etwfe$att_hat, "\n")
cat("Squared error:", (res_etwfe$att_hat - true_tes$att_true)^2, "\n")

## -----------------------------------------------------------------------------
res_betwfe <- betwfeWithSimulatedData(sim_data)

summary(res_betwfe)

## -----------------------------------------------------------------------------
cat("True ATT:        ", true_tes$att_true, "\n")
cat("etwfe() ATT:     ", res_etwfe$att_hat, "\n")
cat("betwfe() ATT:    ", res_betwfe$att_hat, "\n")
cat("etwfe sq. error: ", (res_etwfe$att_hat - true_tes$att_true)^2, "\n")
cat("betwfe sq. error:", (res_betwfe$att_hat - true_tes$att_true)^2, "\n")

## -----------------------------------------------------------------------------
sim_coefs_d0 <- genCoefs(
  R         = 3,
  T         = 6,
  d         = 0,
  density   = 0.5,
  eff_size  = 2,
  seed      = 20260510
)

sim_data_d0 <- simulateData(
  sim_coefs_d0,
  N            = 120,
  sig_eps_sq   = 1,
  sig_eps_c_sq = 1
)

true_tes_d0 <- getTes(sim_coefs_d0)
cat("True overall ATT (no covariates):", true_tes_d0$att_true, "\n")

## -----------------------------------------------------------------------------
res_etwfe_d0 <- etwfeWithSimulatedData(sim_data_d0)
summary(res_etwfe_d0)

## -----------------------------------------------------------------------------
res_fetwfe_d0 <- fetwfeWithSimulatedData(sim_data_d0)
summary(res_fetwfe_d0)

## -----------------------------------------------------------------------------
cat("True ATT:        ", true_tes_d0$att_true, "\n")
cat("etwfe() ATT:     ", res_etwfe_d0$att_hat, "\n")
cat("fetwfe() ATT:    ", res_fetwfe_d0$att_hat, "\n")
cat("etwfe sq. error: ", (res_etwfe_d0$att_hat - true_tes_d0$att_true)^2, "\n")
cat("fetwfe sq. error:", (res_fetwfe_d0$att_hat - true_tes_d0$att_true)^2, "\n")

## -----------------------------------------------------------------------------
eventStudy(res_etwfe)

## ----eval = requireNamespace("ggplot2", quietly = TRUE)-----------------------
plot(res_etwfe)

## -----------------------------------------------------------------------------
eventStudy(res_betwfe)

## ----eval = requireNamespace("ggplot2", quietly = TRUE)-----------------------
plot(res_betwfe)

