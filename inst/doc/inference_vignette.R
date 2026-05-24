## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(fetwfe)

## ----eval = FALSE-------------------------------------------------------------
# fetwfe(..., se_type = "cluster")

## -----------------------------------------------------------------------------
set.seed(2026)
sim_coefs <- genCoefs(R = 3, T = 6, d = 2, density = 0.5, eff_size = 2)
sim_data  <- simulateData(
  sim_coefs,
  N = 120,
  sig_eps_sq = 1,
  sig_eps_c_sq = 0.5
)

res_default <- fetwfeWithSimulatedData(sim_data)
res_cluster <- fetwfeWithSimulatedData(sim_data, se_type = "cluster")

c(
  default = res_default$att_se,
  cluster = res_cluster$att_se
)

## -----------------------------------------------------------------------------
print(res_cluster)

