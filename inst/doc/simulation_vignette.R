## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_chunk$set(echo = TRUE)


## ----setup--------------------------------------------------------------------
# Load necessary libraries
library(dplyr)
library(fetwfe)

## -----------------------------------------------------------------------------
# Generate the coefficient object for simulation
sim_coefs <- genCoefs(
  R         = 3, 
  T         = 6, 
  d         = 2, 
  density   = 0.1, 
  eff_size  = 2, 
  seed      = 101
)

## -----------------------------------------------------------------------------
# Simulate panel data based on the coefficients
sim_data <- simulateData(
  sim_coefs,
  N = 30,
  sig_eps_sq = 5,
  sig_eps_c_sq = 5,
  distribution = "gaussian"
  )

## -----------------------------------------------------------------------------
head(sim_data$pdata)

## -----------------------------------------------------------------------------
result <- fetwfeWithSimulatedData(sim_data)

## -----------------------------------------------------------------------------
# Overall ATT estimate
cat("Estimated Overall ATT:", result$att_hat, "\n")

# 95% confidence interval
ci_lower <- result$att_hat - qnorm(0.975) * result$att_se
ci_upper <- result$att_hat + qnorm(0.975) * result$att_se
cat("95% CI for ATT: [", ci_lower, ", ", ci_upper, "]\n")

# Cohort‐specific ATTs
print(result$catt_df)


## -----------------------------------------------------------------------------
# Extract the true treatment effects
true_tes <- getTes(sim_coefs)

# Print the true overall treatment effect
cat("True Overall ATT:", true_tes$att_true, "\n")

# Print the cohort-specific treatment effects
print(true_tes$actual_cohort_tes)

## -----------------------------------------------------------------------------
squared_error <- (result$att_hat - true_tes$att_true)^2

cat("Squared error of ATT estimate:", squared_error, "\n")

## -----------------------------------------------------------------------------
coefs <- genCoefs(R = 3, T = 6, d = 2, density = 0.1, eff_size = 2, seed = 2025)

result_piped <- coefs |>
  simulateData(N = 30, sig_eps_sq = 5, sig_eps_c_sq = 5) |>
  fetwfeWithSimulatedData()

cat("Estimated Overall ATT from piped workflow:", result_piped$att_hat, "\n")

true_tes_piped <- coefs |> getTes()

# Print the true overall treatment effect
cat("True Overall ATT:", true_tes_piped$att_true, "\n")

# Print the squared estimation error
squared_error_piped = (result_piped$att_hat - true_tes_piped$att_true)^2

cat("Squared estimation error:", squared_error_piped, "\n")

