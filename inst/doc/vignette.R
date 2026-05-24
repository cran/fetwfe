## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)

## -----------------------------------------------------------------------------
# Set seed for reproducibility
set.seed(123456L)

# 20 time periods, 30 individuals, and 5 cohort levels
tmax = 20; imax = 30; nlvls = 5

dat =
  expand.grid(time = 1:tmax, id = 1:imax) |>
  within({
    cohort      = NA
    effect      = NA
    first_treat = NA
    cov1        = NA
    cov2        = NA
    cov3        = NA
    for (chrt in 1:imax) {
      cohort = ifelse(id==chrt, sample.int(nlvls, 1), cohort)
    }
    for (lvls in 1:nlvls) {
      effect      = ifelse(cohort==lvls, sample(2:10, 1), effect)
      first_treat = ifelse(cohort==lvls, sample(1:(tmax+6), 1), first_treat)
    }
    # three time-invariant covariates: one value per individual,
    # drawn AFTER the timing loop so first_treat is unchanged
    for (chrt in 1:imax) {
      cov1 = ifelse(id==chrt, rnorm(1), cov1)
      cov2 = ifelse(id==chrt, rnorm(1), cov2)
      cov3 = ifelse(id==chrt, rnorm(1), cov3)
    }
    first_treat = ifelse(first_treat>tmax, Inf, first_treat)
    treat       = time >= first_treat
    rel_time    = time - first_treat
    y           = id + time + ifelse(treat, effect*rel_time, 0) +
                  0.5*cov1 - 0.7*cov2 + 1.2*cov3 + rnorm(imax*tmax)
    rm(chrt, lvls, cohort, effect)
  })

head(dat)

## -----------------------------------------------------------------------------
library(dplyr)

# Specify column names for the pdata format
time_var <- "time"       # Column for the time period
unit_var <- "unit"       # Column for the unit identifier
treatment <- "treated"   # Column for the treatment dummy indicator
response <- "response"   # Column for the response variable

# Convert the dataset
pdata <- dat |>
  mutate(
    # Rename id to unit and convert to character
    {{ unit_var }} := as.character(id),
    # Ensure treatment dummy is 0/1
    {{ treatment }} := as.integer(treat),
    # Rename y to response
    {{ response }} := y
  ) |>
  select(
    {{ time_var }}, {{ unit_var }}, {{ treatment }}, {{ response }},
    cov1, cov2, cov3
  )

# Preview the resulting pdata dataframe
head(pdata)

## -----------------------------------------------------------------------------

library(fetwfe)

# Run the FETWFE estimator on the simulated data
result <- fetwfe(
  pdata = pdata,                        # The panel dataset
  time_var = "time",                    # The time variable
  unit_var = "unit",                    # The unit identifier
  treatment = "treated",                # The treatment dummy indicator
  response = "response",                # The response variable
  covs = c("cov1", "cov2", "cov3")      # The time-invariant covariates
)

# Display the average treatment effect estimates
summary(result)

## -----------------------------------------------------------------------------
library(bacondecomp)  # for the example data

# Load the example data
data(castle)

# Response: the log homicide rate, so the ATT reads roughly as a
# percentage change in the homicide rate.
castle$l_homicide <- log(castle$homicide)

# Treatment indicator. `cdl` records the share of the year the
# castle-doctrine law was in effect; a state is treated from the year its
# law took effect, so we binarize with `cdl > 0`. `fetwfe()` requires the
# treatment column to be an integer 0/1 absorbing indicator.
castle$treated <- as.integer(castle$cdl > 0)

# Call the estimator. Here
# - 'year' is the time period variable (an integer),
# - 'state' is the unit identifier,
# - 'treated' is the absorbing treatment indicator.
res <- fetwfe(
    pdata = castle,
    time_var = "year",
    unit_var = "state",
    treatment = "treated",
    response = "l_homicide"
    )

summary(res)

# Average treatment effect on the treated units (in percentage point
# units)
100 * res$att_hat

# Conservative 95% confidence interval for ATT (in percentage point units)

low_att <- 100 * (res$att_hat - qnorm(1 - 0.05 / 2) * res$att_se)
high_att <- 100 * (res$att_hat + qnorm(1 - 0.05 / 2) * res$att_se)

c(low_att, high_att)

# Cohort average treatment effects and confidence intervals (in percentage
# point units)

catt_df_pct <- res$catt_df
catt_df_pct[["Estimated TE"]] <- 100 * catt_df_pct[["Estimated TE"]]
catt_df_pct[["SE"]] <- 100 * catt_df_pct[["SE"]]
catt_df_pct[["ConfIntLow"]] <- 100 * catt_df_pct[["ConfIntLow"]]
catt_df_pct[["ConfIntHigh"]] <- 100 * catt_df_pct[["ConfIntHigh"]]

catt_df_pct

## ----zero-effect-demo, message = FALSE----------------------------------------
set.seed(2026)
sim <- genCoefs(R = 3, T = 6, d = 2, density = 0.5, eff_size = 2)
dat <- simulateData(sim, N = 120, sig_eps_sq = 1, sig_eps_c_sq = 0.5)
res_demo <- fetwfeWithSimulatedData(dat, verbose = FALSE)
res_demo$catt_df

## ----tidy-demo, eval = requireNamespace("broom", quietly = TRUE)--------------
library(broom)

tidy_res <- tidy(result)
tidy_res

## ----glance-demo, eval = requireNamespace("broom", quietly = TRUE)------------
glance(result)

## ----augment-demo, eval = requireNamespace("broom", quietly = TRUE)-----------
head(augment(result, data = pdata))

## ----tidy-plot, eval = requireNamespace("broom", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("dplyr", quietly = TRUE)----
library(ggplot2)
tidy_res |> 
  dplyr::filter(term != "ATT") |>
  # Remove non-digits to extract the number, then reorder the factor levels
  dplyr::mutate(term = reorder(term, as.numeric(gsub("\\D", "", term)))) |>
  ggplot(aes(x = term, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = NULL, y = "Cohort treatment effect", title = "Cohort ATTs")

