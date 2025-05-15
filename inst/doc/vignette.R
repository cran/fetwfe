## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)

## -----------------------------------------------------------------------------
# Set seed for reproducibility
set.seed(123456L)

# 20 time periods, 30 individuals, and 5 waves of treatment
tmax = 20; imax = 30; nlvls = 5

dat = 
  expand.grid(time = 1:tmax, id = 1:imax) |>
  within({
    
    cohort      = NA
    effect      = NA
    first_treat = NA
    
    for (chrt in 1:imax) {
      cohort = ifelse(id==chrt, sample.int(nlvls, 1), cohort)
    }
    
    for (lvls in 1:nlvls) {
      effect      = ifelse(cohort==lvls, sample(2:10, 1), effect)
      first_treat = ifelse(cohort==lvls, sample(1:(tmax+6), 1), first_treat)
    }
    
    first_treat = ifelse(first_treat>tmax, Inf, first_treat)
    treat       = time >= first_treat
    rel_time    = time - first_treat
    y           = id + time + ifelse(treat, effect*rel_time, 0) + rnorm(imax*tmax)
    
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
    {{ time_var }}, {{ unit_var }}, {{ treatment }}, {{ response }}
  ) 

# Preview the resulting pdata dataframe
head(pdata)

## -----------------------------------------------------------------------------

library(fetwfe)

# Run the FETWFE estimator on the simulated data
result <- fetwfe(
  pdata = pdata,              # The panel dataset
  time_var = "time",          # The time variable
  unit_var = "unit",          # The unit identifier
  treatment = "treated",      # The treatment dummy indicator
  response = "response"      # The response variable
)

# Display the overall average treatment effect estimate
cat("Estimated Overall ATT:", result$att_hat, "\n")

## -----------------------------------------------------------------------------
library(bacondecomp)  # for the example data

# Load the example data
data(divorce)

set.seed(23451)

# Suppose we wish to estimate the effect of a policy (here represented by the variable "changed")
# on the response "suiciderate_elast_jag" using covariates "murderrate", "lnpersinc", and "afdcrolls".
# Here
# - 'year' is the time period variable (as an integer),
# - 'st' is the unit identifier,
# - 'changed' is the treatment indicator (with 0 = untreated, 1 = treated),
# 
# The `fetwfe()` function will automatically take care of removing units that were treated in the
# first time period.

# Call the estimator
res <- fetwfe(
    pdata=divorce[divorce$sex == 2, ],
    time_var="year",
    unit_var="st",
    treatment="changed",
    covs=c("murderrate", "lnpersinc", "afdcrolls"),
    response="suiciderate_elast_jag"
    )

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

