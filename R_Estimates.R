
rm(list = ls())

library(devtools)
# install.packages('incidence')
# devtools::install_github('annecori/EpiEstim')
library(EpiEstim)
library(tidyverse)
library(incidence)
library(readxl)
library(EpiWeek)
library(gridExtra)



source('R_Estimates_functions.R')

file <- "epicurve.xlsx"

# ----- Parameters
si_mean_days = 14.9
si_sd_days   = 3.7
R_prior      = 10
R_sd_prior   = 5
weeks_window = 4


# ------ Countries

dat <- read_excel(file, sheet = "VEN", col_names = TRUE)
variables_needed <- c("year","week","total_cases","imported", "local","place")
dat <- filter(dat) %>%
  select(variables_needed) 
VEN <- dat

Rt_VEN <- R_EstimateIL (dat=VEN,
              si_mean_days = si_mean_days,
              si_sd_days   = si_sd_days,
              window_units = 'weeks',
              t_window     = weeks_window,
              week_start   = 33,
              year_start   = 2017, 
              R_prior = R_prior,
              R_sd_prior = R_sd_prior)


plot_R_estimates (Rt_VEN, size_text = 18, LPV = 0.90,LPH = 0.85)


