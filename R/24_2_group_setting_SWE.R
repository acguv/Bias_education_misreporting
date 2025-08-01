
# ---------------------------------------------------------------------------- #
# Project:  Estimating bias in educational inequalities in mortality
# Title": Estimate scenarios for 2 groups (US data)
# ---------------------------------------------------------------------------- #
# Content:
#   0. Working directory, packages and functions
#   1. Read data
#   2. Set up the variables/matrices for the scenarios
#   3. Estimate scenarios
#   4. Save results
# ---------------------------------------------------------------------------- #
#     0. Working directory, package and functions
# ---------------------------------------------------------------------------- #
rm(list = ls())

### Setup
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(broom)

# Import functions
source("R/01_Function_Simulation.R")

# ---------------------------------------------------------------------------- #
#     1. Read data
# ---------------------------------------------------------------------------- #

# Underlying mortality (smooth rates)
load("inter_data/SWE/mx_true_swe_2016_2groups.RData")

# WHO standard population 
who_std <- read.csv("Data/WHO_std_single_age_110.csv")

# ---------------------------------------------------------------------------- #
#     2. Set up the variables/matrices for the scenarios
# ---------------------------------------------------------------------------- #

swe_edu <- mx.smooth_df %>%
  rename(pop = e_smooth)

# Define variables
g = 2
ages = unique(mx.smooth_df$age)
n = length(ages)

# WHO standard population
who_std <- who_std %>%
  mutate(Prop = Prop/sum(Prop)) %>%
  rename(age = Age) 

# Define case
case_nm = "case_5"
p = 1          # Case 1, 4, 5
# p = 0.5        # Case 2
# p = 5          # Case 3
# ineq = 0       # Cases 1, 2, 3
# ineq = 0.5     # Case 4
ineq = -0.2     # Case 5

# True mortality rate
gamma <- swe_edu %>%
  mutate(edu_num = case_when(edu == "Low" ~ 1,
                             edu == "Middle" ~ 2,
                             edu == "High" ~ 3)) %>%
  filter(edu_num %in% c(1,3) & sex == "F") %>% # For the first case with only two education groups
  arrange(sex, edu_num, age) %>%
  mutate(mx = case_when(edu == "Low" ~ mx*exp(ineq),     # Change inequality for cases 1, 4, 5
                         TRUE ~ mx)) %>%
  .$mx

# Population exposures
Nx <- swe_edu %>%
  mutate(edu_num = case_when(edu == "Low" ~ 1,
                             edu == "Middle" ~ 2,
                             edu == "High" ~ 3)) %>%
  filter(edu_num %in% c(1,3) & sex == "F") %>% # For the first case with only two education groups
  arrange(sex, edu_num, age) %>%
  mutate(pop = case_when(edu == "High" ~ pop * p,             # Change size for cases 1, 2, 3
                         TRUE ~ pop)) %>%
  mutate(pop = case_when(edu == "Low" ~ pop * exp(-ineq),        # Change inequality for cases 1, 4, 5
                         TRUE ~ pop)) %>%
  .$pop

# Matrix with exposures 
mNx <- diag(Nx) # Matrix of population exposures

# Matrix with coverage (for now it is in identity matrix)
C <- diag(2*n)

# Matrix with age misreporting (for now it is in identity matrix)
P <- diag(2*n)

# Education ranks (for inequality measures)
edu_ranks_2 <- swe_edu %>%
  filter(edu %in% c("Low", "High") & sex == "F") %>%
  mutate(pop = case_when(edu == "High" ~ pop * p,           # Change size for cases 1, 2, 3
                         TRUE ~ pop)) %>%
  mutate(pop = case_when(edu == "Low" ~ pop * exp(-ineq),      # Change inequality for cases 1, 4, 5
                         TRUE ~ pop)) %>%
  group_by(edu) %>%
  summarise(Nx = sum(pop)) %>%
  mutate(freq = Nx/sum(Nx)) %>%
  dplyr::select(-Nx) %>%
  spread(edu, freq) %>%
  mutate(Low1 = 0.5*Low + High,
         High1 = 0.5*High) %>%
  dplyr::select(Low1, High1) %>%
  rename(Low = Low1, High = High1) %>%
  gather(education, Edu_ranks, Low:High) 

# Education weights (for inequality measures)
edu_weights_2 <- swe_edu %>%
  filter(edu %in% c("Low", "High") & sex == "F") %>% 
  mutate(pop = case_when(edu == "High" ~ pop * p,             # Change size for cases 1, 2, 3
                         TRUE ~ pop)) %>%
  mutate(pop = case_when(edu == "Low" ~ pop * exp(-ineq),      # Change inequality for cases 1, 4, 5
                         TRUE ~ pop)) %>%
  group_by(edu) %>%
  summarise(Nx = sum(pop)) %>%
  mutate(freq = Nx/sum(Nx)) %>%
  dplyr::select(-Nx) %>%
  rename(education = edu, edu_weights = freq)

# Vector with education levels
edu <- swe_edu %>%
  mutate(edu_num = case_when(edu == "Low" ~ 1,
                             edu == "Middle" ~ 2,
                             edu == "High" ~ 3)) %>%
  filter(edu_num %in% c(1,3) & sex == "F") %>% # For the first case with only two education groups
  arrange(sex, edu_num, age) %>%
  .$edu

# Mortality rate of the total population
# Estimated as the population-weighted average of the education-specific mortality rates
mu_tot <- swe_edu %>%
  filter(edu %in% c("Low", "High") & sex == "F") %>%
  mutate(pop = case_when(edu == "High" ~ pop * p,            # Change size for cases 1, 2, 3
                         TRUE ~ pop)) %>%
  group_by(age) %>%
  summarise(mu = sum(mx*pop)/sum(pop))


# ---------------------------------------------------------------------------- #
#     3. Estimate scenarios
# ---------------------------------------------------------------------------- #
# Estimate all scenarios of education misreporting

# Define data frame to save results
scen <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), 
                c("education","age", "mu_real", "mu_observed", "ll", "ul", "i", "j"))

for (i in seq(0,.5,.02)) {
  for (j in seq(0,.5,.02)) {
    M_0 <- matrix(0 , n, n)
    M_ll <- diag(1-i, n, n)
    M_lh <- diag(i, n, n)
    M_hl <- diag(j, n, n)
    M_hh <- diag(1-j, n, n)

    # Matrix with education misreporting
    M <- rbind(cbind(M_ll, M_hl), cbind(M_lh, M_hh))
    colSums(M) # Rows have to sum up to 1
    
    temp <- scenario_func(Nx = mNx, gamma = gamma, coverage = C, age_mis = P, edu_mis = M, 
                          ages, g, edu_cat = edu) %>%
      mutate(i = i, j = j)
    
    scen <- rbind(scen, temp)
  }
}

ineq_bias <- run_ineq_measures(ex = scen, seq_i = seq(0,0.5,.02), seq_j = seq(0,0.5,.02),
                                   edu_ranks = edu_ranks_2, 
                                   edu_weights = edu_weights_2)

scen = list(scen = scen, edu_ranks = edu_ranks_2, edu_weights = edu_weights_2, 
              ineq_bias = ineq_bias)

# ---------------------------------------------------------------------------- #
#     3. Save results
# ---------------------------------------------------------------------------- #
save(scen, file = paste0("Results/SWE/",case_nm,"/scen.rds"))



