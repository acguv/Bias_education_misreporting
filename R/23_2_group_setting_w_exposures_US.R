
# ---------------------------------------------------------------------------- #
# Project:  Estimating bias in educational inequalities in mortality
# Author: Ana C. Gomez-Ugarte
# Title: Additional analysis. Estimate scenarios for 2 groups with misreporting
# in death certificates and censu (US data)
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
source("R/Function_Simulation.R")

# ---------------------------------------------------------------------------- #
#     1. Read data
# ---------------------------------------------------------------------------- #

# Underlying mortality (smooth rates)
load("inter_data/mx_true_us_2004_2006_hendi_2groups.RData")

# WHO standard population 
who_std <- read.csv("Data/WHO_std_single_age_90+.csv")

# ---------------------------------------------------------------------------- #
#     2. Set up the variables/matrices for the scenarios
# ---------------------------------------------------------------------------- #

us_edu <- eta.hat_df

# Define variables
g = 2
ages = c(30:90)
n = length(ages)

# WHO standard population
who_std <- who_std %>%
  filter(Age >= 30 & Age<= 90) %>%
  mutate(Prop = Prop/sum(Prop)) %>%
  rename(age = Age) 

# Define case
case_nm = "case_1"
p = 1          # Case 1, 4, 5
# p = 0.5        # Case 2
# p = 5          # Case 3
ineq = 0       # Cases 1, 2, 3
# ineq = 0.5     # Case 4
# ineq = -0.2     # Case 5

# True mortality rate
gamma <- us_edu %>%
  mutate(edu_num = case_when(edu == "Low" ~ 1,
                             edu == "Middle" ~ 2,
                             edu == "High" ~ 3)) %>%
  filter(edu_num %in% c(1,3) & sex == "Males") %>% # For the first case with only two education groups
  arrange(sex, edu_num, age) %>%
  mutate(mx = case_when(edu == "Low" ~ mx*exp(ineq),     # Change inequality for cases 1, 4, 5
                        TRUE ~ mx)) %>%
  .$mx

# Population exposures
Nx <- us_edu %>%
  mutate(edu_num = case_when(edu == "Low" ~ 1,
                             edu == "Middle" ~ 2,
                             edu == "High" ~ 3)) %>%
  filter(edu_num %in% c(1,3) & sex == "Males") %>% # For the first case with only two education groups
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
edu_ranks_2 <- us_edu %>%
  filter(edu %in% c("Low", "High") & sex == "Males") %>%
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
edu_weights_2 <- us_edu %>%
  filter(edu %in% c("Low", "High") & sex == "Males") %>% 
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
edu <- us_edu %>%
  mutate(edu_num = case_when(edu == "Low" ~ 1,
                             edu == "Middle" ~ 2,
                             edu == "High" ~ 3)) %>%
  filter(edu_num %in% c(1,3) & sex == "Males") %>% # For the first case with only two education groups
  arrange(sex, edu_num, age) %>%
  .$edu

# Mortality rate of the total population
# Estimated as the population-weighted average of the education-specific mortality rates
mu_tot <- us_edu %>%
  filter(edu %in% c("Low", "High") & sex == "Males") %>%
  mutate(pop = case_when(edu == "High" ~ pop * p,            # Change size for cases 1, 2, 3
                         TRUE ~ pop)) %>%
  group_by(age) %>%
  summarise(mu = sum(mx*pop)/sum(pop))


# ---------------------------------------------------------------------------- #
#     3. Estimate scenarios
# ---------------------------------------------------------------------------- #
# Estimate all scenarios of education misreporting

# Define data frame to save results
scen_e <- setNames(data.frame(matrix(ncol = 10, nrow = 0)), 
                    c("education","age", "mu_real", "mu_observed", "ll", "ul", "i", "j", "k", "l"))

for (i in seq(0,.7,.1)) {
  for (j in seq(0,.7,.1)) {
    for (k in seq(0,.4,.1)) {
      for (l in seq(0,.4,.1)) {
        # Education misreporting in death certificates
        M_0 <- matrix(0 , n, n)
        M_ll <- diag(1-i, n, n)
        M_lh <- diag(i, n, n)
        M_hl <- diag(j, n, n)
        M_hh <- diag(1-j, n, n)
        
        # Matrix with education misreporting rates in death certificates
        M <- rbind(cbind(M_ll, M_hl), cbind(M_lh, M_hh))
        colSums(M) # Rows have to sum up to 1
        
        # Education misreporting in the census
        M_0_e <- matrix(0 , n, n)
        M_ll_e <- diag(1-k, n, n)
        M_lh_e <- diag(k, n, n)
        M_hl_e <- diag(l, n, n)
        M_hh_e <- diag(1-l, n, n)
        
        # Matrix with education missreporting rates in the census
        M_e <- rbind(cbind(M_ll_e, M_hl_e), cbind(M_lh_e, M_hh_e))
        colSums(M_e) # Rows have to sum up to 1
        
        temp <- simulation_w_exposures(Nx = mNx, gamma = gamma, coverage = C, age_mis = P, edu_mis = M, 
                                       ages, g,  coverage_exp = C, age_mis_exp = P, 
                                       edu_mis_exp = M_e, edu_cat = edu) %>%
          mutate(i = i, j = j, k = k, l = l)
        
      scen_e <- rbind(scen_e, temp)
      }
    }
  }
}

scen_e = list(scen = scen_e, edu_ranks = edu_ranks_2, edu_weights = edu_weights_2)

# ---------------------------------------------------------------------------- #
#     3. Save results
# ---------------------------------------------------------------------------- #
save(scen_e, file = paste0("Results/US/",case_nm,"/scen_e.rds"))



