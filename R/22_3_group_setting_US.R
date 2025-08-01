
# ---------------------------------------------------------------------------- #
# Project:  Estimating bias in educational inequalities in mortality
# Title: Estimate education misreporting scenarios for the 3 groups (US data)
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
library(tidyverse)
library(dplyr)
library(ggplot2)
library(broom)

# ---------------------------------------------------------------------------- #
#     1. Read data
# ---------------------------------------------------------------------------- #

# Import functions
source("R/01_Function_Simulation.R")

# Smoothed mortality rates, deaths and exposures
load("inter_data/US/mx_true_us_2004_2006_hendi_3groups_v2.RData")

# WHO standard population
who_std <- read.csv("Data/WHO_std_single_age_110.csv")

# ---------------------------------------------------------------------------- #
#     2. Set up the variables/matrices for the scenarios
# ---------------------------------------------------------------------------- #
us_edu <- mx.ungrp_df

# Define variables
g = 3
ages = c(30:110)
n = length(ages)
case_nm = "case_1"

# WHO standard population
who_std <- who_std %>%
  # filter(Age >= 30 & Age<= 90) %>%
  mutate(Prop = Prop/sum(Prop)) %>%
  rename(age = Age) 

# True mortality rates
gamma <- us_edu %>%
  mutate(edu_num = case_when(edu == "Low" ~ 1,
                             edu == "Middle" ~ 2,
                             edu == "High" ~ 3)) %>%
  filter(sex == "Males") %>% 
  arrange(sex, edu_num, age) %>%
  .$mx

# Population exposures
Nx <- us_edu %>%
  mutate(edu_num = case_when(edu == "Low" ~ 1,
                             edu == "Middle" ~ 2,
                             edu == "High" ~ 3)) %>%
  filter(sex == "Males") %>% 
  arrange(sex, edu_num, age) %>%
  .$pop

# Matrix of population exposures
mNx <- diag(Nx)

# Matrix with coverage (for now it is in identity matrix)
C <- diag(g*n)

# Matrix with age misreporting (for now it is in identity matrix)
P <- diag(g*n)

# Education ranks (for inequality measures)
edu_ranks_3 <- us_edu %>%
  filter(sex == "Males") %>%
  group_by(edu) %>%
  summarise(Nx = sum(pop)) %>%
  mutate(freq = Nx/sum(Nx)) %>%
  dplyr::select(-Nx) %>%
  spread(edu, freq) %>%
  mutate(Low1 = 0.5*Low + Middle + High,
         Middle1 = 0.5*Middle + High,
         High1 = 0.5*High) %>%
  dplyr::select(Low1, Middle1, High1) %>%
  rename(Low = Low1, Middle = Middle1, High = High1) %>%
  gather(education, Edu_ranks, Low:High)

# Education weights (for inequality measures)
edu_weights_3 <- us_edu %>%
  filter(sex == "Males") %>%
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
  filter(sex == "Males") %>% 
  arrange(sex, edu_num, age) %>%
  .$edu

# Mortality rate of the total population
# Estimated as the population-weighted average of the education-specific mortality rates
mu_tot <- us_edu %>%
  filter(sex == "Males") %>%
  group_by(age) %>%
  summarise(mu = sum(mx*pop)/sum(pop))
  

# ---------------------------------------------------------------------------- #
#     3. Estimate scenarios
# ---------------------------------------------------------------------------- #
# Estimate all scenarios of education misreporting

# Define data frame to save results
scen3 <- setNames(data.frame(matrix(ncol = 10, nrow = 0)), 
                  c("education","age", "mu_real", "mu_observed", "ll", "ul", "i", "j", "k", "l"))

for(i in seq(0,.4,.05)){
  for(j in seq(0, .4, .05)){
    for(k in seq(0, .4, .05)){
      for(l in seq(0, .4, .05)){
        M_0 <- matrix(0 , n, n)
        M_ll <- diag(1-i, n, n)
        M_lm <- diag(i, n, n)
        M_lh <- diag(0, n, n)
        M_ml <- diag(l, n, n)
        M_mm <- diag(1-j-l, n, n)
        M_mh <- diag(j, n, n)
        M_hl <- diag(0, n, n)
        M_hm <- diag(k, n, n)
        M_hh <- diag(1-k, n, n)
        
        # Matrix with education misstatement
        M <- rbind(cbind(M_ll, M_ml, M_hl), cbind(M_lm, M_mm, M_hm), cbind(M_lh, M_mh, M_hh))
        colSums(M) # Rows have to sum up to 1
        
        temp <- scenario_func(Nx = mNx, gamma = gamma, coverage = C, age_mis = P, 
                                edu_mis = M, ages, g, edu) %>%
          mutate(i = i, j = j, k = k, l = l)
        
        scen3 <- rbind(scen3, temp)  
      }
    }
  }
}

ineq_bias3 <- run_ineq_measures_3(ex = scen3, seq_i = seq(0,0.4,.05), seq_j = seq(0,0.4,.05),
                                  seq_k = seq(0,0.4,.05), seq_l = seq(0,0.4,.05), 
                                  edu_ranks = edu_ranks_3, edu_weights = edu_weights_3)


scen3 = list(scen = scen3, edu_ranks = edu_ranks_3, edu_weights = edu_weights_3, 
            ineq_bias = ineq_bias3)


# ---------------------------------------------------------------------------- #
#     4. Save results
# ---------------------------------------------------------------------------- #
save(scen3, file = paste0("Results/US/",case_nm,"/scen3_v2.rds"))
