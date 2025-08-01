
# ---------------------------------------------------------------------------- #
# Project:  Estimating bias in educational inequalities in mortality
# Title: Data cleaning - US mortality from Hendi 2015
# ---------------------------------------------------------------------------- #
# Content:
#   0. Working directory, packages and functions
#   1. Read data 
#   2. Clean and smooth data
#   3. Save results
# ---------------------------------------------------------------------------- #
#     0. Working directory, package and functions
# ---------------------------------------------------------------------------- #
rm(list = ls())

library(dplyr)
library(tidyverse)
library(ungroup)
library(MortalitySmooth)

# Import functions
source("R/01_Function_Simulation.R")

# ---------------------------------------------------------------------------- #
#     1. Read data
# ---------------------------------------------------------------------------- #
us <- read.csv("Data/us_nonhispanic_white_NHIS_hendi.csv", sep = "\t")

# ---------------------------------------------------------------------------- #
#     1. Clean and smooth data
# ---------------------------------------------------------------------------- #

# Option 1: Estimate mortality rates for 2 education groups
us_mx <- us %>%
  mutate(edu = case_when(edu == "HS" | edu == "Less than HS" ~ "Low",
                         edu == "College or more" | edu == "Some college"  ~ "High")) %>%
  group_by(year, sex, edu, agegrp) %>%
  summarise(dx = sum(dx),
            pop = sum(pop)) %>%
  mutate(mx = dx/pop)

# Option 2: Estimate mortality rates for 3 education groups
# us_mx <- us %>%
#   mutate(edu = case_when(edu == "Less than HS" ~ "Low",
#                          edu == "HS" | edu == "Some college" ~ "Middle",
#                          edu == "College or more" ~ "High")) %>%
#   group_by(year, sex, edu, agegrp) %>%
#   summarise(dx = sum(dx),
#             pop = sum(pop)) %>%
#   mutate(mx = dx/pop)

### Ungrouping data into single ages

# Define values
ages <- seq(30,85,5)
n <- length(ages)
sex_categories <- unique(us_mx$sex)
edu_categories <- unique(us_mx$edu)
nl <- 26

# Initial values
mx.ungrp_list <- list()
i <- 1

# Ungroup each sex and education group
for(sex_value in sex_categories){
  for (edu_value in edu_categories) {
    y <- us_mx$dx[us_mx$edu == edu_value & us_mx$year == "2004-2006" & 
                    us_mx$sex == sex_value & us_mx$agegrp >= 30]
    e <- us_mx$pop[us_mx$edu == edu_value & us_mx$year == "2004-2006" & 
                     us_mx$sex == sex_value & us_mx$agegrp >= 30]
    
    e_ungrp <- pclm(ages, y = e, nlast = nl, control = list(lambda = 10E10))$fitted
    mx <- pclm(ages, y = y, nlast = nl, offset = e_ungrp, control = list(lambda = 10E10))
    
      
    mx.ungrp_list[[i]] <- data.frame(age =  seq(ages[1],max(ages)+nl-1,1), sex = sex_value, 
                                      edu = edu_value,
                                      mx = mx$fitted, 
                                      pop = e_ungrp, 
                                      dx = e_ungrp*mx$fitted)
    i = i + 1
    
  }
}

mx.ungrp_df <- do.call(rbind, mx.ungrp_list)


# Plots
mx.ungrp_df %>%
  ggplot() +
  geom_line(aes(x = age, y = log(mx), color = edu)) +
  facet_grid(~ sex)

mx.ungrp_df %>%
  ggplot() +
  geom_line(aes(x = age, y = pop, color = edu)) +
  facet_grid(~ sex)

mx.ungrp_df %>%
  ggplot() +
  geom_line(aes(x = age, y = dx, color = edu)) +
  facet_grid(~ sex)


# ---------------------------------------------------------------------------- #
#     3. Save results
# ---------------------------------------------------------------------------- #

save(mx.ungrp_df, file = "inter_data/US/mx_true_us_2004_2006_hendi_2groups_v2.RData")
# save(mx.ungrp_df, file = "inter_data/US/mx_true_us_2004_2006_hendi_3groups_v2.RData")
