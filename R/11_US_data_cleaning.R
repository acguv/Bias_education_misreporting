
# ---------------------------------------------------------------------------- #
# Project:  Estimating bias in educational inequalities in mortality
# Author: Ana C. Gomez-Ugarte
# Title: Data cleaning - US mortality from Hendi 2015
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

library(dplyr)
library(tidyverse)
library(ungroup)
library(MortalitySmooth)

# Import functions
source("R/Function_Simulation.R")

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
nl <- 6

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
    
    mx <- pclm(ages, y = y, nlast = nl, offset = e, control = list(lambda = 10E10))
    e_ungrp <- pclm(ages, y = e, nlast = nl, control = list(lambda = 10E10))
      
    mx.ungrp_list[[i]] <- data.frame(age = c(30:90), sex = sex_value, 
                                      edu = edu_value,
                                      mx = mx$fitted, 
                                      e_ungrp = e_ungrp$fitted, 
                                      y_ungrp = e_ungrp$fitted*mx$fitted)
    i = i + 1
    
  }
}

mx.ungrp_df <- do.call(rbind, mx.ungrp_list)

# Plots
# mx.smooth_df %>%
#   ggplot() + 
#   geom_line(aes(x = age, y = log(mx), color = edu)) + 
#   facet_grid(~ sex)

# mx.smooth_df %>%
#   ggplot() + 
#   geom_line(aes(x = age, y = e_smooth, color = edu)) + 
#   facet_grid(~ sex)


## Smooth using P-splines

# Define values
ages <- c(30:90)
n <- length(ages)
sex_categories <- unique(mx.ungrp_df$sex)
edu_categories <- unique(mx.ungrp_df$edu)

# Initial values
eta.hat_list <- list()
i <- 1

# Smooth each sex and education group
for(sex_value in sex_categories){
  for (edu_value in edu_categories) {
    y <- mx.ungrp_df$y_ungrp[mx.ungrp_df$edu == edu_value & 
                         mx.ungrp_df$sex == sex_value]
      
    e <- mx.ungrp_df$e_ungrp[mx.ungrp_df$edu == edu_value & 
                                 mx.ungrp_df$sex == sex_value]
    
    n <- length(y) 
    fx <- y/e
    
    B <- MortSmooth_bbase(x=ages, xl=min(ages), xr=max(ages), ndx=12, deg=3) 
    nb <- ncol(B)
    D <- diff(diag(nb), diff=3)
    tDD <- t(D)%*%D
    lambda <- 100
    P <- lambda * tDD
    
    eta <- log((y+1)/(e+1)) 
    for(it in 1:10){
      mu <- e*exp(eta)
      z <- (y - mu)/mu + eta
      W <- diag(c(mu))
      tBWB <- t(B) %*% W %*% B 
      tBWBpP <- tBWB + P
      tBWz <- t(B) %*% W %*% z
      betas <- solve(tBWBpP, tBWz) 
      old.eta <- eta
      eta <- B %*% betas
      dif.eta <- max(abs(old.eta - eta)) 
      if(dif.eta < 1e-6) break
      
    }
    
    xs <- seq(min(ages), max(ages), length=61)
    Bs <- MortSmooth_bbase(x=xs, xl=min(ages), xr=max(ages), ndx=12, deg=3) 
    etas.hat <- Bs%*%betas
    mus.hat <- exp(etas.hat)
    
    eta.hat_list[[i]] <- data.frame(age = ages, sex = sex_value, 
                                    edu = edu_value,
                                    mx = exp(etas.hat),
                                    pop = e, 
                                    dx = y)
    i = i + 1
  }
}

eta.hat_df <- do.call(rbind, eta.hat_list)

# Plot final result
# eta.hat_df %>%
#   ggplot() +
#   geom_line(aes(x = age, y = log(mx), color = edu)) +
#   geom_line(data = mx.ungrp_df , aes(x = age, y = log(mx), color = edu), lty = 2) +
#   facet_grid(~ sex)

# ---------------------------------------------------------------------------- #
#     3. Save results
# ---------------------------------------------------------------------------- #

save(eta.hat_df, file = "inter_data/mx_true_us_2004_2006_hendi_2groups.RData")
# save(eta.hat_df, file = "inter_data/mx_true_us_2004_2006_hendi_3groups.RData")
