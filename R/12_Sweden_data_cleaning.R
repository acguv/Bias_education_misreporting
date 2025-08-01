
# ---------------------------------------------------------------------------- #
# Project:  Estimating bias in educational inequalities in mortality
# Title: Data cleaning - Sweden data from Eurostat
# ---------------------------------------------------------------------------- #
# Content:
#   0. Working directory, packages and functions
#   1. Read data from Eurostat
#   2. Clean and smooth data
#   3. Save results
# ---------------------------------------------------------------------------- #
#     0. Working directory, package and functions
# ---------------------------------------------------------------------------- #
rm(list = ls())

library(dplyr)
library(tidyverse)
library(ungroup)
library(eurostat)
library(ggplot2)

# Import functions
source("R/01_Function_Simulation.R")

# ---------------------------------------------------------------------------- #
#     1. Read data from Eurostat
# ---------------------------------------------------------------------------- #

# Population
pop <- get_eurostat("demo_pjanedu", time_format = "num") 

# Deaths
deaths <- get_eurostat("demo_maeduc", time_format = "num") 

# ---------------------------------------------------------------------------- #
#     2. Clean and smooth data
# ---------------------------------------------------------------------------- #

# Clean population data
pop2 <- pop %>%
  mutate(edu = case_when(isced11=="ED0-2" | isced11 == "UNK" | isced11=="ED3_4" ~ "Low",
                         isced11=="ED5-8" ~ "High",
                         isced11=="TOTAL" ~ "Total")) %>%
  # mutate(edu = case_when(isced11=="ED0-2" | isced11 == "UNK" ~ "Low",
  #                        isced11=="ED3_4" ~ "Middle",
  #                        isced11=="ED5-8" ~ "High",
  #                        isced11=="TOTAL" ~ "Total")) %>%
  filter(age != "TOTAL" & !is.na(edu)) %>%
  mutate(age = case_when(age == "Y_LT1" ~ 0,
                         age == "Y_OPEN" ~ 100,
                         age == "UNK" ~ 999,
                         TRUE ~ as.numeric(as.character(substr(age, 2, 3))))) %>%
  dplyr::select(-freq, -unit, -isced11) %>%
  rename(country = geo, year = TIME_PERIOD, pop = values) %>%
  arrange(country, sex, year, edu, age) %>%
  group_by(country, sex, year, edu, age) %>%
  summarise(pop = sum(pop, na.rm = TRUE)) %>%
  filter(edu != "Total" & country == "SE" & year == 2016)

# Clean mortality data
deaths2 <- deaths %>%
  mutate(edu = case_when(isced11=="ED0-2" | isced11 == "UNK" | isced11=="ED3_4" ~ "Low",
                         isced11=="ED5-8" ~ "High",
                         isced11=="TOTAL" ~ "Total")) %>%
  # mutate(edu = case_when(isced11=="ED0-2" | isced11 == "UNK" ~ "Low",
  #                        isced11=="ED3_4" ~ "Middle",
  #                        isced11=="ED5-8" ~ "High",
  #                        isced11=="TOTAL" ~ "Total")) %>%
  filter(age != "TOTAL" & !is.na(edu)) %>%
  mutate(age = case_when(age == "Y_LT1" ~ 0,
                         age == "Y_OPEN" ~ 100,
                         age == "UNK" ~ 999,
                         TRUE ~ as.numeric(as.character(substr(age, 2, 3))))) %>%
  dplyr::select(-freq, -unit, -isced11) %>%
  rename(country = geo, year = TIME_PERIOD, Dx = values) %>%
  arrange(country, sex, year, edu, age) %>%
  group_by(country, sex, year, edu, age) %>%
  summarise(Dx = sum(Dx, na.rm = TRUE)) %>%
  filter(edu != "Total" & country == "SE" & year == 2016)

# Mortality rates by single age
mx_df <- pop2 %>%
  left_join(deaths2, by = c("country", "sex", "year", "edu", "age")) %>%
  mutate(mx = Dx/pop) %>%
  filter(age >= 30 & age <= 100) %>%
  dplyr::select(edu, pop, Dx, mx, age)


### Ungrouping last age interval until 110+ and smoothing data

# Define values
ages <- unique(mx_df$age)
n <- length(ages)
sex_categories <- unique(mx_df$sex)
edu_categories <- unique(mx_df$edu)
nl <- 11

mx.smooth_list <- list()
i <- 1

# Ungroup each sex and education group
for(sex_value in sex_categories){
  for (edu_value in edu_categories) {
    y <- mx_df$Dx[mx_df$edu == edu_value & mx_df$sex == sex_value]
    e <-  mx_df$pop[mx_df$edu == edu_value & mx_df$sex == sex_value]
    
    e_smooth <- pclm(ages, y = e, nlast = nl, control = list(lambda = 10E10))$fitted
    mx <- pclm(ages, y = y, nlast = nl, offset = e_smooth, control = list(lambda = 10E10))
    
    mx.smooth_list[[i]] <- data.frame(age = c(30:110), sex = sex_value, 
                                      edu = edu_value,
                                      mx = mx$fitted, 
                                      e_smooth = e_smooth, 
                                      y_smooth = e_smooth*mx$fitted)
    i = i + 1
    
  }
}

mx.smooth_df <- do.call(rbind, mx.smooth_list)

# Plot
# mx.smooth_df %>%
#   ggplot() +
#   geom_line(aes(x = age, y = log(mx), color = edu)) +
#   geom_point(data = mx_df, aes(x = age, y = log(mx), color = edu)) +
#   facet_grid(~ sex)

# ---------------------------------------------------------------------------- #
#     3. Save results
# ---------------------------------------------------------------------------- #

save(mx.smooth_df, file = "inter_data/SWE/mx_true_swe_2016_2groups.RData")
# save(mx.smooth_df, file = "inter_data/SWE/mx_true_swe_2016_3groups.RData")


