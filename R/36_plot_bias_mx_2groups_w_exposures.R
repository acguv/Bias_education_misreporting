
# ---------------------------------------------------------------------------- #
# Project:  Estimating bias in educational inequalities in mortality
# Author: Ana C. Gomez-Ugarte
# Title: Plot bias in mortality rates for the 2 group setting
# ---------------------------------------------------------------------------- #
# Content:
#   0. Working directory, packages and functions
#   1. Read data
#   2. Create figures
#   3. Save figures
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
library(ggpubr)

# ---------------------------------------------------------------------------- #
#     1. Read data
# ---------------------------------------------------------------------------- #

# Select case
case_nm = "case_1"

# Underlying mortality
load(paste0("Results/US/",case_nm,"/scen_e.rds"))

# ---------------------------------------------------------------------------- #
#     2. Create figures
# ---------------------------------------------------------------------------- #

# Plot for underreporting of education
under_rate = 0.2   # Define level of underreporting

under_plot <- scen_e$scen %>% 
  mutate(Scenario = case_when(i == 0 & j == 0 & k == 0 & l == under_rate  ~ "Misreported data",
                              i == 0 & j == 0 & k == 0 & l == 0 ~ "Underlying data")) %>%
  filter(!is.na(Scenario)) %>% 
  arrange(education, Scenario, age) %>%
  ggplot() +
  geom_line(aes(x = age, y = log(mu), col = education, linetype = Scenario, 
                group = paste(education, Scenario)), linewidth = 1) +
  theme_classic() + xlab("Age") + 
  ylab("") +
  labs(title = paste0("Underreporting (", under_rate*100, "%)")) +
  scale_x_continuous(breaks = seq(30,90,10)) +
  scale_y_continuous(breaks = seq(-8,-2,2), limits = c(-8.5, -1.6)) +
  scale_size_manual(values = c(0.5, 0.75)) +
  scale_fill_manual(name = "Education", values = c("#781bec", "#f4a300")) + 
  scale_color_manual(name = "Education", values = c("#781bec", "#f4a300"), 
                     breaks = c("Low", "High")) + 
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12), 
        legend.position = "bottom", 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        plot.title = element_text(size = 13, face = "bold")) 


# Plot for overreporting of education
over_rate = 0.2   # Define level of overreporting

over_plot <- scen_e$scen %>% 
  mutate(Scenario = case_when(i == 0 & j == 0 & k == over_rate & l == 0 ~ "Misreported data",
                              i == 0 & j == 0 & k == 0 & l == 0 ~ "Underlying data")) %>%
  filter(!is.na(Scenario)) %>% 
  arrange(education, Scenario, age) %>%
  ggplot() +
  geom_line(aes(x = age, y = log(mu), col = education, linetype = Scenario, 
                group = paste(education, Scenario)), linewidth = 1) +
  theme_classic() + xlab("Age") + 
  ylab("Mortality rate (log scale)") +
  labs(title = paste0("Overreporting (", under_rate*100, "%)")) +
  scale_x_continuous(breaks = seq(30,90,10)) +
  scale_y_continuous(breaks = seq(-8,-2,2), limits = c(-8.5, -1.6)) +
  scale_size_manual(values = c(0.5, 0.75)) +
  scale_fill_manual(name = "Education", values = c("#781bec", "#f4a300")) + 
  scale_color_manual(name = "Education", values = c("#781bec", "#f4a300"), 
                     breaks = c("Low", "High")) + 
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12), 
        legend.position = "bottom", 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        plot.title = element_text(size = 13, face = "bold"))

# Merge both plots
over_under_plot <- ggarrange(over_plot, under_plot, 
          ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")


# Plot for over- and under-reporting of education (Supplementary Material)
over_rate = 0.2   # Define level of overreporting
under_rate = 0.2   # Define level of underreporting

both_plot <- scen_e$scen %>% 
  mutate(Scenario = case_when(i == 0 & j == 0 & k == over_rate & l == under_rate ~ "Misreported data",
                              i == 0 & j == 0 & k == 0 & l == 0 ~ "Underlying data")) %>%
  filter(!is.na(Scenario)) %>% 
  arrange(education, Scenario, age) %>%
  ggplot() +
  geom_line(aes(x = age, y = log(mu), col = education, linetype = Scenario, 
                group = paste(education, Scenario)), linewidth = 1) +
  theme_classic() + xlab("Age") + 
  ylab("Mortality rate (log scale)") +
  labs(title = paste0("Under- (", under_rate*100, "%) and over-reporting (", over_rate*100, "%)")) +
  scale_x_continuous(breaks = seq(30,90,10)) +
  scale_y_continuous(breaks = seq(-8,-2,2), limits = c(-8.5, -1.6)) +
  scale_size_manual(values = c(0.5, 0.75)) +
  scale_fill_manual(name = "Education", values = c("#781bec", "#f4a300")) + 
  scale_color_manual(name = "Education", values = c("#781bec", "#f4a300"), 
                     breaks = c("Low", "High")) + 
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12), 
        legend.position = "bottom", 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        plot.title = element_text(size = 13, face = "bold"))


# ---------------------------------------------------------------------------- #
#     2. Save figures
# ---------------------------------------------------------------------------- #

ggsave(over_under_plot, file = "Figures/mx_2groups_w_edumis_exposures.pdf", width = 8.8, height = 4.2)

ggsave(both_plot, file = "Figures/mx_2groups_both_w_edumis_exposures.pdf", width = 175, units = "mm")


