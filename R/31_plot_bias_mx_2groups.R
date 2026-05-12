
# ---------------------------------------------------------------------------- #
# Project:  Estimating bias in educational inequalities in mortality
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
library(extrafont)

# ---------------------------------------------------------------------------- #
#     1. Read data
# ---------------------------------------------------------------------------- #

# Select case
case_nm = "case_1"

# Underlying mortality
load(paste0("Results/US/",case_nm,"/scen_v2.rds"))

# ---------------------------------------------------------------------------- #
#     2. Create figures
# ---------------------------------------------------------------------------- #

# Plot for underreporting of education
under_rate = 0.2   # Define level of underreporting

under_plot <- scen$scen %>% 
  mutate(Scenario = case_when(i == 0 & j == under_rate ~ "Misreported data",
                              i == 0 & j == 0 ~ "Underlying data")) %>%
  filter(!is.na(Scenario)) %>% 
  arrange(education, Scenario, age) %>%
  ggplot() +
  geom_line(aes(x = age, y = log(mu), col = education, linetype = Scenario, 
                group = paste(education, Scenario)), linewidth = 1) +
  theme_classic() + xlab("Age") + 
  ylab("") +
  labs(title = paste0("(b) Under-reporting")) +
  scale_x_continuous(breaks = seq(30,110,10), labels = c(seq(30,100,10), "110+")) +
  scale_y_continuous(breaks = seq(-8,-2,2), limits = c(-8.5,  -0.4)) +
  scale_size_manual(values = c(0.5, 0.75)) +
  scale_fill_manual(name = "Education", values = c("#781bec", "#f4a300")) + 
  scale_color_manual(name = "Education", values = c("#781bec", "#f4a300"), 
                     breaks = c("Low", "High")) + 
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12), 
        legend.position = "bottom", 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        plot.title = element_text(size = 13, face = "bold"), 
        text=element_text(family="serif")) 


# Plot for overreporting of education
over_rate = 0.2   # Define level of overreporting

over_plot <- scen$scen %>% 
  mutate(Scenario = case_when(i == over_rate & j == 0 ~ "Misreported data",
                              i == 0 & j == 0 ~ "Underlying data")) %>%
  filter(!is.na(Scenario)) %>% 
  arrange(education, Scenario, age) %>%
  ggplot() +
  geom_line(aes(x = age, y = log(mu), col = education, linetype = Scenario, 
                group = paste(education, Scenario)), linewidth = 1) +
  theme_classic() + xlab("Age") + 
  ylab("Mortality rate (log scale)") +
  labs(title = paste0("(a) Over-reporting")) +
  scale_x_continuous(breaks = seq(30,110,10), labels = c(seq(30,100,10), "110+")) +
  scale_y_continuous(breaks = seq(-8,-2,2), limits = c(-8.5, -0.4)) +
  scale_size_manual(values = c(0.5, 0.75)) +
  scale_fill_manual(name = "Education", values = c("#781bec", "#f4a300")) + 
  scale_color_manual(name = "Education", values = c("#781bec", "#f4a300"), 
                     breaks = c("Low", "High")) + 
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12), 
        legend.position = "bottom", 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        plot.title = element_text(size = 13, face = "bold"), 
        text=element_text(family="serif"))

# Merge both plots
over_under_plot <- ggarrange(over_plot, under_plot, 
          ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")


# Plot for over- and under-reporting of education (Supplementary Material)
over_rate = 0.2   # Define level of overreporting
under_rate = 0.2   # Define level of underreporting

both_plot <- scen$scen %>% 
  mutate(Scenario = case_when(i == over_rate & j == under_rate ~ "Misreported data",
                              i == 0 & j == 0 ~ "Underlying data")) %>%
  filter(!is.na(Scenario)) %>% 
  arrange(education, Scenario, age) %>%
  ggplot() +
  geom_line(aes(x = age, y = log(mu), col = education, linetype = Scenario, 
                group = paste(education, Scenario)), linewidth = 1) +
  theme_classic() + xlab("Age") + 
  ylab("Mortality rate (log scale)") +
  labs(title = paste0("Under- (", under_rate*100, " per cent) and over-reporting (", over_rate*100, " per cent)")) +
  scale_x_continuous(breaks = seq(30,110,10), labels = c(seq(30,100,10), "110+")) +
  scale_y_continuous(breaks = seq(-8,-2,2), limits = c(-8.5, -0.4)) +
  scale_size_manual(values = c(0.5, 0.75)) +
  scale_fill_manual(name = "Education", values = c("#781bec", "#f4a300")) + 
  scale_color_manual(name = "Education", values = c("#781bec", "#f4a300"), 
                     breaks = c("Low", "High")) + 
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12), 
        legend.position = "bottom", 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        plot.title = element_text(size = 13, face = "bold"), 
        text=element_text(family="serif"))

# ---------------------------------------------------------------------------- #
#     2. Save figures
# ---------------------------------------------------------------------------- #

ggsave(over_under_plot, file = "Figures/US/mx_2groups_v2.pdf", width = 8.8, height = 4.2, device = cairo_pdf)
# ggsave(over_under_plot, file = "Figures/US/mx_2groups_v2.jpeg", width = 8.8, height = 4.2, dpi = 300)

ggsave(both_plot, file = "Figures/US/mx_2groups_both_v2.pdf", width = 175, units = "mm")


