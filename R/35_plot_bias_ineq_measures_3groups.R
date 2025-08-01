
# ---------------------------------------------------------------------------- #
# Project:  Estimating bias in educational inequalities in mortality
# Title: Plot relative bias in inequality measures for the 3 group setting,
# baseline case
# ---------------------------------------------------------------------------- #
# Content:
#   0. Working directory, packages and functions
#   1. Read data
#   2. Create figure
#   3. Save figure
# ---------------------------------------------------------------------------- #
#     0. Working directory, package and functions
# ---------------------------------------------------------------------------- #
rm(list = ls())

### Setup
library(tidyverse)
library(dplyr)
library(ggplot2)

# Import functions
source("R/01_Function_Simulation.R")

# ---------------------------------------------------------------------------- #
#     1. Read data
# ---------------------------------------------------------------------------- #

load("Results/US/case_1/scen3_v2.rds")

rel_bias <- scen3$ineq_bias$rel_bias_ex

ineq <- scen3$ineq_bias$ineq

# ---------------------------------------------------------------------------- #
#     2. Create figure
# ---------------------------------------------------------------------------- #

selected_measure = "aid"

rel_bias %>%
  filter((j == "0" & l %in% seq(0, 0.4, 0.1)) |(j == "0.1" & l %in% seq(0, 0.4, 0.1)) |
           (j == "0.2" & l %in% seq(0, 0.4, 0.1)) |
           (j == "0.3" & l %in% seq(0, 0.4, 0.1))|
           (j == "0.4" & l %in% seq(0, 0.4, 0.1))) %>%
  relocate(c(i,j,k,l), .before = range_asmr) %>%
  gather("measure", "Bias", "range_asmr":"noi_pair_logit") %>%
  filter(measure == selected_measure) %>%
  filter(!grepl("_logit", measure)) %>%
  mutate(l = factor(as.numeric(as.character(l)), 
                    levels = rev(sort(unique(as.numeric(as.character(l)))))),
         j = factor(j)) %>%
  ggplot(aes(x = i, y = k, fill = Bias)) +
  geom_raster() +
  scale_fill_gradient2(midpoint = 0, limits = c(-1, 1),     
    oob = scales::squish, breaks = c(-1, -.5, 0, .5, 1),   
    labels = c("≤-100%", "-50%", "0%", "50%", "≥100%"), name = "Relative Bias") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent_format(), breaks=seq(0,0.4,.1), expand = c(0, 0)) + 
  scale_y_continuous(labels = scales::percent_format(), breaks=seq(0,0.4,.1), expand = c(0, 0)) +
  xlab("Low as middle (overreporting)") + 
  ylab("High as middle (underreporting)") + 
  theme(panel.grid = element_blank()) + 
  facet_grid(l~j, labeller = as_labeller(c("0" = "0%", "0.1" = "10%",
                                           "0.2" = "20%", "0.3" = "30%", "0.4" = "40%"))) +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 13, face = "bold", hjust = 0),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        legend.position = "right",
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.text.x = element_text(hjust = 0.5)) +
  coord_equal()

# ---------------------------------------------------------------------------- #
#     3. Save figure
# ---------------------------------------------------------------------------- #

ggsave(paste0("Figures/bias_3groups_",selected_measure,"_v2.pdf"), 
              width = 11.55, height = 9.34, device = cairo_pdf)


