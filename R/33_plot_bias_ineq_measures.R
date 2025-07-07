
# ---------------------------------------------------------------------------- #
# Project:  Estimating bias in educational inequalities in mortality
# Author: Ana C. Gomez-Ugarte
# Title: Plot relative bias in inequality measures for the 2 group setting,
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
library(scales)

# ---------------------------------------------------------------------------- #
#     1. Read data
# ---------------------------------------------------------------------------- #
dir_path <- "Results/US"
file_name <- "scen.rds"

rel_bias_cases <- ineq_cases <- list()
i = 1

folders <- dir(dir_path, full.names = FALSE)
for (folder in folders) {
  folder_path <- file.path(dir_path, folder)
  file_path <- file.path(folder_path, file_name)
  
  load(file_path)
  file_data <- scen$ineq_bias
  
  rel_bias <- file_data$rel_bias_ex
  rel_bias$case <- folder
  
  ineq <- file_data$ineq
  ineq$case <- folder
  
  rel_bias_cases[[i]] <- rel_bias
  ineq_cases[[i]] <- ineq
  i = i + 1
}

rel_bias_cases <- do.call(rbind, rel_bias_cases)
ineq_cases <- do.call(rbind, ineq_cases)

# ---------------------------------------------------------------------------- #
#     2. Create figure
# ---------------------------------------------------------------------------- #

# For labels
subtitle_df <- data.frame(
  measure = c("range_ex", "aid"),
  subtitle = c("A", "B"),
  i = 0, j = 0.5) %>%   
  mutate(measure = factor(measure, levels = c("range_ex", "aid")))

# Plot for selected measures
plot_rel_bias_range_ex_aid <- rel_bias_cases %>%
  mutate(case = str_sub(case, start = 6)) %>%
  relocate(c(i,j), .before = range_asmr) %>%
  gather("measure", "Bias", "range_asmr":"noi_pair_logit") %>%
  filter(measure %in% c("range_ex", "aid") & case == 1) %>%
  mutate(measure = factor(measure, levels = c("range_ex", "aid"))) %>%
  ggplot(aes(x = i, y = j, fill = Bias)) +
  geom_raster() +
  geom_text(data = . %>% filter((i == 0.2 & j == 0) |
                                  (i == 0 & j == 0.2)),
            aes(label=c("U", "O", "U", "O")),
            fontface = "bold", size = 3, col = "yellow")  +
  geom_text(data = subtitle_df,
            aes(x = i, y = j, label = subtitle),
            inherit.aes = FALSE,
            hjust = 0, vjust = 1,  
            size = 4,
            fontface = "bold",
            color = "white") +
  geom_point(data = ineq_cases %>%
               mutate(case = str_sub(case, start = 6)) %>%
               relocate(c(i,j), .before = range_asmr) %>%
               gather("measure", "value", "range_asmr":"noi_pair_logit") %>%
               filter(measure %in% c("range_ex","aid") & case == 1) %>%
               mutate(measure = factor(measure, levels = c("range_ex", "aid"))) %>%
               mutate(reversal = case_when(measure %in% c("range_asmr", "range_asmr", "sii_asmr", "par", "paf", "range_ex") ~ ifelse(value <= 0, 1, 0),
                                           measure %in% c("sii_ex", "range_sdv") ~ ifelse(value >= 0, 1, 0),
                                           measure %in% c("ratio_asmr", "ratio_ex", "rii_asmr") ~ ifelse(value <= 1, 1, 0),
                                           measure %in% c("rii_ex", "ratio_sdv") ~ ifelse(value >= 1, 1, 0))) %>%
               filter(reversal == 1), 
             aes(x = i, y = j), colour = "black", fill = NA, pch = 1, size = 1) +
  scale_fill_gradient2(
    midpoint = 0,
    limits = c(-1, 1),     
    oob = scales::squish,     
    breaks = c(-1, -.5, 0, .5, 1),   
    labels = c("≤-100%", "-50%", "0%", "50%", "≥100%")) +
  theme_bw() +
  scale_x_continuous(labels = scales::percent_format(), breaks=seq(0,0.7,.1), expand = c(0, 0)) + 
  scale_y_continuous(labels = scales::percent_format(), breaks=seq(0,0.7,.1), expand = c(0, 0)) +
  xlab("Low to high (overreporting)") + 
  ylab("High to low (underreporting)") + 
  theme(panel.grid = element_blank()) + 
  facet_grid(~measure, labeller = labeller(measure = c("range_ex" =  "Life expectancy range",
                                                       "aid" = "Average inter-group difference"))) +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 13, face = "bold", hjust = 0),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        legend.position = "right",
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines")) +
  coord_equal()

#### Plot for all measures
plot_rel_bias_all <- rel_bias_cases %>% 
  mutate(case = str_sub(case, start = 6)) %>%
  relocate(c(i,j), .before = range_asmr) %>%
  gather("measure", "Bias", "range_asmr":"noi_pair_logit") %>%
  # Select measures to plot
  filter(measure %in% c("aid", "cii_abs", "idll_abs", "noi_pair",
                        "pall_abs", "par", "range_ex",
                        "range_sdv") & case == 1) %>%
  mutate(measure = factor(measure, levels = c("range_ex", "par", "pall_abs",
                                              "cii_abs","aid", "noi_pair", "idll_abs",
                                              "range_sdv"))) %>%   
  ggplot(aes(x = i, y = j, fill = Bias)) +
  geom_raster() +
  scale_fill_gradient2( midpoint = 0, limits = c(-1, 1), oob = scales::squish,     
    breaks = c(-1, -.5, 0, .5, 1), labels = c("≤-100%", "-50%", "0%", "50%", "≥100%"), 
    name = "Relative Bias") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent_format(), breaks=seq(0,0.7,.1), expand = c(0, 0)) + 
  scale_y_continuous(labels = scales::percent_format(), breaks=seq(0,0.7,.1), expand = c(0, 0)) +
  xlab("Low to high (overreporting)") + 
  ylab("High to low (underreporting)") + 
  theme(panel.grid = element_blank()) + 
  facet_wrap(~measure, labeller = labeller(measure = c("range_ex" =  "Range in life expectancy",
                                                       "range_sdv" = "Range in standard deviation of ages-at-death",
                                                       "noi_pair" = "Pairwise non-overlap index", 
                                                       "sii_ex" = "Slope index of inequality", 
                                                       "aid" = "Average inter-group difference", 
                                                       "cii_abs" = "Absolute composite index of inequality",
                                                       "idll_abs" = "Absolute index of dissimilarity in length of life", 
                                                       "pall_abs" = "Absolute population attributable life loss", 
                                                       "par" = "Population attributable risk",
                                                       "ratio_ex" =  "Ratio in life expectancy",
                                                       "ratio_sdv" = "Ratio in standard deviation of ages-at-death",
                                                       "theil" = "Theil index", 
                                                       "rii_ex" = "Relative index of inequality", 
                                                       "pseudo_gini" = "Pseudo-Gini index", 
                                                       "idll_rel" = "Relative index of dissimilarity in length of life", 
                                                       "pall_rel" = "Relative population attributable life loss", 
                                                       "paf" = "Population attributable fraction"))) +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 13, face = "bold", hjust = 0),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 16),
        legend.position = c(0.85, 0.15),
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines"))

# ---------------------------------------------------------------------------- #
#     3. Save figure
# ---------------------------------------------------------------------------- #

ggsave(plot_rel_bias_range_ex_aid, file = "Figures/bias_2groups_v2.pdf", width = 8.7, height = 4.2, device = cairo_pdf)

ggsave(plot_rel_bias_all, file = "Figures/bias_2groups_rel_measures_all.pdf", width = 13.5, height = 13, device = cairo_pdf)


