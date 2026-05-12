
# ---------------------------------------------------------------------------- #
# Project:  Estimating bias in educational inequalities in mortality
# Title: Plot relative bias in inequality measures for the 2 group setting,
# cases 2-5
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
file_name <- "scen_v2.rds"

# dir_path <- "Results/SWE"
# file_name <- "scen.rds"

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


### Summary measures
# rel_bias_cases %>%
#        mutate(case = str_sub(case, start = 6)) %>%
#        relocate(c(i,j), .before = range_asmr) %>%
#        filter(!(i == 0 & j == 0)) %>%
#        gather("measure", "Bias", "range_asmr":"noi_pair_logit") %>%
#        group_by(measure, case) %>%
#        summarise(bias_mean = mean(Bias),
#                  bias_median = median(Bias),
#                  bias_sd = sd(Bias),
#                  bias_min = min(Bias),
#                  bias_max = max(Bias),
#                  bias_neg = sum(ifelse(Bias < 0, 1, 0))/n(),
#                  bias_pos = sum(ifelse(Bias > 0, 1, 0))/n(),
#                  bias_zero = sum(ifelse(Bias == 0, 1, 0))/n()) %>%
#   filter(measure == "aid")

### Percentage of cases with inequality reversals
# ineq_cases %>%
#   mutate(case = str_sub(case, start = 6)) %>%
#   relocate(c(i,j), .before = range_asmr) %>%
#   gather("measure", "value", "range_asmr":"noi_pair_logit") %>%
#   filter(measure == "range_ex") %>%
#   mutate(reversal = case_when(measure %in% c("range_asmr", "range_ex", "sii_asmr", "par", "paf") ~ ifelse(value <= 0, 1, 0),
#                               measure %in% c("sii_ex", "range_sdv") ~ ifelse(value >= 0, 1, 0),
#                               measure %in% c("ratio_asmr", "ratio_ex", "rii_asmr") ~ ifelse(value <= 1, 1, 0),
#                               measure %in% c("rii_ex", "ratio_sdv") ~ ifelse(value >= 1, 1, 0))) %>%
#   group_by(measure, case) %>%
#   summarise(sum(reversal)/n())

# ---------------------------------------------------------------------------- #
#     2. Create figure
# ---------------------------------------------------------------------------- #

sel_measure <- "range_ex"

# For labels
subtitle_df2 <- data.frame(
  # case = c("High inequality", "High inequality", "Low inequality", "Low inequality"),
  measure = c("range_ex"),
  subtitle = c("A2", "A3", "A4", "A5"),
  case = c(2:5),
  i = 0, j = 0.5)

# Plot
rel_bias_cases %>%
  mutate(case = str_sub(case, start = 6)) %>%
  relocate(c(i,j), .before = range_asmr) %>%
  gather("measure", "Bias", "range_asmr":"noi_pair_logit") %>%
  filter(measure %in% c(sel_measure) & case != 1) %>%
  mutate(measure = factor(measure, levels = c("range_ex", "aid"))) %>%
  ggplot(aes(x = i, y = j, fill = Bias)) +
  geom_raster() +
  scale_fill_gradient2(
    midpoint = 0,
    limits = c(-1, 1),     
    oob = scales::squish,    
    breaks = c(-1, -.5, 0, .5, 1),   
    labels = c("≤-100", "-50", "0", "50", "≥100"), 
    name = "Bias\n(percentage)") +
  theme_bw() +
  scale_x_continuous(labels = seq(0,70,10), breaks=seq(0,0.7,.1), expand = c(0, 0)) + 
  scale_y_continuous(labels = seq(0,70,10), breaks=seq(0,0.7,.1), expand = c(0, 0)) +
  xlab("Percentage of education over-reporting") + 
  ylab("Percentage of education under-reporting") + 
  theme(panel.grid = element_blank()) + 
  facet_wrap(~case, labeller = labeller(case = c("2" = "(a) Case 2: Higher percentage of deaths in\nlow-educated group",
                                                         "3" = "(b) Case 3: Lower percentage of deaths in\nlow-educated group",
                                                         "4" = "(c) Case 4: Larger inequality",
                                                         "5" = "(d) Case 5: Smaller inequality"),
                                            case = NA),
             scales = "fixed") +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 13, face = "bold", hjust = 0),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        legend.position = "right",
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.spacing.y = unit(1.5, "lines"),
        text = element_text(family="serif")) +
  # geom_text(data = subtitle_df2,
  #           aes(x = i, y = j, label = subtitle),
  #           inherit.aes = FALSE,
  #           hjust = 0, vjust = 1,   
  #           size = 4,
  #           fontface = "bold",
  #           color = "white", family="serif") +
  geom_point(data = ineq_cases  %>% 
               mutate(case = str_sub(case, start = 6)) %>%
               relocate(c(i,j), .before = range_asmr) %>%
               gather("measure", "value", "range_asmr":"noi_pair_logit") %>%
               filter(measure %in% c(sel_measure) & case != 1) %>%
               mutate(measure = factor(measure, levels = c("range_ex", "noi_pair", "aid"))) %>%
               mutate(reversal = case_when(measure %in% c("range_asmr", "range_asmr", "sii_asmr", "par", "paf", "range_ex") ~ ifelse(value <= 0, 1, 0),
                                           measure %in% c("sii_ex", "range_sdv") ~ ifelse(value >= 0, 1, 0),
                                           measure %in% c("ratio_asmr", "ratio_ex", "rii_asmr") ~ ifelse(value <= 1, 1, 0),
                                           measure %in% c("rii_ex", "ratio_sdv") ~ ifelse(value >= 1, 1, 0))) %>%
               filter(reversal == 1), 
             aes(x = i, y = j), colour = "black", fill = NA, pch = 1, size = 1) +
  # Annotations (only for range in LE)
  geom_abline(data = . %>% filter(case %in% c(4,5)), aes(slope = 2.7, intercept = 0.2), lty = 3, col = "white", size = 0.9) +
  geom_abline(data = . %>% filter(case %in% c(4,5)),aes(slope = 2.7, intercept = -0.25), lty = 3, col = "white", size = 0.9) +
  geom_abline(data = . %>% filter(case %in% c(2,3)), aes(slope = 2.7, intercept = 0), lty = 2, col = "white", size = 1) +
  geom_curve(data = . %>% filter(case == 3), aes(x = 0.15, y = 0.3, xend = 0.35, yend = 0.2),
             arrow = arrow(length = unit(0.3, "cm"), type = "open"),
             color = "white", curvature = -0.4, size = 0.6) +
  geom_curve(data = . %>% filter(case == 2), aes(x = 0.13, y = 0.34, xend = 0.10, yend = 0.4),
             arrow = arrow(length = unit(0.3, "cm"), type = "open"),
             color = "white", curvature = 0.4, size = 0.6) +
  geom_segment(data = . %>% filter(case == 4),aes(x = .20, y = 0.28, xend = 0.25, yend = 0.23),
               arrow = arrow(length = unit(0.3, "cm"), type = "open"),
               color = "white", size = 0.6) +
  geom_segment(data = . %>% filter(case == 4),aes(x = .06, y = 0.38, xend = 0.02, yend = 0.42),
               arrow = arrow(length = unit(0.3, "cm"), type = "open"),
               color = "white", size = 0.6) +
  geom_segment(data = . %>% filter(case == 5),aes(x = .21, y = 0.34, xend = 0.18, yend = 0.36),
               arrow = arrow(length = unit(0.3, "cm"), type = "open"),
               color = "white", size = 0.6) +
  geom_segment(data = . %>% filter(case == 5),aes(x = .03, y = 0.28, xend = 0.06, yend = 0.25),
               arrow = arrow(length = unit(0.3, "cm"), type = "open"),
               color = "white", size = 0.6) +
  coord_equal()

# ---------------------------------------------------------------------------- #
#     3. Save figure
# ---------------------------------------------------------------------------- #

ggsave(paste0("Figures/US/bias_2groups_scenarios_v2_",sel_measure,".pdf"), width = 9.64, height = 9, device = cairo_pdf)
# ggsave(paste0("Figures/SWE/bias_2groups_scenarios_v2_",sel_measure,".pdf"), width = 9.64, height = 9, device = cairo_pdf)
# ggsave(paste0("Figures/US/bias_2groups_scenarios_v2_",sel_measure,".tiff"), width = 10.64, height = 10, dpi = 300)


