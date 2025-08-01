
# ---------------------------------------------------------------------------- #
# Project:  Estimating bias in educational inequalities in mortality
# Title:    Plot age at crossover
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

# ---------------------------------------------------------------------------- #
#     1. Read data
# ---------------------------------------------------------------------------- #
dir_path <- "Results/US"
file_name <- "scen_v2.rds"

cases <- list()
i = 1

folders <- dir(dir_path, full.names = FALSE)
for (folder in folders) {
  folder_path <- file.path(dir_path, folder)
  file_path <- file.path(folder_path, file_name)
  
  load(file_path)
  file_data <- scen$scen
  file_data$case <- folder
  
  cases[[i]] <- file_data
  i = i + 1
}

all_cases <- do.call(rbind, cases)

# ---------------------------------------------------------------------------- #
#     2. Create figure
# ---------------------------------------------------------------------------- #

all_cases %>%
  mutate(case = str_sub(case, start = 6)) %>%
  spread(education, mu) %>%
  filter(High > Low) %>%
  group_by(i, j, case) %>%
  filter(j == 0) %>%
  summarise(cross_age = min(age)) %>%
  ggplot() +
  geom_point(aes(y = cross_age, x = i, color = factor(case), pch = factor(case)), size = 4) +
  theme_classic() +
  ylab("Age at crossover") + xlab("Percentage of overreporting (low to high)") +
  scale_x_continuous(labels = scales::percent_format()) + 
  ylim(c(40,110)) +
  scale_color_manual(name = "Case", values = c("#648FFF", "#785EF0", "#DC267F", "#FE6100", "#FFB000"), 
                     labels = c("1) Baseline inequality\nand death composition",
                                "2) Baseline inequality +\nhigher % of low educ. deaths", 
                                "3) Baseline inequality +\nlower % of low educ. deaths",
                                "4) Higher inequality +\nbaseline death composition", 
                                "5) Lower inequality +\nbaseline death composition"))  +
  scale_shape_manual(name = "Case", values = c(15, 18, 16, 17, 19), 
                     labels = c("1) Baseline inequality\nand death composition",
                                "2) Baseline inequality +\nhigher % of low educ. deaths", 
                                "3) Baseline inequality +\nlower % of low educ. deaths",
                                "4) Higher inequality +\nbaseline death composition", 
                                "5) Lower inequality +\nbaseline death composition"))  +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        legend.position = "right", 
        legend.box="vertical", 
        legend.margin=margin(),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.key.size = unit(1, "lines"),
        legend.spacing.x = unit(0.5, "cm")) +
  guides(color = guide_legend(nrow = 5)) 


# ---------------------------------------------------------------------------- #
#     3. Save figure
# ---------------------------------------------------------------------------- #
ggsave("Figures/US/age_crossover_v2.pdf", width = 175, units = "mm", device = cairo_pdf)
