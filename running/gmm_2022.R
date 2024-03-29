# Header --------------------------------------------
#
# Original Author: Simon König, simonkoenig@live.de
# Created on: 2022-03-11
# 
# Last modified by: Simon König, simonkoenig@live.de
# On: 2023-02-05
#
# Script Description: plot GMM 2022 finisher data for the half marathon
# and find a suitable finishing time for 2023
# 
# Notes: 
#
#
#

library(readxl)
library(lubridate)
library(cowplot)
library(tidyverse)



# load and wrangle data ----------------------------------------------

# raw_results <- read_excel("running/ErgebnislistenErgebnislisteOA.xlsx")
# names(raw_results)
# raw_results

# column types
#"Platz...1" integer 
# "Startnr." integer
# "Name" character
# "NAT" character
# "Team" character
# "Jahrg." integer
# "Klasse" character
# "Platz...8" integer
# "(Brutto)" character
# "Laufzeit" character
# "Diff." character

results <- read_excel("running/ErgebnislistenErgebnislisteOA.xlsx",
                      col_types = c("numeric", "numeric", "text",
                                    "text", "text", "numeric",
                                    "text", "numeric", "text",
                                    "numeric", "text")) %>% 
  drop_na() %>% 
  rename(ID = 1, Number = 2, Name = 3, Nationality = 4, Team = 5, Birthyear = 6,
         Group = 7, Place = 8, Gross = 9, Net = 10, Difference = 11) %>% 
  select(-ID, -Team) %>% 
  # running times are pasted as fractions of a day for some reason
  mutate(Net_seconds = Net*60*60*24,
         Rounded_seconds = floor(Net_seconds),
         Net_approx = seconds_to_period(Rounded_seconds),
         Hours = hour(Net_approx),
         Minutes = minute(Net_approx),
         Seconds = second(Net_approx),
         In_minutes = Hours*60+Minutes+Seconds/60)



# create some summary statistics -----------------------------------

median_time <- median(results$In_minutes)
perc_01 <- quantile(results$In_minutes, 0.01)
perc_10 <- quantile(results$In_minutes, 0.1)
perc_25 <- quantile(results$In_minutes, 0.25)

# check out my position in 2022
my_result <- results %>% filter(Name == "KÖNIG Simon")



# basic plot version ----------------------------------------------

ggplot(results) +
  geom_density(aes(x = In_minutes), color = "grey20",
               fill = "lightgrey", alpha = 0.25) +
  geom_vline(xintercept = median_time, linetype = "dashed") +
  geom_vline(xintercept = perc_01, linetype = "dotted") +
  geom_vline(xintercept = perc_10, color = "#C11C03", size = 1) +
  geom_vline(xintercept = perc_25, linetype = "dotted") +
  geom_vline(xintercept = my_result %>% pull(In_minutes),
             color = "#8E1230") +
  scale_x_continuous(expand = c(0,0), name = "Running Time [minutes]") +
  scale_y_continuous(expand = c(0,0), name = "Density") +
  theme_half_open() +
  ggtitle("GMM Half Marathon Results 2022")



# fancy plot: overall finishing times plus target for 2023 --------------------

# slightly changed from base plot
ggplot(results) +
  # finisher time distributions for 2022
  geom_density(aes(x = In_minutes), color = NA,
               fill = "lightgrey", alpha = 0.6) +
  # median, first and 25th percentile
  geom_vline(xintercept = median_time, linetype = "dashed") +
  geom_vline(xintercept = perc_01, linetype = "dotted") +
  geom_vline(xintercept = perc_25, linetype = "dotted") +
  # 10th percentile (target time for 2023), and finisher time for 2022
  geom_vline(xintercept = perc_10, color = "#C11C03", size = 1) +
  geom_vline(xintercept = my_result %>% pull(In_minutes),
             color = "#8E1230") +
  scale_x_continuous(expand = c(0,0), name = "Running Time [minutes]") +
  scale_y_continuous(expand = c(0,0), name = "Density") +
  # arrows and annotations
  geom_curve(x = 170, y = 0.01, xend = 133, yend = 0.015,
             color = "#8E1230", 
             arrow = arrow(length = unit(0.04, "npc"), type = "closed"),
             curvature = 0.3) +
  geom_curve(x = 75, y = 0.01, xend = 95, yend = 0.015,
             color = "#C11C03", 
             arrow = arrow(length = unit(0.04, "npc"), type = "closed"),
             curvature = -0.5) +
  annotate("text", x = 170, y = 0.0085, 
           label = "Finishing Time 2022",
           color = "#8E1230") +
  # this creates an error and has to be adjusted
  annotate("text", x = 80, y = 0.0085, 
           label = "bold(2023 Goal)", parse = TRUE,
           color = "#C11C03") +
  theme_half_open()
ggsave("running/gmm_2023_target.png",
       dpi = 450, width = 16, height = 12, units = "cm")
