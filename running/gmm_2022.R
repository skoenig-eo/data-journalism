library(readxl)
library(lubridate)
library(cowplot)
library(tidyverse)

raw_results <- read_excel("running/ErgebnislistenErgebnislisteOA.xlsx")
names(raw_results)
raw_results

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

# create some summary statistics
median_time <- median(results$In_minutes)
perc_01 <- quantile(results$In_minutes, 0.01)
perc_10 <- quantile(results$In_minutes, 0.1)
perc_25 <- quantile(results$In_minutes, 0.25)

# check out my position
my_result <- results %>% filter(Name == "KÖNIG Simon")


ggplot(results) +
  geom_density(aes(x = In_minutes)) +
  geom_vline(xintercept = median_time) +
  geom_vline(xintercept = perc_01) +
  geom_vline(xintercept = perc_10) +
  geom_vline(xintercept = perc_25) +
  geom_vline(xintercept = my_result %>% pull(In_minutes)) +
  theme_half_open()
