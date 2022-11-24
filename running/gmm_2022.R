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
  select(Number, Nationality, Birthyear, Group, Place, Gross, Net) %>% 
  # running times are pasted as fractions of a day for some reason
  mutate(Net_seconds = Net*60*60*24,
         Rounded_seconds = floor(Net_seconds),
         Net_approx = seconds_to_period(Rounded_seconds))

