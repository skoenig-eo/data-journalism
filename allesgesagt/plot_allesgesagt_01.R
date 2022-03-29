library(lubridate)
library(cowplot)
library(tidyverse)

# some parsing failures aparently, also some of the data needs to be corrected
allesgesagt_raw <- read_csv("allesgesagt/allesgesagt_raw.csv")
fix(allesgesagt_raw)

allesgesagt <- allesgesagt_raw %>% 
  mutate(Date = make_date(year, month, day),
         Duration = hours*60+minutes)


ggplot(allesgesagt) +
  geom_col(aes(x = Date, y = Duration)) +
  geom_smooth(aes(x = Date, y = Duration), se = FALSE) +
  geom_smooth(aes(x = Date, y = Duration), se = FALSE, method = "lm")
