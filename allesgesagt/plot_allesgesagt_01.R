library(lubridate)
library(cowplot)
library(tidyverse)

# some parsing failures aparently, also some of the data needs to be corrected
allesgesagt_raw <- read_csv("allesgesagt/allesgesagt_raw.csv")
# fix(allesgesagt_raw)

allesgesagt <- allesgesagt_raw %>% 
  mutate(Date = make_date(year, month, day),
         Duration = hours*60+minutes)


ggplot(allesgesagt) +
  geom_smooth(aes(x = Date, y = Duration), se = FALSE, alpha = 0.5,
              color = "darkblue", size = 0.5) +
  geom_col(aes(x = Date, y = Duration)) +
  geom_point(aes(x = Date, y = Duration, color = gender)) +
  # geom_smooth(aes(x = Date, y = Duration), se = FALSE, method = "lm") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 600)) +
  theme_half_open() +
  theme(legend.position = "bottom")

ggplot(allesgesagt) +
  geom_boxplot(aes(x = gender, y = Duration), width = 0.5) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 600)) +
  theme_half_open()
