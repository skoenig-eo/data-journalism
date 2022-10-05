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
  geom_col(aes(x = Date, y = Duration), color = "darkgrey", alpha = 0.75) +
  geom_point(aes(x = Date, y = Duration, color = gender)) +
  scale_color_manual(breaks = c("female", "male"),
                     labels = c("Weiblich", "Männlich"),
                     values = c("#e76f51", "#2a9d8f"),
                     name = "Geschlecht") +
  # geom_smooth(aes(x = Date, y = Duration), se = FALSE, method = "lm") +
  scale_x_date(name = "Datum", expand = c(0.01,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 600), 
                     name = "Dauer [Minuten]") +
  theme_half_open() +
  theme(legend.position = "bottom") 
ggsave("allesgesagt/duration_ts_01.png",
       width = 18, height = 12, units = "cm", dpi = 450)

ggplot(allesgesagt) +
  geom_boxplot(aes(x = gender, y = Duration), width = 0.5) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 600)) +
  theme_half_open()
