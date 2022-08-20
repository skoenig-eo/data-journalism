library(lubridate)
library(cowplot)
library(tidyverse)
library(lubridate)
# library(tidymodels)
library(randomForest)

allesgesagt_raw <- read_csv("allesgesagt/allesgesagt_raw.csv")
allesgesagt <- allesgesagt_raw %>% 
  mutate(Date = make_date(year, month, day),
         Duration = hours*60+minutes,
         DOY = yday(Date))

# create model
allesgesagt_train <- allesgesagt %>% 
  select(gender, year, DOY, location, Duration)

rf_simple <- randomForest(Duration~., data = allesgesagt_train,
                          ntree = 1000, importance = TRUE)
rf_simple$importance