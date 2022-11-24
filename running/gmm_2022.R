library(readxl)
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
                                    "text", "text")) %>% 
  drop_na()
