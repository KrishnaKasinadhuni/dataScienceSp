library(readr)
library(devtools)
library(dplyr)
library(tidyr)
library(readxl)

titanic3_org <- tbl_df(read_excel("titanic3.xls"))

titanic3_clean <- titanic3_org %>% select(everything())
titanic3_clean$embarked[is.na(titanic3_clean$embarked)] <- "S"
titanic3_clean$age[is.na(titanic3_clean$age)] <- round(mean(titanic3_clean$age, na.rm = TRUE))
titanic3_clean$boat[is.na(titanic3_clean$boat)] <- "None"
titanic3_clean <- mutate(titanic3_clean, has_cabin_number = as.numeric(!is.na(titanic3_clean$cabin)))
  
which(is.na(titanic3_clean$embarked))

which(is.na(titanic3_clean$age))

which(is.na(titanic3_clean$boat))

glimpse(titanic3_clean)

write.csv(titanic3_clean, file = 'titanic3_clean.csv')

