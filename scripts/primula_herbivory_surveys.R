### What this script does:
#### 
####
####

#packages:
library(tidyverse); theme_set(theme_classic)
library(lubridate)

# read in herbivory surveys for 2021 and 2022

herb2021 <- read.csv("/Users/Jenna/Dropbox/Williams' Lab/Students/Jenna/Data/PrimulaHerbivory/2021_Primula_Herbivory_Surveys.csv")
herb2021$date.surveyed <- parse_date_time(herb2021$date.surveyed, "m-d-Y")

herb2022 <- read.csv("/Users/Jenna/Dropbox/Williams' Lab/Students/Jenna/Data/PrimulaHerbivory/2022_Primula_Herbivory_Surveys.csv")
herb2022$date.surveyed <- parse_date_time(herb2022$date.surveyed, "m-d-Y")