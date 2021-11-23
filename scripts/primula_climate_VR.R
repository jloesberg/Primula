## Primula
### read in all years of Primula data and weather data from the Cowichan

library(tidyverse); theme_set(theme_classic)
library(lubridate)

## read in cleaned, extrapolated weather station data from WeatherStnInterp_Cowichan_Aug20...4
weather <- read.csv("C:/Users/Jenna/Dropbox/Williams' Lab/Cowichan IDE/LoggerData/weather station/WS_cleaned/cgop_weather_daily_interp.csv")
weather$Date <- parse_date_time(weather$Date, "Y-m-d")

weather$month_year <- format(as.Date(weather$Date), "%Y-%m")


# growing season values and lagged 1 year and 2 year values
### I'm classifying "growing season" as March - June
grow_season <- weather %>%
  mutate(month = month(Date), year = year(Date)) %>%
  filter(month > 2,
         month < 7) %>% 
  group_by(year) %>%
  summarise(grow.season.tot.precip = sum(total_precip_mm),
            grow.season.mean.min.temp = mean(minTemp_C),
            grow.season.mean.max.temp = mean(maxTemp_C)) %>% 
  mutate(grow.season.precip.1yearlag = lag(grow.season.tot.precip),
         grow.season.precip.2yearlag = lag(grow.season.precip.1yearlag),
         grow.season.min.temp.1yearlag = lag(grow.season.mean.min.temp),
         grow.season.min.temp.2yearlag = lag(grow.season.min.temp.1yearlag),
         grow.season.max.temp.1yearlag = lag(grow.season.mean.max.temp),
         grow.season.max.temp.2yearlag = lag(grow.season.max.temp.1yearlag))
         
#winter values and lagged 1 year and 2 year values
## winter is January-March
winter <- weather %>%
  mutate(month = month(Date), year = year(Date)) %>%
  filter(month < 4) %>% 
  group_by(year) %>%
  summarise(winter.tot.precip = sum(total_precip_mm),
            winter.mean.min.temp = mean(minTemp_C),
            winter.mean.max.temp = mean(maxTemp_C)) %>% 
  mutate(winter.precip.1yearlag = lag(winter.tot.precip),
         winter.precip.2yearlag = lag(winter.precip.1yearlag),
         winter.min.temp.1yearlag = lag(winter.mean.min.temp),
         winter.min.temp.2yearlag = lag(winter.min.temp.1yearlag),
         winter.max.temp.1yearlag = lag(winter.mean.max.temp),
         winter.max.temp.2yearlag = lag(winter.max.temp.1yearlag))

#summer values and lagged 1 year and 2 year values
## summer is June-August
summer <- weather %>%
  mutate(month = month(Date), year = year(Date)) %>%
  filter(month > 5,
         month < 9) %>% 
  group_by(year) %>%
  summarise(summer.tot.precip = sum(total_precip_mm),
            summer.mean.min.temp = mean(minTemp_C),
            summer.mean.max.temp = mean(maxTemp_C)) %>% 
  mutate(summer.precip.1yearlag = lag(summer.tot.precip),
         summer.precip.2yearlag = lag(summer.precip.1yearlag),
         summer.min.temp.1yearlag = lag(summer.mean.min.temp),
         summer.min.temp.2yearlag = lag(summer.min.temp.1yearlag),
         summer.max.temp.1yearlag = lag(summer.mean.max.temp),
         summer.max.temp.2yearlag = lag(summer.max.temp.1yearlag))

## put these together:
climate <- cbind(grow_season, winter, summer)

###still to do: add in rest of weather station data for the summer!u
