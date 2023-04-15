## What this script does:

# Reads in cleaned weather station data, makes a single df with mean values and lagged years
#also reads in long term data from climate NA and makes some graphs

library(tidyverse)
library(lubridate)
library(wesanderson)
#library(ggpubr)
theme_set(theme_classic())
Z <- wes_palettes$Zissou1

# read in data from ClimateNA (long term averages)
longterm <- read.csv("C:/Users/Jenna/Dropbox/Williams' Lab/Cowichan IDE/LoggerData/ClimateNA/ClimateNA_CGOP_ClimateData.csv")
#the variables I want here are:
## Tave_sp	spring mean temperature (°C)
##  	Tmax_sp	spring mean maximum temperature (°C)
## Tmin_sp	spring mean minimum temperature (°C)
##  	PPT_sp	spring precipitation (mm)
## MAP	mean annual precipitation (mm)
## they define spring as MArch, April, and May
longterm <- longterm %>% 
  select(c(period,Tave_sp, Tmax_sp, Tmin_sp, PPT_sp, MAP))

## read in cleaned, extrapolated weather station data from WeatherStnInterp_Cowichan_Aug20...
weather <- read.csv("C:/Users/Jenna/Dropbox/Williams' Lab/Cowichan IDE/LoggerData/weather station/WS_cleaned/cgop_weather_daily_interp.csv")
weather$Date <- parse_date_time(weather$Date, "Y-m-d")

weather$month_year <- format(as.Date(weather$Date), "%Y-%m")

#because the 2012 data has NA's, only include the December values (no na's)
weather <- weather %>% filter(Date >= as.Date("2012-12-01"))

# growing season values and lagged 1 year and 2 year values
### I'm classifying "growing season" as March - May, so the same as spring for the climate NA data
grow_season <- weather %>%
  mutate(month = month(Date), year = year(Date)) %>%
  filter(month > 2,
         month < 6)%>% 
  dplyr::group_by(year) %>%
  dplyr::summarize(grow.season.tot.precip = sum(total_precip_mm),
            grow.season.mean.min.temp = mean(minTemp_C),
            grow.season.mean.max.temp = mean(maxTemp_C),
            grow.season.mean.mean.temp = mean(AveTemp_C)) %>% 
  mutate(grow.season.precip.1yearlag = lag(grow.season.tot.precip),
         grow.season.precip.2yearlag = lag(grow.season.precip.1yearlag),
         grow.season.min.temp.1yearlag = lag(grow.season.mean.min.temp),
         grow.season.ave.temp.1yearlag = lag(grow.season.mean.mean.temp),
         grow.season.min.temp.2yearlag = lag(grow.season.min.temp.1yearlag),
         grow.season.max.temp.1yearlag = lag(grow.season.mean.max.temp),
         grow.season.max.temp.2yearlag = lag(grow.season.max.temp.1yearlag),
         grow.season.ave.temp.2yearlag = lag(grow.season.ave.temp.1yearlag))
#this gives a df where we have values for each year

#double check that this is what I want here - probably better to not take the mean of a mean? coudl I just use percentiles from daily averges in teh growign season?
GS_percentile <- grow_season %>% 
  summarize(precip_25th = quantile(grow.season.tot.precip, 0.25),
            precip_75th = quantile(grow.season.tot.precip, 0.75),
            precip_mean = mean(grow.season.tot.precip),
            minT_25th = quantile(grow.season.mean.min.temp, 0.25),
            minT_75th = quantile(grow.season.mean.min.temp, 0.75),
            minT_mean = mean(grow.season.mean.min.temp),
            maxT_25th = quantile(grow.season.mean.max.temp, 0.25),
            maxT_75th = quantile(grow.season.mean.max.temp, 0.75),
            maxT_mean = mean(grow.season.mean.max.temp))


#winter values and lagged 1 year and 2 year values
## winter is January-March # changing this to Dec, Jan, Feb. This is hard bc it spans 2 years
winter <- weather %>%
  mutate(month = month(Date), year = year(Date)) %>%
  filter(month < 3 | month == 12) %>% 
  mutate(year = if_else(month == 12, year+1, year)) %>% # need to make december the same time frame as Jan and Feb
  group_by(year) %>%
  dplyr::summarize(winter.tot.precip = sum(total_precip_mm),
            winter.mean.min.temp = mean(minTemp_C),
            winter.mean.max.temp = mean(maxTemp_C),
            winter.mean.temp = mean(AveTemp_C)) %>%
  mutate(winter.precip.1yearlag = lag(winter.tot.precip),
         winter.precip.2yearlag = lag(winter.precip.1yearlag),
         winter.min.temp.1yearlag = lag(winter.mean.min.temp),
         winter.min.temp.2yearlag = lag(winter.min.temp.1yearlag),
         winter.max.temp.1yearlag = lag(winter.mean.max.temp),
         winter.max.temp.2yearlag = lag(winter.max.temp.1yearlag))

w_percentile <- winter %>% 
  summarize(precip_25th = quantile(winter.tot.precip, 0.25, na.rm = T),
            precip_75th = quantile(winter.tot.precip, 0.75, na.rm = T),
            precip_mean = mean(winter.tot.precip, na.rm = T),
            minT_25th = quantile(winter.mean.min.temp, 0.25, na.rm = T),
            minT_75th = quantile(winter.mean.min.temp, 0.75, na.rm = T),
            minT_mean = mean(winter.mean.min.temp, na.rm = T),
            maxT_25th = quantile(winter.mean.max.temp, 0.25, na.rm = T),
            maxT_75th = quantile(winter.mean.max.temp, 0.75, na.rm = T),
            maxT_mean = mean(winter.mean.max.temp, na.rm = T))

#summer values and lagged 1 year and 2 year values
## summer is June-August
summer <- weather %>%
  mutate(month = month(Date), year = year(Date)) %>%
  filter(month > 5,
         month < 9) %>% 
  group_by(year) %>%
  dplyr::summarize(summer.tot.precip = sum(total_precip_mm),
            summer.mean.min.temp = mean(minTemp_C),
            summer.mean.max.temp = mean(maxTemp_C),
            summer.mean.temp = mean(AveTemp_C)) %>% 
  mutate(summer.precip.1yearlag = lag(summer.tot.precip),
         summer.precip.2yearlag = lag(summer.precip.1yearlag),
         summer.min.temp.1yearlag = lag(summer.mean.min.temp),
         summer.min.temp.2yearlag = lag(summer.min.temp.1yearlag),
         summer.max.temp.1yearlag = lag(summer.mean.max.temp),
         summer.max.temp.2yearlag = lag(summer.max.temp.1yearlag))

s_percentile <- summer %>% 
  summarize(precip_25th = quantile(summer.tot.precip, 0.25),
            precip_75th = quantile(summer.tot.precip, 0.75),
            precip_mean = mean(summer.tot.precip),
            minT_25th = quantile(summer.mean.min.temp, 0.25),
            minT_75th = quantile(summer.mean.min.temp, 0.75),
            minT_mean = mean(summer.mean.min.temp),
            maxT_25th = quantile(summer.mean.max.temp, 0.25),
            maxT_75th = quantile(summer.mean.max.temp, 0.75),
            maxT_mean = mean(summer.mean.max.temp))

## put these together:
climate <- left_join(grow_season, winter, by = "year")
climate <- left_join(climate, summer, by = "year") %>% 
  mutate(year = as.character(year))

climate_percentiles <- rbind(GS_percentile, s_percentile, w_percentile)
climate_percentiles$season <- c("growseason", "summer", "winter")
remove(grow_season, summer, winter, weather, GS_percentile, s_percentile, w_percentile) # take away the dfs I dont need


### 
#Let's look at cliamte across the years of the experiment - how variable is precipitation and temperature?
#what I really want is to make a graphw tih 2 axes, but ggplot is not making that easy! 
#deciding to use the4 1961-1990 long term average - it is not that different from more recent time frames (ie 1991-2020 and 1981-2010)
PPT_sp <- longterm$PPT_sp[[1]]


precip <- climate %>% ggplot(aes(x = year, y = grow.season.tot.precip))+
  geom_col(fill = wes_palettes$Darjeeling2[2], color = "black")+
  labs(y = "Spring (March-May) total precipitation (mm)", x = "Year")+
  geom_hline(yintercept = PPT_sp, linetype='dashed')+
  annotate("text", x = "2020", y = PPT_sp, label = "1961-1990 Spring Precip", vjust = -0.5)+
  scale_y_continuous(expand = c(0,0), n.breaks = 15) 
precip
#ggsave("./Figures/climate/gs.precip.withclimateNA.png", width = 8, height = 8)
TmaxSP<- longterm$Tmax_sp[[1]]
TminSP<- longterm$Tmin_sp[[1]]
TaveSP<- longterm$Tave_sp[[1]]

temp <- climate %>% ggplot(aes(x = year, y = grow.season.mean.max.temp)) + 
  geom_line(aes(group = 1, color = Z[1]), linewidth = 1.25)+
  geom_line(aes(y = grow.season.mean.min.temp, group = 1, color = Z[5]), linewidth = 1.25)+  
  geom_line(aes(y = grow.season.mean.mean.temp, group = 1, color = Z[3]), linewidth = 1.25, color = Z[3])+
  labs(y = "Temperature (C)", x = "Year")+
  theme(legend.position="none")+
  geom_hline(yintercept = TmaxSP, linetype='dashed', color = Z[5])+
  geom_hline(yintercept = TminSP, linetype='dashed', color = Z[1])+
  geom_hline(yintercept = TaveSP, linetype='dashed', color = Z[3])+
  annotate("text", x = "2021", y = TmaxSP, label = "1961-1990 Spring Max Temp", vjust = -0.5)+
  annotate("text", x = "2021", y = TminSP, label = "1961-1990 Spring Min Temp", vjust = -0.5)+
  annotate("text", x = "2021", y = TaveSP, label = "1961-1990 Spring Mean Temp", vjust = -0.5)+
  scale_y_continuous(n.breaks=15)

  
  
temp
#ggsave("./Figures/climate/gs.temp.climateNA.png", width = 8, height = 8)
#arrange <- ggarrange(precip, temp, ncol = 2, nrow = 1)
remove(precip, temp)
