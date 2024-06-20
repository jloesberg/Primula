## What this script does:

# Reads in cleaned weather station data, makes a single df with mean values and lagged years
#also reads in long term data from climate NA and makes some graphs

#################################################################################

library(tidyverse)
library(lubridate)
library(wesanderson)

#library(ggpubr)
theme_set(theme_classic())
Z <- wes_palettes$Zissou1

#################################################################################

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
  dplyr::select(period, Tave_sp, Tmax_sp, Tmin_sp, PPT_sp, MAP)

## read in cleaned, extrapolated weather station data from WeatherStnInterp_Cowichan_Aug20...
weather <- read.csv("C:/Users/Jenna/OneDrive - The University Of British Columbia/Data Projects/Primula/data_for_publication/cgop_weather_daily_interp.csv")
weather$Date <- parse_date_time(weather$Date, "Y-m-d") #may need to change the format to whatever R reads it in as
weather$month_year <- format(as.Date(weather$Date), "%Y-%m")

#because the 2012 data has NA's, only include the December values (no na's)
weather <- weather %>% filter(Date >= as.Date("2012-12-01"),
                              Date <= as.Date("2023-08-15"))
#get total precip, round
# annual <- weather %>% 
#   mutate(month = month(Date), year = year(Date)) %>%
#   group_by(year) %>% 
#   summarize(annual.tot.precip = sum(total_precip_mm),
#             annual.mean.min.temp = round(mean(minTemp_C), 3),
#             annual.mean.max.temp = round(mean(maxTemp_C),3),
#             annual.mean.temp = round(mean(AveTemp_C),3)) %>% 
#   mutate( year = as.character(year))
#note - annual for 2023 is incorrect! doesnt include fall

#################################################################################

##Spring
# growing season values
### I'm classifying "growing season" as March - May, so the same as spring for the climate NA data
spring <- weather %>%
  mutate(month = month(Date), year = year(Date)) %>%
  filter(month > 2,
         month < 6)%>% 
  dplyr::group_by(year) %>%
  dplyr::summarize(spring.tot.precip = sum(total_precip_mm),
            spring.mean.min.temp = round(mean(minTemp_C), 3),
            spring.mean.max.temp = round(mean(maxTemp_C),3),
            spring.mean.temp = round(mean(AveTemp_C),3))
#
weather %>% 
  mutate(month = month(Date)) %>%
  filter(month > 2,
         month < 6) %>% 
  ggplot(aes(Date, total_precip_mm))+
  geom_point()

#this gives a df where we have values for each year

#get percentiles
# GS_percentile <- spring %>% 
#   summarize(precip_25th = quantile(grow.season.tot.precip, 0.25),
#             precip_75th = quantile(grow.season.tot.precip, 0.75),
#             precip_mean = mean(grow.season.tot.precip),
#             minT_25th = quantile(grow.season.mean.min.temp, 0.25),
#             minT_75th = quantile(grow.season.mean.min.temp, 0.75),
#             minT_mean = mean(grow.season.mean.min.temp),
#             maxT_25th = quantile(grow.season.mean.max.temp, 0.25),
#             maxT_75th = quantile(grow.season.mean.max.temp, 0.75),
#             maxT_mean = mean(grow.season.mean.max.temp))
# 
# #################################################################################

##WINTER
##  Dec, Jan, Feb. This is hard bc it spans 2 years
winter <- weather %>%
  mutate(month = month(Date), year = year(Date)) %>%
  filter(month < 3 | month == 12) %>% 
  mutate(year = if_else(month == 12, year+1, year)) %>% # need to make December the same time frame as Jan and Feb
  group_by(year) %>%
  dplyr::summarize(winter.tot.precip = sum(total_precip_mm),
            winter.mean.min.temp = round(mean(minTemp_C), 3),
            winter.mean.max.temp = round(mean(maxTemp_C), 3),
            winter.mean.temp = round(mean(AveTemp_C), 3)) 
# w_percentile <- winter %>% 
#   summarize(precip_25th = quantile(winter.tot.precip, 0.25, na.rm = T),
#             precip_75th = quantile(winter.tot.precip, 0.75, na.rm = T),
#             precip_mean = mean(winter.tot.precip, na.rm = T),
#             minT_25th = quantile(winter.mean.min.temp, 0.25, na.rm = T),
#             minT_75th = quantile(winter.mean.min.temp, 0.75, na.rm = T),
#             minT_mean = mean(winter.mean.min.temp, na.rm = T),
#             maxT_25th = quantile(winter.mean.max.temp, 0.25, na.rm = T),
#             maxT_75th = quantile(winter.mean.max.temp, 0.75, na.rm = T),
#             maxT_mean = mean(winter.mean.max.temp, na.rm = T))

#################################################################################

##SUMMER
#summer values and lagged 1 year and 2 year values
## summer is June-August
summer <- weather %>%
  mutate(month = month(Date), year = year(Date)) %>%
  filter(month > 5,
         month < 9) %>% 
  group_by(year) %>%
  dplyr::summarize(summer.tot.precip = sum(total_precip_mm),
            summer.mean.min.temp = round(mean(minTemp_C), 3),
            summer.mean.max.temp = round(mean(maxTemp_C), 3),
            summer.mean.temp = round(mean(AveTemp_C), 3))
# s_percentile <- summer %>% 
#   summarize(precip_25th = quantile(summer.tot.precip, 0.25),
#             precip_75th = quantile(summer.tot.precip, 0.75),
#             precip_mean = mean(summer.tot.precip),
#             minT_25th = quantile(summer.mean.min.temp, 0.25),
#             minT_75th = quantile(summer.mean.min.temp, 0.75),
#             minT_mean = mean(summer.mean.min.temp),
#             maxT_25th = quantile(summer.mean.max.temp, 0.25),
#             maxT_75th = quantile(summer.mean.max.temp, 0.75),
#             maxT_mean = mean(summer.mean.max.temp))

#################################################################################

##FALL
#fall values and lagged 1 year and 2 year values
## fall is September-November
fall <- weather %>%
  mutate(month = month(Date), year = year(Date)) %>%
  filter(month > 9,
         month < 12) %>% 
  group_by(year) %>%
  dplyr::summarize(fall.tot.precip = sum(total_precip_mm),
                   fall.mean.min.temp = round(mean(minTemp_C), 3),
                   fall.mean.max.temp = round(mean(maxTemp_C), 3),
                   fall.mean.temp = round(mean(AveTemp_C), 3))
# 
# F_percentile <- fall %>% 
#   summarize(precip_25th = quantile(fall.tot.precip, 0.25),
#             precip_75th = quantile(fall.tot.precip, 0.75),
#             precip_mean = mean(fall.tot.precip),
#             minT_25th = quantile(fall.mean.min.temp, 0.25),
#             minT_75th = quantile(fall.mean.min.temp, 0.75),
#             minT_mean = mean(fall.mean.min.temp),
#             maxT_25th = quantile(fall.mean.max.temp, 0.25),
#             maxT_75th = quantile(fall.mean.max.temp, 0.75),
#             maxT_mean = mean(fall.mean.max.temp))

#################################################################################

# #we had the idea of looking at certain time periods where climate may be more important for primula:
# weather$month_day <- format(as.Date(weather$Date), "%m-%d")
# 
# ##(1) just April 1-May 30th climate
# 
# aprilmay <-  weather %>%
#   mutate(month = month(Date), year = year(Date)) %>%
#   filter(month > 3,
#          month < 6)
# aprilmay <- aprilmay %>% 
#   group_by(year) %>%
#   dplyr::summarize(aprilmay.tot.precip = sum(total_precip_mm),
#                    aprilmay.mean.min.temp = mean(minTemp_C),
#                    aprilmay.mean.max.temp = mean(maxTemp_C),
#                    aprilmay.mean.temp = mean(AveTemp_C))
# 
# 
# ##(2) just April 15-May 15 climate - this is clunky but whatever
# 
# april15 <-  weather %>%
#   mutate(month = month(Date), year = year(Date), day = day(Date)) %>%
#   filter(month == 4 & day >= 15)
# may15 <-  weather %>%
#   mutate(month = month(Date), year = year(Date), day = day(Date)) %>%
#   filter(month == 5 & day <= 15)
# 
# april15may15 <- bind_rows(april15, may15)
# remove(april15, may15)
# 
# april15may15 <- april15may15 %>% 
#   group_by(year) %>%
#   dplyr::summarize(april15may15.tot.precip = sum(total_precip_mm),
#                    april15may15.mean.min.temp = mean(minTemp_C),
#                    april15may15.mean.max.temp = mean(maxTemp_C),
#                    april15may15.mean.temp = mean(AveTemp_C))
# 
# #test <- left_join(aprilmay, april15may15) %>% 
#   #left_join(spring)
# 
# 
# spring %>% 
#   ggplot(aes(x = year, y = grow.season.tot.precip))+
#   geom_col(fill = wes_palettes$Darjeeling2[2], color = "black")+
#   labs(y = "Spring (March-May) total precipitation (mm)", x = "Year")+
#   scale_y_continuous(expand = c(0,0), n.breaks = 15) 
# april15may15 %>% 
#   ggplot(aes(x = year, y = april15may15.tot.precip))+
#   geom_col(fill = wes_palettes$Darjeeling2[2], color = "black")+
#   labs(y = "April15 - May15 total precipitation (mm)", x = "Year")+
#   scale_y_continuous(expand = c(0,0), n.breaks = 15) 
# aprilmay %>% 
#   ggplot(aes(x = year, y = aprilmay.tot.precip))+
#   geom_col(fill = wes_palettes$Darjeeling2[2], color = "black")+
#   labs(y = "April 1 - May 31 total precipitation (mm)", x = "Year")+
#   scale_y_continuous(expand = c(0,0), n.breaks = 15)
# 
# spring %>% 
#   ggplot(aes(x = year, y = grow.season.mean.max.temp))+
#   geom_line(aes(group = 1, color = Z[1]), linewidth = 1.25)+
#   geom_line(aes(y = grow.season.mean.min.temp, group = 1, color = Z[5]), linewidth = 1.25)+  
#   geom_line(aes(y = grow.season.mean.temp, group = 1, color = "grey"), linewidth = 1.25, color = Z[3])+
#   labs(y = "Growing Season (March-May) Temperature (C)", x = "Year")+
#   theme(legend.position="none")
# 
# april15may15 %>% 
#   ggplot(aes(x = year, y = april15may15.mean.max.temp))+
#   geom_line(aes(group = 1, color = Z[1]), linewidth = 1.25)+
#   geom_line(aes(y = april15may15.mean.min.temp, group = 1, color = Z[5]), linewidth = 1.25)+  
#   geom_line(aes(y = april15may15.mean.temp, group = 1, color = "grey"), linewidth = 1.25, color = Z[3])+
#   labs(y = "April 15 - May 15 Temperature (C)", x = "Year")+
#   theme(legend.position="none")
# 
# aprilmay %>% 
#   ggplot(aes(x = year, y = aprilmay.mean.max.temp))+
#   geom_line(aes(group = 1, color = Z[1]), linewidth = 1.25)+
#   geom_line(aes(y = aprilmay.mean.min.temp, group = 1, color = Z[5]), linewidth = 1.25)+  
#   geom_line(aes(y = aprilmay.mean.temp, group = 1, color = "grey"), linewidth = 1.25, color = Z[3])+
#   labs(y = "April 1 - May 31 Temperature (C)", x = "Year")+
#   theme(legend.position="none")
# 
# #what we want to know is: Is the whole growing season more/less/same variable as smaller time chunks (ie maybe early march/april are always the same, and differences only matter later on)
# 
# remove(april15may15, aprilmay)

###################################################################################

## put these together:
climate <- left_join(left_join(left_join(spring, winter), summer), fall) %>% 
  mutate(year = as.character(year)) 

#climate_percentiles <- rbind(GS_percentile, s_percentile, w_percentile, F_percentile)
#climate_percentiles$season <- c("growseason", "summer", "winter", "fall")
remove(spring, summer, winter, fall, weather) # take away the dfs I dont need

###################################################################################

climate <- climate %>% 
  filter(year != "2023") #were not estign cliamte in 2023

#here I'm scaling the climate varibles
climate_scale <- climate %>% 
  dplyr::select(1, c(ends_with("tot.precip"), ends_with(".temp")))

j <- sapply(climate_scale, is.numeric)
climate_scale[j] <- scale(climate_scale[j])
climate_scale <- climate_scale %>% 
    mutate_if(is.numeric, round, 3)
remove(j)
# #or
# climate2 <- climate %>% 
#   dplyr::select(1, c(ends_with("tot.precip"), ends_with(".temp"))) %>% 
#   mutate(spring.tot.precip.scale = c(scale(spring.tot.precip)),
#        spring.mean.temp.scale = c(scale(spring.mean.temp)),
#        summer.tot.precip.scale = c(scale(summer.tot.precip)),
#        summer.mean.temp.scale = c(scale(summer.mean.temp)),
#        winter.tot.precip.scale = c(scale(winter.tot.precip)),
#        winter.mean.temp.scale = c(scale(winter.mean.temp)),
#        fall.mean.temp.scale =c(scale(fall.mean.temp)),
#        fall.tot.precip.scale =c(scale(fall.tot.precip))) %>%
#   mutate_if(is.numeric, round, 3)

###################################################################################

#Let's look at climate across the years of the experiment - how variable is precipitation and temperature?
#what I really want is to make a graphw tih 2 axes, but ggplot is not making that easy! 
#deciding to use the4 1961-1990 long term average - it is not that different from more recent time frames (ie 1991-2020 and 1981-2010)
PPT_sp <- longterm$PPT_sp[[1]]

precip <- climate %>% ggplot(aes(x = year, y = spring.tot.precip))+
  geom_col(fill = wes_palettes$Darjeeling2[2], color = "black")+
  labs(y = "Spring (March-May) total precipitation (mm)", x = "Year")+
  geom_hline(yintercept = PPT_sp, linetype='dashed')+
  annotate("text", x = "2020", y = PPT_sp, label = "1961-1990 Spring Precip", vjust = -0.5)+
  scale_y_continuous(expand = c(0,0), n.breaks = 15) 
precip
ggsave("./Figures/climate/gs.precip.withclimateNA.3.12.24.png", width = 8, height = 8)
TmaxSP<- longterm$Tmax_sp[[1]]
TminSP<- longterm$Tmin_sp[[1]]
TaveSP<- longterm$Tave_sp[[1]]

(temp <- climate %>% ggplot(aes(x = year, y = spring.mean.max.temp)) + 
  geom_line(aes(group = 1, color = "#f2623f"), linewidth = 1.25, color = "#f2623f")+
  geom_line(aes(y = spring.mean.min.temp, group = 1, color = "#323697"), linewidth = 1.25, color = "#323697")+  
  geom_line(aes(y = spring.mean.temp, group = 1, color = "#94c8e0"), linewidth = 1.25, color = "#94c8e0")+
  labs(y = "Temperature (C)", x = "Year")+
  theme(legend.position="none")+
  geom_hline(yintercept = TmaxSP, linetype='dashed', color = "#f2623f")+
  geom_hline(yintercept = TminSP, linetype='dashed', color = "#323697")+
  geom_hline(yintercept = TaveSP, linetype='dashed', color = "#94c8e0")+
  annotate("text", x = "2014", y = TmaxSP, label = "1961-1990 Spring Max Temp", vjust = -0.5)+
  annotate("text", x = "2014", y = TminSP, label = "1961-1990 Spring Min Temp", vjust = -0.5)+
  annotate("text", x = "2014", y = TaveSP, label = "1961-1990 Spring Mean Temp", vjust = -0.5)+
  scale_y_continuous(n.breaks=15)+
  geom_vline(xintercept = "2021", linetype = "dashed")+
  annotate( x = "2021", y = 17, label = "Clipping Experiment", vjust = -0.2,geom="label"))
  

temp
#ggsave("./Figures/climate/gs.temp.climateNA.updated.png", width = 8, height = 8)
#arrange <- ggarrange(precip, temp, ncol = 2, nrow = 1)

remove(precip, temp, longterm, annual)


#####################################################################33
# #histrogram for distribution of climate variables
histograms <- climate %>%
  dplyr::select(year, summer.mean.temp, winter.mean.temp, spring.mean.temp, spring.tot.precip)

clip.summer <- climate$summer.mean.temp[climate$year == 2021]
clip.winter <- climate$winter.mean.temp[climate$year == 2021]
clip.spring <- climate$spring.mean.temp[climate$year == 2021]
clip.precip <- climate$spring.tot.precip[climate$year == 2021]


par(mfrow=c(1,4))

hist(histograms$summer.mean.temp, xlab = "degrees C", main = "Summer", col = 'white', border = "black")
abline(v = mean(histograms$summer.mean.temp, na.rm = T),
       col = "steelblue", lty = 2, lwd = 4)
abline(v = mean(histograms$summer.mean.temp, na.rm = T) + sd(histograms$summer.mean.temp, na.rm = T),
       col = "grey", lty = 2, lwd = 3)
abline(v = mean(histograms$summer.mean.temp, na.rm = T) - sd(histograms$summer.mean.temp, na.rm = T),
       col = "grey", lty = 2, lwd = 3)
points(x = clip.summer, y = 0, pch = 19, col = "darkorchid4", cex = 1.5)
 # abline(v = mean(histograms$summer.mean.temp, na.rm = T) - 2*(sd(histograms$summer.mean.temp, na.rm = T)),
 #        col = "grey", lty = 2, lwd = 2)
 # abline(v = mean(histograms$summer.mean.temp, na.rm = T) + 2*(sd(histograms$summer.mean.temp, na.rm = T)),
 #        col = "grey", lty = 2, lwd = 2)



hist(histograms$winter.mean.temp, xlab = "degrees C", main = "Winter", col = 'white', border = "black")
abline(v = mean(histograms$winter.mean.temp, na.rm = T),
       col = "steelblue", lty = 2, lwd = 4)
abline(v = mean(histograms$winter.mean.temp, na.rm = T) + sd(histograms$winter.mean.temp, na.rm = T),
       col = "grey", lty = 2, lwd = 3)
abline(v = mean(histograms$winter.mean.temp, na.rm = T) - sd(histograms$winter.mean.temp, na.rm = T),
       col = "grey", lty = 2, lwd = 3)
points(x = clip.winter, y = 0, pch = 19, col = "darkorchid4", cex = 1.5)



hist(histograms$spring.mean.temp, xlab = "degrees C", main = "Spring", col = 'white', border = "black")
abline(v = mean(histograms$spring.mean.temp, na.rm = T),
       col = "steelblue", lty = 2, lwd = 4)
abline(v = mean(histograms$spring.mean.temp, na.rm = T) + sd(histograms$spring.mean.temp, na.rm = T),
       col = "grey", lty = 2, lwd = 3)
abline(v = mean(histograms$spring.mean.temp, na.rm = T) - sd(histograms$spring.mean.temp, na.rm = T),
       col = "grey", lty = 2, lwd = 3)
points(x = clip.spring,y = 0, pch = 19, col = "darkorchid4", cex = 1.5)


hist(histograms$spring.tot.precip, xlab = "Total Precip (mm)", main = "Spring", col = 'white', border = "black")
abline(v = mean(histograms$spring.tot.precip, na.rm = T),
       col = "steelblue", lty = 2, lwd = 4)
abline(v = mean(histograms$spring.tot.precip, na.rm = T) + sd(histograms$spring.tot.precip, na.rm = T),
       col = "grey", lty = 2, lwd = 3)
abline(v = mean(histograms$spring.tot.precip, na.rm = T) - sd(histograms$spring.tot.precip, na.rm = T),
       col = "grey", lty = 2, lwd = 3)
points(x = clip.precip,y = 0, pch = 19, col = "darkorchid4", cex = 1.5)

#remove(histograms)

write.csv(climate_scale, "C:/Users/Jenna/OneDrive - The University Of British Columbia/Data Projects/Primula/data_for_publication/CGOP_climatevalues_scaled.csv", row.names=FALSE)
write.csv(climate, "C:/Users/Jenna/OneDrive - The University Of British Columbia/Data Projects/Primula/data_for_publication/CGOP_climatevalues.csv", row.names=FALSE)
