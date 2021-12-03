## Primula vital rates with climate data

###still to do: add in rest of weather station data for the summer!
## add in vital rate models

library(tidyverse)
theme_set(theme_classic)
library(AICcmodavg)
library(lme4)
library(lmtest)

# Read in data ------------------------------------------------------------------------

### read in all years of Primula data and weather data from the Cowichan
# Add in climate data:
source("./scripts/Cowichan_climatevalues.R")

# Add in demography data:
source("./scripts/dode_allyears_cleaned.R")

## Join climate data to demography data by year
primula <- left_join(Dodecatheon_lag, climate, by = "year")

remove(climate, Dodecatheon_lag)

# dealing with extra problem tags:
### tag 35 in 2019 didnt have an entry for pflower or no.flowers, I'm not certain whether it flowered or not. so, just taking it out
primula <- primula %>% filter(tag != "35")

## Now to build the models. 

# Growth Vital Rates ------------------------------------------------------

### Do vital rates differ between years?

gm1 <- lm(log(ros.area) ~ log(ros.areaTminus1), data = primula)
gm2 <- lm(log(ros.area) ~ log(ros.areaTminus1) + year, data = primula)
gm3 <- lm(log(ros.area) ~ log(ros.areaTminus1)*year, data = primula)

lrtest(gm1, gm2)
lrtest(gm1, gm3)
# YES

### Do vital rates differ between treatments?

gm1 <- lm(log(ros.area) ~ log(ros.areaTminus1), data = primula)
gm2 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt, data = primula)
gm3 <- lm(log(ros.area) ~ log(ros.areaTminus1)* trt, data = primula)

lrtest(gm1, gm2)
lrtest(gm1, gm3)

# YES 

# What does this tell us? Growth does change across years, and it does change across treatment. However, treatment likely depends on year (some years are drier/wetter, so this will affect strength of treatments/how different they are?). How to test this: Make a model with both and test for a significant interaction? What does the extra water in 2021 mean for this (independent of how much it rained)

gm1 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt, data = primula)
gm2 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt*year, data = primula)
gm3 <- lm(log(ros.area) ~ log(ros.areaTminus1)*trt + year, data = primula)

lrtest(gm1, gm2)
lrtest(gm1, gm3)

# What does this tell us: yes there is a significant interaction between year and treatment (read: the influence of treatment on growth varies across years)
#############################################################################
# Does flowering in the previous year affect size this year?

gm1 <- lm(log(ros.area) ~ log(ros.areaTminus1), data = primula)
gm2 <- lm(log(ros.area) ~ log(ros.areaTminus1) + pflowerTminus1, data = primula)

lrtest(gm1, gm2)
# there is a size cost of reproduction (size and reproducing last year affects size this year)

# test <- primula %>% filter(!is.na(ros.areaTminus1))
# test2 <- primula %>% filter(!is.na(ros.areaTminus1) & !is.na(pflowerTminus1))
# test3 <- anti_join(test, test2)

##########################################################################
# Candidate models
gm.min <- lm(log(ros.area) ~ log(ros.areaTminus1), data = primula)
gm.min2 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt, data = primula)
gm.min3 <- lm(log(ros.area) ~ log(ros.areaTminus1) + pflowerTminus1, data = primula)
gm.min4 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt + pflowerTminus1, data = primula)
gm.min5 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt*pflowerTminus1, data = primula)
########################################################################################
gm1 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt + grow.season.tot.precip, data = primula)
gm2 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt*grow.season.tot.precip + pflowerTminus1, data = primula)
gm3 <- lm(log(ros.area) ~ log(ros.areaTminus1) + grow.season.tot.precip, data = primula)
gm4 <- lm(log(ros.area) ~ log(ros.areaTminus1) + grow.season.tot.precip + pflowerTminus1, data = primula)
#
gm5 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt + grow.season.mean.min.temp, data = primula)
gm6 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt*grow.season.mean.min.temp, data = primula)
gm7 <- lm(log(ros.area) ~ log(ros.areaTminus1) + grow.season.mean.min.temp, data = primula)
#
gm8 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt + grow.season.mean.max.temp, data = primula)
gm9 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt*grow.season.mean.max.temp, data = primula)
gm10 <- lm(log(ros.area) ~ log(ros.areaTminus1) + grow.season.mean.max.temp, data = primula)
#
gm11 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt + winter.mean.min.temp, data = primula)
gm12 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt*winter.mean.min.temp, data = primula)
gm13 <- lm(log(ros.area) ~ log(ros.areaTminus1) + winter.mean.min.temp, data = primula)
#
gm14 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt + winter.tot.precip, data = primula)
gm15 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt*winter.tot.precip, data = primula)
gm16 <- lm(log(ros.area) ~ log(ros.areaTminus1) + winter.tot.precip, data = primula)
#
gm17 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt + winter.mean.max.temp, data = primula)
gm18 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt*winter.mean.max.temp, data = primula)
gm19 <- lm(log(ros.area) ~ log(ros.areaTminus1) + winter.mean.max.temp, data = primula)
#
gm20 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt + summer.mean.max.temp, data = primula)
gm21 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt*summer.mean.max.temp, data = primula)
gm22 <- lm(log(ros.area) ~ log(ros.areaTminus1) + summer.mean.max.temp, data = primula)
#
gm23 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt + summer.mean.min.temp, data = primula)
gm24 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt*summer.mean.min.temp, data = primula)
gm25 <- lm(log(ros.area) ~ log(ros.areaTminus1) + summer.mean.min.temp, data = primula)
#
gm26 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt + summer.tot.precip, data = primula)
gm27 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt*summer.tot.precip, data = primula)
gm28 <- lm(log(ros.area) ~ log(ros.areaTminus1) + summer.tot.precip, data = primula)
##
growth <- lst(gm1, gm2, gm3, gm4, gm5, gm6, gm7, gm8, gm9, gm10, gm.min, gm.min2, gm.min3, gm.min4, gm.min5, gm11, gm12, gm13, gm14, gm15, gm16, gm17, gm18, gm19, gm20, gm21, gm22, gm23, gm24, gm25, gm26, gm27, gm28)

growth_AICc<-aictab(cand.set = growth, modnames = NULL,second.ord=TRUE,nobs=NULL,sort=TRUE)

primula %>% ggplot(aes(log(ros.areaTminus1), log(ros.area))) +
  geom_jitter(height = .02)+
  theme_classic()+
  geom_smooth(method = lm, se=FALSE, fullrange = TRUE)+
  expand_limits(x = 0, y = 0)+
  facet_wrap(~year)



#########################################################################################
# Probability of flowering  -------------------------------------------------------------


### Do vital rates differ between years?

fm1 <- glm(pflower ~ log(ros.areaTminus1), data = primula, family = "binomial")
fm2 <- glm(pflower ~ log(ros.areaTminus1) + year, data = primula, family = "binomial")
fm3 <- glm(pflower ~ log(ros.areaTminus1)*year, data = primula, family = "binomial")

lrtest(fm1, fm2)
lrtest(fm1, fm3)
# NO

### Do vital rates differ between treatments?

fm1 <- glm(pflower ~ log(ros.areaTminus1), data = primula, family = "binomial")
fm2 <- glm(pflower ~ log(ros.areaTminus1) + trt, data = primula, family = "binomial")
fm3 <- glm(pflower ~ log(ros.areaTminus1)*trt, data = primula, family = "binomial")

lrtest(fm1, fm2)
lrtest(fm1, fm3)

# YES 

# What does this tell us? The probability of flowering does not change across years, but it does change across treatment. However, treatment likely depends on year (some years are drier/wetter, so this will affect strength of treatments/how different they are?). How to test this: Make a model with both and test for a significant interaction? What does the extra water in 2021 mean for this (independent of how much it rained)

fm1 <- glm(pflower ~ log(ros.areaTminus1) + trt, data = primula, family = "binomial")
fm2 <- glm(pflower ~ log(ros.areaTminus1) + trt*year, data = primula, family = "binomial")
fm3 <- glm(pflower ~ log(ros.areaTminus1)*trt + year, data = primula, family = "binomial")

lrtest(fm1, fm2)
lrtest(fm1, fm3)

# What does this tell us: yes there is a significant interaction between year and treatment (read: the influence of treatment on pflower varies across years)
#############################################################################
# Does flowering in the previous year affect flowering this year?

fm1 <- glm(pflower ~ log(ros.areaTminus1), data = primula, family = "binomial")
fm2 <- glm(pflower ~ log(ros.areaTminus1) + pflowerTminus1, data = primula, family = "binomial")

lrtest(fm1, fm2)
# there is a flowering cost of reproduction

##########################################################################
# Candidate models
fm.min <- glm(pflower ~ log(ros.areaTminus1), data = primula, family = "binomial")
fm.min2 <- glm(pflower ~ log(ros.areaTminus1) + trt, data = primula, family = "binomial")
fm.min3 <- glm(pflower ~ log(ros.areaTminus1) + pflowerTminus1, data = primula, family = "binomial")
fm.min4 <- glm(pflower ~ log(ros.areaTminus1) + trt + pflowerTminus1, data = primula, family = "binomial")
fm.min5 <- glm(pflower ~ log(ros.areaTminus1) + trt*pflowerTminus1, data = primula, family = "binomial")
########################################################################################
fm1 <- glm(pflower ~ log(ros.areaTminus1) + trt + grow.season.tot.precip, data = primula, family = "binomial")
fm2 <- glm(pflower ~ log(ros.areaTminus1) + trt*grow.season.tot.precip + pflowerTminus1, data = primula, family = "binomial")
fm3 <- glm(pflower ~ log(ros.areaTminus1) + grow.season.tot.precip, data = primula, family = "binomial")
fm4 <- glm(pflower ~ log(ros.areaTminus1) + grow.season.tot.precip + pflowerTminus1, data = primula, family = "binomial")
#
fm5 <- glm(pflower ~ log(ros.areaTminus1) + trt + grow.season.mean.min.temp, data = primula, family = "binomial")
fm6 <- glm(pflower ~ log(ros.areaTminus1) + trt*grow.season.mean.min.temp, data = primula, family = "binomial")
fm7 <- glm(pflower ~ log(ros.areaTminus1) + grow.season.mean.min.temp, data = primula, family = "binomial")
#
fm8 <- glm(pflower ~ log(ros.areaTminus1) + trt + grow.season.mean.max.temp, data = primula, family = "binomial")
fm9 <- glm(pflower ~ log(ros.areaTminus1) + trt*grow.season.mean.max.temp, data = primula, family = "binomial")
fm10 <- glm(pflower ~ log(ros.areaTminus1) + grow.season.mean.max.temp, data = primula, family = "binomial")
#
fm11 <- glm(pflower ~ log(ros.areaTminus1) + trt + winter.mean.min.temp, data = primula, family = "binomial")
fm12 <- glm(pflower ~ log(ros.areaTminus1) + trt*winter.mean.min.temp, data = primula, family = "binomial")
fm13 <- glm(pflower ~ log(ros.areaTminus1) + winter.mean.min.temp, data = primula, family = "binomial")
#
fm14 <- glm(pflower ~ log(ros.areaTminus1) + trt + winter.tot.precip, data = primula, family = "binomial")
fm15 <- glm(pflower ~ log(ros.areaTminus1) + trt*winter.tot.precip, data = primula, family = "binomial")
fm16 <- glm(pflower ~ log(ros.areaTminus1) + winter.tot.precip, data = primula, family = "binomial")
#
fm17 <- glm(pflower ~ log(ros.areaTminus1) + trt + winter.mean.max.temp, data = primula, family = "binomial")
fm18 <- glm(pflower ~ log(ros.areaTminus1) + trt*winter.mean.max.temp, data = primula, family = "binomial")
fm19 <- glm(pflower ~ log(ros.areaTminus1) + winter.mean.max.temp, data = primula, family = "binomial")
#
fm20 <- glm(pflower ~ log(ros.areaTminus1) + trt + summer.mean.max.temp, data = primula, family = "binomial")
fm21 <- glm(pflower ~ log(ros.areaTminus1) + trt*summer.mean.max.temp, data = primula, family = "binomial")
fm22 <- glm(pflower ~ log(ros.areaTminus1) + summer.mean.max.temp, data = primula, family = "binomial")
#
fm23 <- glm(pflower ~ log(ros.areaTminus1) + trt + summer.mean.min.temp, data = primula, family = "binomial")
fm24 <- glm(pflower ~ log(ros.areaTminus1) + trt*summer.mean.min.temp, data = primula, family = "binomial")
fm25 <- glm(pflower ~ log(ros.areaTminus1) + summer.mean.min.temp, data = primula)
#
fm26 <- glm(pflower ~ log(ros.areaTminus1) + trt + summer.tot.precip, data = primula)
fm27 <- glm(pflower ~ log(ros.areaTminus1) + trt*summer.tot.precip, data = primula)
fm28 <- glm(pflower ~ log(ros.areaTminus1) + summer.tot.precip, data = primula, family = "binomial")
##
pflower <- lst(fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm.min, fm.min2, fm.min3, fm.min4, fm.min5, fm11, fm12, fm13, fm14, fm15, fm16, fm17, fm18, fm19, fm20, fm21, fm22, fm23, fm24, fm25, fm26, fm27, fm28)

flowering_AICc<-aictab(cand.set = pflower, modnames = NULL,second.ord=TRUE,nobs=NULL,sort=TRUE)

primula %>% ggplot(aes(log(ros.areaTminus1), pflower)) +
  geom_jitter(height = .02)+
  theme_classic()+
  geom_smooth(method = glm, method.args= list(family="binomial"), se=FALSE, fullrange = TRUE)+
  expand_limits(x = 0, y = 0)+
facet_wrap(~trt)
##Why is it lower for the irrigated plots??


# We then tested for correlations between year random effects and each of the climate variables: mean daily precipitation, mean daily temperature, and proportion wet days across each season (spring, summer, fall, winter) and across one transition year (May 16, year t – May 15 year t+1). 

#########################################################################################
# Probability of surviving  -------------------------------------------------------------


### Does survival differ between years?

sm1 <- glm(psurvival ~ log(ros.areaTminus1), data = primula, family = "binomial")
sm2 <- glm(psurvival ~ log(ros.areaTminus1) + year, data = primula, family = "binomial")
sm3 <- glm(psurvival ~ log(ros.areaTminus1)*year, data = primula, family = "binomial")

lrtest(sm1, sm2)
lrtest(sm1, sm3)
# NO

### Does survival differ between treatments?

sm1 <- glm(psurvival ~ log(ros.areaTminus1), data = primula, family = "binomial")
sm2 <- glm(psurvival ~ log(ros.areaTminus1) + trt, data = primula, family = "binomial")
sm3 <- glm(psurvival ~ log(ros.areaTminus1)*trt, data = primula, family = "binomial")

lrtest(sm1, sm2)
lrtest(sm1, sm3)

# NO 

# What does this tell us? The probability of flowering does not change across years, but it does change across treatment. However, treatment likely depends on year (some years are drier/wetter, so this will affect strength of treatments/how different they are?). How to test this: Make a model with both and test for a significant interaction? What does the extra water in 2021 mean for this (independent of how much it rained)

sm1 <- glm(psurvival ~ log(ros.areaTminus1) + trt, data = primula, family = "binomial")
sm2 <- glm(psurvival ~ log(ros.areaTminus1) + trt*year, data = primula, family = "binomial")
sm3 <- glm(psurvival ~ log(ros.areaTminus1)*trt + year, data = primula, family = "binomial")

lrtest(sm1, sm2)
lrtest(sm1, sm3)

# What does this tell us: no there is not a significant interaction between year and treatment (read: the influence of treatment on psurvival does not vary across years)
#############################################################################
# Does flowering in the previous year affect survival this year?

sm1 <- glm(psurvival ~ log(ros.areaTminus1), data = primula, family = "binomial")
sm2 <- glm(psurvival ~ log(ros.areaTminus1) + pflowerTminus1, data = primula, family = "binomial")

lrtest(sm1, sm2)
# there is not a survival cost of reproduction

##########################################################################
# Candidate models
sm.min <- glm(psurvival ~ log(ros.areaTminus1), data = primula, family = "binomial")
sm.min2 <- glm(psurvival ~ log(ros.areaTminus1) + trt, data = primula, family = "binomial")
sm.min3 <- glm(psurvival ~ log(ros.areaTminus1) + pflowerTminus1, data = primula, family = "binomial")
sm.min4 <- glm(psurvival ~ log(ros.areaTminus1) + trt + pflowerTminus1, data = primula, family = "binomial")
sm.min5 <- glm(psurvival ~ log(ros.areaTminus1) + trt*pflowerTminus1, data = primula, family = "binomial")
########################################################################################
sm1 <- glm(psurvival ~ log(ros.areaTminus1) + trt + grow.season.tot.precip, data = primula, family = "binomial")
sm2 <- glm(psurvival ~ log(ros.areaTminus1) + trt*grow.season.tot.precip + pflowerTminus1, data = primula, family = "binomial")
sm3 <- glm(psurvival ~ log(ros.areaTminus1) + grow.season.tot.precip, data = primula, family = "binomial")
sm4 <- glm(psurvival ~ log(ros.areaTminus1) + grow.season.tot.precip + pflowerTminus1, data = primula, family = "binomial")
#
sm5 <- glm(psurvival ~ log(ros.areaTminus1) + trt + grow.season.mean.min.temp, data = primula, family = "binomial")
sm6 <- glm(psurvival ~ log(ros.areaTminus1) + trt*grow.season.mean.min.temp, data = primula, family = "binomial")
sm7 <- glm(psurvival ~ log(ros.areaTminus1) + grow.season.mean.min.temp, data = primula, family = "binomial")
#
sm8 <- glm(psurvival ~ log(ros.areaTminus1) + trt + grow.season.mean.max.temp, data = primula, family = "binomial")
sm9 <- glm(psurvival ~ log(ros.areaTminus1) + trt*grow.season.mean.max.temp, data = primula, family = "binomial")
sm10 <- glm(psurvival ~ log(ros.areaTminus1) + grow.season.mean.max.temp, data = primula, family = "binomial")
#
sm11 <- glm(psurvival ~ log(ros.areaTminus1) + trt + winter.mean.min.temp, data = primula, family = "binomial")
sm12 <- glm(psurvival ~ log(ros.areaTminus1) + trt*winter.mean.min.temp, data = primula, family = "binomial")
sm13 <- glm(psurvival ~ log(ros.areaTminus1) + winter.mean.min.temp, data = primula, family = "binomial")
#
sm14 <- glm(psurvival ~ log(ros.areaTminus1) + trt + winter.tot.precip, data = primula, family = "binomial")
sm15 <- glm(psurvival ~ log(ros.areaTminus1) + trt*winter.tot.precip, data = primula, family = "binomial")
sm16 <- glm(psurvival ~ log(ros.areaTminus1) + winter.tot.precip, data = primula, family = "binomial")
#
sm17 <- glm(psurvival ~ log(ros.areaTminus1) + trt + winter.mean.max.temp, data = primula, family = "binomial")
sm18 <- glm(psurvival ~ log(ros.areaTminus1) + trt*winter.mean.max.temp, data = primula, family = "binomial")
sm19 <- glm(psurvival ~ log(ros.areaTminus1) + winter.mean.max.temp, data = primula, family = "binomial")
#
sm20 <- glm(psurvival ~ log(ros.areaTminus1) + trt + summer.mean.max.temp, data = primula, family = "binomial")
sm21 <- glm(psurvival ~ log(ros.areaTminus1) + trt*summer.mean.max.temp, data = primula, family = "binomial")
sm22 <- glm(psurvival ~ log(ros.areaTminus1) + summer.mean.max.temp, data = primula, family = "binomial")
#
sm23 <- glm(psurvival ~ log(ros.areaTminus1) + trt + summer.mean.min.temp, data = primula, family = "binomial")
sm24 <- glm(psurvival ~ log(ros.areaTminus1) + trt*summer.mean.min.temp, data = primula, family = "binomial")
sm25 <- glm(psurvival ~ log(ros.areaTminus1) + summer.mean.min.temp, data = primula)
#
sm26 <- glm(psurvival ~ log(ros.areaTminus1) + trt + summer.tot.precip, data = primula)
sm27 <- glm(psurvival ~ log(ros.areaTminus1) + trt*summer.tot.precip, data = primula)
sm28 <- glm(psurvival ~ log(ros.areaTminus1) + summer.tot.precip, data = primula, family = "binomial")
##
psurvival <- lst(sm1, sm2, sm3, sm4, sm5, sm6, sm7, sm8, sm9, sm10, sm.min, sm.min2, sm.min3, sm.min4, sm.min5, sm11, sm12, sm13, sm14, sm15, sm16, sm17, sm18, sm19, sm20, sm21, sm22, sm23, sm24, sm25, sm26, sm27, sm28)

survival_AICc<-aictab(cand.set = psurvival, modnames = NULL,second.ord=TRUE,nobs=NULL,sort=TRUE)

primula %>% ggplot(aes(log(ros.areaTminus1), psurvival)) +
  geom_jitter(height = .02)+
  theme_classic()+
  geom_smooth(method = glm, method.args= list(family="binomial"), se=FALSE, fullrange = TRUE)+
  expand_limits(x = 0, y = 0)+
  facet_wrap(~year)
# what is happening to survival in 2018??

# 12/2 leaving off: Seems like mean max temp matters alot for flowering and growth, mean min temp matters for survival. Seeing reproductive and growth CoR. Not quite sure how to handle year vs treatment effects



## From Jenn's paper
# We then tested for correlations between year random effects and each of the climate variables: mean daily precipitation, mean daily temperature, and proportion wet days across each season (spring, summer, fall, winter) and across one transition year (May 16, year t – May 15 year t+1). 


