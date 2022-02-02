## Primula vital rates with climate data

library(tidyverse)
library(AICcmodavg)
library(lme4)
library(lmtest)
library(ggeffects)
library("ggiraphExtra")
library(effects)
library(wesanderson)
FF <- wes_palettes$FantasticFox1
Z <- wes_palettes$Zissou1
R <- wes_palettes$Royal2

# Problem with model fit for poisson models
# make growth graph the other way (lines for trt, graphs as climate)
# graphs are hard to make!

# Read in data ------------------------------------------------------------------------

### read in all years of Primula data and weather data from the Cowichan
# Add in climate data:
source("./scripts/Cowichan_climatevalues.R")

# Add in demography data:
source("./scripts/dode_allyears_cleaned.R")

## Join climate data to demography data by year
primula <- left_join(Dodecatheon, climate, by = "year")

remove(climate, Dodecatheon)
theme_set(theme_classic()) #for ggplot
## Now to build the models. 

# Growth Vital Rates ------------------------------------------------------
#####################################################################################################
primula %>% ggplot(aes(x = log(ros.areaTminus1), y = log(ros.area), color = trt)) +
  geom_point() +
  geom_smooth(method = "lm")
boxplot(log(ros.area) ~ trt, data = primula)

##########################################################################################################
### Do vital rates differ between years? 
# Not sure if these are of any use? skip down to next section for model selection
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
gm3 <- aov(log(ros.area) ~ trt, data = primula)
summary(gm3)
anova(gm3)
TukeyHSD(gm3)
plot(TukeyHSD(gm3))
#yes, size differs between treatments, on average plants are biggest in irrigated, smallest in drought
# > TukeyHSD(gm3)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = log(ros.area) ~ trt, data = primula)
# 
# $trt
# diff         lwr        upr     p adj
# drought-control   -0.2449422 -0.34165828 -0.1482260 0.0000000
# irrigated-control  0.1669537  0.06909098  0.2648165 0.0001917
# irrigated-drought  0.4118959  0.31133634  0.5124554 0.0000000

# YES 

# What does this tell us? Growth does change across years, and it does change across treatment. However, treatment likely depends on year (some years are drier/wetter, so this will affect strength of treatments/how different they are?). How to test this: Make a model with both and test for a significant interaction? What does the extra water in 2021 mean for this (independent of how much it rained)

gm1 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt, data = primula)
gm2 <- lm(log(ros.area) ~ log(ros.areaTminus1) + trt*year, data = primula)
gm3 <- lm(log(ros.area) ~ log(ros.areaTminus1)*trt + year, data = primula)

lrtest(gm1, gm2)
lrtest(gm1, gm3)
lrtest(gm2, gm3)

# What does this tell us: yes there is a significant interaction between year and treatment (read: the influence of treatment on growth varies across years)
#############################################################################
# Does flowering in the previous year affect size this year?

gm1 <- lm(log(ros.area) ~ log(ros.areaTminus1), data = primula)
gm2 <- lm(log(ros.area) ~ log(ros.areaTminus1) + pflowerTminus1, data = primula)

lrtest(gm1, gm2)
# size and reproducing last year affects size this year) - but this seems backwards - plants that flower are bigger the next year

##########################################################################
# Candidate models
##
gm.min <- lmer(log.ros.areaT1 ~ log.ros.area + trt + (1|plot) + (1|year), data = primula, REML = F)
gm.min2 <- lmer(log.ros.areaT1 ~ log.ros.area*trt + (1|plot) + (1|year), data = primula, REML = F)
gm.min3 <- lmer(log.ros.areaT1 ~ log.ros.area + (1|plot) + (1|year), data = primula, REML = F)
########################################################################################
##grow season total precip
gm1 <- lmer(log.ros.areaT1 ~ log.ros.area + trt + grow.season.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm2 <- lmer(log.ros.areaT1 ~ log.ros.area + trt*grow.season.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm3 <- lmer(log.ros.areaT1 ~ log.ros.area*trt+ grow.season.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm4 <- lmer(log.ros.areaT1 ~ log.ros.area + grow.season.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm5 <- lmer(log.ros.areaT1 ~ log.ros.area*grow.season.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm6 <- lmer(log.ros.areaT1 ~ log.ros.area*trt*grow.season.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm.1 <- lmer(log.ros.areaT1 ~ log.ros.area*trt+ trt*grow.season.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm.2 <- lmer(log.ros.areaT1 ~ log.ros.area*grow.season.tot.precip + trt + (1|plot) + (1|year), data = primula, REML = F) #forgot this interaction!
###last years grow season precip
gm1lag <- lmer(log.ros.areaT1 ~ log.ros.area + trt + grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm1.1lag <- lmer(log.ros.areaT1 ~ log.ros.area*grow.season.precip.1yearlag + trt +  + (1|plot) + (1|year), data = primula, REML = F)
gm2lag <- lmer(log.ros.areaT1 ~ log.ros.area + trt*grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm3lag <- lmer(log.ros.areaT1 ~ log.ros.area*trt + grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm4lag <- lmer(log.ros.areaT1 ~ log.ros.area + grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm5lag <- lmer(log.ros.areaT1 ~ log.ros.area*grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm6lag <- lmer(log.ros.areaT1 ~ log.ros.area*trt*grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm.3 <- lmer(log.ros.areaT1 ~ log.ros.area*trt+ trt*grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm.4 <- lmer(log.ros.areaT1 ~ log.ros.area*grow.season.precip.1yearlag + trt + (1|plot) + (1|year), data = primula, REML = F) 
#grow season min temp
gm7 <- lmer(log.ros.areaT1 ~ log.ros.area + trt + grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm8 <- lmer(log.ros.areaT1 ~ log.ros.area*trt + grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm9 <- lmer(log.ros.areaT1 ~ log.ros.area + grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm10 <- lmer(log.ros.areaT1 ~ log.ros.area*grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm11 <- lmer(log.ros.areaT1 ~ log.ros.area + trt*grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm12 <- lmer(log.ros.areaT1 ~ log.ros.area*trt*grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm.5 <- lmer(log.ros.areaT1 ~ log.ros.area*trt+ trt*grow.season.mean.min.temp + (1|plot) + (1|year), data = primula, REML = F)
gm.6 <- lmer(log.ros.areaT1 ~ log.ros.area*grow.season.mean.min.temp + trt + (1|plot) + (1|year), data = primula, REML = F) 
# last years grow season min temp
gm7lag <- lmer(log.ros.areaT1 ~ log.ros.area + trt + grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm8lag <- lmer(log.ros.areaT1 ~ log.ros.area*trt + grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm9lag <- lmer(log.ros.areaT1 ~ log.ros.area + grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm10lag <- lmer(log.ros.areaT1 ~ log.ros.area*grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm11lag <- lmer(log.ros.areaT1 ~ log.ros.area + trt*grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm12lag <- lmer(log.ros.areaT1 ~ log.ros.area*trt*grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm.7 <- lmer(log.ros.areaT1 ~ log.ros.area*trt+ trt*grow.season.min.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm.8 <- lmer(log.ros.areaT1 ~ log.ros.area*grow.season.min.temp.1yearlag + trt + (1|plot) + (1|year), data = primula, REML = F) 
# grow season max temp
gm13 <- lmer(log.ros.areaT1 ~ log.ros.area + trt + grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm14 <- lmer(log.ros.areaT1 ~ log.ros.area*trt + grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm15 <- lmer(log.ros.areaT1 ~ log.ros.area + grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm16 <- lmer(log.ros.areaT1 ~ log.ros.area*grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm17 <- lmer(log.ros.areaT1 ~ log.ros.area + trt*grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, REML = F) ###
gm18 <- lmer(log.ros.areaT1 ~ log.ros.area*trt*grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm.9 <- lmer(log.ros.areaT1 ~ log.ros.area*trt+ trt*grow.season.mean.max.temp + (1|plot) + (1|year), data = primula, REML = F)
gm.10 <- lmer(log.ros.areaT1 ~ log.ros.area*grow.season.mean.max.temp + trt + (1|plot) + (1|year), data = primula, REML = F) 
# last year grow seasons max temp
gm13lag <- lmer(log.ros.areaT1 ~ log.ros.area + trt + grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm14lag <- lmer(log.ros.areaT1 ~ log.ros.area*trt + grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm15lag <- lmer(log.ros.areaT1 ~ log.ros.area + grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm16lag <- lmer(log.ros.areaT1 ~ log.ros.area*grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm17lag <- lmer(log.ros.areaT1 ~ log.ros.area + trt*grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm18lag <- lmer(log.ros.areaT1 ~ log.ros.area*trt*grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm.11 <- lmer(log.ros.areaT1 ~ log.ros.area*trt+ trt*grow.season.max.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm.12 <- lmer(log.ros.areaT1 ~ log.ros.area*grow.season.max.temp.1yearlag + trt + (1|plot) + (1|year), data = primula, REML = F) 
## summer max temp
gm20 <- lmer(log.ros.areaT1 ~ log.ros.area + trt + summer.mean.max.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm21 <- lmer(log.ros.areaT1 ~ log.ros.area + trt*summer.mean.max.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm22 <- lmer(log.ros.areaT1 ~ log.ros.area + summer.mean.max.temp + (1|plot) + (1|year), data = primula, REML = F)
gm23 <- lmer(log.ros.areaT1 ~ log.ros.area*trt + summer.mean.max.temp + (1|plot) + (1|year), data = primula, REML = F)
gm24 <- lmer(log.ros.areaT1 ~ log.ros.area*summer.mean.max.temp + (1|plot) + (1|year), data = primula, REML = F)
gm19 <- lmer(log.ros.areaT1 ~ log.ros.area*trt*summer.mean.max.temp + (1|plot) + (1|year), data = primula, REML = F)
gm.13 <- lmer(log.ros.areaT1 ~ log.ros.area*trt+ trt*summer.mean.max.temp + (1|plot) + (1|year), data = primula, REML = F)
gm.14 <- lmer(log.ros.areaT1 ~ log.ros.area*summer.mean.max.temp + trt + (1|plot) + (1|year), data = primula, REML = F) 
# last year's summer max temp
gm19lag <- lmer(log.ros.areaT1 ~ log.ros.area + trt + summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm20lag <- lmer(log.ros.areaT1 ~ log.ros.area + trt*summer.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm21lag <- lmer(log.ros.areaT1 ~ log.ros.area + summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm22lag <- lmer(log.ros.areaT1 ~ log.ros.area*trt + summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm23lag <- lmer(log.ros.areaT1 ~ log.ros.area*summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm24lag <- lmer(log.ros.areaT1 ~ log.ros.area*trt*summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm.15 <- lmer(log.ros.areaT1 ~ log.ros.area*trt+ trt*summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm.16 <- lmer(log.ros.areaT1 ~ log.ros.area*summer.max.temp.1yearlag + trt + (1|plot) + (1|year), data = primula, REML = F) 
# 2 years ago summer max temp
gm25lag <- lmer(log.ros.areaT1 ~ log.ros.area + trt + summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm26lag <- lmer(log.ros.areaT1 ~ log.ros.area + trt*summer.max.temp.2yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm27lag <- lmer(log.ros.areaT1 ~ log.ros.area + summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm28lag <- lmer(log.ros.areaT1 ~ log.ros.area*trt + summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm29lag <- lmer(log.ros.areaT1 ~ log.ros.area*summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm30lag <- lmer(log.ros.areaT1 ~ log.ros.area*trt*summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm.17 <- lmer(log.ros.areaT1 ~ log.ros.area*trt+ trt*summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm.18 <- lmer(log.ros.areaT1 ~ log.ros.area*summer.max.temp.2yearlag + trt + (1|plot) + (1|year), data = primula, REML = F) 
# summer min temp
gm25 <- lmer(log.ros.areaT1 ~ log.ros.area + trt + summer.mean.min.temp + (1|plot) + (1|year), data = primula, REML = F)
gm26 <- lmer(log.ros.areaT1 ~ log.ros.area + trt*summer.mean.min.temp + (1|plot) + (1|year), data = primula, REML = F)
gm27 <- lmer(log.ros.areaT1 ~ log.ros.area + summer.mean.min.temp + (1|plot) + (1|year), data = primula, REML = F)
gm28 <- lmer(log.ros.areaT1 ~ log.ros.area*trt + summer.mean.min.temp + (1|plot) + (1|year), data = primula, REML = F)
gm29 <- lmer(log.ros.areaT1 ~ log.ros.area*summer.mean.min.temp + (1|plot) + (1|year), data = primula, REML = F)
gm30 <- lmer(log.ros.areaT1 ~ log.ros.area*trt*summer.mean.min.temp + (1|plot) + (1|year), data = primula, REML = F)
gm.19 <- lmer(log.ros.areaT1 ~ log.ros.area*trt+ trt*summer.mean.min.temp + (1|plot) + (1|year), data = primula, REML = F)
gm.20 <- lmer(log.ros.areaT1 ~ log.ros.area*summer.mean.min.temp + trt + (1|plot) + (1|year), data = primula, REML = F) 
# summer min temp lagged
gm31lag <- lmer(log.ros.areaT1 ~ log.ros.area + trt + summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm32lag <- lmer(log.ros.areaT1 ~ log.ros.area + trt*summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm33lag <- lmer(log.ros.areaT1 ~ log.ros.area + summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm34lag <- lmer(log.ros.areaT1 ~ log.ros.area*trt + summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm35lag <- lmer(log.ros.areaT1 ~ log.ros.area*summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm36lag <- lmer(log.ros.areaT1 ~ log.ros.area*trt*summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm.21 <- lmer(log.ros.areaT1 ~ log.ros.area*trt+ trt*summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm.22 <- lmer(log.ros.areaT1 ~ log.ros.area*summer.min.temp.1yearlag + trt + (1|plot) + (1|year), data = primula, REML = F) 
# summer precip
gm31 <- lmer(log.ros.areaT1 ~ log.ros.area + trt + summer.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm32 <- lmer(log.ros.areaT1 ~ log.ros.area + trt*summer.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm33 <- lmer(log.ros.areaT1 ~ log.ros.area + summer.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm34 <- lmer(log.ros.areaT1 ~ log.ros.area*trt + summer.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm35 <- lmer(log.ros.areaT1 ~ log.ros.area*summer.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm36 <- lmer(log.ros.areaT1 ~ log.ros.area*trt*summer.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm.23 <- lmer(log.ros.areaT1 ~ log.ros.area*trt+ trt*summer.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm.24 <- lmer(log.ros.areaT1 ~ log.ros.area*summer.tot.precip + trt + (1|plot) + (1|year), data = primula, REML = F) 
#summer precip lag
gm37lag <- lmer(log.ros.areaT1 ~ log.ros.area + trt + summer.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm38lag <- lmer(log.ros.areaT1 ~ log.ros.area + trt*summer.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm39lag <- lmer(log.ros.areaT1 ~ log.ros.area + summer.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm40lag <- lmer(log.ros.areaT1 ~ log.ros.area*trt + summer.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm41lag <- lmer(log.ros.areaT1 ~ log.ros.area*summer.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm42lag <- lmer(log.ros.areaT1 ~ log.ros.area*trt*summer.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm.25 <- lmer(log.ros.areaT1 ~ log.ros.area*trt+ trt*summer.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm.26 <- lmer(log.ros.areaT1 ~ log.ros.area*summer.precip.1yearlag + trt + (1|plot) + (1|year), data = primula, REML = F) 
# current year winter:
gm37 <- lmer(log.ros.areaT1 ~ log.ros.area + trt + winter.mean.min.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm38 <- lmer(log.ros.areaT1 ~ log.ros.area + trt*winter.mean.min.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm39 <- lmer(log.ros.areaT1 ~ log.ros.area + winter.mean.min.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm40 <- lmer(log.ros.areaT1 ~ log.ros.area*trt + winter.mean.min.temp + (1|plot) + (1|year), data = primula, REML = F)
gm41 <- lmer(log.ros.areaT1 ~ log.ros.area*winter.mean.min.temp + (1|plot) + (1|year), data = primula, REML = F)
gm42 <- lmer(log.ros.areaT1 ~ log.ros.area*trt*winter.mean.min.temp + (1|plot) + (1|year), data = primula, REML = F)
gm43 <- lmer(log.ros.areaT1 ~ log.ros.area*trt+ trt*winter.mean.min.temp + (1|plot) + (1|year), data = primula, REML = F)
gm44 <- lmer(log.ros.areaT1 ~ log.ros.area*winter.mean.min.temp + trt + (1|plot) + (1|year), data = primula, REML = F) 
#
gm45 <- lmer(log.ros.areaT1 ~ log.ros.area + trt + winter.tot.precip+ (1|plot) + (1|year), data = primula, REML = F)
gm46 <- lmer(log.ros.areaT1 ~ log.ros.area + trt*winter.tot.precip+ (1|plot) + (1|year), data = primula, REML = F)
gm47 <- lmer(log.ros.areaT1 ~ log.ros.area + winter.tot.precip+ (1|plot) + (1|year), data = primula, REML = F)
gm48 <- lmer(log.ros.areaT1 ~ log.ros.area*trt + winter.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm49 <- lmer(log.ros.areaT1 ~ log.ros.area*winter.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm50 <- lmer(log.ros.areaT1 ~ log.ros.area*trt*winter.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm51 <- lmer(log.ros.areaT1 ~ log.ros.area*trt+ trt*winter.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm52 <- lmer(log.ros.areaT1 ~ log.ros.area*winter.tot.precip + trt + (1|plot) + (1|year), data = primula, REML = F) 
#
gm53 <- lmer(log.ros.areaT1 ~ log.ros.area + trt + winter.mean.max.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm54 <- lmer(log.ros.areaT1 ~ log.ros.area + trt*winter.mean.max.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm55 <- lmer(log.ros.areaT1 ~ log.ros.area + winter.mean.max.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm56 <- lmer(log.ros.areaT1 ~ log.ros.area*trt + winter.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm57 <- lmer(log.ros.areaT1 ~ log.ros.area*winter.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm58 <- lmer(log.ros.areaT1 ~ log.ros.area*trt*winter.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm59 <- lmer(log.ros.areaT1 ~ log.ros.area*trt+ trt*winter.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm60 <- lmer(log.ros.areaT1 ~ log.ros.area*winter.tot.precip + trt + (1|plot) + (1|year), data = primula, REML = F) 
#lagged winter:
#
gm61 <- lmer(log.ros.areaT1 ~ log.ros.area + trt + winter.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm62 <- lmer(log.ros.areaT1 ~ log.ros.area + trt*winter.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm63 <- lmer(log.ros.areaT1 ~ log.ros.area + winter.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm64 <- lmer(log.ros.areaT1 ~ log.ros.area*trt + winter.min.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm65 <- lmer(log.ros.areaT1 ~ log.ros.area*winter.min.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm66 <- lmer(log.ros.areaT1 ~ log.ros.area*trt*winter.min.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm67 <- lmer(log.ros.areaT1 ~ log.ros.area*trt+ trt*winter.min.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm68 <- lmer(log.ros.areaT1 ~ log.ros.area*winter.min.temp.1yearlag + trt + (1|plot) + (1|year), data = primula, REML = F) 
#
gm69 <- lmer(log.ros.areaT1 ~ log.ros.area + trt + winter.precip.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm70 <- lmer(log.ros.areaT1 ~ log.ros.area + trt*winter.precip.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm71 <- lmer(log.ros.areaT1 ~ log.ros.area + winter.precip.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm72 <- lmer(log.ros.areaT1 ~ log.ros.area*trt + winter.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm73 <- lmer(log.ros.areaT1 ~ log.ros.area*winter.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm74 <- lmer(log.ros.areaT1 ~ log.ros.area*trt*winter.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm75 <- lmer(log.ros.areaT1 ~ log.ros.area*trt+ trt*winter.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm76 <- lmer(log.ros.areaT1 ~ log.ros.area*winter.precip.1yearlag + trt + (1|plot) + (1|year), data = primula, REML = F) 
#
gm77 <- lmer(log.ros.areaT1 ~ log.ros.area + trt + winter.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm78 <- lmer(log.ros.areaT1 ~ log.ros.area + trt*winter.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm79 <- lmer(log.ros.areaT1 ~ log.ros.area + winter.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm80 <- lmer(log.ros.areaT1 ~ log.ros.area*trt + winter.max.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm81 <- lmer(log.ros.areaT1 ~ log.ros.area*winter.max.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm82 <- lmer(log.ros.areaT1 ~ log.ros.area*trt*winter.max.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm83 <- lmer(log.ros.areaT1 ~ log.ros.area*trt+ trt*winter.max.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm84 <- lmer(log.ros.areaT1 ~ log.ros.area*winter.max.temp.1yearlag + trt + (1|plot) + (1|year), data = primula, REML = F) 


###
#growth <- lst(gm.min, gm.min2, gm.min3, gm1, gm2, gm3, gm4, gm5, gm6, gm7, gm8, gm9, gm10, gm11, gm12, gm13, gm14, gm15, gm16, gm17, gm18, gm19, gm20, gm21, gm22, gm23, gm24, gm25, gm26, gm27, gm28, gm29, gm30, gm31, gm32, gm33, gm34, gm35, gm36, gm37, gm38, gm39, gm40, gm41, gm42, gm43, gm44, gm45, gm1lag, gm2lag, gm3lag, gm4lag, gm5lag, gm6lag, gm7lag, gm8lag, gm8lag, gm9lag, gm10lag, gm11lag, gm12lag, gm13lag, gm14lag, gm15lag, gm16lag, gm17lag, gm18lag, gm19lag, gm20lag, gm21lag, gm22lag, gm23lag, gm24lag, gm25lag, gm26lag, gm27lag, gm28lag, gm29lag, gm30lag, gm31lag, gm32lag, gm33lag, gm34lag, gm35lag, gm36lag, gm37lag, gm38lag, gm39lag, gm40lag, gm41lag, gm42lag, gm1.1, gm1.1lag, gm3.5)
growth <- mget(ls(pattern = "^gm")) #make a list of all of those models that start with gm (^ means start with)
growth_AICc<-aictab(cand.set = growth, modnames = NULL,second.ord=TRUE,nobs=NULL,sort=TRUE)

# primula %>% ggplot(aes(log.ros.area, log.ros.areaT1)) +
#   geom_jitter(height = .02)+
#   theme_classic()+
#   geom_smooth(method = lmer, se=FALSE, fullrange = TRUE)+
#   expand_limits(x = 0, y = 0)+
#   facet_wrap(~year)

#gm.9 and gm17 are the best and basically the same ( delta AIC is < 2)
summary(gm.9)
summary(gm17)
 #since gm17 has fewer terms, for now I'll go with gm17 because it is the simpler model

###########################################################################################################################
# Here's what works for predicting and graphing. It's not great (especially where I've put the quartiles in by hand, but don't know a better way!)
# control plots
pred.cont.25.dat <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), grow.season.mean.max.temp = 16.68, trt = "control") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
pred.cont.25 <- predict(gm17, newdata = pred.cont.25.dat, re.form=~0) #re.form = ~0 tells it to not include random effects
pred.cont.25 <- as.data.frame(pred.cont.25)
pred.cont.25 <- cbind(pred.cont.25, pred.cont.25.dat)
pred.cont.75.dat <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), grow.season.mean.max.temp = 18.55, trt = "control") #this is making a set of data that the model will predict points for. size 0:6, 75th quarile of summer precip, and control plots!
pred.cont.75 <- predict(gm17, newdata = pred.cont.75.dat, re.form=~0)
pred.cont.75 <- as.data.frame(pred.cont.75)
pred.cont.75 <- cbind(pred.cont.75, pred.cont.75.dat)
pred.cont.av.dat <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), grow.season.mean.max.temp = 17.47, trt = "control") #this is making a set of data that the model will predict points for. size 0:6, av of summer precip, and control plots!
pred.cont.av <- predict(gm17, newdata = pred.cont.av.dat, re.form=~0)
pred.cont.av <- as.data.frame(pred.cont.av)
pred.cont.av <- cbind(pred.cont.av, pred.cont.av.dat)
#irrigated plots
pred.irr.25.dat <-  expand.grid(log.ros.area = seq(0.6,5.6, by = .1), grow.season.mean.max.temp = 16.68, trt = "irrigated") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
pred.irr.25 <- predict(gm17, newdata = pred.irr.25.dat, re.form=~0)
pred.irr.25 <- as.data.frame(pred.irr.25)
pred.irr.25 <- cbind(pred.irr.25, pred.irr.25.dat)
pred.irr.75.dat <-  expand.grid(log.ros.area = seq(0.6,5.6, by = .1), grow.season.mean.max.temp = 18.55, trt = "irrigated") #this is making a set of data that the model will predict points for. size 0:6, 75th quarile of summer precip, and control plots!
pred.irr.75 <- predict(gm17, newdata = pred.irr.75.dat, re.form=~0)
pred.irr.75 <- as.data.frame(pred.irr.75)
pred.irr.75 <- cbind(pred.irr.75, pred.irr.75.dat)
pred.irr.av.dat <-  expand.grid(log.ros.area = seq(0.6,5.6, by = .1), grow.season.mean.max.temp = 17.47, trt = "irrigated") #this is making a set of data that the model will predict points for. size 0:6, av of summer precip, and control plots!
pred.irr.av <- predict(gm17, newdata = pred.irr.av.dat, re.form=~0)
pred.irr.av <- as.data.frame(pred.irr.av)
pred.irr.av <- cbind(pred.irr.av, pred.irr.av.dat)
#drought plots
pred.drt.25.dat <-  expand.grid(log.ros.area = seq(0.8,5.2, by = .1), grow.season.mean.max.temp = 16.68, trt = "drought") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
pred.drt.25 <- predict(gm17, newdata = pred.drt.25.dat, re.form=~0)
pred.drt.25 <- as.data.frame(pred.drt.25)
pred.drt.25 <- cbind(pred.drt.25, pred.drt.25.dat)
pred.drt.75.dat <-  expand.grid(log.ros.area = seq(0.8,5.2, by = .1), grow.season.mean.max.temp = 18.55, trt = "drought") #this is making a set of data that the model will predict points for. size 0:6, 75th quarile of summer precip, and control plots!
pred.drt.75 <- predict(gm17, newdata = pred.drt.75.dat, re.form=~0)
pred.drt.75 <- as.data.frame(pred.drt.75)
pred.drt.75 <- cbind(pred.drt.75, pred.drt.75.dat)
pred.drt.av.dat <-  expand.grid(log.ros.area = seq(0.8,5.2, by = .1), grow.season.mean.max.temp = 17.47, trt = "drought") #this is making a set of data that the model will predict points for. size 0:6, av of summer precip, and control plots!
pred.drt.av <- predict(gm17, newdata = pred.drt.av.dat, re.form=~0)
pred.drt.av <- as.data.frame(pred.drt.av)
pred.drt.av <- cbind(pred.drt.av, pred.drt.av.dat)


ggplot(subset(primula, trt == "control"), aes(x=log.ros.area, y=log.ros.areaT1))+
  geom_point(color = R[5], size = 2) +
  geom_line(data = pred.cont.25, aes(x = log.ros.area, y = pred.cont.25), color = FF[3], linetype = 2, size = 1) + #25th perc.gs max temp
  geom_line(data = pred.cont.75, aes(x = log.ros.area, y = pred.cont.75), color = FF[5], linetype = 2, size = 1) +  #75th perc. gs max temp
  geom_line(data = pred.cont.av, aes(x = log.ros.area, y = pred.cont.av), color = "black", size = 2) + #av  gs max temp
   labs(title = "Control")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 6))+
  geom_abline(slope=1, intercept = 0, linetype = 2, color= "grey", size = 1)
   
primula %>% 
  filter(trt == "irrigated") %>% 
  ggplot(aes(x=log.ros.area, y=log.ros.areaT1))+
     geom_point(color = Z[2], size = 2) +
     geom_line(data = pred.irr.25, aes(x = log.ros.area, y = pred.irr.25), color = FF[3], linetype = 2, size = 1) + #25th perc. gs max temp
     geom_line(data = pred.irr.75, aes(x = log.ros.area, y = pred.irr.75), color = FF[5], linetype = 2, size = 1) +  #75th perc. gs max temp
     geom_line(data = pred.irr.av, aes(x = log.ros.area, y = pred.irr.av), color = "black", size = 2) + #av gs max temp
  labs(title = "Irrigated")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 6))+
  geom_abline(slope=1, intercept = 0, linetype = 2, color= "grey", size = 1)

primula %>% 
  filter(trt == "drought") %>% 
  ggplot(aes(x=log.ros.area, y=log.ros.areaT1))+
  geom_point(color = FF[1], size = 2) +
  geom_line(data = pred.drt.25, aes(x = log.ros.area, y = pred.drt.25), color = FF[3], linetype = 2, size = 1) + #25th perc. gs max temp
  geom_line(data = pred.drt.75, aes(x = log.ros.area, y = pred.drt.75), color = FF[5], linetype = 2, size = 1) +  #75th perc. gs max temp
  geom_line(data = pred.drt.av, aes(x = log.ros.area, y = pred.drt.av), color = "black", size = 2) + #av gs max temp
  labs(title = "Drought")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 6))+
  geom_abline(slope=1, intercept = 0, linetype = 2, color= "grey", size = 1)

# all together using just average temp (no percentiles)
primula %>% 
  ggplot(aes(x=log.ros.area, y=log.ros.areaT1))+
  geom_point() +
  geom_line(data = pred.irr.av, aes(x = log.ros.area, y = pred.irr.av), color = FF[3], size = 1.5) + #irr
  geom_line(data = pred.cont.av, aes(x = log.ros.area, y = pred.cont.av), color = R[5], size = 1.5) +  #control
  geom_line(data = pred.drt.av, aes(x = log.ros.area, y = pred.drt.av), color = FF[4], size = 1.5) + #drt
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 6))+
  geom_abline(slope=1, intercept = 0, linetype = 2, color= "grey", size = 1) 
# not sure how I can get legend without making dummy graph and getting it from there...
# should these slopes be varying? 

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
# Does flowering in the previous year affect flowering this year?

fm1 <- glm(pflower ~ log(ros.areaTminus1), data = primula, family = "binomial")
fm2 <- glm(pflower ~ log(ros.areaTminus1) + pflowerTminus1, data = primula, family = "binomial")
fm3 <- glm(pflower ~ log(ros.areaTminus1)+ trt, data = primula, family = "binomial")
fm4 <- glm(pflower ~ log(ros.areaTminus1) +trt + pflowerTminus1, data = primula, family = "binomial")

lrtest(fm1, fm2, fm3, fm4)



##########################################################################
# Candidate models
fm.min <- glmer(pflowerT1 ~ log.ros.area + trt + (1|plot) + (1|year), data = primula, family = "binomial")
fm.min2 <- glmer(pflowerT1 ~ log.ros.area*trt + (1|plot) + (1|year), data = primula, family = "binomial")
fm.min3 <- glmer(pflowerT1 ~ log.ros.area + (1|plot) + (1|year), data = primula, family = "binomial")
fm.min4 <- glmer(pflowerT1 ~ log.ros.area + pflower + (1|plot) + (1|year), data = primula, family = "binomial")
fm.min5 <- glmer(pflowerT1 ~ log.ros.area + pflower + trt + (1|plot) + (1|year), data = primula, family = "binomial")
fm.min6 <- glmer(pflowerT1 ~ log.ros.area + pflower*trt + (1|plot) + (1|year), data = primula, family = "binomial")
fm.min7 <- glmer(pflowerT1 ~ log.ros.area*trt + pflower + (1|plot) + (1|year), data = primula, family = "binomial")
fm.min8 <- glmer(pflowerT1 ~ log.ros.area*pflower*trt + (1|plot) + (1|year), data = primula, family = "binomial")
fm.min9 <- glmer(pflowerT1 ~ log.ros.area*trt + pflower*trt + (1|plot) + (1|year), data = primula, family = "binomial")
fm.min10 <- glmer(pflowerT1 ~ log.ros.area*pflower + (1|plot) + (1|year), data = primula, family = "binomial")
fm.min11 <- glmer(pflowerT1 ~ log.ros.area*pflower + trt + (1|plot) + (1|year), data = primula, family = "binomial")

# if these don't include, climate, which is the best model?
pflowering <- mget(ls(pattern = "^fm")) #make a list of all of those models that start with gm (^ means start with)
pflowering<-aictab(cand.set = pflowering, modnames = NULL,second.ord=TRUE,nobs=NULL,sort=TRUE)
#if run without climate: best model is 11: log.ros.area*pflower + trt - this would make sense to me! But, adding climate does make better models, and adding pflower doesnt sig make better model
summary(fm.min11)

########################################################################################
##grow season total precip
fm1 <- glmer(pflowerT1 ~ log.ros.area + trt + grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm2 <- glmer(pflowerT1 ~ log.ros.area + trt*grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm3 <- glmer(pflowerT1 ~ log.ros.area*trt+ grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm4 <- glmer(pflowerT1 ~ log.ros.area + grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm5 <- glmer(pflowerT1 ~ log.ros.area*grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm6 <- glmer(pflowerT1 ~ log.ros.area*trt*grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm.1 <- glmer(pflowerT1 ~ log.ros.area*trt+ trt*grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm.2 <- glmer(pflowerT1 ~ log.ros.area*grow.season.tot.precip + trt + (1|plot) + (1|year), data = primula, family = "binomial") #forgot this interaction!
###last years grow season precip
fm1lag <- glmer(pflowerT1 ~ log.ros.area + trt + grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm2lag <- glmer(pflowerT1 ~ log.ros.area + trt*grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm3lag <- glmer(pflowerT1 ~ log.ros.area*trt + grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm4lag <- glmer(pflowerT1 ~ log.ros.area + grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm5lag <- glmer(pflowerT1 ~ log.ros.area*grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm6lag <- glmer(pflowerT1 ~ log.ros.area*trt*grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm.3 <- glmer(pflowerT1 ~ log.ros.area*trt+ trt*grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm.4 <- glmer(pflowerT1 ~ log.ros.area*grow.season.precip.1yearlag + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
#grow season min temp
fm7 <- glmer(pflowerT1 ~ log.ros.area + trt + grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm8 <- glmer(pflowerT1 ~ log.ros.area*trt + grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm9 <- glmer(pflowerT1 ~ log.ros.area + grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm10 <- glmer(pflowerT1 ~ log.ros.area*grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm11 <- glmer(pflowerT1 ~ log.ros.area + trt*grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm12 <- glmer(pflowerT1 ~ log.ros.area*trt*grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm.5 <- glmer(pflowerT1 ~ log.ros.area*trt+ trt*grow.season.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm.6 <- glmer(pflowerT1 ~ log.ros.area*grow.season.mean.min.temp + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
# last years grow season min temp
fm7lag <- glmer(pflowerT1 ~ log.ros.area + trt + grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm8lag <- glmer(pflowerT1 ~ log.ros.area*trt + grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm9lag <- glmer(pflowerT1 ~ log.ros.area + grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm10lag <- glmer(pflowerT1 ~ log.ros.area*grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm11lag <- glmer(pflowerT1 ~ log.ros.area + trt*grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm12lag <- glmer(pflowerT1 ~ log.ros.area*trt*grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm.7 <- glmer(pflowerT1 ~ log.ros.area*trt + trt*grow.season.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")###
fm.8 <- glmer(pflowerT1 ~ log.ros.area*grow.season.min.temp.1yearlag + trt + (1|plot) + (1|year), data = primula, family = "binomial")
fm.f1 <- glmer(pflowerT1 ~ log.ros.area + trt + grow.season.mean.max.temp + pflower + (1|plot) + (1|year), data = primula, family = "binomial")
fm.f2 <- glmer(pflowerT1 ~ log.ros.area + grow.season.min.temp.1yearlag + pflower + (1|plot) + (1|year), data = primula, family = "binomial")
fm.f3 <- glmer(pflowerT1 ~ log.ros.area + grow.season.min.temp.1yearlag + trt*pflower + (1|plot) + (1|year), data = primula, family = "binomial")
fm.f4 <- glmer(pflowerT1 ~ log.ros.area + grow.season.min.temp.1yearlag*trt + pflower + (1|plot) + (1|year), data = primula, family = "binomial")
fm.f5 <- glmer(pflowerT1 ~ log.ros.area*trt + grow.season.min.temp.1yearlag + pflower + (1|plot) + (1|year), data = primula, family = "binomial")
fm.f6 <- glmer(pflowerT1 ~ log.ros.area*trt + grow.season.min.temp.1yearlag*trt + pflower + (1|plot) + (1|year), data = primula, family = "binomial")
fm.f7 <- glmer(pflowerT1 ~ log.ros.area*pflower + trt + grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm.f8 <- glmer(pflowerT1 ~ log.ros.area*pflower+ grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")

# grow season max temp
fm13 <- glmer(pflowerT1 ~ log.ros.area + trt + grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm14 <- glmer(pflowerT1 ~ log.ros.area*trt + grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm15 <- glmer(pflowerT1 ~ log.ros.area + grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm16 <- glmer(pflowerT1 ~ log.ros.area*grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm17 <- glmer(pflowerT1 ~ log.ros.area + trt*grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm18 <- glmer(pflowerT1 ~ log.ros.area*trt*grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm.9 <- glmer(pflowerT1 ~ log.ros.area*trt+ trt*grow.season.mean.max.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm.10 <- glmer(pflowerT1 ~ log.ros.area*grow.season.mean.max.temp + trt + (1|plot) + (1|year), data = primula, family = "binomial")
fm.f9 <- glmer(pflowerT1 ~ log.ros.area + trt + grow.season.mean.max.temp + pflower + (1|plot) + (1|year), data = primula, family = "binomial")
fm.f10 <- glmer(pflowerT1 ~ log.ros.area + grow.season.mean.max.temp + pflower + (1|plot) + (1|year), data = primula, family = "binomial")
fm.f11 <- glmer(pflowerT1 ~ log.ros.area + grow.season.mean.max.temp + trt*pflower + (1|plot) + (1|year), data = primula, family = "binomial")
fm.f12 <- glmer(pflowerT1 ~ log.ros.area + grow.season.mean.max.temp*trt + pflower + (1|plot) + (1|year), data = primula, family = "binomial")
fm.f13 <- glmer(pflowerT1 ~ log.ros.area*trt + grow.season.mean.max.temp + pflower + (1|plot) + (1|year), data = primula, family = "binomial")
fm.f14 <- glmer(pflowerT1 ~ log.ros.area*trt + grow.season.mean.max.temp*trt + pflower + (1|plot) + (1|year), data = primula, family = "binomial")
fm.f15 <- glmer(pflowerT1 ~ log.ros.area*pflower + trt + grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm.f16 <- glmer(pflowerT1 ~ log.ros.area*pflower+ grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
# last year grow seasons max temp
fm13lag <- glmer(pflowerT1 ~ log.ros.area + trt + grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm14lag <- glmer(pflowerT1 ~ log.ros.area*trt + grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm15lag <- glmer(pflowerT1 ~ log.ros.area + grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm16lag <- glmer(pflowerT1 ~ log.ros.area*grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm17lag <- glmer(pflowerT1 ~ log.ros.area + trt*grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm18lag <- glmer(pflowerT1 ~ log.ros.area*trt*grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm.11 <- glmer(pflowerT1 ~ log.ros.area*trt+ trt*grow.season.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm.12 <- glmer(pflowerT1 ~ log.ros.area*grow.season.max.temp.1yearlag + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
## summer max temp
fm20 <- glmer(pflowerT1 ~ log.ros.area + trt + summer.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm21 <- glmer(pflowerT1 ~ log.ros.area + trt*summer.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm22 <- glmer(pflowerT1 ~ log.ros.area + summer.mean.max.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm23 <- glmer(pflowerT1 ~ log.ros.area*trt + summer.mean.max.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm24 <- glmer(pflowerT1 ~ log.ros.area*summer.mean.max.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm19 <- glmer(pflowerT1 ~ log.ros.area*trt*summer.mean.max.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm.13 <- glmer(pflowerT1 ~ log.ros.area*trt+ trt*summer.mean.max.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm.14 <- glmer(pflowerT1 ~ log.ros.area*summer.mean.max.temp + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
# last year's summer max temp
fm19lag <- glmer(pflowerT1 ~ log.ros.area + trt + summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm20lag <- glmer(pflowerT1 ~ log.ros.area + trt*summer.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm21lag <- glmer(pflowerT1 ~ log.ros.area + summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm22lag <- glmer(pflowerT1 ~ log.ros.area*trt + summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm23lag <- glmer(pflowerT1 ~ log.ros.area*summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm24lag <- glmer(pflowerT1 ~ log.ros.area*trt*summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm.15 <- glmer(pflowerT1 ~ log.ros.area*trt+ trt*summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm.16 <- glmer(pflowerT1 ~ log.ros.area*summer.max.temp.1yearlag + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
# 2 years ago summer max temp
fm25lag <- glmer(pflowerT1 ~ log.ros.area + trt + summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm26lag <- glmer(pflowerT1 ~ log.ros.area + trt*summer.max.temp.2yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm27lag <- glmer(pflowerT1 ~ log.ros.area + summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm28lag <- glmer(pflowerT1 ~ log.ros.area*trt + summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm29lag <- glmer(pflowerT1 ~ log.ros.area*summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm30lag <- glmer(pflowerT1 ~ log.ros.area*trt*summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm.17 <- glmer(pflowerT1 ~ log.ros.area*trt+ trt*summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm.18 <- glmer(pflowerT1 ~ log.ros.area*summer.max.temp.2yearlag + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
# summer min temp
fm25 <- glmer(pflowerT1 ~ log.ros.area + trt + summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm26 <- glmer(pflowerT1 ~ log.ros.area + trt*summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm27 <- glmer(pflowerT1 ~ log.ros.area + summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm28 <- glmer(pflowerT1 ~ log.ros.area*trt + summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm29 <- glmer(pflowerT1 ~ log.ros.area*summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm30 <- glmer(pflowerT1 ~ log.ros.area*trt*summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm.19 <- glmer(pflowerT1 ~ log.ros.area*trt+ trt*summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm.20 <- glmer(pflowerT1 ~ log.ros.area*summer.mean.min.temp + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
# summer min temp lagged
fm31lag <- glmer(pflowerT1 ~ log.ros.area + trt + summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm32lag <- glmer(pflowerT1 ~ log.ros.area + trt*summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm33lag <- glmer(pflowerT1 ~ log.ros.area + summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm34lag <- glmer(pflowerT1 ~ log.ros.area*trt + summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm35lag <- glmer(pflowerT1 ~ log.ros.area*summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm36lag <- glmer(pflowerT1 ~ log.ros.area*trt*summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm.21 <- glmer(pflowerT1 ~ log.ros.area*trt+ trt*summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm.22 <- glmer(pflowerT1 ~ log.ros.area*summer.min.temp.1yearlag + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
# summer precip
fm31 <- glmer(pflowerT1 ~ log.ros.area + trt + summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm32 <- glmer(pflowerT1 ~ log.ros.area + trt*summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm33 <- glmer(pflowerT1 ~ log.ros.area + summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm34 <- glmer(pflowerT1 ~ log.ros.area*trt + summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm35 <- glmer(pflowerT1 ~ log.ros.area*summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm36 <- glmer(pflowerT1 ~ log.ros.area*trt*summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm.23 <- glmer(pflowerT1 ~ log.ros.area*trt+ trt*summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm.24 <- glmer(pflowerT1 ~ log.ros.area*summer.tot.precip + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
#summer precip lag
fm37lag <- glmer(pflowerT1 ~ log.ros.area + trt + summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm38lag <- glmer(pflowerT1 ~ log.ros.area + trt*summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm39lag <- glmer(pflowerT1 ~ log.ros.area + summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm40lag <- glmer(pflowerT1 ~ log.ros.area*trt + summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm41lag <- glmer(pflowerT1 ~ log.ros.area*summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm42lag <- glmer(pflowerT1 ~ log.ros.area*trt*summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm.25 <- glmer(pflowerT1 ~ log.ros.area*trt+ trt*summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm.26 <- glmer(pflowerT1 ~ log.ros.area*summer.precip.1yearlag + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
# current year winter:
fm37 <- glmer(pflowerT1 ~ log.ros.area + trt + winter.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm38 <- glmer(pflowerT1 ~ log.ros.area + trt*winter.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm39 <- glmer(pflowerT1 ~ log.ros.area + winter.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm40 <- glmer(pflowerT1 ~ log.ros.area*trt + winter.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm41 <- glmer(pflowerT1 ~ log.ros.area*winter.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm42 <- glmer(pflowerT1 ~ log.ros.area*trt*winter.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm43 <- glmer(pflowerT1 ~ log.ros.area*trt+ trt*winter.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm44 <- glmer(pflowerT1 ~ log.ros.area*winter.mean.min.temp + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
#
fm45 <- glmer(pflowerT1 ~ log.ros.area + trt + winter.tot.precip+ (1|plot) + (1|year), data = primula, family = "binomial")
fm46 <- glmer(pflowerT1 ~ log.ros.area + trt*winter.tot.precip+ (1|plot) + (1|year), data = primula, family = "binomial")
fm47 <- glmer(pflowerT1 ~ log.ros.area + winter.tot.precip+ (1|plot) + (1|year), data = primula, family = "binomial")
fm48 <- glmer(pflowerT1 ~ log.ros.area*trt + winter.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm49 <- glmer(pflowerT1 ~ log.ros.area*winter.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm50 <- glmer(pflowerT1 ~ log.ros.area*trt*winter.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm51 <- glmer(pflowerT1 ~ log.ros.area*trt+ trt*winter.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm52 <- glmer(pflowerT1 ~ log.ros.area*winter.tot.precip + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
#
fm53 <- glmer(pflowerT1 ~ log.ros.area + trt + winter.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm54 <- glmer(pflowerT1 ~ log.ros.area + trt*winter.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm55 <- glmer(pflowerT1 ~ log.ros.area + winter.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm56 <- glmer(pflowerT1 ~ log.ros.area*trt + winter.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm57 <- glmer(pflowerT1 ~ log.ros.area*winter.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm58 <- glmer(pflowerT1 ~ log.ros.area*trt*winter.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm59 <- glmer(pflowerT1 ~ log.ros.area*trt+ trt*winter.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm60 <- glmer(pflowerT1 ~ log.ros.area*winter.tot.precip + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
#lagged winter:
#
fm61 <- glmer(pflowerT1 ~ log.ros.area + trt + winter.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm62 <- glmer(pflowerT1 ~ log.ros.area + trt*winter.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm63 <- glmer(pflowerT1 ~ log.ros.area + winter.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm64 <- glmer(pflowerT1 ~ log.ros.area*trt + winter.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm65 <- glmer(pflowerT1 ~ log.ros.area*winter.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm66 <- glmer(pflowerT1 ~ log.ros.area*trt*winter.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm67 <- glmer(pflowerT1 ~ log.ros.area*trt+ trt*winter.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm68 <- glmer(pflowerT1 ~ log.ros.area*winter.min.temp.1yearlag + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
#
fm69 <- glmer(pflowerT1 ~ log.ros.area + trt + winter.precip.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm70 <- glmer(pflowerT1 ~ log.ros.area + trt*winter.precip.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm71 <- glmer(pflowerT1 ~ log.ros.area + winter.precip.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm72 <- glmer(pflowerT1 ~ log.ros.area*trt + winter.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm73 <- glmer(pflowerT1 ~ log.ros.area*winter.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm74 <- glmer(pflowerT1 ~ log.ros.area*trt*winter.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm75 <- glmer(pflowerT1 ~ log.ros.area*trt+ trt*winter.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm76 <- glmer(pflowerT1 ~ log.ros.area*winter.precip.1yearlag + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
#
fm77 <- glmer(pflowerT1 ~ log.ros.area + trt + winter.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm78 <- glmer(pflowerT1 ~ log.ros.area + trt*winter.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm79 <- glmer(pflowerT1 ~ log.ros.area + winter.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm80 <- glmer(pflowerT1 ~ log.ros.area*trt + winter.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm81 <- glmer(pflowerT1 ~ log.ros.area*winter.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm82 <- glmer(pflowerT1 ~ log.ros.area*trt*winter.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm83 <- glmer(pflowerT1 ~ log.ros.area*trt+ trt*winter.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm84 <- glmer(pflowerT1 ~ log.ros.area*winter.max.temp.1yearlag + trt + (1|plot) + (1|year), data = primula, family = "binomial") 


###
#flowering1 <- lst(fm.min, fm.min2, fm.min3, fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17, fm18, fm19, fm20, fm21, fm22, fm23, fm24, fm25, fm26, fm27, fm28, fm29, fm30, fm31, fm32, fm33, fm34, fm35, fm36, fm37, fm38, fm39, fm40, gm41, fm42, fm43, fm44, fm45, fm1lag, fm2lag, fm3lag, fm4lag, fm5lag, fm6lag, fm7lag, fm8lag, fm9lag, fm10lag, fm11lag, fm12lag, fm13lag, fm14lag, fm15lag, fm16lag, fm17lag, fm18lag, fm19lag, fm20lag, fm21lag, fm22lag, fm23lag, fm24lag, fm25lag, fm26lag, fm27lag, fm28lag, fm29lag, fm30lag, fm31lag, fm32lag, fm33lag, fm34lag, fm35lag, fm36lag, fm37lag, fm38lag, fm38lag, fm39lag, fm40lag, fm41lag, fm42lag)
flowering <- mget(ls(pattern = "^fm")) #make a list of all of those models that start with gm (^ means start with)
flowering <- flowering %>%  purrr::list_modify("fm.f4" = NULL) %>%  purrr::list_modify("fm11lag" = NULL) %>%  purrr::list_modify("fm.9" = NULL) %>%  
  purrr::list_modify("fm.f14" = NULL) %>% purrr::list_modify("fm.f12" = NULL) %>% purrr::list_modify("fm.f15" = NULL)  %>% purrr::list_modify("fm17" = NULL) %>% 
  purrr::list_modify("fm18" = NULL) %>% purrr::list_modify("fm.25" = NULL) %>% purrr::list_modify("fm2" = NULL) %>% purrr::list_modify("fm30" = NULL)# these didnt converge and have low delta AIC's
flowering<-aictab(cand.set = flowering, modnames = NULL,second.ord=TRUE,nobs=NULL,sort=TRUE)

# there is a convergence problem here with a lot of the models (this table also shows models that didnt converge) ones with delta AIC <2:
# fm.f6 (delta aic = 0) and fm.7 (1.9). Very tempting to choose the pflower one
summary(fm.7)
# Need to update this with new best fit model:
# graphing: log.ros.area * trt + trt * grow.season.min.temp.1yearlag +      (1 | plot) + (1 | year)
# Heres' what works for predicting and graphing
# control plots
pred.cont.25.dat <-  expand.grid(log.ros.area = seq(.4, 6, by = .1), grow.season.min.temp.1yearlag = 5.49, trt = "control") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots! seq(0, 6, by = .1) is the same as 0:6 but smaller increments
pred.cont.25 <- predict(fm.7, newdata = pred.cont.25.dat, type="response", re.form=~0) #re.form = ~0 tells it to not include random effects
pred.cont.25 <- as.data.frame(pred.cont.25)
pred.cont.25 <- cbind(pred.cont.25, pred.cont.25.dat)
pred.cont.75.dat <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), grow.season.min.temp.1yearlag = 5.84, trt = "control") #this is making a set of data that the model will predict points for. size 0:6, 75th quarile of summer precip, and control plots!
pred.cont.75 <- predict(fm.7, newdata = pred.cont.75.dat, type="response", re.form=~0)
pred.cont.75 <- as.data.frame(pred.cont.75)
pred.cont.75 <- cbind(pred.cont.75, pred.cont.75.dat)
pred.cont.av.dat <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), grow.season.min.temp.1yearlag = 5.80, trt = "control") #this is making a set of data that the model will predict points for. size 0:6, av of summer precip, and control plots!
pred.cont.av <- predict(fm.7, newdata = pred.cont.av.dat, type="response", re.form=~0)
pred.cont.av <- as.data.frame(pred.cont.av)
pred.cont.av <- cbind(pred.cont.av, pred.cont.av.dat)
#irrigated plots
pred.irr.25.dat <-  expand.grid(log.ros.area = seq(0.6, 5.6, by = .1), grow.season.min.temp.1yearlag = 5.49, trt = "irrigated") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
pred.irr.25 <- predict(fm.7, newdata = pred.irr.25.dat, type="response", re.form=~0)
pred.irr.25 <- as.data.frame(pred.irr.25)
pred.irr.25 <- cbind(pred.irr.25, pred.irr.25.dat)
pred.irr.75.dat <-  expand.grid(log.ros.area = seq(0.6, 5.6, by = .1), grow.season.min.temp.1yearlag = 5.84, trt = "irrigated") #this is making a set of data that the model will predict points for. size 0:6, 75th quarile of summer precip, and control plots!
pred.irr.75 <- predict(fm.7, newdata = pred.irr.75.dat, type="response", re.form=~0)
pred.irr.75 <- as.data.frame(pred.irr.75)
pred.irr.75 <- cbind(pred.irr.75, pred.irr.75.dat)
pred.irr.av.dat <-  expand.grid(log.ros.area = seq(0.6, 5.6, by = .1), grow.season.min.temp.1yearlag = 5.80, trt = "irrigated") #this is making a set of data that the model will predict points for. size 0:6, av of summer precip, and control plots!
pred.irr.av <- predict(fm.7, newdata = pred.irr.av.dat, type="response", re.form=~0)
pred.irr.av <- as.data.frame(pred.irr.av)
pred.irr.av <- cbind(pred.irr.av, pred.irr.av.dat)
#drought plots
pred.drt.25.dat <-  expand.grid(log.ros.area = seq(0.4, 5.3, by = .1), grow.season.min.temp.1yearlag = 5.49, trt = "drought") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
pred.drt.25 <- predict(fm.7, newdata = pred.drt.25.dat, type="response", re.form=~0)
pred.drt.25 <- as.data.frame(pred.drt.25)
pred.drt.25 <- cbind(pred.drt.25, pred.drt.25.dat)
pred.drt.75.dat <-  expand.grid(log.ros.area = seq(0.4, 5.3, by = .1), grow.season.min.temp.1yearlag = 5.84, trt = "drought") #this is making a set of data that the model will predict points for. size 0:6, 75th quarile of summer precip, and control plots!
pred.drt.75 <- predict(fm.7, newdata = pred.drt.75.dat, type="response", re.form=~0)
pred.drt.75 <- as.data.frame(pred.drt.75)
pred.drt.75 <- cbind(pred.drt.75, pred.drt.75.dat)
pred.drt.av.dat <-  expand.grid(log.ros.area = seq(0.4, 5.3, by = .1), grow.season.min.temp.1yearlag = 5.80, trt = "drought") #this is making a set of data that the model will predict points for. size 0:6, av of summer precip, and control plots!
pred.drt.av <- predict(fm.7, newdata = pred.drt.av.dat, type="response", re.form=~0)
pred.drt.av <- as.data.frame(pred.drt.av)
pred.drt.av <- cbind(pred.drt.av, pred.drt.av.dat)

ggplot(subset(primula, trt == "control"), aes(x=log.ros.area, y=pflowerT1))+
  geom_jitter(color = R[5], height = 0.025, size = 2) +
  geom_line(data = pred.cont.25, aes(x = log.ros.area, y = pred.cont.25), color = FF[3], linetype = 2, size = 1) + #25th perc.gs max temp
  geom_line(data = pred.cont.75, aes(x = log.ros.area, y = pred.cont.75), color = FF[5], linetype = 2, size = 1) +  #75th perc. gs max temp
  geom_line(data = pred.cont.av, aes(x = log.ros.area, y = pred.cont.av), color = "black", size = 1.2) + #av  gs max temp
  labs(title = "Control")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 6))

primula %>% 
  filter(trt == "irrigated") %>% 
  ggplot(aes(x=log.ros.area, y=pflowerT1))+
  geom_jitter(color = Z[2], height = 0.025, size = 2) +
  geom_line(data = pred.irr.25, aes(x = log.ros.area, y = pred.irr.25, color = "25th Percentile"), color = FF[3], linetype = 2, size = 1) + #25th perc. gs max temp
  geom_line(data = pred.irr.75, aes(x = log.ros.area, y = pred.irr.75), color = FF[5], linetype = 2, size = 1) +  #75th perc. gs max temp
  geom_line(data = pred.irr.av, aes(x = log.ros.area, y = pred.irr.av), color = "black", size = 1.2) + #av gs max temp
  labs(title = "Irrigated")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 6)) #make the graph start in the corner

primula %>% 
  filter(trt == "drought") %>% 
  ggplot(aes(x=log.ros.area, y=pflowerT1))+
  geom_jitter(color = FF[1], height = 0.025, size = 2) +
  geom_line(data = pred.drt.25, aes(x = log.ros.area, y = pred.drt.25), color = FF[3], linetype = 2, size = 1) + #25th perc. gs max temp
  geom_line(data = pred.drt.75, aes(x = log.ros.area, y = pred.drt.75), color = FF[5], linetype = 2, size = 1) +  #75th perc. gs max temp
  geom_line(data = pred.drt.av, aes(x = log.ros.area, y = pred.drt.av), color = "black", size = 1.2) + #av gs max temp
  labs(title = "Drought")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 6))

# all together using just average temp (no percentiles)
primula %>% 
  ggplot(aes(x=log.ros.area, y=pflowerT1))+
  geom_jitter(height = 0.025, size = 2) +
  geom_line(data = pred.irr.av, aes(x = log.ros.area, y = pred.irr.av), color = FF[3], size = 1.5) + #25th perc. gs max temp
  geom_line(data = pred.cont.av, aes(x = log.ros.area, y = pred.cont.av), color = R[5], size = 1.5) +  #75th perc. gs max temp
  geom_line(data = pred.drt.av, aes(x = log.ros.area, y = pred.drt.av), color = FF[4], size = 1.5) + #av gs max temp
  labs(title = "Climate Averages")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 6))
# not sure how I can get legend without making dummy graph and getting it from there...

#########################################################################################
# Probability of surviving  -------------------------------------------------------------


### Does survival differ between years?

sm1 <- glm(psurvivalT1 ~ log.ros.area, data = primula, family = "binomial")
sm2 <- glm(psurvivalT1 ~ log.ros.area + year, data = primula, family = "binomial")
sm3 <- glm(psurvivalT1 ~ log.ros.area*year, data = primula, family = "binomial")

lrtest(sm1, sm2)
lrtest(sm1, sm3)
# yes

### Does survival differ between treatments?

sm1 <- glm(psurvivalT1 ~ log.ros.area, data = primula, family = "binomial")
sm2 <- glm(psurvivalT1 ~ log.ros.area + trt, data = primula, family = "binomial")
sm3 <- glm(psurvivalT1 ~ log.ros.area*trt, data = primula, family = "binomial")

lrtest(sm1, sm2)
lrtest(sm1, sm3)

# NO 

# What does this tell us? The probability of flowering does not change across years, but it does change across treatment. However, treatment likely depends on year (some years are drier/wetter, so this will affect strength of treatments/how different they are?). How to test this: Make a model with both and test for a significant interaction? What does the extra water in 2021 mean for this (independent of how much it rained)

sm1 <- glm(psurvivalT1 ~ log.ros.area + trt, data = primula, family = "binomial")
sm2 <- glm(psurvivalT1 ~ log.ros.area + trt*year, data = primula, family = "binomial")
sm3 <- glm(psurvivalT1 ~ log.ros.area*trt + year, data = primula, family = "binomial")

lrtest(sm1, sm2)
lrtest(sm1, sm3)

# What does this tell us: no there is not a significant interaction between year and treatment (read: the influence of treatment on psurvival does not vary across years)
#############################################################################
# Does flowering in the previous year affect survival this year?

sm1 <- glm(psurvivalT1 ~ log.ros.area, data = primula, family = "binomial")
sm2 <- glm(psurvivalT1 ~ log.ros.area + pflower, data = primula, family = "binomial")

lrtest(sm1, sm2)
# there is not a survival cost of reproduction

##########################################################################

# Candidate models
sm.min <- glmer(psurvivalT1 ~ log.ros.area + trt + (1|plot) + (1|year), data = primula, family = "binomial")
sm.min2 <- glmer(psurvivalT1 ~ log.ros.area*trt + (1|plot) + (1|year), data = primula, family = "binomial")
sm.min3 <- glmer(psurvivalT1 ~ log.ros.area + (1|plot) + (1|year), data = primula, family = "binomial")
###
##grow season total precip
sm1 <- glmer(psurvivalT1 ~ log.ros.area + trt + grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm2 <- glmer(psurvivalT1 ~ log.ros.area + trt*grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm3 <- glmer(psurvivalT1 ~ log.ros.area*trt+ grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm4 <- glmer(psurvivalT1 ~ log.ros.area + grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm5 <- glmer(psurvivalT1 ~ log.ros.area*grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm6 <- glmer(psurvivalT1 ~ log.ros.area*trt*grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm.1 <- glmer(psurvivalT1 ~ log.ros.area*trt+ trt*grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm.2 <- glmer(psurvivalT1 ~ log.ros.area*grow.season.tot.precip + trt + (1|plot) + (1|year), data = primula, family = "binomial") #forgot this interaction!
###last years grow season precip
sm1lag <- glmer(psurvivalT1 ~ log.ros.area + trt + grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm2lag <- glmer(psurvivalT1 ~ log.ros.area + trt*grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm3lag <- glmer(psurvivalT1 ~ log.ros.area*trt + grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm4lag <- glmer(psurvivalT1 ~ log.ros.area + grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm5lag <- glmer(psurvivalT1 ~ log.ros.area*grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm6lag <- glmer(psurvivalT1 ~ log.ros.area*trt*grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm.3 <- glmer(psurvivalT1 ~ log.ros.area*trt+ trt*grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm.4 <- glmer(psurvivalT1 ~ log.ros.area*grow.season.precip.1yearlag + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
#grow season min temp
sm7 <- glmer(psurvivalT1 ~ log.ros.area + trt + grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm8 <- glmer(psurvivalT1 ~ log.ros.area*trt + grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm9 <- glmer(psurvivalT1 ~ log.ros.area + grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm10 <- glmer(psurvivalT1 ~ log.ros.area*grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm11 <- glmer(psurvivalT1 ~ log.ros.area + trt*grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm12 <- glmer(psurvivalT1 ~ log.ros.area*trt*grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm.5 <- glmer(psurvivalT1 ~ log.ros.area*trt+ trt*grow.season.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm.6 <- glmer(psurvivalT1 ~ log.ros.area*grow.season.mean.min.temp + trt + (1|plot) + (1|year), data = primula, family = "binomial")
# last years grow season min temp
sm7lag <- glmer(psurvivalT1 ~ log.ros.area + trt + grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm8lag <- glmer(psurvivalT1 ~ log.ros.area*trt + grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm9lag <- glmer(psurvivalT1 ~ log.ros.area + grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm10lag <- glmer(psurvivalT1 ~ log.ros.area*grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial") ###
sm11lag <- glmer(psurvivalT1 ~ log.ros.area + trt*grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm12lag <- glmer(psurvivalT1 ~ log.ros.area*trt*grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm.f7 <- glmer(psurvivalT1 ~ log.ros.area*pflower + trt + grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm.f8 <- glmer(psurvivalT1 ~ log.ros.area*pflower+ grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
# grow season max temp
sm13 <- glmer(psurvivalT1 ~ log.ros.area + trt + grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm14 <- glmer(psurvivalT1 ~ log.ros.area*trt + grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm15 <- glmer(psurvivalT1 ~ log.ros.area + grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm16 <- glmer(psurvivalT1 ~ log.ros.area*grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm17 <- glmer(psurvivalT1 ~ log.ros.area + trt*grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm18 <- glmer(psurvivalT1 ~ log.ros.area*trt*grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm.9 <- glmer(psurvivalT1 ~ log.ros.area*trt+ trt*grow.season.mean.max.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm.10 <- glmer(psurvivalT1 ~ log.ros.area*grow.season.mean.max.temp + trt + (1|plot) + (1|year), data = primula, family = "binomial")
# last year grow seasons max temp
sm13lag <- glmer(psurvivalT1 ~ log.ros.area + trt + grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm14lag <- glmer(psurvivalT1 ~ log.ros.area*trt + grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm15lag <- glmer(psurvivalT1 ~ log.ros.area + grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm16lag <- glmer(psurvivalT1 ~ log.ros.area*grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm17lag <- glmer(psurvivalT1 ~ log.ros.area + trt*grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm18lag <- glmer(psurvivalT1 ~ log.ros.area*trt*grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm.11 <- glmer(psurvivalT1 ~ log.ros.area*trt+ trt*grow.season.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm.12 <- glmer(psurvivalT1 ~ log.ros.area*grow.season.max.temp.1yearlag + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
## summer max temp
sm20 <- glmer(psurvivalT1 ~ log.ros.area + trt + summer.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm21 <- glmer(psurvivalT1 ~ log.ros.area + trt*summer.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm22 <- glmer(psurvivalT1 ~ log.ros.area + summer.mean.max.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm23 <- glmer(psurvivalT1 ~ log.ros.area*trt + summer.mean.max.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm24 <- glmer(psurvivalT1 ~ log.ros.area*summer.mean.max.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm19 <- glmer(psurvivalT1 ~ log.ros.area*trt*summer.mean.max.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm.13 <- glmer(psurvivalT1 ~ log.ros.area*trt+ trt*summer.mean.max.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm.14 <- glmer(psurvivalT1 ~ log.ros.area*summer.mean.max.temp + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
# last year's summer max temp
sm19lag <- glmer(psurvivalT1 ~ log.ros.area + trt + summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm20lag <- glmer(psurvivalT1 ~ log.ros.area + trt*summer.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm21lag <- glmer(psurvivalT1 ~ log.ros.area + summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm22lag <- glmer(psurvivalT1 ~ log.ros.area*trt + summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm23lag <- glmer(psurvivalT1 ~ log.ros.area*summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm24lag <- glmer(psurvivalT1 ~ log.ros.area*trt*summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm.15 <- glmer(psurvivalT1 ~ log.ros.area*trt+ trt*summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm.16 <- glmer(psurvivalT1 ~ log.ros.area*summer.max.temp.1yearlag + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
# 2 years ago summer max temp
sm25lag <- glmer(psurvivalT1 ~ log.ros.area + trt + summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm26lag <- glmer(psurvivalT1 ~ log.ros.area + trt*summer.max.temp.2yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm27lag <- glmer(psurvivalT1 ~ log.ros.area + summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm28lag <- glmer(psurvivalT1 ~ log.ros.area*trt + summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm29lag <- glmer(psurvivalT1 ~ log.ros.area*summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm30lag <- glmer(psurvivalT1 ~ log.ros.area*trt*summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm.17 <- glmer(psurvivalT1 ~ log.ros.area*trt+ trt*summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm.18 <- glmer(psurvivalT1 ~ log.ros.area*summer.max.temp.2yearlag + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
# summer min temp
sm25 <- glmer(psurvivalT1 ~ log.ros.area + trt + summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm26 <- glmer(psurvivalT1 ~ log.ros.area + trt*summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm27 <- glmer(psurvivalT1 ~ log.ros.area + summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm28 <- glmer(psurvivalT1 ~ log.ros.area*trt + summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm29 <- glmer(psurvivalT1 ~ log.ros.area*summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm30 <- glmer(psurvivalT1 ~ log.ros.area*trt*summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm.19 <- glmer(psurvivalT1 ~ log.ros.area*trt+ trt*summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm.20 <- glmer(psurvivalT1 ~ log.ros.area*summer.mean.min.temp + trt + (1|plot) + (1|year), data = primula, family = "binomial")
# summer min temp lagged
sm31lag <- glmer(psurvivalT1 ~ log.ros.area + trt + summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm32lag <- glmer(psurvivalT1 ~ log.ros.area + trt*summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm33lag <- glmer(psurvivalT1 ~ log.ros.area + summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm34lag <- glmer(psurvivalT1 ~ log.ros.area*trt + summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm35lag <- glmer(psurvivalT1 ~ log.ros.area*summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm36lag <- glmer(psurvivalT1 ~ log.ros.area*trt*summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm.21 <- glmer(psurvivalT1 ~ log.ros.area*trt+ trt*summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm.22 <- glmer(psurvivalT1 ~ log.ros.area*summer.min.temp.1yearlag + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
# summer precip
sm31 <- glmer(psurvivalT1 ~ log.ros.area + trt + summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm32 <- glmer(psurvivalT1 ~ log.ros.area + trt*summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm33 <- glmer(psurvivalT1 ~ log.ros.area + summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm34 <- glmer(psurvivalT1 ~ log.ros.area*trt + summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm35 <- glmer(psurvivalT1 ~ log.ros.area*summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm36 <- glmer(psurvivalT1 ~ log.ros.area*trt*summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm.23 <- glmer(psurvivalT1 ~ log.ros.area*trt+ trt*summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm.24 <- glmer(psurvivalT1 ~ log.ros.area*summer.tot.precip + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
#summer precip lag
sm37lag <- glmer(psurvivalT1 ~ log.ros.area + trt + summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm38lag <- glmer(psurvivalT1 ~ log.ros.area + trt*summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm39lag <- glmer(psurvivalT1 ~ log.ros.area + summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm40lag <- glmer(psurvivalT1 ~ log.ros.area*trt + summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm41lag <- glmer(psurvivalT1 ~ log.ros.area*summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm42lag <- glmer(psurvivalT1 ~ log.ros.area*trt*summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm.25 <- glmer(psurvivalT1 ~ log.ros.area*trt+ trt*summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm.26 <- glmer(psurvivalT1 ~ log.ros.area*summer.precip.1yearlag + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
# current year winter:
sm37 <- glmer(psurvivalT1 ~ log.ros.area + trt + winter.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm38 <- glmer(psurvivalT1 ~ log.ros.area + trt*winter.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm39 <- glmer(psurvivalT1 ~ log.ros.area + winter.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm40 <- glmer(psurvivalT1 ~ log.ros.area*trt + winter.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm41 <- glmer(psurvivalT1 ~ log.ros.area*winter.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm42 <- glmer(psurvivalT1 ~ log.ros.area*trt*winter.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm43 <- glmer(psurvivalT1 ~ log.ros.area*trt+ trt*winter.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm44 <- glmer(psurvivalT1 ~ log.ros.area*winter.mean.min.temp + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
#
sm45 <- glmer(psurvivalT1 ~ log.ros.area + trt + winter.tot.precip+ (1|plot) + (1|year), data = primula, family = "binomial")
sm46 <- glmer(psurvivalT1 ~ log.ros.area + trt*winter.tot.precip+ (1|plot) + (1|year), data = primula, family = "binomial")
sm47 <- glmer(psurvivalT1 ~ log.ros.area + winter.tot.precip+ (1|plot) + (1|year), data = primula, family = "binomial")
sm48 <- glmer(psurvivalT1 ~ log.ros.area*trt + winter.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm49 <- glmer(psurvivalT1 ~ log.ros.area*winter.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm50 <- glmer(psurvivalT1 ~ log.ros.area*trt*winter.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm51 <- glmer(psurvivalT1 ~ log.ros.area*trt+ trt*winter.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm52 <- glmer(psurvivalT1 ~ log.ros.area*winter.tot.precip + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
#
sm53 <- glmer(psurvivalT1 ~ log.ros.area + trt + winter.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm54 <- glmer(psurvivalT1 ~ log.ros.area + trt*winter.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm55 <- glmer(psurvivalT1 ~ log.ros.area + winter.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm56 <- glmer(psurvivalT1 ~ log.ros.area*trt + winter.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm57 <- glmer(psurvivalT1 ~ log.ros.area*winter.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm58 <- glmer(psurvivalT1 ~ log.ros.area*trt*winter.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm59 <- glmer(psurvivalT1 ~ log.ros.area*trt+ trt*winter.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm60 <- glmer(psurvivalT1 ~ log.ros.area*winter.tot.precip + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
#lagged winter:
#
sm61 <- glmer(psurvivalT1 ~ log.ros.area + trt + winter.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm62 <- glmer(psurvivalT1 ~ log.ros.area + trt*winter.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm63 <- glmer(psurvivalT1 ~ log.ros.area + winter.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm64 <- glmer(psurvivalT1 ~ log.ros.area*trt + winter.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm65 <- glmer(psurvivalT1 ~ log.ros.area*winter.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm66 <- glmer(psurvivalT1 ~ log.ros.area*trt*winter.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm67 <- glmer(psurvivalT1 ~ log.ros.area*trt+ trt*winter.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm68 <- glmer(psurvivalT1 ~ log.ros.area*winter.min.temp.1yearlag + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
#
sm69 <- glmer(psurvivalT1 ~ log.ros.area + trt + winter.precip.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm70 <- glmer(psurvivalT1 ~ log.ros.area + trt*winter.precip.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm71 <- glmer(psurvivalT1 ~ log.ros.area + winter.precip.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm72 <- glmer(psurvivalT1 ~ log.ros.area*trt + winter.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm73 <- glmer(psurvivalT1 ~ log.ros.area*winter.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm74 <- glmer(psurvivalT1 ~ log.ros.area*trt*winter.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm75 <- glmer(psurvivalT1 ~ log.ros.area*trt+ trt*winter.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm76 <- glmer(psurvivalT1 ~ log.ros.area*winter.precip.1yearlag + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
#
sm77 <- glmer(psurvivalT1 ~ log.ros.area + trt + winter.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm78 <- glmer(psurvivalT1 ~ log.ros.area + trt*winter.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm79 <- glmer(psurvivalT1 ~ log.ros.area + winter.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm80 <- glmer(psurvivalT1 ~ log.ros.area*trt + winter.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm81 <- glmer(psurvivalT1 ~ log.ros.area*winter.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm82 <- glmer(psurvivalT1 ~ log.ros.area*trt*winter.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm83 <- glmer(psurvivalT1 ~ log.ros.area*trt+ trt*winter.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm84 <- glmer(psurvivalT1 ~ log.ros.area*winter.max.temp.1yearlag + trt + (1|plot) + (1|year), data = primula, family = "binomial") 
###
surv.list <- mget(ls(pattern = "^sm")) #make a list of all of those models that start with gm (^ means start with)
surv<-aictab(cand.set = surv.list, modnames = NULL,second.ord=TRUE,nobs=NULL,sort=TRUE)
#theres probably a better way, but went through and picked out the ones that converged:
short.surv <- lst(sm.min3, sm4lag, sm9, sm9lag, sm10lag,sm11lag, sm13, sm.f8, sm15, sm15lag, sm22, sm33lag, sm32, sm33, sm34, sm40lag, sm37, sm39, sm41, sm53, sm55, sm63, sm65, sm78, sm79, sm80, sm81) #ones that converged
short.surv <- aictab(cand.set = short.surv, modnames = NULL,second.ord=TRUE,nobs=NULL,sort=TRUE)
# best: sm10lag and sm65 - which to choose??
#next - make graphs for it!

# sm10lag
pred.25.dat <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), grow.season.min.temp.1yearlag = 5.49) #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip seq(0, 6, by = .1) is the same as 0:6 but smaller increments
pred.25 <- predict(sm10lag, newdata = pred.25.dat, type="response", re.form=~0) #re.form = ~0 tells it to not include random effects
pred.25 <- as.data.frame(pred.25)
pred.25 <- cbind(pred.25, pred.25.dat)
pred.75.dat <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), grow.season.min.temp.1yearlag = 5.84) #this is making a set of data that the model will predict points for. size 0:6, 75th quarile of summer precip, and control plots!
pred.75 <- predict(sm10lag, newdata = pred.75.dat, type="response", re.form=~0)
pred.75 <- as.data.frame(pred.75)
pred.75 <- cbind(pred.75, pred.75.dat)
pred.av.dat <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), grow.season.min.temp.1yearlag = 5.80) #this is making a set of data that the model will predict points for. size 0:6, av of summer precip, and control plots!
pred.av <- predict(sm10lag, newdata = pred.av.dat, type="response", re.form=~0)
pred.av <- as.data.frame(pred.av)
pred.av <- cbind(pred.av, pred.av.dat)

primula %>% 
  ggplot(aes(x=log.ros.area, y = psurvivalT1))+
  geom_jitter(height = .025, size = 1) +
  geom_line(data = pred.25, aes(x = log.ros.area, y = pred.25), color = FF[1], linetype = 2, size = 1) + #25th perc. gs max temp
  geom_line(data = pred.75, aes(x = log.ros.area, y = pred.75), color = FF[4], linetype = 2, size = 1) +  #75th perc. gs max temp
  geom_line(data = pred.av, aes(x = log.ros.area, y = pred.av), color = "black", size = 1.2) + #av gs max temp
  scale_x_continuous(expand = c(0, 0), limits = c(0, 6)) + #make the graph start in the corner
  labs(title= "sm10lag")

#sm65
pred.25.dat <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), winter.min.temp.1yearlag = .143) #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip seq(0, 6, by = .1) is the same as 0:6 but smaller increments
pred.25 <- predict(sm65, newdata = pred.25.dat, type="response", re.form=~0) #re.form = ~0 tells it to not include random effects
pred.25 <- as.data.frame(pred.25)
pred.25 <- cbind(pred.25, pred.25.dat)
pred.75.dat <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), winter.min.temp.1yearlag = 0.829) #this is making a set of data that the model will predict points for. size 0:6, 75th quarile of summer precip, and control plots!
pred.75 <- predict(sm65, newdata = pred.75.dat, type="response", re.form=~0)
pred.75 <- as.data.frame(pred.75)
pred.75 <- cbind(pred.75, pred.75.dat)
pred.av.dat <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), winter.min.temp.1yearlag = 0.834) #this is making a set of data that the model will predict points for. size 0:6, av of summer precip, and control plots!
pred.av <- predict(sm65, newdata = pred.av.dat, type="response", re.form=~0)
pred.av <- as.data.frame(pred.av)
pred.av <- cbind(pred.av, pred.av.dat)

primula %>% 
  ggplot(aes(x=log.ros.area, y = psurvivalT1))+
  geom_jitter(height = .025, size = 1) +
  geom_line(data = pred.25, aes(x = log.ros.area, y = pred.25), color = FF[1], linetype = 2, size = 1) + #25th perc. gs max temp
  geom_line(data = pred.75, aes(x = log.ros.area, y = pred.75), color = FF[4], linetype = 2, size = 1) +  #75th perc. gs max temp
  geom_line(data = pred.av, aes(x = log.ros.area, y = pred.av), color = "black", size = 1.2) + #av gs max temp
  scale_x_continuous(expand = c(0, 0), limits = c(0, 6)) + #make the graph start in the corner
  labs(title= "sm65")


########################################################

primula %>% ggplot(aes(log.ros.area, psurvivalT1)) +
  geom_jitter(height = .02)+
  theme_classic()+
  geom_smooth(method = glm, method.args= list(family="binomial"), se=FALSE, fullrange = TRUE)+
  expand_limits(x = 0, y = 0)+
  facet_wrap(~year)
# what is happening to survival in 2017? Probably something to do with data collection in 2016 and 2017 (not being able to find them?) - not sure how to deal with this - likely a dormanct problem

##################################################################################################################################################################################
####Number of flowers##############################################################################################################################################################################
# should be be only for plants that flowered? Yes
# However, there's a problem with singular fits if I include RE. Should I try without RE? Even the most simple model (c.min3) won't converge
# also need to make some decisions about flowering vs seeding, because theyre not the same. If flowers were eaten, it flowered, but had 0 flowers and no seeds...
ggplot(subset(primula, flow.sum > 0), aes(log.ros.area, flow.sum))+
  geom_point()+
  facet_wrap(~year)
#  


noflow.min <- glm(flow.sum ~ log.ros.area + trt, data = subset(primula,flow.sum > 0), family = "poisson")
noflow.min2 <- glm(flow.sum ~ log.ros.area*trt, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.min3 <- glm(flow.sum ~ log.ros.area, data = subset(primula, flow.sum > 0), family = "poisson")
###
##grow season total precip
noflow1 <- glm(flow.sum ~ log.ros.area + trt + grow.season.tot.precip, data = subset(primula, flow.sum > 0), family = "poisson")
noflow2 <- glm(flow.sum ~ log.ros.area + trt*grow.season.tot.precip, data = subset(primula, flow.sum > 0), family = "poisson")
noflow3 <- glm(flow.sum ~ log.ros.area*trt+ grow.season.tot.precip, data = subset(primula, flow.sum > 0), family = "poisson")
noflow4 <- glm(flow.sum ~ log.ros.area + grow.season.tot.precip, data = subset(primula, flow.sum > 0), family = "poisson")
noflow5 <- glm(flow.sum ~ log.ros.area*grow.season.tot.precip, data = subset(primula, flow.sum > 0), family = "poisson")
noflow6 <- glm(flow.sum ~ log.ros.area*trt*grow.season.tot.precip, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.1 <- glm(flow.sum ~ log.ros.area*trt+ trt*grow.season.tot.precip , data = primula, family = "poisson")
noflow.2<- glm(flow.sum ~ log.ros.area*grow.season.tot.precip + trt , data = primula, family = "poisson") 
###last years grow season precip
noflow1lag <- glm(flow.sum ~ log.ros.area + trt + grow.season.precip.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow2lag <- glm(flow.sum ~ log.ros.area + trt*grow.season.precip.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow3lag <- glm(flow.sum ~ log.ros.area*trt + grow.season.precip.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow4lag <- glm(flow.sum ~ log.ros.area + grow.season.precip.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow5lag <- glm(flow.sum ~ log.ros.area*grow.season.precip.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow6lag <- glm(flow.sum ~ log.ros.area*trt*grow.season.precip.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.3 <- glm(flow.sum ~ log.ros.area*trt+ trt*grow.season.precip.1yearlag , data = primula, family = "poisson")
noflow.4<- glm(flow.sum ~ log.ros.area*grow.season.precip.1yearlag + trt , data = primula, family = "poisson") 
#grow season min temp
noflow7 <- glm(flow.sum ~ log.ros.area + trt + grow.season.mean.min.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow8 <- glm(flow.sum ~ log.ros.area*trt + grow.season.mean.min.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow9 <- glm(flow.sum ~ log.ros.area + grow.season.mean.min.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow10 <- glm(flow.sum ~ log.ros.area*grow.season.mean.min.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow11 <- glm(flow.sum ~ log.ros.area + trt*grow.season.mean.min.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow12 <- glm(flow.sum ~ log.ros.area*trt*grow.season.mean.min.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.5 <- glm(flow.sum ~ log.ros.area*trt+ trt*grow.season.mean.min.temp , data = primula, family = "poisson")
noflow.6<- glm(flow.sum ~ log.ros.area*grow.season.mean.min.temp + trt , data = primula, family = "poisson")
# last years grow season min temp
noflow7lag <- glm(flow.sum ~ log.ros.area + trt + grow.season.min.temp.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow8lag <- glm(flow.sum ~ log.ros.area*trt + grow.season.min.temp.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow9lag <- glm(flow.sum ~ log.ros.area + grow.season.min.temp.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow10lag <- glm(flow.sum ~ log.ros.area*grow.season.min.temp.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson") ###
noflow11lag <- glm(flow.sum ~ log.ros.area + trt*grow.season.min.temp.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow12lag <- glm(flow.sum ~ log.ros.area*trt*grow.season.min.temp.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.7 <- glm(flow.sum ~ log.ros.area*trt+ trt*grow.season.min.temp.1yearlag , data = primula, family = "poisson")
noflow.8<- glm(flow.sum ~ log.ros.area*grow.season.min.temp.1yearlag + trt , data = primula, family = "poisson")
# grow season max temp
noflow13 <- glm(flow.sum ~ log.ros.area + trt + grow.season.mean.max.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow14 <- glm(flow.sum ~ log.ros.area*trt + grow.season.mean.max.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow15 <- glm(flow.sum ~ log.ros.area + grow.season.mean.max.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow16 <- glm(flow.sum ~ log.ros.area*grow.season.mean.max.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow17 <- glm(flow.sum ~ log.ros.area + trt*grow.season.mean.max.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow18 <- glm(flow.sum ~ log.ros.area*trt*grow.season.mean.max.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.9 <- glm(flow.sum ~ log.ros.area*trt+ trt*grow.season.mean.max.temp , data = primula, family = "poisson")
noflow.10<- glm(flow.sum ~ log.ros.area*grow.season.mean.max.temp + trt , data = primula, family = "poisson")
# last year grow seasons max temp
noflow13lag <- glm(flow.sum ~ log.ros.area + trt + grow.season.max.temp.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow14lag <- glm(flow.sum ~ log.ros.area*trt + grow.season.max.temp.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow15lag <- glm(flow.sum ~ log.ros.area + grow.season.max.temp.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow16lag <- glm(flow.sum ~ log.ros.area*grow.season.max.temp.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow17lag <- glm(flow.sum ~ log.ros.area + trt*grow.season.max.temp.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow18lag <- glm(flow.sum ~ log.ros.area*trt*grow.season.max.temp.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.11 <- glm(flow.sum ~ log.ros.area*trt+ trt*grow.season.max.temp.1yearlag , data = primula, family = "poisson")
noflow.12<- glm(flow.sum ~ log.ros.area*grow.season.max.temp.1yearlag + trt , data = primula, family = "poisson")
## summer max temp
noflow20 <- glm(flow.sum ~ log.ros.area + trt + summer.mean.max.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow21 <- glm(flow.sum ~ log.ros.area + trt*summer.mean.max.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow22 <- glm(flow.sum ~ log.ros.area + summer.mean.max.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow23 <- glm(flow.sum ~ log.ros.area*trt + summer.mean.max.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow24 <- glm(flow.sum ~ log.ros.area*summer.mean.max.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow19 <- glm(flow.sum ~ log.ros.area*trt*summer.mean.max.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.13 <- glm(flow.sum ~ log.ros.area*trt+ trt*summer.mean.max.temp , data = primula, family = "poisson")
noflow.14 <- glm(flow.sum ~ log.ros.area*summer.mean.max.temp + trt , data = primula, family = "poisson")
# last year's summer max temp
noflow19lag <- glm(flow.sum ~ log.ros.area + trt + summer.max.temp.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow20lag <- glm(flow.sum ~ log.ros.area + trt*summer.max.temp.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow21lag <- glm(flow.sum ~ log.ros.area + summer.max.temp.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow22lag <- glm(flow.sum ~ log.ros.area*trt + summer.max.temp.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow23lag <- glm(flow.sum ~ log.ros.area*summer.max.temp.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow24lag <- glm(flow.sum ~ log.ros.area*trt*summer.max.temp.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.15 <- glm(flow.sum ~ log.ros.area*trt+ trt*summer.max.temp.1yearlag , data = primula, family = "poisson")
noflow.16 <- glm(flow.sum ~ log.ros.area*summer.max.temp.1yearlag + trt , data = primula, family = "poisson")
# 2 years ago summer max temp
noflow25lag <- glm(flow.sum ~ log.ros.area + trt + summer.max.temp.2yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow26lag <- glm(flow.sum ~ log.ros.area + trt*summer.max.temp.2yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow27lag <- glm(flow.sum ~ log.ros.area + summer.max.temp.2yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow28lag <- glm(flow.sum ~ log.ros.area*trt + summer.max.temp.2yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow29lag <- glm(flow.sum ~ log.ros.area*summer.max.temp.2yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow30lag <- glm(flow.sum ~ log.ros.area*trt*summer.max.temp.2yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.17 <- glm(flow.sum ~ log.ros.area*trt+ trt*summer.max.temp.2yearlag , data = primula, family = "poisson")
noflow.18 <- glm(flow.sum ~ log.ros.area*summer.max.temp.2yearlag + trt , data = primula, family = "poisson")
# summer min temp
noflow25 <- glm(flow.sum ~ log.ros.area + trt + summer.mean.min.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow26 <- glm(flow.sum ~ log.ros.area + trt*summer.mean.min.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow27 <- glm(flow.sum ~ log.ros.area + summer.mean.min.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow28 <- glm(flow.sum ~ log.ros.area*trt + summer.mean.min.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow29 <- glm(flow.sum ~ log.ros.area*summer.mean.min.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow30 <- glm(flow.sum ~ log.ros.area*trt*summer.mean.min.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.19 <- glm(flow.sum ~ log.ros.area*trt+ trt*summer.mean.min.temp , data = primula, family = "poisson")
noflow.20 <- glm(flow.sum ~ log.ros.area*summer.mean.min.temp + trt , data = primula, family = "poisson")
# summer min temp lagged
noflow31lag <- glm(flow.sum ~ log.ros.area + trt + summer.min.temp.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow32lag <- glm(flow.sum ~ log.ros.area + trt*summer.min.temp.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow33lag <- glm(flow.sum ~ log.ros.area + summer.min.temp.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow34lag <- glm(flow.sum ~ log.ros.area*trt + summer.min.temp.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow35lag <- glm(flow.sum ~ log.ros.area*summer.min.temp.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow36lag <- glm(flow.sum ~ log.ros.area*trt*summer.min.temp.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.21 <- glm(flow.sum ~ log.ros.area*trt+ trt*summer.min.temp.1yearlag , data = primula, family = "poisson")
noflow.22 <- glm(flow.sum ~ log.ros.area*summer.min.temp.1yearlag + trt , data = primula, family = "poisson")
# summer precip
noflow31 <- glm(flow.sum ~ log.ros.area + trt + summer.tot.precip, data = subset(primula, flow.sum > 0), family = "poisson")
noflow32 <- glm(flow.sum ~ log.ros.area + trt*summer.tot.precip, data = subset(primula, flow.sum > 0), family = "poisson") #3
noflow33 <- glm(flow.sum ~ log.ros.area + summer.tot.precip, data = subset(primula, flow.sum > 0), family = "poisson") ##1
noflow34 <- glm(flow.sum ~ log.ros.area*trt + summer.tot.precip, data = subset(primula, flow.sum > 0), family = "poisson")
noflow35 <- glm(flow.sum ~ log.ros.area*summer.tot.precip, data = subset(primula, flow.sum > 0), family = "poisson")##2
noflow36 <- glm(flow.sum ~ log.ros.area*trt*summer.tot.precip, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.23 <- glm(flow.sum ~ log.ros.area*trt+ trt*summer.tot.precip , data = primula, family = "poisson")
noflow.24 <- glm(flow.sum ~ log.ros.area*summer.tot.precip + trt , data = primula, family = "poisson")
#summer precip lag
noflow37lag <- glm(flow.sum ~ log.ros.area + trt + summer.precip.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow38lag <- glm(flow.sum ~ log.ros.area + trt*summer.precip.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow39lag <- glm(flow.sum ~ log.ros.area + summer.precip.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow40lag <- glm(flow.sum ~ log.ros.area*trt + summer.precip.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow41lag <- glm(flow.sum ~ log.ros.area*summer.precip.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow42lag <- glm(flow.sum ~ log.ros.area*trt*summer.precip.1yearlag, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.25 <- glm(flow.sum ~ log.ros.area*trt+ trt*summer.precip.1yearlag , data = primula, family = "poisson")
noflow.26 <- glm(flow.sum ~ log.ros.area*summer.precip.1yearlag + trt , data = primula, family = "poisson")
#
noflow37 <- glm(flow.sum ~ log.ros.area + trt + winter.mean.min.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow38 <- glm(flow.sum ~ log.ros.area + trt*winter.mean.min.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow39 <- glm(flow.sum ~ log.ros.area + winter.mean.min.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.27 <- glm(flow.sum ~ log.ros.area*trt + winter.mean.min.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.28 <- glm(flow.sum ~ log.ros.area*winter.mean.min.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.29<- glm(flow.sum ~ log.ros.area*trt*winter.mean.min.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.30 <- glm(flow.sum ~ log.ros.area*trt+ trt*winter.mean.min.temp , data = primula, family = "poisson")
noflow.31 <- glm(flow.sum ~ log.ros.area*winter.mean.min.temp + trt , data = primula, family = "poisson")
#
noflow40 <- glm(flow.sum ~ log.ros.area + trt + winter.tot.precip, data = subset(primula, flow.sum > 0), family = "poisson")
noflow41 <- glm(flow.sum ~ log.ros.area + trt*winter.tot.precip, data = subset(primula, flow.sum > 0), family = "poisson")
noflow42 <- glm(flow.sum ~ log.ros.area + winter.tot.precip, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.32 <- glm(flow.sum ~ log.ros.area*trt + winter.tot.precip, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.33 <- glm(flow.sum ~ log.ros.area*winter.tot.precip, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.34<- glm(flow.sum ~ log.ros.area*trt*winter.tot.precip, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.35 <- glm(flow.sum ~ log.ros.area*trt+ trt*winter.tot.precip , data = primula, family = "poisson")
noflow.36 <- glm(flow.sum ~ log.ros.area*winter.tot.precip + trt , data = primula, family = "poisson")
#
noflow43 <- glm(flow.sum ~ log.ros.area + trt + winter.mean.max.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow44 <- glm(flow.sum ~ log.ros.area + trt*winter.mean.max.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow45 <- glm(flow.sum ~ log.ros.area + winter.mean.max.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.37 <- glm(flow.sum ~ log.ros.area*trt + winter.mean.max.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.38 <- glm(flow.sum ~ log.ros.area*winter.mean.max.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.39<- glm(flow.sum ~ log.ros.area*trt*winter.mean.max.temp, data = subset(primula, flow.sum > 0), family = "poisson")
noflow.40 <- glm(flow.sum ~ log.ros.area*trt+ trt*winter.mean.max.temp , data = primula, family = "poisson")
noflow.41 <- glm(flow.sum ~ log.ros.area*winter.mean.max.temp + trt , data = primula, family = "poisson")

noflow.test <- glm(flow.sum ~ log.ros.area+ winter.mean.min.temp + winter.tot.precip, data = primula, family = "poisson")
count.list <- mget(ls(pattern = "^noflow"))
#count <- lst(c.min, c.min2, c.min3, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30, c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c1lag, c2lag, c3lag, c4lag, c5lag, c6lag, c7lag, c8lag, c8lag, c9lag, c10lag, c11lag, c12lag, c13lag, c14lag, c15lag, c16lag, c17lag, c18lag, c19lag, c20lag, c21lag, c22lag, c23lag, c24lag, c25lag, c26lag, c27lag, c28lag, c29lag, c30lag, c31lag, c32lag, c33lag, c34lag, c35lag, c36lag, c37lag, c38lag, c38lag, c39lag, c40lag, c41lag, c42lag)
count_AIC<-aictab(cand.set = count.list, modnames = NULL,second.ord=TRUE,nobs=NULL,sort=TRUE)

summary(noflow42)
summary(noflow9)
summary(noflow39)
# these are all the same AIC but different cliamte models - which to choose?
#log.ros.area + winter.tot.precip, 
#noflow42 is the best (not by much - really similar to the other climate versions - b
pred.cont.25.dat <-  expand.grid(log.ros.area = seq(1.9, 5.7, by = .1), winter.tot.precip = 298.9) #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots! seq(0, 6, by = .1) is the same as 0:6 but smaller increments
pred.cont.25 <- predict(noflow42, newdata = pred.cont.25.dat, type="response", re.form=~0) #re.form = ~0 tells it to not include random effects
pred.cont.25 <- as.data.frame(pred.cont.25)
pred.cont.25 <- cbind(pred.cont.25, pred.cont.25.dat)
pred.cont.75.dat <-  expand.grid(log.ros.area = seq(1.9, 5.7, by = .1), winter.tot.precip = 391.2) #this is making a set of data that the model will predict points for. size 0:6, 75th quarile of summer precip, and control plots!
pred.cont.75 <- predict(noflow42, newdata = pred.cont.75.dat, type="response", re.form=~0)
pred.cont.75 <- as.data.frame(pred.cont.75)
pred.cont.75 <- cbind(pred.cont.75, pred.cont.75.dat)
pred.cont.av.dat <-  expand.grid(log.ros.area = seq(1.9, 5.7, by = .1), winter.tot.precip = 361.13) #this is making a set of data that the model will predict points for. size 0:6, av of summer precip, and control plots!
pred.cont.av <- predict(noflow42, newdata = pred.cont.av.dat, type="response", re.form=~0)
pred.cont.av <- as.data.frame(pred.cont.av)
pred.cont.av <- cbind(pred.cont.av, pred.cont.av.dat)

ggplot(subset(primula, flow.sum > 0), aes(x=log.ros.area, y= flow.sum))+
  geom_point(color = "orchid4", size = 1.5) +
  geom_line(data = pred.cont.25, aes(x = log.ros.area, y = pred.cont.25), color = FF[2], linetype = 2, size = 1) + #25th perc. winter tot precip
  geom_line(data = pred.cont.75, aes(x = log.ros.area, y = pred.cont.75), color = FF[3], linetype = 2, size = 1) +  #75th perc. winter tot precip
  geom_line(data = pred.cont.av, aes(x = log.ros.area, y = pred.cont.av), color = "black", size = 1.2) + #av  gs max temp
  scale_x_continuous(expand = c(0, 0), limits = c(0, 6))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
  labs(y = "Number of Flowers")
#when wetter, makes more flowers

# what does the log.ros.area + grow.season.mean.min.temp,  look like?
pred.cont.25.dat <-  expand.grid(log.ros.area = seq(1.9, 5.7, by = .1), grow.season.mean.min.temp = 5.49) #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots! seq(0, 6, by = .1) is the same as 0:6 but smaller increments
pred.cont.25 <- predict(noflow9, newdata = pred.cont.25.dat, type="response", re.form=~0) #re.form = ~0 tells it to not include random effects
pred.cont.25 <- as.data.frame(pred.cont.25)
pred.cont.25 <- cbind(pred.cont.25, pred.cont.25.dat)
pred.cont.75.dat <-  expand.grid(log.ros.area = seq(1.9, 5.7, by = .1), grow.season.mean.min.temp = 5.84) #this is making a set of data that the model will predict points for. size 0:6, 75th quarile of summer precip, and control plots!
pred.cont.75 <- predict(noflow9, newdata = pred.cont.75.dat, type="response", re.form=~0)
pred.cont.75 <- as.data.frame(pred.cont.75)
pred.cont.75 <- cbind(pred.cont.75, pred.cont.75.dat)
pred.cont.av.dat <-  expand.grid(log.ros.area = seq(1.9, 5.7, by = .1), grow.season.mean.min.temp = 5.80) #this is making a set of data that the model will predict points for. size 0:6, av of summer precip, and control plots!
pred.cont.av <- predict(noflow9, newdata = pred.cont.av.dat, type="response", re.form=~0)
pred.cont.av <- as.data.frame(pred.cont.av)
pred.cont.av <- cbind(pred.cont.av, pred.cont.av.dat)

ggplot(subset(primula, flow.sum > 0), aes(x=log.ros.area, y=flow.sum))+
  geom_point(color = "orchid4", size = 1.5) +
  geom_line(data = pred.cont.25, aes(x = log.ros.area, y = pred.cont.25), color = FF[3], linetype = 2, size = 1) + #25th perc.gs max temp
  geom_line(data = pred.cont.75, aes(x = log.ros.area, y = pred.cont.75), color = FF[5], linetype = 2, size = 1) +  #75th perc. gs max temp
  geom_line(data = pred.cont.av, aes(x = log.ros.area, y = pred.cont.av), color = "black", size = 1.2) + #av  gs max temp
  scale_x_continuous(expand = c(0, 0), limits = c(0, 6))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
  labs(y = "Number of Flowers")
# so the cooler it is in the growing season, the more flowers you make (if too hot, wont have too many flowers)

# what does the log.ros.area +  winter.mean.min.temp,  look like?
pred.cont.25.dat <-  expand.grid(log.ros.area = seq(1.9, 5.7, by = .1), winter.mean.min.temp = 0.143) #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots! seq(0, 6, by = .1) is the same as 0:6 but smaller increments
pred.cont.25 <- predict(noflow39, newdata = pred.cont.25.dat, type="response", re.form=~0) #re.form = ~0 tells it to not include random effects
pred.cont.25 <- as.data.frame(pred.cont.25)
pred.cont.25 <- cbind(pred.cont.25, pred.cont.25.dat)
pred.cont.75.dat <-  expand.grid(log.ros.area = seq(1.9, 5.7, by = .1), winter.mean.min.temp = 0.829) #this is making a set of data that the model will predict points for. size 0:6, 75th quarile of summer precip, and control plots!
pred.cont.75 <- predict(noflow39, newdata = pred.cont.75.dat, type="response", re.form=~0)
pred.cont.75 <- as.data.frame(pred.cont.75)
pred.cont.75 <- cbind(pred.cont.75, pred.cont.75.dat)
pred.cont.av.dat <-  expand.grid(log.ros.area = seq(1.9, 5.7, by = .1), winter.mean.min.temp = 0.833) #this is making a set of data that the model will predict points for. size 0:6, av of summer precip, and control plots!
pred.cont.av <- predict(noflow39, newdata = pred.cont.av.dat, type="response", re.form=~0)
pred.cont.av <- as.data.frame(pred.cont.av)
pred.cont.av <- cbind(pred.cont.av, pred.cont.av.dat)

ggplot(subset(primula, flow.sum > 0), aes(x=log.ros.area, y=flow.sum))+
  geom_point(color = "orchid4", size = 1.5) +
  geom_line(data = pred.cont.25, aes(x = log.ros.area, y = pred.cont.25), color = FF[3], linetype = 2, size = 1) + #25th perc.gs max temp
  geom_line(data = pred.cont.75, aes(x = log.ros.area, y = pred.cont.75), color = FF[5], linetype = 2, size = 1) +  #75th perc. gs max temp
  geom_line(data = pred.cont.av, aes(x = log.ros.area, y = pred.cont.av), color = "black", size = 1.2) + #av  gs max temp
  scale_x_continuous(expand = c(0, 0), limits = c(0, 6))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
  labs(y = "Number of Flowers")

# # these are the ones that dont converge
# c.min <- glmer(flow.sum ~ log.ros.area + trt + (1|plot) + (1|year), data = subset(primula, flow.sum > 0), family = "poisson")
# c.min2 <- glmer(flow.sum ~ log.ros.area*trt + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c.min3 <- glmer(flow.sum ~ log.ros.area + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# # ###
# # ##grow season total precip
#  c1 <- glmer(flow.sum ~ log.ros.area + trt + grow.season.tot.precip + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c2 <- glmer(no.flowers ~ log.ros.area + trt*grow.season.tot.precip + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c3 <- glmer(no.flowers ~ log.ros.area*trt+ grow.season.tot.precip + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c4 <- glmer(no.flowers ~ log.ros.area + grow.season.tot.precip + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c5 <- glmer(no.flowers ~ log.ros.area*grow.season.tot.precip + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c6 <- glmer(no.flowers ~ log.ros.area*trt*grow.season.tot.precip + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# ###last years grow season precip
# c1lag <- glmer(no.flowers ~ log.ros.area + trt + grow.season.precip.1yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c2lag <- glmer(no.flowers ~ log.ros.area + trt*grow.season.precip.1yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c3lag <- glmer(no.flowers ~ log.ros.area*trt + grow.season.precip.1yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c4lag <- glmer(no.flowers ~ log.ros.area + grow.season.precip.1yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c5lag <- glmer(no.flowers ~ log.ros.area*grow.season.precip.1yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c6lag <- glmer(no.flowers ~ log.ros.area*trt*grow.season.precip.1yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# #grow season min temp
# c7 <- glmer(no.flowers ~ log.ros.area + trt + grow.season.mean.min.temp+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c8 <- glmer(no.flowers ~ log.ros.area*trt + grow.season.mean.min.temp+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c9 <- glmer(no.flowers ~ log.ros.area + grow.season.mean.min.temp+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c10 <- glmer(no.flowers ~ log.ros.area*grow.season.mean.min.temp+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c11 <- glmer(no.flowers ~ log.ros.area + trt*grow.season.mean.min.temp+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c12 <- glmer(no.flowers ~ log.ros.area*trt*grow.season.mean.min.temp+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# # last years grow season min temp
# c7lag <- glmer(no.flowers ~ log.ros.area + trt + grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c8lag <- glmer(no.flowers ~ log.ros.area*trt + grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c9lag <- glmer(no.flowers ~ log.ros.area + grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c10lag <- glmer(no.flowers ~ log.ros.area*grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson") ###
# c11lag <- glmer(no.flowers ~ log.ros.area + trt*grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c12lag <- glmer(no.flowers ~ log.ros.area*trt*grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# # grow season max temp
# c13 <- glmer(no.flowers ~ log.ros.area + trt + grow.season.mean.max.temp+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c14 <- glmer(no.flowers ~ log.ros.area*trt + grow.season.mean.max.temp+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c15 <- glmer(no.flowers ~ log.ros.area + grow.season.mean.max.temp+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c16 <- glmer(no.flowers ~ log.ros.area*grow.season.mean.max.temp+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c17 <- glmer(no.flowers ~ log.ros.area + trt*grow.season.mean.max.temp+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c18 <- glmer(no.flowers ~ log.ros.area*trt*grow.season.mean.max.temp+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# # last year grow seasons max temp
# c13lag <- glmer(no.flowers ~ log.ros.area + trt + grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c14lag <- glmer(no.flowers ~ log.ros.area*trt + grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c15lag <- glmer(no.flowers ~ log.ros.area + grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c16lag <- glmer(no.flowers ~ log.ros.area*grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c17lag <- glmer(no.flowers ~ log.ros.area + trt*grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c18lag <- glmer(no.flowers ~ log.ros.area*trt*grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# ## summer max temp
# c20 <- glmer(no.flowers ~ log.ros.area + trt + summer.mean.max.temp+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c21 <- glmer(no.flowers ~ log.ros.area + trt*summer.mean.max.temp+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c22 <- glmer(no.flowers ~ log.ros.area + summer.mean.max.temp + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c23 <- glmer(no.flowers ~ log.ros.area*trt + summer.mean.max.temp + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c24 <- glmer(no.flowers ~ log.ros.area*summer.mean.max.temp + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c19 <- glmer(no.flowers ~ log.ros.area*trt*summer.mean.max.temp + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# # last year's summer max temp
# c19lag <- glmer(no.flowers ~ log.ros.area + trt + summer.max.temp.1yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c20lag <- glmer(no.flowers ~ log.ros.area + trt*summer.max.temp.1yearlag+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c21lag <- glmer(no.flowers ~ log.ros.area + summer.max.temp.1yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c22lag <- glmer(no.flowers ~ log.ros.area*trt + summer.max.temp.1yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c23lag <- glmer(no.flowers ~ log.ros.area*summer.max.temp.1yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c24lag <- glmer(no.flowers ~ log.ros.area*trt*summer.max.temp.1yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# # 2 years ago summer max temp
# c25lag <- glmer(no.flowers ~ log.ros.area + trt + summer.max.temp.2yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c26lag <- glmer(no.flowers ~ log.ros.area + trt*summer.max.temp.2yearlag+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c27lag <- glmer(no.flowers ~ log.ros.area + summer.max.temp.2yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c28lag <- glmer(no.flowers ~ log.ros.area*trt + summer.max.temp.2yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c29lag <- glmer(no.flowers ~ log.ros.area*summer.max.temp.2yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c30lag <- glmer(no.flowers ~ log.ros.area*trt*summer.max.temp.2yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# # summer min temp
# c25 <- glmer(no.flowers ~ log.ros.area + trt + summer.mean.min.temp + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c26 <- glmer(no.flowers ~ log.ros.area + trt*summer.mean.min.temp + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c27 <- glmer(no.flowers ~ log.ros.area + summer.mean.min.temp + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c28 <- glmer(no.flowers ~ log.ros.area*trt + summer.mean.min.temp + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c29 <- glmer(no.flowers ~ log.ros.area*summer.mean.min.temp + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c30 <- glmer(no.flowers ~ log.ros.area*trt*summer.mean.min.temp + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# # summer min temp lagged
# c31lag <- glmer(no.flowers ~ log.ros.area + trt + summer.min.temp.1yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c32lag <- glmer(no.flowers ~ log.ros.area + trt*summer.min.temp.1yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c33lag <- glmer(no.flowers ~ log.ros.area + summer.min.temp.1yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c34lag <- glmer(no.flowers ~ log.ros.area*trt + summer.min.temp.1yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c35lag <- glmer(no.flowers ~ log.ros.area*summer.min.temp.1yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c36lag <- glmer(no.flowers ~ log.ros.area*trt*summer.min.temp.1yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# # summer precip
# c31 <- glmer(no.flowers ~ log.ros.area + trt + summer.tot.precip + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c32 <- glmer(no.flowers ~ log.ros.area + trt*summer.tot.precip + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c33 <- glmer(no.flowers ~ log.ros.area + summer.tot.precip + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c34 <- glmer(no.flowers ~ log.ros.area*trt + summer.tot.precip + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c35 <- glmer(no.flowers ~ log.ros.area*summer.tot.precip + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c36 <- glmer(no.flowers ~ log.ros.area*trt*summer.tot.precip + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# #summer precip lag
# c37lag <- glmer(no.flowers ~ log.ros.area + trt + summer.precip.1yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c38lag <- glmer(no.flowers ~ log.ros.area + trt*summer.precip.1yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c39lag <- glmer(no.flowers ~ log.ros.area + summer.precip.1yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c40lag <- glmer(no.flowers ~ log.ros.area*trt + summer.precip.1yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c41lag <- glmer(no.flowers ~ log.ros.area*summer.precip.1yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c42lag <- glmer(no.flowers ~ log.ros.area*trt*summer.precip.1yearlag + (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# #
# c37 <- glmer(no.flowers ~ log.ros.area + trt + winter.mean.min.temp+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c38 <- glmer(no.flowers ~ log.ros.area + trt*winter.mean.min.temp+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c39 <- glmer(no.flowers ~ log.ros.area + winter.mean.min.temp+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# #
# c40 <- glmer(no.flowers ~ log.ros.area + trt + winter.tot.precip+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c41 <- glmer(no.flowers ~ log.ros.area + trt*winter.tot.precip+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c42 <- glmer(no.flowers ~ log.ros.area + winter.tot.precip+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# #
# c43 <- glmer(no.flowers ~ log.ros.area + trt + winter.mean.max.temp+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c44 <- glmer(no.flowers ~ log.ros.area + trt*winter.mean.max.temp+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# c45 <- glmer(no.flowers ~ log.ros.area + winter.mean.max.temp+ (1|plot) + (1|year), data = subset(primula, pflower > 0), family = "poisson")
# 
# ###
# ###
# #count <- lst(c.min, c.min2, c.min3, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30, c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c1lag, c2lag, c3lag, c4lag, c5lag, c6lag, c7lag, c8lag, c8lag, c9lag, c10lag, c11lag, c12lag, c13lag, c14lag, c15lag, c16lag, c17lag, c18lag, c19lag, c20lag, c21lag, c22lag, c23lag, c24lag, c25lag, c26lag, c27lag, c28lag, c29lag, c30lag, c31lag, c32lag, c33lag, c34lag, c35lag, c36lag, c37lag, c38lag, c38lag, c39lag, c40lag, c41lag, c42lag)
# 
# #only ones that converged/no warnings
# count <- lst(c.min, c.min2, c.min3, c9, c7, c7lag, c8lag, c9lag, c10lag, c11lag, c13, c15, c13lag, c14lag, c15lag, c20, c22, c19lag, c20lag, c21lag, c25lag, c27lag, c25, c26, c27, c28, c31lag, c33lag, c34lag, c31, c32, c33, c34, c37lag, c38lag, c39lag, c40lag, c37, c38, c39, c43, c45)
# 
# count_AIC<-aictab(cand.set = count, modnames = NULL,second.ord=TRUE,nobs=NULL,sort=TRUE)
# #c32 is the best. Is it fishy that it is so much better (delta AIC >48 difference) than the other ones??
# ## log.ros.area + trt * summer.tot.precip 
# 
# pred.cont.25.dat <-  expand.grid(log.ros.area = seq(0, 6, by = .1), summer.tot.precip = 46.2, trt = "control") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots! seq(0, 6, by = .1) is the same as 0:6 but smaller increments
# pred.cont.25 <- predict(c32, newdata = pred.cont.25.dat, type="response", re.form=~0) #re.form = ~0 tells it to not include random effects
# pred.cont.25 <- as.data.frame(pred.cont.25)
# pred.cont.25 <- cbind(pred.cont.25, pred.cont.25.dat)
# pred.cont.75.dat <-  expand.grid(log.ros.area = seq(0, 6, by = .1), summer.tot.precip = 67.3, trt = "control") #this is making a set of data that the model will predict points for. size 0:6, 75th quarile of summer precip, and control plots!
# pred.cont.75 <- predict(c32, newdata = pred.cont.75.dat, type="response", re.form=~0)
# pred.cont.75 <- as.data.frame(pred.cont.75)
# pred.cont.75 <- cbind(pred.cont.75, pred.cont.75.dat)
# pred.cont.av.dat <-  expand.grid(log.ros.area = seq(0, 6, by = .1), summer.tot.precip = 60.4, trt = "control") #this is making a set of data that the model will predict points for. size 0:6, av of summer precip, and control plots!
# pred.cont.av <- predict(c32, newdata = pred.cont.av.dat, type="response", re.form=~0)
# pred.cont.av <- as.data.frame(pred.cont.av)
# pred.cont.av <- cbind(pred.cont.av, pred.cont.av.dat)
# #irrigated plots
# pred.irr.25.dat <-  expand.grid(log.ros.area = seq(0, 6, by = .1), summer.tot.precip = 46.2, trt = "irrigated") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
# pred.irr.25 <- predict(c32, newdata = pred.irr.25.dat, type="response", re.form=~0)
# pred.irr.25 <- as.data.frame(pred.irr.25)
# pred.irr.25 <- cbind(pred.irr.25, pred.irr.25.dat)
# pred.irr.75.dat <-  expand.grid(log.ros.area = seq(0, 6, by = .1), summer.tot.precip = 67.3, trt = "irrigated") #this is making a set of data that the model will predict points for. size 0:6, 75th quarile of summer precip, and control plots!
# pred.irr.75 <- predict(c32, newdata = pred.irr.75.dat, type="response", re.form=~0)
# pred.irr.75 <- as.data.frame(pred.irr.75)
# pred.irr.75 <- cbind(pred.irr.75, pred.irr.75.dat)
# pred.irr.av.dat <-  expand.grid(log.ros.area = seq(0, 6, by = .1), summer.tot.precip = 60.4, trt = "irrigated") #this is making a set of data that the model will predict points for. size 0:6, av of summer precip, and control plots!
# pred.irr.av <- predict(c32, newdata = pred.irr.av.dat, type="response", re.form=~0)
# pred.irr.av <- as.data.frame(pred.irr.av)
# pred.irr.av <- cbind(pred.irr.av, pred.irr.av.dat)
# #drought plots
# pred.drt.25.dat <-  expand.grid(log.ros.area = seq(0, 6, by = .1), summer.tot.precip = 46.2, trt = "drought") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
# pred.drt.25 <- predict(c32, newdata = pred.drt.25.dat, type="response", re.form=~0)
# pred.drt.25 <- as.data.frame(pred.drt.25)
# pred.drt.25 <- cbind(pred.drt.25, pred.drt.25.dat)
# pred.drt.75.dat <-  expand.grid(log.ros.area = seq(0, 6, by = .1), summer.tot.precip = 67.3, trt = "drought") #this is making a set of data that the model will predict points for. size 0:6, 75th quarile of summer precip, and control plots!
# pred.drt.75 <- predict(c32, newdata = pred.drt.75.dat, type="response", re.form=~0)
# pred.drt.75 <- as.data.frame(pred.drt.75)
# pred.drt.75 <- cbind(pred.drt.75, pred.drt.75.dat)
# pred.drt.av.dat <-  expand.grid(log.ros.area = seq(0, 6, by = .1), summer.tot.precip = 60.4, trt = "drought") #this is making a set of data that the model will predict points for. size 0:6, av of summer precip, and control plots!
# pred.drt.av <- predict(c32, newdata = pred.drt.av.dat, type="response", re.form=~0)
# pred.drt.av <- as.data.frame(pred.drt.av)
# pred.drt.av <- cbind(pred.drt.av, pred.drt.av.dat)
# 
# ggplot(subset(subset(primula, pflower > 0), trt == "control"), aes(x=log.ros.area, y=no.flowers))+
#   geom_point(color = R[5], size = 2) +
#   geom_line(data = pred.cont.25, aes(x = log.ros.area, y = pred.cont.25), color = FF[3], linetype = 2, size = 1) + #25th perc.gs max temp
#   geom_line(data = pred.cont.75, aes(x = log.ros.area, y = pred.cont.75), color = FF[5], linetype = 2, size = 1) +  #75th perc. gs max temp
#   geom_line(data = pred.cont.av, aes(x = log.ros.area, y = pred.cont.av), color = "black", size = 1.2) + #av  gs max temp
#   labs(title = "Control")+
#   scale_x_continuous(expand = c(0, 0), limits = c(0, 6))
# 
# subset(primula, pflower > 0) %>% 
#   filter(trt == "irrigated") %>% 
#   ggplot(aes(x=log.ros.area, y=pflowerT1))+
#   geom_jitter(color = Z[2], height = 0.025, size = 2) +
#   geom_line(data = pred.irr.25, aes(x = log.ros.area, y = pred.irr.25, color = "25th Percentile"), color = FF[3], linetype = 2, size = 1) + #25th perc. gs max temp
#   geom_line(data = pred.irr.75, aes(x = log.ros.area, y = pred.irr.75), color = FF[5], linetype = 2, size = 1) +  #75th perc. gs max temp
#   geom_line(data = pred.irr.av, aes(x = log.ros.area, y = pred.irr.av), color = "black", size = 1.2) + #av gs max temp
#   labs(title = "Irrigated")+
#   scale_x_continuous(expand = c(0, 0), limits = c(0, 6)) #make the graph start in the corner
# 
# subset(primula, pflower > 0) %>% 
#   filter(trt == "drought") %>% 
#   ggplot(aes(x=log.ros.area, y=pflowerT1))+
#   geom_jitter(color = FF[1], height = 0.025, size = 2) +
#   geom_line(data = pred.drt.25, aes(x = log.ros.area, y = pred.drt.25), color = FF[3], linetype = 2, size = 1) + #25th perc. gs max temp
#   geom_line(data = pred.drt.75, aes(x = log.ros.area, y = pred.drt.75), color = FF[5], linetype = 2, size = 1) +  #75th perc. gs max temp
#   geom_line(data = pred.drt.av, aes(x = log.ros.area, y = pred.drt.av), color = "black", size = 1.2) + #av gs max temp
#   labs(title = "Drought")+
#   scale_x_continuous(expand = c(0, 0), limits = c(0, 6))
# 
