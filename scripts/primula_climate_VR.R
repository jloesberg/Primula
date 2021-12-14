## Primula vital rates with climate data

###still to do: add in rest of weather station data for the summer!
## add in vital rate models

library(tidyverse)
theme_set(theme_classic())
library(AICcmodavg)
library(lme4)
library(lmtest)
library(ggeffects)
library("ggiraphExtra")
library(effects)
library(wesanderson)
FF <- wes_palettes$FantasticFox1

# Problem with model fit for binomial models
# Big problem with N - just filter out all NA's?

# Read in data ------------------------------------------------------------------------

### read in all years of Primula data and weather data from the Cowichan
# Add in climate data:
source("./scripts/Cowichan_climatevalues.R")

# Add in demography data:
source("./scripts/dode_allyears_cleaned.R")

## Join climate data to demography data by year
primula <- left_join(Dodecatheon_lag, climate, by = "year")

remove(climate, Dodecatheon_lag)
primula$log <- NA
# dealing with extra problem tags:
### tag 35 in 2019 didnt have an entry for pflower or no.flowers, I'm not certain whether it flowered or not. so, just taking it out
primula <- primula %>% filter(tag != "35") %>% 
  mutate(plot = as.character(plot),
         log.ros.areaTminus1 = log(ros.areaTminus1),
         log.ros.area = log(ros.area))

## Now to build the models. 

# Growth Vital Rates ------------------------------------------------------
#####################################################################################################
primula %>% ggplot(aes(x = log(ros.areaTminus1), y = log(ros.area), color = trt)) +
  geom_point() +
  geom_smooth(method = "lm")
boxplot(log(ros.area) ~ trt, data = primula)


##########################################################################################################
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
gm.min <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt + (1|plot) + (1|year), data = primula, REML = F)
gm.min2 <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt + (1|plot) + (1|year), data = primula, REML = F)
gm.min3 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + (1|plot) + (1|year), data = primula, REML = F)
########################################################################################
##grow season total precip
gm1 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt + grow.season.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm2 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt*grow.season.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm3 <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt+ grow.season.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm4 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + grow.season.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm5 <- lmer(log(ros.area) ~ log(ros.areaTminus1)*grow.season.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm6 <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt*grow.season.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
###last years grow season precip
gm1lag <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt + grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm2lag <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt*grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm3lag <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt + grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm4lag <- lmer(log(ros.area) ~ log(ros.areaTminus1) + grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm5lag <- lmer(log(ros.area) ~ log(ros.areaTminus1)*grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm6lag <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt*grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
#grow season min temp
gm7 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt + grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm8 <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt + grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm9 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm10 <- lmer(log(ros.area) ~ log(ros.areaTminus1)*grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm11 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt*grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm12 <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt*grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, REML = F)
# last years grow season min temp
gm7lag <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt + grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm8lag <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt + grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm9lag <- lmer(log(ros.area) ~ log(ros.areaTminus1) + grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm10lag <- lmer(log(ros.area) ~ log(ros.areaTminus1)*grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm11lag <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt*grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm12lag <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt*grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
# grow season max temp
gm13 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt + grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm14 <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt + grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm15 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm16 <- lmer(log(ros.area) ~ log(ros.areaTminus1)*grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm17 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt*grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm18 <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt*grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, REML = F)
# last year grow seasons max temp
gm13lag <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt + grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm14lag <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt + grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm15lag <- lmer(log(ros.area) ~ log(ros.areaTminus1) + grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm16lag <- lmer(log(ros.area) ~ log(ros.areaTminus1)*grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm17lag <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt*grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm18lag <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt*grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
## summer max temp
gm20 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt + summer.mean.max.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm21 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt*summer.mean.max.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm22 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + summer.mean.max.temp + (1|plot) + (1|year), data = primula, REML = F)
gm23 <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt + summer.mean.max.temp + (1|plot) + (1|year), data = primula, REML = F)
gm24 <- lmer(log(ros.area) ~ log(ros.areaTminus1)*summer.mean.max.temp + (1|plot) + (1|year), data = primula, REML = F)
gm19 <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt*summer.mean.max.temp + (1|plot) + (1|year), data = primula, REML = F)
# last year's summer max temp
gm19lag <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt + summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm20lag <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt*summer.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm21lag <- lmer(log(ros.area) ~ log(ros.areaTminus1) + summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm22lag <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt + summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm23lag <- lmer(log(ros.area) ~ log(ros.areaTminus1)*summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm24lag <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt*summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
# 2 years ago summer max temp
gm25lag <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt + summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm26lag <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt*summer.max.temp.2yearlag+ (1|plot) + (1|year), data = primula, REML = F)
gm27lag <- lmer(log(ros.area) ~ log(ros.areaTminus1) + summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm28lag <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt + summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm29lag <- lmer(log(ros.area) ~ log(ros.areaTminus1)*summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm30lag <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt*summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, REML = F)
# summer min temp
gm20 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt + summer.mean.min.temp + (1|plot) + (1|year), data = primula, REML = F)
gm21 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt*summer.mean.min.temp + (1|plot) + (1|year), data = primula, REML = F)
gm22 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + summer.mean.min.temp + (1|plot) + (1|year), data = primula, REML = F)
gm23 <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt + summer.mean.min.temp + (1|plot) + (1|year), data = primula, REML = F)
gm24 <- lmer(log(ros.area) ~ log(ros.areaTminus1)*summer.mean.min.temp + (1|plot) + (1|year), data = primula, REML = F)
gm25 <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt*summer.mean.min.temp + (1|plot) + (1|year), data = primula, REML = F)
# summer min temp lagged
gm31lag <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt + summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm32lag <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt*summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm33lag <- lmer(log(ros.area) ~ log(ros.areaTminus1) + summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm34lag <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt + summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm35lag <- lmer(log(ros.area) ~ log(ros.areaTminus1)*summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm36lag <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt*summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
# summer precip
gm26 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt + summer.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm27 <- lmer(log.ros.area ~ log.ros.areaTminus1 + trt*summer.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm28 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + summer.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm29 <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt + summer.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm30 <- lmer(log(ros.area) ~ log(ros.areaTminus1)*summer.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
gm31 <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt*summer.tot.precip + (1|plot) + (1|year), data = primula, REML = F)
#summer precip lag
gm37lag <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt + summer.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm38lag <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt*summer.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm39lag <- lmer(log(ros.area) ~ log(ros.areaTminus1) + summer.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm40lag <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt + summer.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm41lag <- lmer(log(ros.area) ~ log(ros.areaTminus1)*summer.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
gm42lag <- lmer(log(ros.area) ~ log(ros.areaTminus1)*trt*summer.precip.1yearlag + (1|plot) + (1|year), data = primula, REML = F)
#
gm32 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt + winter.mean.min.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm33 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt*winter.mean.min.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm34 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + winter.mean.min.temp+ (1|plot) + (1|year), data = primula, REML = F)
#
gm35 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt + winter.tot.precip+ (1|plot) + (1|year), data = primula, REML = F)
gm36 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt*winter.tot.precip+ (1|plot) + (1|year), data = primula, REML = F)
gm37 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + winter.tot.precip+ (1|plot) + (1|year), data = primula, REML = F)
#
gm38 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt + winter.mean.max.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm39 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + trt*winter.mean.max.temp+ (1|plot) + (1|year), data = primula, REML = F)
gm40 <- lmer(log(ros.area) ~ log(ros.areaTminus1) + winter.mean.max.temp+ (1|plot) + (1|year), data = primula, REML = F)

###
growth <- lst(gm.min, gm.min2, gm.min3, gm1, gm2, gm3, gm4, gm5, gm6, gm7, gm8, gm9, gm10, gm11, gm12, gm13, gm14, gm15, gm16, gm17, gm18, gm19, gm20, gm21, gm22, gm23, gm24, gm25, gm26, gm27, gm28, gm29, gm30, gm31, gm32, gm33, gm34, gm35, gm36, gm37, gm38, gm39, gm40, gm1lag, gm2lag, gm3lag, gm4lag, gm5lag, gm6lag, gm7lag, gm8lag, gm8lag, gm9lag, gm10lag, gm11lag, gm12lag, gm13lag, gm14lag, gm15lag, gm16lag, gm17lag, gm18lag, gm19lag, gm20lag, gm21lag, gm22lag, gm23lag, gm24lag, gm25lag, gm26lag, gm27lag, gm28lag, gm29lag, gm30lag, gm31lag, gm32lag, gm33lag, gm34lag, gm35lag, gm36lag, gm37lag, gm38lag, gm38lag, gm39lag, gm40lag, gm41lag, gm42lag)

growth_AICc<-aictab(cand.set = growth, modnames = NULL,second.ord=TRUE,nobs=NULL,sort=TRUE)

primula %>% ggplot(aes(log(ros.areaTminus1), log(ros.area))) +
  geom_jitter(height = .02)+
  theme_classic()+
  geom_smooth(method = lmer, se=FALSE, fullrange = TRUE)+
  expand_limits(x = 0, y = 0)+
  facet_wrap(~year)

#the three way interactions are the best! No idea how to graph them. But could graph the precip x trt two way interaction...
summary(gm19)
summary(gm31)
summary(gm27) #this is the not-3 way interation one

# make a graph:
test2 <- primula %>% filter(!is.na(ros.areaTminus1),
                            !is.na(ros.area),
                            !is.na(summer.tot.precip))

test2$pred <- predict(gm27, newdata = test2, re.form=NA)


ee <- effect(c("log.ros.areaTminus1", "trt:summer.tot.precip"), gm27)
ect <- effect(term = c("log.ros.areaTminus1", "trt", "summer.tot.precip"), mod = gm27)
r <- as.data.frame(ee)

## giving up:
ggplot(test2, aes(x=log.ros.areaTminus1, y=log.ros.area, color=trt))+
  geom_point(alpha = 0.4) +
  #scale_shape_manual(values=c(1,16,), name='trt', labels=c('control','drought','irrigated'))+
  geom_smooth(method = lm, se = FALSE, fullrange = T)+
  scale_color_manual(values = c(FF[2], FF[1], FF[3]))+
  labs(x = 'Log(Rosette Size T0)', y = 'Log(Rosette Size T1)', color = "Treatment")
  


ggplot(test2, aes(x=log.ros.areaTminus1, y=pred, color=trt))+
  geom_point() +
  #scale_shape_manual(values=c(1,16,), name='trt', labels=c('control','drought','irrigated'))+
  geom_point(aes(as.numeric(pred))) +
   facet_wrap(1~year)
   scale_linetype_discrete(name='trt', labels=c('control','drought','irrigated'))+
  labs(x = 'Log(Rosette Size T0)', y = 'Log(Rosette Size T1)', color = "Treatment")+
   
### this doesnt work:
fixed <- data.frame(fixef(gm27)) #adding fixed effects so I can add to regression line
fixed
fixed[2,1] * primula$log(ros.areaTminus1) + fixed[5,1]* primula$summer.tot.precip

primula %>% 
      ggplot(aes(log(ros.areaTminus1), log(ros.area), color = trt))+
      geom_point()+ 
      scale_color_manual(values = c(FF[2], FF[1], FF[3]))+ #
      expand_limits(x = 0, y = 0)+
      geom_abline(intercept = fixed[1,1], slope =(, color = FF[2])# +#control +
      geom_abline(intercept = (fixed[1,1] + fixed[3,1]), slope =(fixed[6,1] * + fixed[2,1] + fixed[5,1]),color = FF[3]))  #drought
        
plot.m1 <- data.frame(gm27@frame, fitted.re = fitted(gm27))
fixed.m1 <- data.frame(fixef(gm27))

#there must be a better way, but finding coefficients by hand
control_int <- fixed.m1[1,1]
control_slope <- fixed.m1[2,1] + fixed.m1[5,1]

ggplot(plot.m1, aes(x = log.ros.areaTminus1., y = log.ros.area.)) + geom_point()+
  geom_line(aes(y = fitted.re, color = trt), linetype = 2) + 
  scale_x_continuous(breaks = 0:3)+ 
  geom_abline(intercept = fixed.m1[1,1], slope = fixed.m1[2,1]) 
    + theme_bw()

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
# Candidate model
fm.min <- glmer(pflower ~ log(ros.areaTminus1) + trt + (1|plot) + (1|year), data = primula, family = "binomial"(link = logit))
fm.min2 <- glmer(pflower ~ log(ros.areaTminus1)*trt + (1|plot) + (1|year), data = primula, family = "binomial")
fm.min3 <- glmer(pflower ~ log(ros.areaTminus1) + (1|plot) + (1|year), data = primula, family = "binomial")
########################################################################################
##grow season total precip
fm1 <- glmer(pflower ~ log(ros.areaTminus1) + trt + grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial"(link = logit))
fm2 <- glmer(pflower ~ log(ros.areaTminus1) + trt*grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm3 <- glmer(pflower ~ log(ros.areaTminus1)*trt+ grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm4 <- glmer(pflower ~ log(ros.areaTminus1) + grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm5 <- glmer(pflower ~ log(ros.areaTminus1)*grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm6 <- glmer(pflower ~ log(ros.areaTminus1)*trt*grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
###last years grow season precip
fm1lag <- glmer(pflower ~ log(ros.areaTminus1) + trt + grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm2lag <- glmer(pflower ~ log(ros.areaTminus1) + trt*grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm3lag <- glmer(pflower ~ log(ros.areaTminus1)*trt + grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm4lag <- glmer(pflower ~ log(ros.areaTminus1) + grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm5lag <- glmer(pflower ~ log(ros.areaTminus1)*grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm6lag <- glmer(pflower ~ log(ros.areaTminus1)*trt*grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
#grow season min temp
fm7 <- glmer(pflower ~ log(ros.areaTminus1) + trt + grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm8 <- glmer(pflower ~ log(ros.areaTminus1)*trt + grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm9 <- glmer(pflower ~ log(ros.areaTminus1) + grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm10 <- glmer(pflower ~ log(ros.areaTminus1)*grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm11 <- glmer(pflower ~ log(ros.areaTminus1) + trt*grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm12 <- glmer(pflower ~ log(ros.areaTminus1)*trt*grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
# last years grow season min temp
fm7lag <- glmer(pflower ~ log(ros.areaTminus1) + trt + grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm8lag <- glmer(pflower ~ log(ros.areaTminus1)*trt + grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm9lag <- glmer(pflower ~ log(ros.areaTminus1) + grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm10lag <- glmer(pflower ~ log(ros.areaTminus1)*grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm11lag <- glmer(pflower ~ log(ros.areaTminus1) + trt*grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm12lag <- glmer(pflower ~ log(ros.areaTminus1)*trt*grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
# grow season max temp
fm13 <- glmer(pflower ~ log(ros.areaTminus1) + trt + grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm14 <- glmer(pflower ~ log(ros.areaTminus1)*trt + grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm15 <- glmer(pflower ~ log(ros.areaTminus1) + grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm16 <- glmer(pflower ~ log(ros.areaTminus1)*grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm17 <- glmer(pflower ~ log(ros.areaTminus1) + trt*grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm18 <- glmer(pflower ~ log(ros.areaTminus1)*trt*grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
# last year grow seasons max temp
fm13lag <- glmer(pflower ~ log(ros.areaTminus1) + trt + grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm14lag <- glmer(pflower ~ log(ros.areaTminus1)*trt + grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm15lag <- glmer(pflower ~ log(ros.areaTminus1) + grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm16lag <- glmer(pflower ~ log(ros.areaTminus1)*grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm17lag <- glmer(pflower ~ log(ros.areaTminus1) + trt*grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm18lag <- glmer(pflower ~ log(ros.areaTminus1)*trt*grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
## summer max temp
fm20 <- glmer(pflower ~ log(ros.areaTminus1) + trt + summer.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm21 <- glmer(pflower ~ log(ros.areaTminus1) + trt*summer.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm22 <- glmer(pflower ~ log(ros.areaTminus1) + summer.mean.max.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm23 <- glmer(pflower ~ log(ros.areaTminus1)*trt + summer.mean.max.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm24 <- glmer(pflower ~ log(ros.areaTminus1)*summer.mean.max.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm19 <- glmer(pflower ~ log(ros.areaTminus1)*trt*summer.mean.max.temp + (1|plot) + (1|year), data = primula, family = "binomial")
# last year's summer max temp
fm19lag <- glmer(pflower ~ log(ros.areaTminus1) + trt + summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm20lag <- glmer(pflower ~ log(ros.areaTminus1) + trt*summer.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm21lag <- glmer(pflower ~ log(ros.areaTminus1) + summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm22lag <- glmer(pflower ~ log(ros.areaTminus1)*trt + summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm23lag <- glmer(pflower ~ log(ros.areaTminus1)*summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm24lag <- glmer(pflower ~ log(ros.areaTminus1)*trt*summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
# 2 years ago summer max temp
fm25lag <- glmer(pflower ~ log(ros.areaTminus1) + trt + summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm26lag <- glmer(pflower ~ log(ros.areaTminus1) + trt*summer.max.temp.2yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
fm27lag <- glmer(pflower ~ log(ros.areaTminus1) + summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm28lag <- glmer(pflower ~ log(ros.areaTminus1)*trt + summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm29lag <- glmer(pflower ~ log(ros.areaTminus1)*summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm30lag <- glmer(pflower ~ log(ros.areaTminus1)*trt*summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
# summer min temp
fm20 <- glmer(pflower ~ log(ros.areaTminus1) + trt + summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm21 <- glmer(pflower ~ log(ros.areaTminus1) + trt*summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm22 <- glmer(pflower ~ log(ros.areaTminus1) + summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm23 <- glmer(pflower ~ log(ros.areaTminus1)*trt + summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm24 <- glmer(pflower ~ log(ros.areaTminus1)*summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
fm25 <- glmer(pflower ~ log(ros.areaTminus1)*trt*summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
# summer min temp lagged
fm31lag <- glmer(pflower ~ log(ros.areaTminus1) + trt + summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm32lag <- glmer(pflower ~ log(ros.areaTminus1) + trt*summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm33lag <- glmer(pflower ~ log(ros.areaTminus1) + summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm34lag <- glmer(pflower ~ log(ros.areaTminus1)*trt + summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm35lag <- glmer(pflower ~ log(ros.areaTminus1)*summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm36lag <- glmer(pflower ~ log(ros.areaTminus1)*trt*summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
# summer precip
fm26 <- glmer(pflower ~ log(ros.areaTminus1) + trt + summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm27 <- glmer(pflower ~ log(ros.areaTminus1) + trt*summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm28 <- glmer(pflower ~ log(ros.areaTminus1) + summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm29 <- glmer(pflower ~ log(ros.areaTminus1)*trt + summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm30 <- glmer(pflower ~ log(ros.areaTminus1)*summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
fm31 <- glmer(pflower ~ log(ros.areaTminus1)*trt*summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
#summer precip lag
fm37lag <- glmer(pflower ~ log(ros.areaTminus1) + trt + summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm38lag <- glmer(pflower ~ log(ros.areaTminus1) + trt*summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm39lag <- glmer(pflower ~ log(ros.areaTminus1) + summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm40lag <- glmer(pflower ~ log(ros.areaTminus1)*trt + summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm41lag <- glmer(pflower ~ log(ros.areaTminus1)*summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
fm42lag <- glmer(pflower ~ log(ros.areaTminus1)*trt*summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
#
fm32 <- glmer(pflower ~ log(ros.areaTminus1) + trt + winter.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm33 <- glmer(pflower ~ log(ros.areaTminus1) + trt*winter.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm34 <- glmer(pflower ~ log(ros.areaTminus1) + winter.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
#
fm35 <- glmer(pflower ~ log(ros.areaTminus1) + trt + winter.tot.precip+ (1|plot) + (1|year), data = primula, family = "binomial")
fm36 <- glmer(pflower ~ log(ros.areaTminus1) + trt*winter.tot.precip+ (1|plot) + (1|year), data = primula, family = "binomial")
fm37 <- glmer(pflower ~ log(ros.areaTminus1) + winter.tot.precip+ (1|plot) + (1|year), data = primula, family = "binomial")
#
fm38 <- glmer(pflower ~ log(ros.areaTminus1) + trt + winter.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm39 <- glmer(pflower ~ log(ros.areaTminus1) + trt*winter.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
fm40 <- glmer(pflower ~ log(ros.areaTminus1) + winter.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")

###
flowering1 <- lst(fm.min, fm.min2, fm.min3, fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, fm14, fm15, fm16, fm17, fm18, fm19, fm20, fm21, fm22, fm23, fm24, fm25, fm26, fm27, fm28, fm29, fm30, fm31, fm32, fm33, fm34, fm35, fm36, fm37, fm38, fm39, fm40, fm1lag, fm2lag, fm3lag, fm4lag, fm5lag, fm6lag, fm7lag, fm8lag, fm8lag, fm9lag, fm10lag, fm11lag, fm12lag, fm13lag, fm14lag, fm15lag, fm16lag, fm17lag, fm18lag, fm19lag, fm20lag, fm21lag, fm22lag, fm23lag, fm24lag, fm25lag, fm26lag, fm27lag, fm28lag, fm29lag, fm30lag, fm31lag, fm32lag, fm33lag, fm34lag, fm35lag, fm36lag, fm37lag, fm38lag, fm38lag, fm39lag, fm40lag, fm41lag, fm42lag)

flowering1<-aictab(cand.set = growth, modnames = NULL,second.ord=TRUE,nobs=NULL,sort=TRUE)

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

# Candidate model
sm.min <- glmer(psurvival ~ log(ros.areaTminus1) + trt + (1|plot) + (1|year), data = primula, family = "binomial"(link = logit))
sm.min2 <- glmer(pflower ~ log(ros.areaTminus1)*trt + (1|plot) + (1|year), data = primula, family = "binomial")
sm.min3 <- glmer(pflower ~ log(ros.areaTminus1) + (1|plot) + (1|year), data = primula, family = "binomial")
########################################################################################
##grow season total precip
sm1 <- glmer(pflower ~ log(ros.areaTminus1) + trt + grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial"(link = logit))
sm2 <- glmer(pflower ~ log(ros.areaTminus1) + trt*grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm3 <- glmer(pflower ~ log(ros.areaTminus1)*trt+ grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm4 <- glmer(pflower ~ log(ros.areaTminus1) + grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm5 <- glmer(pflower ~ log(ros.areaTminus1)*grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm6 <- glmer(pflower ~ log(ros.areaTminus1)*trt*grow.season.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
###last years grow season precip
sm1lag <- glmer(pflower ~ log(ros.areaTminus1) + trt + grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm2lag <- glmer(pflower ~ log(ros.areaTminus1) + trt*grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm3lag <- glmer(pflower ~ log(ros.areaTminus1)*trt + grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm4lag <- glmer(pflower ~ log(ros.areaTminus1) + grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm5lag <- glmer(pflower ~ log(ros.areaTminus1)*grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm6lag <- glmer(pflower ~ log(ros.areaTminus1)*trt*grow.season.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
#grow season min temp
sm7 <- glmer(pflower ~ log(ros.areaTminus1) + trt + grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm8 <- glmer(pflower ~ log(ros.areaTminus1)*trt + grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm9 <- glmer(pflower ~ log(ros.areaTminus1) + grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm10 <- glmer(pflower ~ log(ros.areaTminus1)*grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm11 <- glmer(pflower ~ log(ros.areaTminus1) + trt*grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm12 <- glmer(pflower ~ log(ros.areaTminus1)*trt*grow.season.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
# last years grow season min temp
sm7lag <- glmer(pflower ~ log(ros.areaTminus1) + trt + grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm8lag <- glmer(pflower ~ log(ros.areaTminus1)*trt + grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm9lag <- glmer(pflower ~ log(ros.areaTminus1) + grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm10lag <- glmer(pflower ~ log(ros.areaTminus1)*grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm11lag <- glmer(pflower ~ log(ros.areaTminus1) + trt*grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm12lag <- glmer(pflower ~ log(ros.areaTminus1)*trt*grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
# grow season max temp
sm13 <- glmer(pflower ~ log(ros.areaTminus1) + trt + grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm14 <- glmer(pflower ~ log(ros.areaTminus1)*trt + grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm15 <- glmer(pflower ~ log(ros.areaTminus1) + grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm16 <- glmer(pflower ~ log(ros.areaTminus1)*grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm17 <- glmer(pflower ~ log(ros.areaTminus1) + trt*grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm18 <- glmer(pflower ~ log(ros.areaTminus1)*trt*grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
# last year grow seasons max temp
sm13lag <- glmer(pflower ~ log(ros.areaTminus1) + trt + grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm14lag <- glmer(pflower ~ log(ros.areaTminus1)*trt + grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm15lag <- glmer(pflower ~ log(ros.areaTminus1) + grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm16lag <- glmer(pflower ~ log(ros.areaTminus1)*grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm17lag <- glmer(pflower ~ log(ros.areaTminus1) + trt*grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm18lag <- glmer(pflower ~ log(ros.areaTminus1)*trt*grow.season.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
## summer max temp
sm20 <- glmer(pflower ~ log(ros.areaTminus1) + trt + summer.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm21 <- glmer(pflower ~ log(ros.areaTminus1) + trt*summer.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm22 <- glmer(pflower ~ log(ros.areaTminus1) + summer.mean.max.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm23 <- glmer(pflower ~ log(ros.areaTminus1)*trt + summer.mean.max.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm24 <- glmer(pflower ~ log(ros.areaTminus1)*summer.mean.max.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm19 <- glmer(pflower ~ log(ros.areaTminus1)*trt*summer.mean.max.temp + (1|plot) + (1|year), data = primula, family = "binomial")
# last year's summer max temp
sm19lag <- glmer(pflower ~ log(ros.areaTminus1) + trt + summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm20lag <- glmer(pflower ~ log(ros.areaTminus1) + trt*summer.max.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm21lag <- glmer(pflower ~ log(ros.areaTminus1) + summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm22lag <- glmer(pflower ~ log(ros.areaTminus1)*trt + summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm23lag <- glmer(pflower ~ log(ros.areaTminus1)*summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm24lag <- glmer(pflower ~ log(ros.areaTminus1)*trt*summer.max.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
# 2 years ago summer max temp
sm25lag <- glmer(pflower ~ log(ros.areaTminus1) + trt + summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm26lag <- glmer(pflower ~ log(ros.areaTminus1) + trt*summer.max.temp.2yearlag+ (1|plot) + (1|year), data = primula, family = "binomial")
sm27lag <- glmer(pflower ~ log(ros.areaTminus1) + summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm28lag <- glmer(pflower ~ log(ros.areaTminus1)*trt + summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm29lag <- glmer(pflower ~ log(ros.areaTminus1)*summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm30lag <- glmer(pflower ~ log(ros.areaTminus1)*trt*summer.max.temp.2yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
# summer min temp
sm20 <- glmer(pflower ~ log(ros.areaTminus1) + trt + summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm21 <- glmer(pflower ~ log(ros.areaTminus1) + trt*summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm22 <- glmer(pflower ~ log(ros.areaTminus1) + summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm23 <- glmer(pflower ~ log(ros.areaTminus1)*trt + summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm24 <- glmer(pflower ~ log(ros.areaTminus1)*summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
sm25 <- glmer(pflower ~ log(ros.areaTminus1)*trt*summer.mean.min.temp + (1|plot) + (1|year), data = primula, family = "binomial")
# summer min temp lagged
sm31lag <- glmer(pflower ~ log(ros.areaTminus1) + trt + summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm32lag <- glmer(pflower ~ log(ros.areaTminus1) + trt*summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm33lag <- glmer(pflower ~ log(ros.areaTminus1) + summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm34lag <- glmer(pflower ~ log(ros.areaTminus1)*trt + summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm35lag <- glmer(pflower ~ log(ros.areaTminus1)*summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm36lag <- glmer(pflower ~ log(ros.areaTminus1)*trt*summer.min.temp.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
# summer precip
sm26 <- glmer(pflower ~ log(ros.areaTminus1) + trt + summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm27 <- glmer(pflower ~ log(ros.areaTminus1) + trt*summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm28 <- glmer(pflower ~ log(ros.areaTminus1) + summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm29 <- glmer(pflower ~ log(ros.areaTminus1)*trt + summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm30 <- glmer(pflower ~ log(ros.areaTminus1)*summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
sm31 <- glmer(pflower ~ log(ros.areaTminus1)*trt*summer.tot.precip + (1|plot) + (1|year), data = primula, family = "binomial")
#summer precip lag
sm37lag <- glmer(pflower ~ log(ros.areaTminus1) + trt + summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm38lag <- glmer(pflower ~ log(ros.areaTminus1) + trt*summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm39lag <- glmer(pflower ~ log(ros.areaTminus1) + summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm40lag <- glmer(pflower ~ log(ros.areaTminus1)*trt + summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm41lag <- glmer(pflower ~ log(ros.areaTminus1)*summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
sm42lag <- glmer(pflower ~ log(ros.areaTminus1)*trt*summer.precip.1yearlag + (1|plot) + (1|year), data = primula, family = "binomial")
#
sm32 <- glmer(pflower ~ log(ros.areaTminus1) + trt + winter.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm33 <- glmer(pflower ~ log(ros.areaTminus1) + trt*winter.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm34 <- glmer(pflower ~ log(ros.areaTminus1) + winter.mean.min.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
#
sm35 <- glmer(pflower ~ log(ros.areaTminus1) + trt + winter.tot.precip+ (1|plot) + (1|year), data = primula, family = "binomial")
sm36 <- glmer(pflower ~ log(ros.areaTminus1) + trt*winter.tot.precip+ (1|plot) + (1|year), data = primula, family = "binomial")
sm37 <- glmer(pflower ~ log(ros.areaTminus1) + winter.tot.precip+ (1|plot) + (1|year), data = primula, family = "binomial")
#
sm38 <- glmer(pflower ~ log(ros.areaTminus1) + trt + winter.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm39 <- glmer(pflower ~ log(ros.areaTminus1) + trt*winter.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm40 <- glmer(pflower ~ log(ros.areaTminus1) + winter.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")

###
survival1 <- lst(sm.min, sm.min2, sm.min3, sm1, sm2, sm3, sm4, sm5, sm6, sm7, sm8, sm9, sm10, sm11, sm12, sm13, sm14, sm15, sm16, sm17, sm18, sm19, sm20, sm21, sm22, sm23, sm24, sm25, sm26, sm27, sm28, sm29, sm30, sm31, sm32, sm33, sm34, sm35, sm36, sm37, sm38, sm39, sm40, sm1lag, sm2lag, sm3lag, sm4lag, sm5lag, sm6lag, sm7lag, sm8lag, sm8lag, sm9lag, sm10lag, sm11lag, sm12lag, sm13lag, sm14lag, sm15lag, sm16lag, sm17lag, sm18lag, sm19lag, sm20lag, sm21lag, sm22lag, sm23lag, sm24lag, sm25lag, sm26lag, sm27lag, sm28lag, sm29lag, sm30lag, sm31lag, sm32lag, sm33lag, sm34lag, sm35lag, sm36lag, sm37lag, sm38lag, sm38lag, sm39lag, sm40lag, sm41lag, sm42lag)

survival1<-aictab(cand.set = growth, modnames = NULL,second.ord=TRUE,nobs=NULL,sort=TRUE)
######################################################################################################################################################################
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


