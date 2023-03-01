#read
library(tidyverse)
library(lubridate)
library(AICcmodavg)
library(lme4)
library(nlme)
library(car)
library(wesanderson) # for colors

theme_set(theme_classic()) #for ggplot
FF <- wes_palettes$FantasticFox1
Z <- wes_palettes$Zissou1
R <- wes_palettes$Royal2
I <- wes_palettes$IsleofDogs1
################################################################################

# Add in demography data:
source("./scripts/dode_allyears_cleaned.R")

#add in tags in clipping experiment

clip <- read.csv("./data/2021_Primula_Clipping.csv")
clip <- clip %>% 
  select(1:8, -c(coor, date.of.clipping)) %>% 
  mutate(tag = as.character(tag),
         plot = as.character(plot),
         initial.flower = no.flowers.at.start +no.buds.at.start)

# for now I'm just using both treatments as a factor, could also do sm instead of trt
#########################################################################
#adding in soil moisture data
sm <- read.csv("C:/Users/Jenna/Dropbox/Williams' Lab/Students/Jenna/loggerdata/cleaned.data/SM_aprjun15avg_allyears.csv")
sm <- sm %>% mutate(plot = as.character(plot),
                    year = as.character(year))

# #is this the sm I want? just one value for each year?
# #does soil moisture depend on treatment in 2021?
# ggplot(data = subset(sm, year == "2021"), aes(trt, sm_avg))+
#   geom_boxplot()
# ggplot(data = subset(sm, year == "2022"), aes(trt, sm_avg))+
#   geom_boxplot()
# ggplot(data = sm, aes(trt, sm_avg, color= trt))+
#   geom_boxplot()+
#   facet_wrap(~year)+
#   scale_color_manual(values = c(Z[3], Z[5], Z[1]))+
#   labs(y = "Average Growing Season Soil Moisture (VWC)", x = "")+
#   geom_point(alpha = .25)
# #I've solved some of the problems with outliers
# # Moving forward: 5 and 7 seem too high in 2022! whats up with that?? here's a df without them:
# test <- subset(sm, plot!="5"|year!="2022")
# test <- subset(test, plot!="7"|year!="2022")
# ggplot(data = test, aes(trt, sm_avg, color= trt))+
#   geom_boxplot()+
#   facet_wrap(~year)+
#   scale_color_manual(values = c(Z[3], Z[5], Z[1]))+
#   labs(y = "Average Growing Season Soil Moisture (VWC)", x = "")+
#   geom_point(alpha = .25)
# 
# #how does each plot change per year?
# ggplot(data = sm, aes(year, sm_avg, color= trt))+
#   geom_line()+
#   facet_wrap(~plot)+
#   scale_color_manual(values = c(Z[3], Z[5], Z[1]))+
#   labs(y = "Average Growing Season Soil Moisture (VWC)", x = "")
# 
# 
# m1 <- lm(sm_avg ~ trt*year, data = sm)
# m2 <- lm(sm_avg ~ trt, data = subset(sm, year == "2021"))
# m3 <- lm(sm_avg ~ trt, data = subset(sm, year == "2022"))
# summary(m1)
# summary(m2)
# summary(m3)
#adding in hand measurements from 2022 - in the future could have a seperate cleaning script
# hand <- read.csv("C:/Users/Jenna/Dropbox/Williams' Lab/Cowichan IDE/Data & Plot info/SoilMoistureHandMeasurements/2022_IDE_SoilMoisture_HandMeasurements.csv")
# plots_trt<-read.csv("/Users/Jenna/Dropbox/Williams' Lab/Cowichan IDE/Data & Plot Info/IDE_plotinfo.csv", header=T)
# plots_trt <- plots_trt %>% 
#   rename(Plot = plot)
# hand <- left_join(hand,plots_trt, by = "Plot")
# hand <- hand %>% 
#   unite("date.time", Date, Time, sep=" ", remove = F)
# hand$date.time <- parse_date_time(hand$date.time, "m/d/Y %H:%M")
# 
# hand.test <- hand %>% 
#   group_by(date.time, Plot) %>% 
#   summarize(mean.sm = mean(soil.moisture_VWC, na.rm = T))
# hand.test <-   left_join(hand.test, plots_trt, by = "Plot")
# ggplot(hand, aes(as.factor(Plot), soil.moisture_VWC, color =trt))+
#   geom_boxplot()+
#   facet_wrap(~as.factor(date.time))
# april <- hand.test %>% 
#   mutate(month = month(date.time)) %>% 
#   filter(month == "4")
# ggplot(april, aes(trt, mean.sm))+
#   geom_boxplot()
# 
# m1 <- lm(soil.moisture_VWC~trt+date.time, data = hand)
# summary(m1)
# m2 <- lm(mean.sm~trt*date.time, data = hand.test)
# summary(m2)
#########################################################################



#Sletvold paper: 
# fruitset  = proportion of flowers developing into mature fruits. Should I try this?
# adding in seed csv from 2021:
seeds21 <- read.csv("C:/Users/Jenna/Dropbox/Williams' Lab/Cowichan IDE/Cowichan_DemographyData/Dodecatheon/2021_Dodecatheon_Seed_Counts.csv")

#In 2021, which flowers made it to being a capsule?
seeds21 <- subset(seeds21, tag != "1556") # not an experiment plant
seeds21 <- subset(seeds21, tag != 1998) # not an experiment plant
seeds21 <- subset(seeds21, tag != 1813)
seeds21 <- seeds21 %>% 
  mutate(pseeds = if_else(no.seeds > 0, 1, 0),
         tag = as.character(tag),
         plot = as.character(plot))
seeds21$pseeds[is.na(seeds21$pseeds)] <- 1 #assuming the plants where we couldn't count capsules seperately had all their capsules contribute to seed total


cap <- seeds21 %>% group_by(tag, plot) %>% 
  summarize(no.success.cap = sum(pseeds))

remove(seeds21)
#adding cap to the clip df

clip <- left_join(clip, cap)
remove(cap)
clip$no.success.cap[is.na(clip$no.success.cap)] <- 0
clip$initial.flower[clip$tag == 1490] <- 6
clip$initial.flower[clip$tag == 5769] <- 5
clip$initial.flower[clip$tag == 962] <- 6
clip$initial.flower[clip$tag == 5746] <- 5
clip$no.success.cap[clip$tag == 540] <- 4
clip$no.success.cap[clip$tag == 693] <- 2

############################################################################
#add in clipping experiment treatments

clip <-  left_join(clip, Dodecatheon, by = c("tag", "plot")) %>% 
  filter(year == 2021) %>% 
  mutate(fruitset = no.success.cap/initial.flower) %>% 
  mutate(pflower = as.factor(pflower))
# these are all of the tags in the clipping experiment for 2021 and 2022
#add soil moisture:
clip <- left_join(clip, sm)

#subsetting - get rid of 12 and 14 (only 3 observations per plot - maybe this will help?)

clip.sub <- clip %>% filter(plot != "12") %>% 
  filter(plot != "14")
################################################################################
# First thing's first - make a bunch of graphs!

#growth
ggplot(clip, aes(x = log.ros.area, y = log.ros.areaT1))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  facet_wrap(~plot)
ggplot(clip, aes(x = log.ros.area, y = log.ros.areaT1))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  facet_wrap(~trt)
ggplot(clip, aes(x = log.ros.area, y = log.ros.areaT1, color = treatment))+
  geom_point(aes(size = fruitset))+
  geom_smooth(method = "lm", se = F)+
  facet_wrap(~trt)

#pflower:
ggplot(clip, aes(x = log.ros.area, y = pflowerT1))+
  geom_point()+
  geom_smooth(method = "glm", method.args = list(family = binomial), se = F)+
  facet_wrap(~plot)
ggplot(clip, aes(x = log.ros.area, y = pflowerT1))+
  geom_point(aes(size = fruitset))+
  geom_smooth(method = "glm", method.args = list(family = binomial), se = F)+
  facet_wrap(~trt)
ggplot(clip, aes(x = log.ros.area, y = pflowerT1))+
  geom_point(aes(size = fruitset, color = trt))+
  geom_smooth(method = "glm", method.args = list(family = binomial), se = F)+
  facet_wrap(~treatment)


#survival
ggplot(clip, aes(x = log.ros.area, y = psurvivalT1))+
  geom_point()+
  geom_smooth(method = "glm", method.args = list(family = binomial), se = F)+
  facet_wrap(~plot)


#no.flowers
ggplot(clip, aes(x = log.ros.area, y = no.flowersT1))+
  geom_point()+
  geom_smooth(method = "glm", method.args = list(family = poisson), se = F)+
  facet_wrap(~plot)
ggplot(clip, aes(x = log.ros.area, y = no.flowersT1))+
  geom_point()+
  geom_smooth(method = "glm", method.args = list(family = poisson), se = F)+
  facet_wrap(~trt)
ggplot(clip, aes(x = log.ros.area, y = no.flowersT1))+
  geom_point()+
  geom_smooth(method = "glm", method.args = list(family = poisson), se = F)+
  facet_wrap(~treatment)
ggplot(clip, aes(x = treatment, y = no.flowersT1))+
  geom_boxplot()
################################################################################
# does fruitset differ between IDE treatments?

ggplot(clip, aes(x = trt, y = fruitset))+
  geom_boxplot() #irrigated plots had higher fruitset
ggplot(clip, aes(x = treatment, y = fruitset))+
  geom_boxplot() +
  geom_point()
hist(clip$fruitset)
################################################################################
#GROWTH



ggplot(clip.sub, aes(x = log.ros.area, y = log.ros.areaT1))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  facet_wrap(~plot)
ggplot(clip.sub, aes(x = log.ros.area, y = log.ros.areaT1))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  facet_wrap(~trt)
ggplot(clip.sub, aes(x = log.ros.area, y = log.ros.areaT1))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  facet_wrap(~treatment)

#with random effects
gnull<- lmer(log.ros.areaT1 ~ log.ros.area +(1|plot), data = clip.sub, REML = F)
gnull1<- lmer(log.ros.areaT1 ~ log.ros.area + treatment + (1|plot), data = clip.sub, REML= F)
anova(gnull, gnull1, test="Chisq") #this is LRT?
summary(gnull1)
# this one is marginally significant - could look into the tiny ones/the ones that shrank - these seem strange

#try regular lm w/o mixed effects
greg<- lm(log.ros.areaT1 ~ log.ros.area, data = clip)
greg1<- lm(log.ros.areaT1 ~ log.ros.area +trt, data = clip)
anova(greg, greg1)
#nope null model is best





#build models - not sure if model selection is the right thing here
gnull<- lmer(log.ros.areaT1 ~ log.ros.area +(1|plot), data = clip, REML= F)
gnull1 <- lmer(log.ros.areaT1 ~ log.ros.area + trt+ (1|plot), data = clip, REML= F)
gnull2 <- lmer(log.ros.areaT1 ~ log.ros.area +treatment + (1|plot), data = clip, REML= F)
gnull3 <- lmer(log.ros.areaT1 ~ log.ros.area +fruitset + (1|plot), data = clip, REML= F)
g <- lmer(log.ros.areaT1 ~ log.ros.area + treatment + trt + (1|plot), data = clip, REML= F)
g1 <- lmer(log.ros.areaT1 ~ log.ros.area*treatment + trt + (1|plot), data = clip, REML= F)
g2 <- lmer(log.ros.areaT1 ~ log.ros.area*trt + treatment + (1|plot), data = clip, REML= F)
g3 <- lmer(log.ros.areaT1 ~ log.ros.area + trt*treatment + (1|plot), data = clip,  REML= F)
g4 <- lmer(log.ros.areaT1 ~ log.ros.area + fruitset + trt + (1|plot), data = clip, REML= F)
g5 <- lmer(log.ros.areaT1 ~ log.ros.area*fruitset + sm_avg + (1|plot), data = clip, REML= F)
g6 <- lmer(log.ros.areaT1 ~ log.ros.area*sm_avg + fruitset + (1|plot), data = clip, REML= F)
g7 <- lmer(log.ros.areaT1 ~ log.ros.area + sm_avg*fruitset + (1|plot), data = clip,  REML= F)


growth <- mget(ls(pattern = "^g"))
growth_AICc<-aictab(cand.set = growth, modnames = NULL,second.ord=TRUE,nobs=NULL,sort=TRUE)
#nope no effect on growth - gnull is best
# #how to evaluate model: likelihood ratio test, test it against null model
# size <- lmer(log.ros.areaT1 ~ log.ros.area* treatment *trt + (1|plot), data = clip, REML = F)
# anova(gnull,size,test="Chisq")
# anova(gnull,g1,test="Chisq")
# summary(size)
# 
# #this is what the brassica paper used for their mixed effect models
# car::Anova(size, type="III")
# Anova(size) #type 2

#using nmle. this doesnt work yet
# m1 <- lme(log.ros.areaT1~log.ros.area+ treatment,random=~1|plot,data=clip)
# summary(m1)
# anova(m1)
# car::Anova(m1, type="III")


#g3 is the ideal model - I'd hope that the effect of the clipping treatment depends on the climate treatment! Make a graph with this one and go from there!

# summary(g3)
# Anova(g3, type = "III")
# # making a graph for g:
# pred.cont.none <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), treatment = "none", trt = "control") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
# pred.cont.none <- cbind(as.data.frame(predict(g3, newdata = pred.cont.none, re.form=~0)), pred.cont.none)#re.form = ~0 tells it to not include random effects
# colnames(pred.cont.none)[1] = "pred.y"
# 
# pred.irr.none <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), treatment = "none", trt = "irrigated") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
# pred.irr.none <- cbind(as.data.frame(predict(g3, newdata = pred.irr.none, re.form=~0)), pred.irr.none)#re.form = ~0 tells it to not include random effects
# colnames(pred.irr.none)[1] = "pred.y"
# 
# pred.dr.none <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), treatment = "none", trt = "drought") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
# pred.dr.none <- cbind(as.data.frame(predict(g3, newdata = pred.dr.none, re.form=~0)), pred.dr.none)#re.form = ~0 tells it to not include random effects
# colnames(pred.dr.none)[1] = "pred.y"
# 
# pred.cont.half <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), treatment = "half", trt = "control") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
# pred.cont.half <- cbind(as.data.frame(predict(g3, newdata = pred.cont.half, re.form=~0)), pred.cont.half)#re.form = ~0 tells it to not include random effects
# colnames(pred.cont.half)[1] = "pred.y"
# 
# pred.irr.half <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), treatment = "half", trt = "irrigated") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
# pred.irr.half <- cbind(as.data.frame(predict(g3, newdata = pred.irr.half, re.form=~0)), pred.irr.half)#re.form = ~0 tells it to not include random effects
# colnames(pred.irr.half)[1] = "pred.y"
# 
# pred.dr.half <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), treatment = "half", trt = "drought") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
# pred.dr.half <- cbind(as.data.frame(predict(g3, newdata = pred.dr.half, re.form=~0)), pred.dr.half)#re.form = ~0 tells it to not include random effects
# colnames(pred.dr.half)[1] = "pred.y"
# 
# pred.cont.all <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), treatment = "all", trt = "control") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
# pred.cont.all <- cbind(as.data.frame(predict(g3, newdata = pred.cont.all, re.form=~0)), pred.cont.all)#re.form = ~0 tells it to not include random effects
# colnames(pred.cont.all)[1] = "pred.y"
# 
# pred.irr.all <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), treatment = "all", trt = "irrigated") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
# pred.irr.all <- cbind(as.data.frame(predict(g3, newdata = pred.irr.all, re.form=~0)), pred.irr.all)#re.form = ~0 tells it to not include random effects
# colnames(pred.irr.all)[1] = "pred.y"
# 
# pred.dr.all <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), treatment = "all", trt = "drought") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
# pred.dr.all <- cbind(as.data.frame(predict(g3, newdata = pred.dr.all, re.form=~0)), pred.dr.all)#re.form = ~0 tells it to not include random effects
# colnames(pred.dr.all)[1] = "pred.y"
# 
# ## graphs
# 
# ggplot(subset(clip, trt == "control"), aes(x=log.ros.area, y=log.ros.areaT1))+
#   geom_point( size = 2) +
#   geom_line(data = pred.cont.all, aes(x = log.ros.area, y = pred.y), color = Z[3], linetype = "dotted", size = 1) +
#   geom_line(data = pred.cont.half, aes(x = log.ros.area, y = pred.y), color = Z[3], linetype = "dashed", size = 1) +  
#   geom_line(data = pred.cont.none, aes(x = log.ros.area, y = pred.y), color = Z[3], size = 2) +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 6))+
#   scale_x_continuous(expand = c(0, 0), limits = c(0, 6))+
#   geom_abline(slope=1, intercept = 0, linetype = 2, color= "grey", size = 1)
# ggplot(subset(clip, trt == "irrigated"), aes(x=log.ros.area, y=log.ros.areaT1))+
#   geom_point( size = 2) +
#   geom_line(data = pred.irr.all, aes(x = log.ros.area, y = pred.y), color = Z[1], linetype = "dotted", size = 1) +
#   geom_line(data = pred.irr.half, aes(x = log.ros.area, y = pred.y), color = Z[1], linetype = "dashed", size = 1) +  
#   geom_line(data = pred.irr.none, aes(x = log.ros.area, y = pred.y), color = Z[1], size = 2)+
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 6))+
#   scale_x_continuous(expand = c(0, 0), limits = c(0, 6))+
#   geom_abline(slope=1, intercept = 0, linetype = 2, color= "grey", size = 1)
# ggplot(subset(clip, trt == "control"), aes(x=log.ros.area, y=log.ros.areaT1))+
#   geom_point( size = 2) +
#   geom_line(data = pred.dr.all, aes(x = log.ros.area, y = pred.y), color = Z[5], linetype = "dotted", size = 1) +
#   geom_line(data = pred.dr.half, aes(x = log.ros.area, y = pred.y), color = Z[5], linetype = "dashed", size = 1) +  
#   geom_line(data = pred.dr.none, aes(x = log.ros.area, y = pred.y), color = Z[5], size = 2)+
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 6))+
#   scale_x_continuous(expand = c(0, 0), limits = c(0, 6))+
#   geom_abline(slope=1, intercept = 0, linetype = 2, color= "grey", size = 1)
# 


################################################################################
 #pflower
ggplot(clip.sub, aes(x=log.ros.area, y=pflowerT1))+
  geom_point( size = 2) +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = F)+
  facet_wrap(~trt)
ggplot(clip.sub, aes(x=log.ros.area, y=pflowerT1))+
  geom_point( size = 2) +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = F)+
  facet_wrap(~plot)
ggplot(clip.sub, aes(x=log.ros.area, y=pflowerT1))+
  geom_point( size = 2) +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = F)+
  facet_wrap(~treatment)
ggplot(clip, aes(x=log.ros.area, y=pflowerT1))+
  geom_point( size = 2) +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = F)

#with RE but subsetted
fnull <- glmer(pflowerT1 ~ log.ros.area + (1|plot), data = clip.sub, family = "binomial")
f1 <- glmer(pflowerT1 ~ log.ros.area + trt + (1|plot), data = clip.sub, family = "binomial")
anova(fnull, f1, test="Chisq") #LTR test
summary(f1)
#whelp, nope nothing improves the model

f2 <- glmer(pflowerT1 ~ log.ros.area + trt + (1|plot), data = clip.sub, family = "binomial")
f3 <- glmer(pflowerT1 ~ log.ros.area * treatment + (1|plot), data = clip.sub, family = "binomial")
f4 <- glmer(pflowerT1 ~ log.ros.area + treatment + trt+ (1|plot), data = clip.sub, family = "binomial")
f5 <- glmer(pflowerT1 ~ log.ros.area + treatment*trt+ (1|plot), data = clip.sub, family = "binomial")

f4 <- glmer(pflowerT1 ~ log.ros.area + fruitset + trt+ (1|plot), data = clip, family = "binomial")
f5 <- glmer(pflowerT1 ~ log.ros.area * fruitset + (1|plot), data = clip, family = "binomial")
f6 <- glmer(pflowerT1 ~ log.ros.area * sm_avg + (1|plot), data = clip, family = "binomial")
f7 <- glmer(pflowerT1 ~ log.ros.area + sm_avg + (1|plot), data = clip, family = "binomial")
f8 <- glmer(pflowerT1 ~ log.ros.area + sm_avg + fruitset + (1|plot), data = clip, family = "binomial")





pflower <- mget(ls(pattern = "^f"))
pflower_AICc<-aictab(cand.set = pflower, modnames = NULL,second.ord=TRUE,nobs=NULL,sort=TRUE)



#not mixed models?
freg <- glm(pflowerT1 ~ log.ros.area, data = clip, family = "binomial")
freg1 <-glm(pflowerT1 ~ log.ros.area + trt, data = clip, family = "binomial")
anova(freg, freg1, test="Chisq") #LTR test
summary(freg1)
#whelp, nope nothing improves the model

fnull4 <-glm(pflowerT1 ~ log.ros.area + sm_avg, data = clip, family = "binomial")
fnull5 <-glm(pflowerT1 ~ log.ros.area + treatment*trt, data = clip, family = "binomial")




fm <- glm(pflowerT1 ~ log.ros.area+trt*fruitset, data = clip, family = "binomial")
anova(freg,fm,test="Chisq") #LTR test
Anova(freg, type = "III")

ggplot(clip, aes(log.ros.area, fruitset))+
  geom_point()



# pred.cont.none <-  expand.grid(log.ros.area = seq(2.5, 6, by = .1), treatment = "none", trt = "control") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
# pred.cont.none <- cbind(as.data.frame(predict(fm, newdata = pred.cont.none, type = "response")), pred.cont.none)#re.form = ~0 tells it to not include random effects
# colnames(pred.cont.none)[1] = "pred.y"
# 
# pred.irr.none <-  expand.grid(log.ros.area = seq(2.5, 6, by = .1), treatment = "none", trt = "irrigated") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
# pred.irr.none <- cbind(as.data.frame(predict(fm, newdata = pred.irr.none, type = "response")), pred.irr.none)#re.form = ~0 tells it to not include random effects
# colnames(pred.irr.none)[1] = "pred.y"
# 
# pred.dr.none <-  expand.grid(log.ros.area = seq(2.5, 6, by = .1), treatment = "none", trt = "drought") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
# pred.dr.none <- cbind(as.data.frame(predict(fm, newdata = pred.dr.none, type = "response")), pred.dr.none)#re.form = ~0 tells it to not include random effects
# colnames(pred.dr.none)[1] = "pred.y"
# 
# pred.cont.half <-  expand.grid(log.ros.area = seq(2.5, 6, by = .1), treatment = "half", trt = "control") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
# pred.cont.half <- cbind(as.data.frame(predict(fm, newdata = pred.cont.half, type = "response")), pred.cont.half)#re.form = ~0 tells it to not include random effects
# colnames(pred.cont.half)[1] = "pred.y"
# 
# pred.irr.half <-  expand.grid(log.ros.area = seq(2.5, 6, by = .1), treatment = "half", trt = "irrigated") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
# pred.irr.half <- cbind(as.data.frame(predict(fm, newdata = pred.irr.half, type = "response")), pred.irr.half)#re.form = ~0 tells it to not include random effects
# colnames(pred.irr.half)[1] = "pred.y"
# 
# pred.dr.half <-  expand.grid(log.ros.area = seq(2.5, 6, by = .1), treatment = "half", trt = "drought") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
# pred.dr.half <- cbind(as.data.frame(predict(fm, newdata = pred.dr.half, type = "response")), pred.dr.half)#re.form = ~0 tells it to not include random effects
# colnames(pred.dr.half)[1] = "pred.y"
# 
# pred.cont.all <-  expand.grid(log.ros.area = seq(2.5, 6, by = .1), treatment = "all", trt = "control") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
# pred.cont.all <- cbind(as.data.frame(predict(fm, newdata = pred.cont.all, type = "response")), pred.cont.all)#re.form = ~0 tells it to not include random effects
# colnames(pred.cont.all)[1] = "pred.y"
# 
# pred.irr.all <-  expand.grid(log.ros.area = seq(2.5, 6, by = .1), treatment = "all", trt = "irrigated") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
# pred.irr.all <- cbind(as.data.frame(predict(fm, newdata = pred.irr.all, type = "response")), pred.irr.all)#re.form = ~0 tells it to not include random effects
# colnames(pred.irr.all)[1] = "pred.y"
# 
# pred.dr.all <-  expand.grid(log.ros.area = seq(2.5, 6, by = .1), treatment = "all", trt = "drought") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
# pred.dr.all <- cbind(as.data.frame(predict(fm, newdata = pred.dr.all, type = "response")), pred.dr.all)#re.form = ~0 tells it to not include random effects
# colnames(pred.dr.all)[1] = "pred.y"
# 
# ## graphs
# 
# ggplot(subset(clip, trt == "control"), aes(x=log.ros.area, y=pflowerT1))+
#   geom_point( size = 2) +
#   geom_line(data = pred.cont.all, aes(x = log.ros.area, y = pred.y), color = Z[3], size = 1) +
#   geom_line(data = pred.cont.half, aes(x = log.ros.area, y = pred.y), color = Z[3], linetype = "dashed", size = 1) +  
#   geom_line(data = pred.cont.none, aes(x = log.ros.area, y = pred.y), color = Z[3], linetype = "dotted", size = 2) +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 1))+
#   labs(x = "log(Rosette Area) in 2021", y = "Probability of Flowering in 2022") +
#   scale_x_continuous(expand = c(0, 0), limits = c(0, 6))
# ggplot(subset(clip, trt == "irrigated"), aes(x=log.ros.area, y=pflowerT1))+
#   geom_point( size = 2) +
#   geom_line(data = pred.irr.all, aes(x = log.ros.area, y = pred.y), color = Z[1], size = 1) +
#   geom_line(data = pred.irr.half, aes(x = log.ros.area, y = pred.y), color = Z[1], linetype = "dashed", size = 1) +  
#   geom_line(data = pred.irr.none, aes(x = log.ros.area, y = pred.y), color = Z[1], linetype = "dotted", size = 2)+
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 1))+
#   labs(x = "log(Rosette Area) in 2021", y = "Probability of Flowering in 2022") +
#   scale_x_continuous(expand = c(0, 0), limits = c(0, 6))
# ggplot(subset(clip, trt == "drought"), aes(x=log.ros.area, y=pflowerT1))+
#   geom_point( size = 2) +
#   geom_line(data = pred.dr.all, aes(x = log.ros.area, y = pred.y), color = Z[5], size = 1) +
#   geom_line(data = pred.dr.half, aes(x = log.ros.area, y = pred.y), color = Z[5], linetype = "dashed", size = 1) +  
#   geom_line(data = pred.dr.none, aes(x = log.ros.area, y = pred.y), color = Z[5], linetype = "dotted", size = 2)+
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 1))+
#   labs(x = "log(Rosette Area) in 2021", y = "Probability of Flowering in 2022") +
#   scale_x_continuous(expand = c(0, 0), limits = c(0, 6))

# where I'm leaving it: pflower model is weird. and not a mixed model. Probably next step is to look at it as a continuous variable.
################################################################################
#no.flowers
ggplot( subset(clip.sub, pflowerT1 == "1"), aes(x=log.ros.area, y=no.flowersT1))+
  geom_point( size = 2) +
  geom_smooth(method = "glm", method.args = list(family = poisson), se = F)+
  facet_wrap(~trt)
ggplot( subset(clip.sub, pflowerT1 == "1"), aes(x=log.ros.area, y=no.flowersT1))+
  geom_point( size = 2) +
  geom_smooth(method = "glm", method.args = list(family = poisson), se = F)+
  facet_wrap(~plot)
ggplot( subset(clip.sub, pflowerT1 == "1"), aes(x=log.ros.area, y=no.flowersT1))+
  geom_point( size = 2) +
  geom_smooth(method = "glm", method.args = list(family = poisson), se = F)+
  facet_wrap(~treatment)
#
flnull <- glmer(no.flowersT1 ~ log.ros.area + (1|plot), data = subset(clip.sub, pflowerT1 == "1"), family = "poisson")
fl1 <- glmer(no.flowersT1 ~ log.ros.area + treatment+trt + (1|plot), data = subset(clip.sub, pflowerT1 == "1"), family = "poisson")
fl2 <- glmer(no.flowersT1 ~ log.ros.area + fruitset*trt + (1|plot), data = subset(clip.sub, pflowerT1 == "1"), family = "poisson")
#these don't run, try without RE
flnull <- glm(no.flowersT1 ~ log.ros.area, data = subset(clip.sub, pflowerT1 == "1"), family = "poisson")
fl1 <- glm(no.flowersT1 ~ log.ros.area + treatment, data = subset(clip.sub, pflowerT1 == "1"), family = "poisson")
anova(flnull, fl1,test="Chisq") #LTR test


summary(fl2)
anova(flnull, fl2,test="Chisq") #LTR test
summary(flnull)
Anova(fm, type = "III")


################################################################################
#survival
## what to do with NA plants? set to zero
clip$psurvivalT1[is.na(clip$psurvivalT1)] <- "0"


ggplot(clip, aes(x=log.ros.area, y=as.numeric(psurvivalT1))) +
  geom_point( size = 2) +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = F)+
  facet_wrap(~trt)
ggplot(clip, aes(x=log.ros.area, y=as.numeric(psurvivalT1))) +
  geom_point( size = 2) +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = F)+
  facet_wrap(~treatment)

#wi
snull <- glm(as.factor(psurvivalT1) ~ log.ros.area, data = clip.sub, family = "binomial")
snull1 <- glm(as.factor(psurvivalT1) ~ log.ros.area + trt, data = clip.sub, family = "binomial")

anova(snull, snull1,test="Chisq") #LTR test


#try no mixed model

################################################################################
# Plan B! 

# calculate relative growth rate - did plants that were clipped grow more or less than plants that weren't

clip <- clip %>%
  mutate(rgr = log.ros.areaT1-log.ros.area)

clip %>% ggplot(aes(trt, rgr))+
  geom_boxplot()+
  facet_wrap(~treatment)

m1 <- lmer(rgr~trt+ (1|plot), data = clip, REML = F)
anova(m1)
summary(m1)
#drought plants grew more than control plants! and irrigated grew less than control plants? hmm that seems weird. But no effect of clipping treatment on growth 


#not using size in model - this may be weird..
#GROWTH

ggplot(clip.sub, aes(x = trt, y = log.ros.areaT1))+
  geom_boxplot()
ggplot(clip.sub, aes(x = plot, y = log.ros.areaT1, color = trt))+
  geom_boxplot()
ggplot(clip.sub, aes(x = treatment, y = log.ros.areaT1))+
  geom_boxplot()+
  facet_wrap(~trt)

#with random effects
gnull<- lmer(log.ros.areaT1 ~ trt +(1|plot), data = clip.sub, REML = F)
gnull1<- lmer(log.ros.areaT1 ~ treatment + (1|plot), data = clip.sub, REML= F)
gnull2<- lmer(log.ros.areaT1 ~ treatment + trt + (1|plot), data = clip.sub, REML= F)
anova(gnull, gnull2, test="Chisq") #this is LRT?
summary(gnull2)

#try regular lm w/o mixed effects
greg<- lm(log.ros.areaT1 ~ log.ros.area, data = clip)
greg1<- lm(log.ros.areaT1 ~ log.ros.area +trt, data = clip)
anova(greg, greg1)
#nope null model is best

#with random effects
gnull<- lmer(log.ros.areaT1 ~ trt +(1|plot), data = clip.sub, REML = F)
gnull1<- lmer(log.ros.areaT1 ~  fruitset*trt + (1|plot), data = clip.sub, REML= F)
anova(gnull, gnull1, test="Chisq") #this is LRT?
summary(gnull1)

fnull<- lmer(pflowerT1 ~ (1|plot), data = clip.sub, REML = F)
fnull1<- lmer(pflowerT1 ~ fruitset + (1|plot), data = clip.sub, REML= F)
fnull2<- lmer(pflowerT1 ~ treatment + trt + (1|plot), data = clip.sub, REML= F)
anova(fnull, fnull1, test="Chisq") #this is LRT?
summary(gnull2)

#######################################################################################
#adding in extra plants:
# need to decide what non-reproductive plants to add back back - not tiny ones at least (aim - to be at the size required to flower, but didnt? Does that seem reasonable?)
# what sizes are plants in 2021 to 2022
Dodecatheon.21 <- Dodecatheon %>% 
  filter(year == "2021")
#of the plants that flowered in 2021, what was their min size? it doesnt look like I need to set a max size
ggplot(Dodecatheon.21, aes(log.ros.area, log.ros.areaT1, color = pflower)) +
  geom_jitter()+
  facet_wrap(~trt)

min <- Dodecatheon.21 %>% 
  group_by(pflower, trt) %>% 
  summarize(min.size = min(log.ros.area, na.rm = TRUE))

ggplot(subset(Dodecatheon.21, !is.na(pflower)), aes(log.ros.area))+
  geom_histogram()+
  facet_wrap(pflower~trt)

clip2 <- Dodecatheon.21 %>% 
  filter(log.ros.area > 2) %>% 
  mutate(pflower = as.factor(pflower))
join <-  left_join(clip2, clip) %>% 
  mutate(treatment = if_else(is.na(treatment) & pflower == 0, "veg", treatment))

###
ggplot(join, aes(x = log.ros.area, y = log.ros.areaT1))+
  geom_jitter()+
  geom_smooth(method = "lm", se = F)+
  facet_wrap(~treatment)

gnull<- lmer(log.ros.areaT1 ~ trt +(1|plot), data = clip.sub, REML = F)
gnull1<- lmer(log.ros.areaT1 ~ treatment + (1|plot), data = clip.sub, REML= F)
gnull2<- lmer(log.ros.areaT1 ~ treatment + trt + (1|plot), data = clip.sub, REML= F)
anova(gnull, gnull2, test="Chisq") #this is LRT?
summary(gnull2)

#######################################################################################
test <- clip.sub %>% filter(pflower =="1") %>% 
  mutate(pseeds = if_else(fruitset == 0, 0, 1))
snull <- glm(as.factor(psurvivalT1) ~ log.ros.area, data = clip.sub, family = "binomial")

t1 <- glm(pseeds ~ log.ros.area, data = test, family = "binomial")
t2 <- glm(pseeds ~ log.ros.area+trt, data = test, family = "binomial")
t3<- glm(pseeds ~ log.ros.area*trt, data = test, family = "binomial")
anova(t1, t2, test="Chisq") #this is LR
anova(t2, t3, test="Chisq") #this is LR

summary(t2)
         