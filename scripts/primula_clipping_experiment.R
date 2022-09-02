#read
library(tidyverse)
library(AICcmodavg)
library(lme4)
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
# for now I'm jus using both treatments as a facotr, could also 

#add in clipping experiment treatments

clip <-  left_join(clip, Dodecatheon, by = "tag") %>% 
  filter(year == 2021) %>% 
  select(-plot.y) %>% 
  rename(plot = plot.x)
# these are all of the tags in the clipping experiment for 2021 and 2022
################################################################################

#build models - model selection?? Not sure which to choose
gnull<- lmer(log.ros.areaT1 ~ log.ros.area +(1|plot), data = clip, REML= F)
gnull1 <- lmer(log.ros.areaT1 ~ log.ros.area + trt+ (1|plot), data = clip, REML= F)
gnull2 <- lmer(log.ros.areaT1 ~ log.ros.area +treatment + (1|plot), data = clip, REML= F)
g <- lmer(log.ros.areaT1 ~ log.ros.area + treatment + trt + (1|plot), data = clip, REML= F)
g1 <- lmer(log.ros.areaT1 ~ log.ros.area*treatment + trt + (1|plot), data = clip, REML= F)
g2 <- lmer(log.ros.areaT1 ~ log.ros.area*trt + treatment + (1|plot), data = clip, REML= F)
g3 <- lmer(log.ros.areaT1 ~ log.ros.area + trt*treatment + (1|plot), data = clip, REML= F)
growth <- mget(ls(pattern = "^g"))
growth_AICc<-aictab(cand.set = growth, modnames = NULL,second.ord=TRUE,nobs=NULL,sort=TRUE)
summary(g)
#g3 is the ideal model - I'd hope that the effect of the cliping treatment depends on the climate treatment! Make a graph with this one and go from there!

summary(g3)
# making a graph for g:
pred.cont.none <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), treatment = "none", trt = "control") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
pred.cont.none <- cbind(as.data.frame(predict(g3, newdata = pred.cont.none, re.form=~0)), pred.cont.none)#re.form = ~0 tells it to not include random effects
colnames(pred.cont.none)[1] = "pred.y"

pred.irr.none <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), treatment = "none", trt = "irrigated") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
pred.irr.none <- cbind(as.data.frame(predict(g3, newdata = pred.irr.none, re.form=~0)), pred.irr.none)#re.form = ~0 tells it to not include random effects
colnames(pred.irr.none)[1] = "pred.y"

pred.dr.none <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), treatment = "none", trt = "drought") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
pred.dr.none <- cbind(as.data.frame(predict(g3, newdata = pred.dr.none, re.form=~0)), pred.dr.none)#re.form = ~0 tells it to not include random effects
colnames(pred.dr.none)[1] = "pred.y"

pred.cont.half <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), treatment = "half", trt = "control") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
pred.cont.half <- cbind(as.data.frame(predict(g3, newdata = pred.cont.half, re.form=~0)), pred.cont.half)#re.form = ~0 tells it to not include random effects
colnames(pred.cont.half)[1] = "pred.y"

pred.irr.half <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), treatment = "half", trt = "irrigated") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
pred.irr.half <- cbind(as.data.frame(predict(g3, newdata = pred.irr.half, re.form=~0)), pred.irr.half)#re.form = ~0 tells it to not include random effects
colnames(pred.irr.half)[1] = "pred.y"

pred.dr.half <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), treatment = "half", trt = "drought") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
pred.dr.half <- cbind(as.data.frame(predict(g3, newdata = pred.dr.half, re.form=~0)), pred.dr.half)#re.form = ~0 tells it to not include random effects
colnames(pred.dr.half)[1] = "pred.y"

pred.cont.all <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), treatment = "all", trt = "control") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
pred.cont.all <- cbind(as.data.frame(predict(g3, newdata = pred.cont.all, re.form=~0)), pred.cont.all)#re.form = ~0 tells it to not include random effects
colnames(pred.cont.all)[1] = "pred.y"

pred.irr.all <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), treatment = "all", trt = "irrigated") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
pred.irr.all <- cbind(as.data.frame(predict(g3, newdata = pred.irr.all, re.form=~0)), pred.irr.all)#re.form = ~0 tells it to not include random effects
colnames(pred.irr.all)[1] = "pred.y"

pred.dr.all <-  expand.grid(log.ros.area = seq(0.4, 6, by = .1), treatment = "all", trt = "drought") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
pred.dr.all <- cbind(as.data.frame(predict(g3, newdata = pred.dr.all, re.form=~0)), pred.dr.all)#re.form = ~0 tells it to not include random effects
colnames(pred.dr.all)[1] = "pred.y"

## graphs

ggplot(subset(clip, trt == "control"), aes(x=log.ros.area, y=log.ros.areaT1))+
  geom_point( size = 2) +
  geom_line(data = pred.cont.all, aes(x = log.ros.area, y = pred.y), color = Z[3], linetype = "dotted", size = 1) +
  geom_line(data = pred.cont.half, aes(x = log.ros.area, y = pred.y), color = Z[3], linetype = "dashed", size = 1) +  
  geom_line(data = pred.cont.none, aes(x = log.ros.area, y = pred.y), color = Z[3], size = 2) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 6))+
  geom_abline(slope=1, intercept = 0, linetype = 2, color= "grey", size = 1)
ggplot(subset(clip, trt == "irrigated"), aes(x=log.ros.area, y=log.ros.areaT1))+
  geom_point( size = 2) +
  geom_line(data = pred.irr.all, aes(x = log.ros.area, y = pred.y), color = Z[1], linetype = "dotted", size = 1) +
  geom_line(data = pred.irr.half, aes(x = log.ros.area, y = pred.y), color = Z[1], linetype = "dashed", size = 1) +  
  geom_line(data = pred.irr.none, aes(x = log.ros.area, y = pred.y), color = Z[1], size = 2)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 6))+
  geom_abline(slope=1, intercept = 0, linetype = 2, color= "grey", size = 1)
ggplot(subset(clip, trt == "control"), aes(x=log.ros.area, y=log.ros.areaT1))+
  geom_point( size = 2) +
  geom_line(data = pred.dr.all, aes(x = log.ros.area, y = pred.y), color = Z[5], linetype = "dotted", size = 1) +
  geom_line(data = pred.dr.half, aes(x = log.ros.area, y = pred.y), color = Z[5], linetype = "dashed", size = 1) +  
  geom_line(data = pred.dr.none, aes(x = log.ros.area, y = pred.y), color = Z[5], size = 2)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 6))+
  geom_abline(slope=1, intercept = 0, linetype = 2, color= "grey", size = 1)


#this is promising - plants that were clipped were bigger. Some strange things going on with the
#other treatments though
################################################################################
 #pflower
#wi
fm <- glm(pflowerT1 ~ log.ros.area + treatment + trt, data = clip, family = "binomial")
summary(fm)

pred.cont.none <-  expand.grid(log.ros.area = seq(2.5, 6, by = .1), treatment = "none", trt = "control") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
pred.cont.none <- cbind(as.data.frame(predict(fm, newdata = pred.cont.none, type = "response")), pred.cont.none)#re.form = ~0 tells it to not include random effects
colnames(pred.cont.none)[1] = "pred.y"

pred.irr.none <-  expand.grid(log.ros.area = seq(2.5, 6, by = .1), treatment = "none", trt = "irrigated") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
pred.irr.none <- cbind(as.data.frame(predict(fm, newdata = pred.irr.none, type = "response")), pred.irr.none)#re.form = ~0 tells it to not include random effects
colnames(pred.irr.none)[1] = "pred.y"

pred.dr.none <-  expand.grid(log.ros.area = seq(2.5, 6, by = .1), treatment = "none", trt = "drought") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
pred.dr.none <- cbind(as.data.frame(predict(fm, newdata = pred.dr.none, type = "response")), pred.dr.none)#re.form = ~0 tells it to not include random effects
colnames(pred.dr.none)[1] = "pred.y"

pred.cont.half <-  expand.grid(log.ros.area = seq(2.5, 6, by = .1), treatment = "half", trt = "control") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
pred.cont.half <- cbind(as.data.frame(predict(fm, newdata = pred.cont.half, type = "response")), pred.cont.half)#re.form = ~0 tells it to not include random effects
colnames(pred.cont.half)[1] = "pred.y"

pred.irr.half <-  expand.grid(log.ros.area = seq(2.5, 6, by = .1), treatment = "half", trt = "irrigated") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
pred.irr.half <- cbind(as.data.frame(predict(fm, newdata = pred.irr.half, type = "response")), pred.irr.half)#re.form = ~0 tells it to not include random effects
colnames(pred.irr.half)[1] = "pred.y"

pred.dr.half <-  expand.grid(log.ros.area = seq(2.5, 6, by = .1), treatment = "half", trt = "drought") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
pred.dr.half <- cbind(as.data.frame(predict(fm, newdata = pred.dr.half, type = "response")), pred.dr.half)#re.form = ~0 tells it to not include random effects
colnames(pred.dr.half)[1] = "pred.y"

pred.cont.all <-  expand.grid(log.ros.area = seq(2.5, 6, by = .1), treatment = "all", trt = "control") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
pred.cont.all <- cbind(as.data.frame(predict(fm, newdata = pred.cont.all, type = "response")), pred.cont.all)#re.form = ~0 tells it to not include random effects
colnames(pred.cont.all)[1] = "pred.y"

pred.irr.all <-  expand.grid(log.ros.area = seq(2.5, 6, by = .1), treatment = "all", trt = "irrigated") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
pred.irr.all <- cbind(as.data.frame(predict(fm, newdata = pred.irr.all, type = "response")), pred.irr.all)#re.form = ~0 tells it to not include random effects
colnames(pred.irr.all)[1] = "pred.y"

pred.dr.all <-  expand.grid(log.ros.area = seq(2.5, 6, by = .1), treatment = "all", trt = "drought") #this is making a set of data that the model will predict points for. size 0:6, 25th quarile of summer precip, and control plots!
pred.dr.all <- cbind(as.data.frame(predict(fm, newdata = pred.dr.all, type = "response")), pred.dr.all)#re.form = ~0 tells it to not include random effects
colnames(pred.dr.all)[1] = "pred.y"

## graphs

ggplot(subset(clip, trt == "control"), aes(x=log.ros.area, y=pflowerT1))+
  geom_point( size = 2) +
  geom_line(data = pred.cont.all, aes(x = log.ros.area, y = pred.y), color = Z[3], linetype = "dotted", size = 1) +
  geom_line(data = pred.cont.half, aes(x = log.ros.area, y = pred.y), color = Z[3], linetype = "dashed", size = 1) +  
  geom_line(data = pred.cont.none, aes(x = log.ros.area, y = pred.y), color = Z[3], size = 2) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1))+
  labs(x = "log(Rosette Area) in 2021", y = "Probability of Flowering in 2022") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 6))
ggplot(subset(clip, trt == "irrigated"), aes(x=log.ros.area, y=pflowerT1))+
  geom_point( size = 2) +
  geom_line(data = pred.irr.all, aes(x = log.ros.area, y = pred.y), color = Z[1], linetype = "dotted", size = 1) +
  geom_line(data = pred.irr.half, aes(x = log.ros.area, y = pred.y), color = Z[1], linetype = "dashed", size = 1) +  
  geom_line(data = pred.irr.none, aes(x = log.ros.area, y = pred.y), color = Z[1], size = 2)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1))+
  labs(x = "log(Rosette Area) in 2021", y = "Probability of Flowering in 2022") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 6))
ggplot(subset(clip, trt == "control"), aes(x=log.ros.area, y=pflowerT1))+
  geom_point( size = 2) +
  geom_line(data = pred.dr.all, aes(x = log.ros.area, y = pred.y), color = Z[5], linetype = "dotted", size = 1) +
  geom_line(data = pred.dr.half, aes(x = log.ros.area, y = pred.y), color = Z[5], linetype = "dashed", size = 1) +  
  geom_line(data = pred.dr.none, aes(x = log.ros.area, y = pred.y), color = Z[5], size = 2)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1))+
  labs(x = "log(Rosette Area) in 2021", y = "Probability of Flowering in 2022") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 6))



