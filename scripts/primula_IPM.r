##IPM for Primula data

#load best fit models from primula_climate_VR.r"

gm17 <- lmer(log.ros.areaT1 ~ log.ros.area + trt * grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, REML = F) ###
fm.f16 <- glmer(pflowerT1 ~ log.ros.area * pflower + grow.season.mean.max.temp+ (1|plot) + (1|year), data = primula, family = "binomial")
sm10lag <- glmer(psurvivalT1 ~ log.ros.area*grow.season.min.temp.1yearlag+ (1|plot) + (1|year), data = primula, family = "binomial") ###
noflow9 <- glm(flow.sum ~ log.ros.area + grow.season.mean.min.temp, data = subset(primula, flow.sum > 0), family = "poisson")
