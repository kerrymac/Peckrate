rm(list= ls()) # Clear the global environment before we begin

dframe1 <- read.csv(file.choose())    # Select " Peck and Pace Oct23a "



names(dframe1)
str(dframe1)
library(glmmTMB)
library(ggstatsplot)
library(plyr)
library(dplyr)
library(dotwhisker)
library(hrbrthemes)
library(car)
library(ggeffects)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(arm)
library(lme4)
library(MuMIn)
library(broom.mixed)
library(ggstance)
library(insight)
library(jtools)
library(AICcmodavg)
library(standardize)
library(bbmle)
library(TMB)
library(pscl)  #for zeroinflated models
write('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', file = "~/.Renviron", append = TRUE)
Sys.which("make")
library(ggbiplot)
library(parameters)
library(DHARMa)
library(dotwhisker)
library(broom)
library(dplyr)
library(mfx)
library(dotwhisker)
library(performance)
library(see)
library(patchwork)




dframe1$field<-as.factor(dframe1$field)
dframe1$yearf <- as.factor(dframe1$year)
dframe1$monthsf<- as.factor(dframe1$months)
dframe1$weekf<- as.factor(dframe1$weeks)
dframe1$dlpincf<- as.factor(dframe1$dlpinc)
dframe1$sheepf<- as.factor(dframe1$sheep)

dframe1$ha.s<-as.numeric(scale(dframe1$ha))
dframe1$route4.s<-as.numeric(scale(dframe1$route4))
dframe1$building.exp.s<-as.numeric(scale(dframe1$building.exp))
dframe1$flock.s<-as.numeric(scale(dframe1$flock))
dframe1$weeks.s<-as.numeric(scale(dframe1$weeks))
dframe1$temp.s<-as.numeric(scale(dframe1$temp))
dframe1$dl2tw.s<-as.numeric(scale(dframe1$dl2tw))

names(dframe1)
dframe1o <- subset (dframe1, crop != "Oats")
str(dframe1o$crop)
summary(dframe1o)

######
str(dframe1)


dframe1oc <- subset (dframe1o, crop == "Italian rye")
dframe1op <- subset (dframe1o, crop == "Mixed perennial")
dframe1op624 <- subset (dframe1o, field != "624")

dframe4 <- subset (dframe1, year == "2017/18") #201718
dframe5 <- subset (dframe1, year == "2018/19") #201819
dframe6 <- subset (dframe2, year == "2017/18") #201718



#all crops by month and year
 
ggplot(data=dframe1, mapping=aes(x=monthsf,y=peck)) +
  geom_boxplot (notch=T) +
  facet_wrap(~yearf) +  #comparing across both years  
    labs(title = "Extent of grass dieback and field selection")

ggplot(data=dframe1c, mapping=aes(x=monthsf,y=peck)) +
  geom_boxplot (notch=T) +
  facet_wrap(~yearf) +  #comparing across both years  
  labs(title = "Extent of grass dieback and field selection")


ggplot(data=dframe1o, mapping=aes(x=monthsf,y=peck)) +
  geom_boxplot (notch=T) +
  facet_wrap(~yearf) +  #comparing across both years  
  labs(title = "Extent of grass dieback and field selection")

#Mixed perennial and month

ggplot(data=dframe1o,mapping=aes(x=crop,y=peck)) + # ordered by mean and variables  between years
  geom_boxplot(notch=T)+
  facet_wrap(~monthsf,nrow=2)+
  labs(title = "swans")


 ggplot(data=dframe1o,mapping=aes(x=crop,y=pace)) + # ordered by mean and variables  between years
  geom_boxplot(notch=T)+
  facet_wrap(~monthsf,nrow=2)+
  labs(title = "swans")


e# Italian rye and seasons peck and pace

ggplot(data=dframe1c,mapping=aes(x=crop,y=peck)) + # ordered by mean and variables  between years
  geom_boxplot(notch=T)+
  facet_wrap(~monthsf,nrow=2)+
  labs(title = "swans")


ggplot(data=dframe1c,mapping=aes(x=crop,y=pace)) + # ordered by mean and variables  between years
  geom_boxplot(notch=T)+
  facet_wrap(~monthsf ,nrow=2)+
  labs(title = "swans")



ggplot(data=dframe1c,mapping=aes(x=dlpincf,y=peck)) + # ordered by mean and variables  between years
  geom_boxplot(notch=T)+
  facet_wrap(~monthsf,nrow=2)+
  labs(title = "swans")


ggplot(data=dframe1,mapping=aes(x=week,y=sward1)) + # ordered by mean and variables  between years
  geom_point(aes(colour=field)) +
  facet_wrap(~yearf,nrow=2) +
  labs(title = "swans")


ggplot(data=dframe1,mapping=aes(x=dlpincf,y=pace)) + # ordered by mean and variables  between years
  geom_point(aes(colour=field))+
  facet_wrap(~yearf,nrow=2)+
  labs(title = "swans")

data1op <- na.omit(dframe1op)
data1o <- na.omit(dframe1o)
str(data1o)


m1 <- glmmTMB(peck ~  crop*weeks.s + age  + route4.s + sector3*dlpinc*weeks.s + ha.s +   flock.s + I(dlpinc^2)  + dl2tw.s   + dlpinc  + yearf +  temp.s + sheepf + (1|date/field)  ,
              data = dframe1o, na.action=na.fail, family=poisson)
 
summary(m1)


fittedModel<-(m1top)


simulationOutput <- simulateResiduals(fittedModel = fittedModel, refit = F)  #new simulated data comparison

testUniformity(simulationOutput)

testDispersion(simulationOutput)
res <- simulateResiduals(fittedModel)
plotResiduals(res, dframe1o$route4)

testOutliers(simulationOutput, type="bootstrap")

par(mfrow = c(2,2))
plotResiduals(res, dframe1o$route4)
plotResiduals(res, dframe1o$weeks)
plotResiduals(res, dframe1o$flock)
plotResiduals(res, dframe1o$age)
plotResiduals(res, dframe1o$yearf)
plotResiduals(res, dframe1o$sector4)
plotResiduals(res, dframe1o$dl2tw)
plotResiduals(res, dframe1o$crop)
plotResiduals(res, dframe1o$ha)
plotResiduals(res, dframe1o$age)
plotResiduals(res, dframe1o$dlpinc)



simulationOutput = recalculateResiduals(simulationOutput, group = data1rs$percentilef)
testUniformity(simulationOutput)
simulationOutput = recalculateResiduals(simulationOutput, group = data1rs$weekf)
testUniformity(simulationOutput)
simulationOutput = recalculateResiduals(simulationOutput, group = data1rs$fieldf)
testUniformity(simulationOutput)

testDispersion(res)
testQuantiles(simulationOutput)





#Warning message:
  #In (function (start, objective, gradient = NULL, hessian = NULL,  :
   #               NA/NaN function evaluation
#The warning means that at some point in the fitting process, an 'illegal' set of parameter values was tried;
#this is mildly surprising and would be worth chasing down at some point. For example, it's possible that some 
#computation under/overflowed. But, provided that everything looks sensible (graphical diagnostics, parameter values, etc.),
#it's probably not something to worry about much.

dredge_m1<-dredge(m1)
weeks*route4
dredge_m1

m1top <- glmmTMB(peck ~  crop*weeks.s + age + dlpinc + flock.s + I(dlpinc^2)  + dl2tw.s   + route4.s+weeks.s  + dlpinc  + yearf  + sheepf + (1|date/field)  ,
              data = dframe1o, na.action=na.fail, family=poisson)

m1topnull <- glmmTMB(peck ~ 1 + (1|date/field)  ,
                 data = dframe1o, na.action=na.fail, family=poisson)
summary(m1topnull)

summary(m1top)
parameters(m1top)
check_collinearity(m1top)


r.squaredGLMM(m1top)




m1top <- glmmTMB(peck ~  crop*weeks + age + dlpinc + flock + I(dlpinc^2)  + dl2tw   + route4*weeks  + dlpinc  + yearf  + sheepf + (1|date/field)  ,
                 data = dframe1o, na.action=na.fail, family=poisson)



m1 <- glmmTMB(peck ~   crop*weeks.s+age  +  sector3 + route4.s*dlpinc  + dlpinc*weeks.s + dlpinc + ha.s +   flock.s + I(dlpinc^2)  + dl2tw.s   + dlpinc  + yearf +  temp.s + sheepf + (1|date/field)  ,
              data = dframe1o, na.action=na.fail, family=poisson)


summary(m1)
#predict1 <- ggpredict(m1, c("route4","weeks [5,15,25]"),

#predict1 <- ggpredict(m2, c("route4","weeks [8, 16,24]"),

#predict1 <- ggpredict(m1, c("route4 [all]","weeks [8,16,24]"),

#predict1 <- ggpredict(m1top, c("route4[5,10,15]","weeks [8,16,24]"),
                      
                      predict1 <- ggpredict(m1, c("weeks.s","crop"),
                    ci.lvl = 0.95,
                      type = "fe",
                      allow.new.levels=TRUE,
                      typical = "mean",
                      condition = NULL,
                      back.transform = TRUE,
                      ppd = FALSE,
                      vcov.fun = NULL,
                      vcov.type = NULL,
                      vcov.args = NULL,
                      interval = "confidence")


predict1

print(predict1)
plot(predict1)

Plot1 <- plot(predict1)+
  labs(title =  labs(title = "Peck Rate and Infrastructure Proximity")) +
  labs(subtitle ="Daylight increment = 7/10,Flock =50,Age = adult,\n daylight =0.45, Year = 1, Sheep = 0",
       y = "Peckrate/minute",x = "Route Exp.Index") +
  scale_colour_discrete(name="Weeks")+
  theme(plot.subtitle = element_text(size=12))+
  theme(plot.title = element_text(colour="black",size=20),legend.position ="right") +
  theme(legend.title = element_text(colour="black", size=14, face="bold"))+
  theme(legend.key.size = unit(4, 'cm'), #change legend key size
        legend.key.height = unit(2, 'cm'), #change legend key height
        legend.key.width = unit(2, 'cm'), #change legend key width
        legend.text = element_text(size=12)) +
  theme(plot.title = element_text(size=20)) +
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=18,colour = "black"))

Plot1


predict1 <- ggpredict(m1, c("weeks","crop"),
                                  ci.lvl = 0.95,
                      type = "fe",
                      allow.new.levels=TRUE,
                      typical = "mean",
                      condition = NULL,
                      back.transform = TRUE,
                      ppd = FALSE,
                      vcov.fun = NULL,
                      vcov.type = NULL,
                      vcov.args = NULL,
                      interval = "confidence")


predict1

print(predict1)
plot(predict1)
Plot1 <- plot(predict1)+
  labs(title =  labs(title = "Pace Rate and Infrastructure Proximity")) +
  labs(subtitle ="Adjusted for flock size",
       y = "Peckrate/minute",x = "Route Exp.Index") +
  scale_colour_discrete(name="Weeks")+
  theme(plot.subtitle = element_text(size=12))+
  theme(plot.title = element_text(colour="black",size=20),legend.position ="right") +
  theme(legend.title = element_text(colour="black", size=14, face="bold"))+
  theme(legend.key.size = unit(4, 'cm'), #change legend key size
        legend.key.height = unit(2, 'cm'), #change legend key height
        legend.key.width = unit(2, 'cm'), #change legend key width
        legend.text = element_text(size=12)) +
  theme(plot.title = element_text(size=20)) +
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=18,colour = "black"))

Plot1

predict1 <- ggpredict(m1, c("dlpinc"),
                      ci.lvl = 0.95,
                      type = "fe",
                      allow.new.levels=TRUE,
                      typical = "mean",
                      condition = NULL,
                      back.transform = TRUE,
                      ppd = FALSE,
                      vcov.fun = NULL,
                      vcov.type = NULL,
                      vcov.args = NULL,
                      interval = "confidence")


plot(predict1)

predict1 <- ggpredict(m1, c("weeks [8,16,24]","route4[5,10,15]"),  
                      ci.lvl = 0.95,
                      type = "fe",
                      allow.new.levels=TRUE,
                      typical = "mean",
                      condition = NULL,
                      back.transform = TRUE,
                      ppd = FALSE,
                      vcov.fun = NULL,
                      vcov.type = NULL,
                      vcov.args = NULL,
                      interval = "confidence")


predict1

print(predict1)
plot(predict1)

Plot1 <- plot(predict1)+
  labs(title =  labs(title = "Peck Rate and Infrastructure Proximity")) +
  labs(subtitle ="Daylight increment = 7/10, Year = 1, Sheep = 0",
       y = "Peckrate/minute",x = "Route Exp.Index") +
  scale_colour_discrete(name="Weeks")+
  theme(plot.subtitle = element_text(size=12))+
  theme(plot.title = element_text(colour="black",size=20),legend.position ="right") +
  theme(legend.title = element_text(colour="black", size=14, face="bold"))+
  theme(legend.key.size = unit(4, 'cm'), #change legend key size
        legend.key.height = unit(2, 'cm'), #change legend key height
        legend.key.width = unit(2, 'cm'), #change legend key width
        legend.text = element_text(size=12)) +
  theme(plot.title = element_text(size=20)) +
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=18,colour = "black"))

Plot1

obs<-seq(nrow(dframe1o))

m2 <- glmmTMB(pace ~  crop*weeks.s + age + sector3 + ha.s +  dlpinc + flock.s + I(dlpinc^2)  + dl2tw.s   + route4.s*weeks.s  + dlpinc  + yearf +  temp.s + sheepf + (1|date/field) + (1|obs)  ,
              data = dframe1o, na.action=na.fail, family=poisson)

summary(m2)

m2a <- glmmTMB(pace ~    dl2tw  + crop*weeks + age + sector3 +sheepf +  route4*weeks    +(1|date/field) + (1|obs), 
              data = dframe1o, na.action=na.fail, family=poisson)
check_collinearity(m2)
summary(m2a)

m2_dredge <-dredge(m2)

m2_dredge
data1op <- na.omit(dframe1op)
obs<-seq(nrow(data1op))


m2top <- glmmTMB(pace ~  crop*weeks.s + age  + ha.s +  flock.s + dl2tw.s + route4.s*weeks.s  + dlpinc  + yearf  + sheepf + (1|date/field) + (1|obs)  ,
              data = dframe1o, na.action=na.fail, family=poisson)
m2topa <- glmmTMB(pace ~  crop*weeks + age  + ha +   flock + dl2tw + route4*weeks  + dlpinc  + yearf  + sheepf + (1|date/field) + (1|obs)  ,
                 data = dframe1o, na.action=na.fail, family=poisson)
summary(m2top)
parameters(m2top)
r.squaredGLMM(m2top)

summary(m2topa)
parameters(m2topa)
r.squaredGLMM(m2topa)

check_collinearity(m2top)
m2topnull<- glmmTMB(pace ~ 1 + (1|date/field) + (1|obs)  ,
                  data = dframe1o, na.action=na.fail, family=poisson)
summary(m2topnull)
predict1 <- ggpredict(m2a, c("route4[5:15]","weeks[8,16,24]"),  
                      ci.lvl = 0.95,
                      type = "fe",
                      allow.new.levels=TRUE,
                      typical = "mean",
                      condition = NULL,
                      back.transform = TRUE,
                      ppd = FALSE,
                      vcov.fun = NULL,
                      vcov.type = NULL,
                      vcov.args = NULL,
                      interval = "confidence")


predict1

print(predict1)
plot(predict1)

Plot1 <- plot(predict1)+
  labs(title =  labs(title = "Pace Rate and Infrastructure Proximity")) +
  labs(subtitle ="Daylight increment = 7/10,Flock =50,Age = adult,\n daylight =0.45, Year = 1, Sheep = 0",
       y = "Pacerate/minute",x = "Route Exp.Index") +
  scale_colour_discrete(name="Weeks")+
  theme(plot.subtitle = element_text(size=12))+
  theme(plot.title = element_text(colour="black",size=20),legend.position ="right") +
  theme(legend.title = element_text(colour="black", size=14, face="bold"))+
  theme(legend.key.size = unit(4, 'cm'), #change legend key size
        legend.key.height = unit(2, 'cm'), #change legend key height
        legend.key.width = unit(2, 'cm'), #change legend key width
        legend.text = element_text(size=12)) +
  theme(plot.title = element_text(size=20)) +
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=18,colour = "black"))

Plot1



fittedModel<-(m2a)


simulationOutput <- simulateResiduals(fittedModel = fittedModel, refit = F)  #new simulated data comparison

testUniformity(simulationOutput)

testDispersion(simulationOutput)
res <- simulateResiduals(fittedModel)
plotResiduals(res, dframe1o$route4)

testOutliers(simulationOutput, type="bootstrap")

par(mfrow = c(3,4))
plotResiduals(res, dframe1o$route4)
plotResiduals(res, dframe1o$weeks)
plotResiduals(res, dframe1o$flock)
plotResiduals(res, dframe1o$age)
plotResiduals(res, dframe1o$yearf)
plotResiduals(res, dframe1o$sector4)
plotResiduals(res, dframe1o$dl2tw)
plotResiduals(res, dframe1o$crop)
plotResiduals(res, dframe1o$ha)
plotResiduals(res, dframe1o$age)
plotResiduals(res, dframe1o$dlpinc)



simulationOutput = recalculateResiduals(simulationOutput, group = data1rs$percentilef)
testUniformity(simulationOutput)
simulationOutput = recalculateResiduals(simulationOutput, group = data1rs$weekf)
testUniformity(simulationOutput)
simulationOutput = recalculateResiduals(simulationOutput, group = data1rs$fieldf)
testUniformity(simulationOutput)

testDispersion(res)
testQuantiles(simulationOutput)









testCategorical(simulationOutput, catPred = dframe1op$yearf)# tests residuals against a categorical predictor

simulationOutput <- simulateResiduals(fittedModel = fittedModel, n = 250, use.u = T)
simulationOutput <- simulateResiduals(fittedModel = fittedModel, n = 250, re.form = T)

# for 
simulationOutput = recalculateResiduals(simulationOutput, group = dframe1op$date)
testUniformity(simulationOutput)
simulationOutput = recalculateResiduals(simulationOutput, group = dframe1op$weekf)
testUniformity(simulationOutput)
simulationOutput = recalculateResiduals(simulationOutput, group = dframe1op$fieldf)
testUniformity(simulationOutput)


testUniformity(simulationOutput)
##Dont forget to recalc simulation output back to whole model




simulationOutput = recalculateResiduals(simulationOutput, group = dframe1$fieldf)
testUniformity(simulationOutput)
plot(simulationOutput)


simulationOutput = recalculateResiduals(simulationOutput, group = dframe1$percentilef)
testUniformity(simulationOutput)
plot(simulationOutput)

simulationOutput = recalculateResiduals(simulationOutput, group = dframe1$weekf)
testUniformity(simulationOutput)
plot(simulationOutput)


simulationOutput <- simulateResiduals(fittedModel = fittedModel, plot = T)

plot(simulationOutput)
simulationOutput <- simulateResiduals(fittedModel = fittedModel, plot = T)
res <- simulateResiduals(fittedModel)
plot(res)
