# 697B

library(foreign)
library(Amelia)
library(MASS)
library(effects)
library(car)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(psych)
library(ggplot2)
library(anytime)

library(dplyr)
library(pscl)
library(lmtest)
install.packages("tidymv")
library(tidymv)

install.packages("ggiraph")
require(ggiraph)
install.packages("ggiraphExtra")
require(ggiraphExtra)
require(plyr)
install.packages("ciTools")
require(ciTools)

csrepression <- read.dta('newfinaldata_statav12.dta')

csrepression$poptotal <- csrepression$SPPOPTOTL
csrepression$poplog <- log(csrepression$SPPOPTOTL)

csrepression$csrepress_BIRT <- csrepression$v2csreprss
is.factor(csrepression$v2csreprss_ord)
summary(csrepression$v2csreprss_ord)
csrepression$csrepress.r <- factor(csrepression$v2csreprss_ord,
                                   labels= c("Severe", "Substantial", "Moderate", "Weak", "None"),
                                   ordered = TRUE)
csrepression$csrepress.r <- recode(csrepression$v2csreprss_ord, "0=4; 1=3; 2=2;3=1;4=0;NA=NA")
table(csrepression$csrepress.r)

csrepression$csrepress.r <- factor(csrepression$csrepress.r,
                                   labels= c("None", "Weak", "Moderate", "Substantial", "Severe"),
                                   ordered = TRUE)
csrepression$csrepress.r.2fac <- recode(csrepression$csrepress.r, "4=1;3=1;2=1;5=1;0=0;NA=NA")
csrepression$csrepress.r.2fac.numeric <- as.numeric(csrepression$csrepress.r.2fac)

table(csrepression$gov_v)
table(csrepression$rebel_v)
table(csrepression$nego_settl)
repressmodeldata <- subset(csrepression, peacefailure != 1 | NA,
                           select =c(csrepress.r, csrepress.r.2fac, csrepress.r.2fac.numeric, polity2, logpop, physint_lagged, wardur,
                                     pts_s_lagged, pts_h_lagged, pts_a_lagged, physint, logbattledeath,
                                     v2csreprss, v2csreprss_ord, pko, nego_settl, rebel_v, gov_v, gdplag,
                                     pop, polity2, peace_agreement, battledeath, peacefailure, country,
                                     year, pko, poptotal, poplog, csrepress_BIRT))

table(repressmodeldata$csrepress.r, repressmodeldata$vic)

# DV : csrepress.r.2fac, where 1=repression
# IV : vic, where 1=settlement
# only if peacefailure != 1
# peacefailure = 0 when there is no peacefailure, then peacefailure =1 and the observations end

repressmodeldata$vic <- 0
repressmodeldata$vic <- as.numeric(repressmodeldata$rebel_v + repressmodeldata$gov_v)
repressmodeldata$vic.r <- car::recode(repressmodeldata$vic, "0=1; 1=0; NA=NA")
table(repressmodeldata$vic.r)

## model

model1<-glm(csrepress.r.2fac~1, data=repressmodeldata, family=binomial("logit"))
summary(model1)
exp(model1$coefficients)

hitmiss(model1)

model2<-glm(csrepress.r.2fac~vic.r, data=repressmodeldata, family=binomial("logit"))
summary(model2)
hitmiss(model2)

## ggplot

nd <- data.frame(vic.r=seq(from=0,to=1,by=0.01))
mod2_p <- predict(model2, nd) %>% plogis()
plot.data<-data.frame(x=nd[,1],y=mod2_p)
lb_mod2 <- add_ci(nd, model2)[,3] # from ciTools
ub_mod2 <- add_ci(nd,model2)[,4]

plot_m2 <- ggplot(plot.data, aes(x=x, y=y), ymax=1)+
  geom_line(colour="black", alpha=1)+
  labs(title="Prob of Repression by Settlement")+
  xlab("Settlement")+
  ylab("Probability of Repression") +
  geom_ribbon(fill= "green", alpha=0.15, aes(ymin=lb_mod2, ymax=ub_mod2))
plot_m2


# From Chris

nd <-cbind(1, seq(0,1, by=1))
nd %*% coef(model2) %>% plogis()
sim_beta<-MASS::mvrnorm(1000, coef(model2), vcov(model2))
head(sim_beta)
dim(sim_beta)
lower_bound=nd %*% t(sim_beta) %>% t() %>% plogis() %>% apply(2, quantile, 0.025)
upper_bound=nd %*% t(sim_beta) %>% t() %>% plogis() %>% apply(2, quantile, 0.975)
median=nd %*% t(sim_beta) %>% t() %>% plogis() %>% apply(2, quantile, 0.5)
plot.dat<-data.frame(lower_bound, upper_bound, median, vic=seq(0,1, 1))

ggplot(plot.dat, aes(x=as.factor(vic), 
                     y=median, 
                     ymax=upper_bound, 
                     ymin=lower_bound, group=1))+
  geom_line()+
  geom_ribbon(fill="blue", alpha=0.15)+
  xlab("settlement")+
  ylab("repression")


