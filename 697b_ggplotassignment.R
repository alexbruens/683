# 697B
# GGplot assignment
# Alex Bruens

library(foreign)
library(car)
library(ggplot2)

library(MASS)
library(dplyr)
library(pscl)
library(lmtest)
library(tidymv)
require(ciTools)
#####
# csrepression <- read.dta('newfinaldata_statav12.dta')
#csrepression$poptotal <- csrepression$SPPOPTOTL
#csrepression$poplog <- log(csrepression$SPPOPTOTL)
#csrepression$csrepress_BIRT <- csrepression$v2csreprss
#is.factor(csrepression$v2csreprss_ord)
#summary(csrepression$v2csreprss_ord)
csrepression$csrepress.r <- factor(csrepression$v2csreprss_ord,
                                   labels= c("Severe", "Substantial", "Moderate", "Weak", "None"),
                                   ordered = TRUE)
csrepression$csrepress.r <- car::recode(csrepression$v2csreprss_ord, "0=4; 1=3; 2=2;3=1;4=0;NA=NA")
#table(csrepression$csrepress.r)

csrepression$csrepress.r <- factor(csrepression$csrepress.r,
                                   labels= c("None", "Weak", "Moderate", "Substantial", "Severe"),
                                   ordered = TRUE)
#csrepression$csrepress.r.2fac <- car::recode(csrepression$csrepress.r, "4=1;3=1;2=1;5=1;0=0;NA=NA")
#csrepression$csrepress.r.2fac.numeric <- as.numeric(csrepression$csrepress.r.2fac)

#table(csrepression$gov_v)
#table(csrepression$rebel_v)
#table(csrepression$nego_settl)
repressmodeldata <- subset(csrepression, peacefailure != 1 | NA,
                           select =c(csrepress.r, csrepress.r.2fac, csrepress.r.2fac.numeric, polity2, logpop, physint_lagged, wardur,
                                     pts_s_lagged, pts_h_lagged, pts_a_lagged, physint, logbattledeath,
                                     v2csreprss, v2csreprss_ord, pko, nego_settl, rebel_v, gov_v, gdplag,
                                     pop, polity2, peace_agreement, battledeath, peacefailure, country,
                                     year, pko, poptotal, poplog, csrepress_BIRT))

repressmodeldata$vic <- 0
repressmodeldata$vic <- as.numeric(repressmodeldata$rebel_v + repressmodeldata$gov_v)
repressmodeldata$vic.r <- car::recode(repressmodeldata$vic, "0=1; 1=0; NA=NA")
#table(repressmodeldata$vic.r)

#table(repressmodeldata$csrepress.r, repressmodeldata$vic)

# DV : csrepress.r.2fac, where 1=repression
# IV : vic.r , where 1=settlement, 0= other victory
# only if peacefailure != 1
# peacefailure = 0 when there is no peacefailure, then peacefailure =1 and the observations end

##### model ####

model2<-glm(csrepress.r.2fac~vic.r, data=repressmodeldata, family=binomial("logit"))
summary(model2)
hitmiss(model2)

vcov(model2)
coef(model2)

pred.mvrnorm <- mvrnorm(1000, coef(model2), vcov(model2))
nd <- cbind(1, vic.r=seq(from=0,to=1,by=0.01))
pred_mean <- pred.mvrnorm %*% t(nd) %>% pnorm() %>% apply(2, mean)
pred_lb <- pred.mvrnorm %*% t(nd) %>% pnorm() %>% apply(2, quantile, 0.025) #pnorm to make it probabilities
pred_ub <- pred.mvrnorm %*% t(nd) %>% pnorm() %>% apply(2, quantile, 0.975)

m2plotd <- data.frame(pred_mean, pred_lb, pred_ub, vic.r=seq(from=0,to=1,by=0.01))
m2plotd

ggplot(m2plotd, aes(x=vic.r, y=pred_mean, ymax=pred_ub, ymin=pred_lb))+
  geom_line(colour="red", alpha=1)+
  labs(title="Prob of Repression by Termination")+
  xlab("Termination; 1=negotiated settlement")+
  ylab("Probability of Repression") +
  geom_ribbon(alpha=0.15)



##### ggplot####

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

#### From Chris ####

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




#### Ordered Logit Parallel Odds ####
as.factor(repressmodeldata$csrepress.r)

paramodel <- polr(csrepress.r ~ vic.r, data=repressmodeldata, Hess=TRUE) 
paramodel %>% summary()

repressmodeldata$csrepress.r.0 <- case_when(repressmodeldata$csrepress.r %in% c("None") ~ 1,
                                            repressmodeldata$csrepress.r %in% c("Weak", "Moderate", "Substantial", "Severe") ~ 0)

repressmodeldata$csrepress.r.1 <- case_when(repressmodeldata$csrepress.r %in% c("None", "Weak") ~ 1,
                                            repressmodeldata$csrepress.r %in% c("Moderate", "Substantial", "Severe") ~ 0)

repressmodeldata$csrepress.r.2 <- case_when(repressmodeldata$csrepress.r %in% c("None", "Weak", "Moderate") ~ 1,
                                            repressmodeldata$csrepress.r %in% c("Substantial", "Severe") ~ 0)

repressmodeldata$csrepress.r.3 <- case_when(repressmodeldata$csrepress.r %in% c("None", "Weak", "Moderate", "Substantial") ~ 1,
                                            repressmodeldata$csrepress.r %in% c("Severe") ~ 0)

repressmodeldata$csrepress.r.4 <- case_when(repressmodeldata$csrepress.r %in% c("None", "Weak", "Moderate", "Substantial", "Severe") ~ 1,
                                            repressmodeldata$csrepress.r %in% c() ~ 0)

paramodel.0 <- glm(csrepress.r.0 ~ vic.r, data=repressmodeldata, family=binomial("logit"))
paramodel.1 <- glm(csrepress.r.1 ~ vic.r, data=repressmodeldata, family=binomial("logit"))
paramodel.2 <- glm(csrepress.r.2 ~ vic.r, data=repressmodeldata, family=binomial("logit"))
paramodel.3 <- glm(csrepress.r.3 ~ vic.r, data=repressmodeldata, family=binomial("logit"))
paramodel.4 <- glm(csrepress.r.4 ~ vic.r, data=repressmodeldata, family=binomial("logit"))

paracoefs <- c(paramodel.0$coefficients["vic.r"], paramodel.1$coefficients["vic.r"], paramodel.2$coefficients["vic.r"], paramodel.3$coefficients["vic.r"], paramodel.4$coefficients["vic.r"])
paranames <- c("None", "Weak", "Moderate", "Substantial", "Severe")

paralleloddscoefs <- data.frame(paranames,paracoefs)
paralleloddscoefs

require(brant)
brant(paramodel)

table(repressmodeldata$vic.r, repressmodeldata$csrepress.r)

