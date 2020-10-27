# Forecasting Assignment
# POL683

# Data
# Oct 2020
#### Prep ####
library(readxl)
library(dplyr)
library(tidyr)
library(MASS)
library(car)
library(ggplot2)

polls <- read.csv("./president_polls.csv", header= T) %>% as.data.frame()
polls_states <- subset(polls, state !="NA", candidate_party="DEM"|"REP")
polls_national <- subset(polls, is.na(state), candidate_party="DEM"|"REP")
count(polls_national) + count(polls_states) # = total n in full dataset

#RUN ONLY ONCE
polls_states <- do.call("cbind", split(polls_states, rep(c(1,2), length.out=nrow(polls_states))))

state_avgs <- polls_states %>%
  group_by(state = `1.state`) %>%
  summarize(mean_dem_pct = mean(`1.pct`)/100,
            mean_rep_pct = mean(`2.pct`)/100,
            mean_sample = mean(`1.sample_size`))
populationdata <- read_excel("./statepopulation.xlsx", col_names=T)
state_avgs <- merge(state_avgs, populationdata, by="state", all.x=T)

registrationdata <- read.csv("./state_regvoters.csv", T)
names(registrationdata)[names(registrationdata)=="State"] <- "state"
state_avgs <- merge(state_avgs, registrationdata, by="state", all.x=T)

state_avgs$bidenwin <- ifelse(state_avgs$mean_dem_pct > state_avgs$mean_rep_pct, 1, 0)
state_avgs$trumpwin <- ifelse(state_avgs$mean_dem_pct < state_avgs$mean_rep_pct, 1, 0)

state_avgs$bidenmeanvotes <- (state_avgs$mean_dem_pct/100) * (state_avgs$jul2019est*state_avgs$registeredPerc)
state_avgs$trumpmeanvotes <- (state_avgs$mean_rep_pct/100) * (state_avgs$jul2019est*state_avgs$registeredPerc)

totalvoters <- sum(state_avgs$totalRegistered, na.rm=T)

covid <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv", T)
covid<- aggregate(cbind(covid$cases, covid$deaths), by=list(state=covid$state), FUN=sum)
names(covid)[names(covid)=="V1"] <- "cases"
names(covid)[names(covid)=="V2"] <- "deaths"
state_avgs <- merge(state_avgs, covid, by="state", all.x=T)

##### Models -- PREDICTING THE % BIDEN GETS ####

lm1 <- lm(mean_dem_pct ~ cases + deaths + changepct + registeredPerc, state_avgs)
summary(lm1)

pred.mvrnorm <- mvrnorm(1000, coef(lm1), vcov(lm1))
nd <- cbind(1, cases=casesv, deaths=deathsv, changepct=changepctv, registeredPerc=registeredPercv)
pred_mean <- pred.mvrnorm %*% t(nd) %>% plogis() %>% apply(2, mean)
pred_lb <- pred.mvrnorm %*% t(nd) %>% plogis() %>% apply(2, quantile, 0.025)
pred_ub <- pred.mvrnorm %*% t(nd) %>% plogis() %>% apply(2, quantile, 0.975)

cbind(Point=mean(pred_mean), CI_lower=mean(pred_lb), CI_upper=mean(pred_ub))
# BIDEN VOTE SHARE: 0.613778 (0.5792429, 0.6482682)

#####
m1 <- glm(bidenwin ~ cases + deaths + changepct + registeredPerc, state_avgs, family=binomial("logit"))
summary(m1)
exp(coef(m1))

set.seed(88898)

casesv <- seq(from=min(state_avgs$cases, na.rm=T), to=max(state_avgs$cases, na.rm=T), by=83377.64)
deathsv <- seq(from=min(state_avgs$deaths, na.rm=T), to=max(state_avgs$deaths, na.rm=T), by=6004.247)
changepctv <- seq(0,0.999,0.001)
registeredPercv <- seq(0,0.999,0.001)

pred.mvrnorm <- mvrnorm(1000, coef(m1), vcov(m1))
nd <- cbind(1,
            cases=casesv,
            deaths=deathsv,
            changepct=changepctv,
            registeredPerc=registeredPercv)
pred_mean <- pred.mvrnorm %*% t(nd) %>% plogis() %>% apply(2, mean)
hist(pred_mean)

pred_mean[pred_mean <0.501] %>% length()

#pred_lb <- pred.mvrnorm %*% t(nd) %>% pnorm() %>% apply(2, quantile, 0.025) #pnorm to make it probabilities
#pred_ub <- pred.mvrnorm %*% t(nd) %>% pnorm() %>% apply(2, quantile, 0.975)

#m1plot <- data.frame(pred_mean, pred_lb, pred_ub, bidenwin=seq(0,0.9999,0.0002035002))

#ggplot(m1plot, aes(x=bidenwin, y=pred_mean, ymax=pred_ub, ymin=pred_lb))+
#  geom_line(colour="black", alpha=1) +
#  labs(title="Probability of Biden Win")+
#  xlab("Covariates (Covid Deaths, Change in Pop [2010-2019], % of Registered Voters")+
#  ylab("Prob of Biden Win") +
#  geom_ribbon(alpha=0.15)
#hist(pred_mean)

### ?

pred_mean <- pred.mvrnorm %*% t(nd) %>% plogis() %>% apply(2, mean)

pred_lb <- pred.mvrnorm %*% t(nd) %>% plogis() %>% apply(2, quantile, 0.025)
pred_ub <- pred.mvrnorm %*% t(nd) %>% plogis() %>% apply(2, quantile, 0.975)

m1plot <- data.frame(pred_mean, pred_lb, pred_ub, bidenwin=seq(0,0.999,0.001))

ggplot(m1plot, aes(x=bidenwin, y=pred_mean, ymax=pred_ub, ymin=pred_lb))+
  geom_line(colour="black", alpha=1) +
  labs(title="Probability of Biden Win")+
  xlab("Covariates (Covid Deaths, Change in Pop [2010-2019], % of Registered Voters")+
  ylab("Prob of Biden Win") +
  geom_ribbon(alpha=0.15)


