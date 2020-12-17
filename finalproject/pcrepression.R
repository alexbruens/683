# POL 683 Final
# Alex Bruens

library(readstata13)
library(MASS)
library(dplyr)
library(ggplot2)
library(car)
#library(lme4)
#library(lmtest)
#require(ggiraph)
#require(ggiraphExtra)

library(nnet)
library(likert)

library(ggeffects)
library(tidymv)
library(tidyr)

data <- read.dta13('./finaldata_endyear_PTS.dta') #in gihub
data_agreements <- read.dta13('./finaldata_agreementyear.dta') #also in github

#####Recoding####
# Recoding
data$outcome1 <- car::recode(data$Outcome, "1=1;2=0;3=0;4=0;5=0;6=0")
data$outcome2 <- car::recode(data$Outcome, "1=0;2=1;3=0;4=0;5=0;6=0")
data$outcome3 <- car::recode(data$Outcome, "1=0;2=0;3=1;4=0;5=0;6=0")
data$outcome4 <- car::recode(data$Outcome, "1=0;2=0;3=0;4=1;5=0;6=0")
data$outcome5 <- car::recode(data$Outcome, "1=0;2=0;3=0;4=0;5=1;6=0")
data$outcome6 <- car::recode(data$Outcome, "1=0;2=0;3=0;4=0;5=0;6=1")

data$v2csreprss_ord <- factor(data$v2csreprss_ord,
                              levels=c("0","1","2","3","4"),
                              labels=c("Severe","Substantial","Moderate","Weak","No"))
data$v2csreprss_ord <- reverse.levels(data$v2csreprss_ord)

data$v2csreprss_ord_1lead <- factor(data$v2csreprss_ord_1lead,
                                levels=c("0","1","2","3","4"),
                                labels=c("Severe","Substantial","Moderate","Weak","No"))
data$v2csreprss_ord_1lead <- reverse.levels(data$v2csreprss_ord_1lead)

data$v2csreprss_ord_3lead <- factor(data$v2csreprss_ord_3lead,
                                    levels=c("0","1","2","3","4"),
                                    labels=c("Severe","Substantial","Moderate","Weak","No"))
data$v2csreprss_ord_3lead <- reverse.levels(data$v2csreprss_ord_3lead)

data$v2csreprss_ord_5lead <- factor(data$v2csreprss_ord_5lead,
                                    levels=c("0","1","2","3","4"),
                                    labels=c("Severe","Substantial","Moderate","Weak","No"))
data$v2csreprss_ord_5lead <- reverse.levels(data$v2csreprss_ord_5lead)

data_agreements$v2csreprss_ord_1lead <- factor(data_agreements$v2csreprss_ord_1lead,
                                    levels=c("0","1","2","3","4"),
                                    labels=c("Severe","Substantial","Moderate","Weak","No"))
data_agreements$v2csreprss_ord_1lead <- reverse.levels(data_agreements$v2csreprss_ord_1lead)

data_agreements$v2csreprss_ord_3lead <- factor(data_agreements$v2csreprss_ord_3lead,
                                    levels=c("0","1","2","3","4"),
                                    labels=c("Severe","Substantial","Moderate","Weak","No"))
data_agreements$v2csreprss_ord_3lead <- reverse.levels(data_agreements$v2csreprss_ord_3lead)

data_agreements$v2csreprss_ord_5lead <- factor(data_agreements$v2csreprss_ord_5lead,
                                    levels=c("0","1","2","3","4"),
                                    labels=c("Severe","Substantial","Moderate","Weak","No"))
data_agreements$v2csreprss_ord_5lead <- reverse.levels(data_agreements$v2csreprss_ord_5lead)

data$Outcome.factor <- factor(data$Outcome,
                              levels=c("1","2","3","4","5","6"),
                              labels=c("PeaceAgree","Ceasefire","GovVic","RebVic","LowAct", "ActDies"))

#####Xtabs#####
ftable(xtabs(~  Outcome.factor + v2csreprss_ord_1lead, data))
ftable(xtabs(~  Outcome.factor + v2csreprss_ord_3lead, data))
ftable(xtabs(~  Outcome.factor + v2csreprss_ord_5lead, data))

lattice::wireframe(v2csreprss_ord_1lead ~ Outcome.factor + v2csreprss_ord, data= data, aspect=1, panel.aspect=0.5, scales = list(arrows=F, distance=1.5),
          screen= list(z=45, x=-80, y=-3), main = "CSO Repression (t+1), CSO Repression (t), War Outcome", zlab= list("CSO Repression (t+1)", just=c("right")),
          xlab="Outcome", ylab="CSO Repression (t)", drape=T)

#####Models####
#Models

lead1_model <- multinom(v2csreprss_ord_1lead ~ Outcome.factor +  v2csreprss, data, family=binomial(link = "logit"))
lead1_model %>% summary()
lead1_model %>% coef() %>% exp()
summary(lead1_model,Wald.ratios=T) %>% .$Wald.ratios %>% abs() %>% pnorm(lower.tail=F) * 2

length(residuals(lead1_model))

data$represspredicted_1lead <- predict(lead1_model, newdata = data, "class")
tab1 <- table(data$v2csreprss_ord_1lead, data$represspredicted_1lead)
round((sum(diag(tab1))/sum(tab1))*100,2)

lead3_model <- multinom(v2csreprss_ord_3lead ~ Outcome.factor + v2csreprss, data, family=binomial(link = "logit"))
lead3_model %>% summary()
lead3_model %>% coef() %>% exp()
summary(lead3_model,Wald.ratios=T) %>% .$Wald.ratios %>% abs() %>% pnorm(lower.tail=F) * 2

length(residuals(lead1_model))

data$represspredicted_3lead <- predict(lead3_model, newdata = data, "class")
tab3 <- table(data$v2csreprss_ord_3lead, data$represspredicted_3lead)
round((sum(diag(tab3))/sum(tab3))*100,2)

lead5_model <- multinom(v2csreprss_ord_5lead ~ Outcome.factor + v2csreprss, data, family=binomial(link = "logit"))
lead5_model %>% summary()
lead5_model %>% coef() %>% exp()
summary(lead5_model,Wald.ratios=T) %>% .$Wald.ratios %>% abs() %>% pnorm(lower.tail=F) * 2

length(residuals(lead1_model))

data$represspredicted_5lead <- predict(lead5_model, newdata = data, "class")
tab5 <- table(data$v2csreprss_ord_5lead, data$represspredicted_5lead)
round((sum(diag(tab5))/sum(tab5))*100,2)

# Plots

eff1 <- ggeffect(lead1_model, terms = "Outcome.factor", ci.level=0.98)

eff1 %>%
  filter(response.level %in% c("Moderate", "Substantial", "Weak")) %>%
  ggplot(., aes(x, predicted, group=response.level)) + 
  geom_point() +
  facet_wrap(~response.level) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), alpha = 1, size=0.7) +
  labs(x = 'Outcome', y = 'Predicted Probability') +
  ylim(c(0, 1)) +
  ggtitle("Predicted Probability of CSO Repression t+1 by Termination Type") +
  theme(axis.text.x = element_text(face="bold",angle=45))


eff3 <- ggeffect(lead3_model, terms = "Outcome.factor")
eff3 %>%
  filter(response.level %in% c("Moderate", "Substantial", "Weak")) %>%
  ggplot(., aes(x, predicted, group=response.level)) + 
  geom_point() +
  facet_wrap(~response.level) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), alpha = 1, size=0.7) +
  labs(x = 'Outcome', y = 'Predicted Probability') +
  ylim(c(0, 1)) +
  ggtitle("Predicted Probability of CSO Repression t+3 by Termination Type") +
  theme(axis.text.x = element_text(face="bold",angle=45))

eff5 <- ggeffect(lead5_model, terms = "Outcome.factor")
eff5 %>%
  filter(response.level %in% c("Moderate", "Substantial", "Weak")) %>%
  ggplot(., aes(x, predicted, group=response.level)) + 
  geom_point() +
  facet_wrap(~response.level) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), alpha = 1, size=0.7) +
  labs(x = 'Outcome', y = 'Predicted Probability') +
  ylim(c(0, 1)) +
  ggtitle("Predicted Probability of CSO Repression t+5 by Termination Type") +
  theme(axis.text.x = element_text(face="bold",angle=45))


# Weak and Severe

eff3 %>%
  filter(response.level %in% c("No", "Severe")) %>%
  ggplot(., aes(x, predicted, group=response.level)) + 
  geom_point() +
  facet_wrap(~response.level) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), alpha = 1, size=0.7) +
  labs(x = 'Outcome', y = 'Predicted Probability') +
  ylim(c(0, 1)) +
  ggtitle("Predicted Probability of CSO Repression t+3 by Termination Type") +
  theme(axis.text.x = element_text(face="bold",angle=45))

# Agreements

data_agreements$change1to5 <- as.numeric(data_agreements$v2csreprss_ord_1lead) - as.numeric(data_agreements$v2csreprss_ord_5lead)
# More negative, more severe

agreement_data_condensed <- cbind(data_agreements$ConflictEp, data_agreements$Year, data_agreements$Location, data_agreements$v2csreprss_ord_1lead, data_agreements$v2csreprss_ord_5lead, data_agreements$change1to5) %>% as.data.frame()
colnames(agreement_data_condensed) <- c("ConflictEpisode","Year","Location", "CSO R (t+1)", "CSO R (t+5)", "ChangeCSOR")

ggplot(data_agreements, aes(y= as.factor(ConflictEp), x=change1to5)) +
  geom_bar(stat="identity",colour="black", position="dodge", fill="lightblue") +
  geom_text(size=4, aes(label=Location), position=position_stack(vjust=0), hjust="left", color="black") +
  ylab("Conflict Episode") + xlab("Change in CSO Repression Level 1 Year to 5 Years After Agreement") +
  ggtitle("Change in CSO Repression After Peace Agreements")

plot(agreement_data_condensed$`CSO R (t+1)`)

#####

# Ordered
model1 <- polr(v2csreprss_ord_3lead ~ Outcome.factor + CumulativeIntensity + v2csreprss, data)
brant::brant(model1)

data$csrord_11 <- as.numeric(car::recode(as.numeric(data$v2csreprss_ord_1lead), "1=1;2=0;3=0;4=0;5=0"))
data$csrord_12 <- as.numeric(car::recode(as.numeric(data$v2csreprss_ord_1lead), "1=0;2=1;3=0;4=0;5=0"))

# Parallel Odds Test
glm(as.factor(csrord_11) ~ Outcome.factor + CumulativeIntensity + v2csreprss, data, family=binomial("logit")) %>% summary()
glm(as.factor(csrord_12) ~ Outcome.factor + CumulativeIntensity + v2csreprss, data, family=binomial("logit")) %>% summary()

#####

#model1 %>% summary()
#coef(model1)
#coef.matrix<-(c(coef(model1), model1$zeta))
#new.data <- c(0,1,0,0,0,1,0,0,0)
#coef.matrix %*% new.data %>% plogis()

# PTS comparison ?
#data$PTS_A_1lead
#pts_model1 <- polr(as.factor(PTS_A_1lead)~ Outcome.factor + v2csreprss, data)
#pts_1eff <- ggeffects::ggeffect(pts_model1, terms = "Outcome.factor")
#ggplot(pts_1eff) +
#  aes(x = x, y = predicted, color = response.level, group=response.level) +
#  geom_point(size=2.3) +
#  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), alpha = 1, size=0.7) +
#  labs(x = 'Outcome', y = 'Predicted Probability', color="PTS") +
#  ylim(c(0, 1)) +
#  ggtitle("Predicted Probability of PTS Repression t+1 by Termination Type")

