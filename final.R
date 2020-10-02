# 667 Final
# Alex Bruens
# December 19, 2019

getwd()
setwd('./Desktop/Fall2019/POL667/data')

#### Packages and libraries ####

install.packages("Amelia")
install.packages("MASS")
install.packages("effects")
install.packages("psych")
install.packages("anytime")


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

#### Building the dataset ####

vdem <- readRDS('./V-Dem-CY-Full+Others-v9.rds')
civso <- subset(vdem, year > 1946 & year < 2005,
                select=c(country_name, year, COWcode, v2csreprss,
                         v2csreprss_ord, v2cseeorgs, v2cseeorgs_ord,
                         v2cscnsult, v2cscnsult_ord, v2csstruc_0, v2csstruc_1, v2csstruc_2,
                         v2csstruc_3))
head(civso)
tail(civso)
write.dta(civso, "civso_variables.dta")

# In Stata: merge with Joshi and Mason 2011

csrepression <- read.dta('newfinaldata_statav12.dta')

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
repressmodeldata$gdplag_original <- repressmodeldata$gdplag
repressmodeldata$gdplag <- log(repressmodeldata$gdplag)

summary(repressmodeldata$gdplag)

#### Checking subsetted repression ####

justsevere <- subset(csrepression, csrepress.r==4 & peacefailure != 1 | NA,
                                    select =c(csrepress.r, polity2, logpop, physint_lagged, wardur,
                                              pts_s_lagged, pts_h_lagged, pts_a_lagged, physint, logbattledeath,
                                              v2csreprss, v2csreprss_ord, pko, nego_settl, rebel_v, gov_v, gdplag,
                                              pop, polity2, peace_agreement, battledeath, peacefailure, country,
                                              year, pko, poptotal, poplog, csrepress_BIRT))


justnegotiatedsettlements <- subset(csrepression, nego_settl==1 & peacefailure != 1 | NA,
                                    select =c(csrepress.r, polity2, logpop, physint_lagged, wardur,
                                              pts_s_lagged, pts_h_lagged, pts_a_lagged, physint, logbattledeath,
                                              v2csreprss, v2csreprss_ord, pko, nego_settl, rebel_v, gov_v, gdplag,
                                              pop, polity2, peace_agreement, battledeath, peacefailure, country,
                                              year, pko, poptotal, poplog, csrepress_BIRT))
View(justnegotiatedsettlements)
summary(justnegotiatedsettlements$country)

negsettle_withsevererepression <- subset(justnegotiatedsettlements, csrepress.r==4,
                                        select =c(csrepress.r, polity2, logpop, physint_lagged, wardur,
                                                  pts_s_lagged, pts_h_lagged, pts_a_lagged, physint, logbattledeath,
                                                  v2csreprss, v2csreprss_ord, pko, nego_settl, rebel_v, gov_v, gdplag,
                                                  pop, polity2, peace_agreement, battledeath, peacefailure, country,
                                                  year, pko, poptotal, poplog, csrepress_BIRT))
View(negsettle_withsevererepression)

negsettle_withMODrepression <- subset(justnegotiatedsettlements, csrepress.r==3,
                                         select =c(csrepress.r, polity2, logpop, physint_lagged, wardur,
                                                   pts_s_lagged, pts_h_lagged, pts_a_lagged, physint, logbattledeath,
                                                   v2csreprss, v2csreprss_ord, pko, nego_settl, rebel_v, gov_v, gdplag,
                                                   pop, polity2, peace_agreement, battledeath, peacefailure, country,
                                                   year, pko, poptotal, poplog, csrepress_BIRT))
View(negsettle_withMODrepression)

negsettle_withNOrepression <- subset(justnegotiatedsettlements, csrepress.r==0,
                                      select =c(csrepress.r, polity2, logpop, physint_lagged, wardur,
                                                pts_s_lagged, pts_h_lagged, pts_a_lagged, physint, logbattledeath,
                                                v2csreprss, v2csreprss_ord, pko, nego_settl, rebel_v, gov_v, gdplag,
                                                pop, polity2, peace_agreement, battledeath, peacefailure, country,
                                                year, pko, poptotal, poplog, csrepress_BIRT))
View(negsettle_withNOrepression)

#### Checking subsetted victories ####

justrebelvic <- subset(csrepression, rebel_v==1 & peacefailure != 1 | NA,
                                    select =c(csrepress.r, polity2, logpop, physint_lagged, wardur,
                                              pts_s_lagged, pts_h_lagged, pts_a_lagged, physint, logbattledeath,
                                              v2csreprss, v2csreprss_ord, pko, nego_settl, rebel_v, gov_v, gdplag,
                                              pop, polity2, peace_agreement, battledeath, peacefailure, country,
                                              year, pko, poptotal, poplog, csrepress_BIRT))
View(justrebelvic)
summary(justrebelvic$country)

justrebelvic_withsevererepression <- subset(justrebelvic, csrepress.r==4,
                                         select =c(csrepress.r, polity2, logpop, physint_lagged, wardur,
                                                   pts_s_lagged, pts_h_lagged, pts_a_lagged, physint, logbattledeath,
                                                   v2csreprss, v2csreprss_ord, pko, nego_settl, rebel_v, gov_v, gdplag,
                                                   pop, polity2, peace_agreement, battledeath, peacefailure, country,
                                                   year, pko, poptotal, poplog, csrepress_BIRT))
View(justrebelvic_withsevererepression)

justrebelvic_withMODrepression <- subset(justrebelvic, csrepress.r==3,
                                      select =c(csrepress.r, polity2, logpop, physint_lagged, wardur,
                                                pts_s_lagged, pts_h_lagged, pts_a_lagged, physint, logbattledeath,
                                                v2csreprss, v2csreprss_ord, pko, nego_settl, rebel_v, gov_v, gdplag,
                                                pop, polity2, peace_agreement, battledeath, peacefailure, country,
                                                year, pko, poptotal, poplog, csrepress_BIRT))
View(justrebelvic_withMODrepression)

justrebelvic_withNOrepression <- subset(justrebelvic, csrepress.r==0,
                                     select =c(csrepress.r, polity2, logpop, physint_lagged, wardur,
                                               pts_s_lagged, pts_h_lagged, pts_a_lagged, physint, logbattledeath,
                                               v2csreprss, v2csreprss_ord, pko, nego_settl, rebel_v, gov_v, gdplag,
                                               pop, polity2, peace_agreement, battledeath, peacefailure, country,
                                               year, pko, poptotal, poplog, csrepress_BIRT))
View(justrebelvic_withNOrepression)


#### Subsetting for models ####
table(csrepression$gov_v)
table(csrepression$rebel_v)
table(csrepression$nego_settl)
csrepression$poptotal <- csrepression$SPPOPTOTL
csrepression$poplog <- log(csrepression$poptotal)
repressmodeldata <- subset(csrepression, peacefailure != 1 | NA,
                           select =c(csrepress.r, polity2, logpop, physint_lagged, wardur,
                                     pts_s_lagged, pts_h_lagged, pts_a_lagged, physint, logbattledeath,
                                     v2csreprss, v2csreprss_ord, pko, nego_settl, rebel_v, gov_v, gdplag,
                                     pop, polity2, peace_agreement, battledeath, peacefailure, country,
                                     year, pko, poptotal, poplog, csrepress_BIRT))
repressmodeldata$csrepress.r <- as.numeric(repressmodeldata$csrepress.r)
repressmodeldata$csrepress.r.2fac <- recode(repressmodeldata$csrepress.r, "4=1; 3=1; 2=1;3=1;0=0;NA=NA")
repressmodeldata$csrepress.r.2fac.numeric <- as.numeric(repressmodeldata$csrepress.r.2fac)

somalia_repression <- subset(csrepression, cowcode==520,
                           select =c(csrepress.r, cowcode, polity2, logpop, physint_lagged, wardur,
                                     pts_s_lagged, pts_h_lagged, pts_a_lagged, physint, logbattledeath,
                                     v2csreprss, v2csreprss_ord, pko, nego_settl, rebel_v, gov_v, gdplag,
                                     pop, polity2, peace_agreement, battledeath, peacefailure, country,
                                     year, pko, poptotal, poplog, csrepress_BIRT))
somalia_repression$csrepress.r <- as.numeric(somalia_repression$csrepress.r)

ggplot(data=subset(somalia_repression, !is.na(csrepress.r)), aes(year)) +
  geom_smooth(aes(x=year, y=csrepress.r)) +
  xlab("Year") + scale_x_continuous(breaks = scales::pretty_breaks(n = 20), limits = c(1980,2004)) +
  ylab("Severity") +
  ggtitle("CS Repression in Somalia, 1980-2004") +
  theme(axis.text.x=element_text(angle=60, hjust=1))

specific_repression <- subset(csrepression, cowcode==145,
                             select =c(csrepress.r, cowcode, polity2, logpop, physint_lagged, wardur,
                                       pts_s_lagged, pts_h_lagged, pts_a_lagged, physint, logbattledeath,
                                       v2csreprss, v2csreprss_ord, pko, nego_settl, rebel_v, gov_v, gdplag,
                                       pop, polity2, peace_agreement, battledeath, peacefailure, country,
                                       year, pko, poptotal, poplog, csrepress_BIRT))

ggplot(data=subset(specific_repression, !is.na(csrepress.r)), aes(year)) +
  xlab("Year") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20), limits = c(1950,2005)) +
  geom_point(aes(y=csrepress.r), size = 1.5, color = "blue") +
  ylab("Severity") +
  ggtitle("CS Repression in Bolivia, 1950-2005") + 
  theme(axis.text.x=element_text(angle=60, hjust=1))

# DV : csrepress.r
# IV : nego_settl , gov_v , rebel_v
# C  : polity2 + gdplag + poplog + physint_lagged + pko
# C2 : polity2 + gdplag + poplog + physint_lagged + pko
# C3 : polity2 + gdplag + poplog + physint_lagged + pko
# only if peacefailure != 1
# peacefailure = 0 when there is no peacefailure, then peacefailure =1 and the observations end

# parallel odds assumption??

#### Models ####
repress_negsettle_physint <- lm(csrepress.r ~ nego_settl + gov_v + rebel_v + polity2 + gdplag + poplog + physint_lagged + pko,
                                data=repressmodeldata)
summary(repress_negsettle_physint)
tab_model(repress_negsettle_physint)

repress_govv_physint <- lm(csrepress.r ~ gov_v + nego_settl + rebel_v + polity2 + gdplag + poplog + physint_lagged + pko,
                                data=repressmodeldata)
summary(repress_govv_physint)
tab_model(repress_govv_physint)

repress_rebelv_physint <- lm(csrepress.r ~ rebel_v + nego_settl + gov_v + polity2 + gdplag + poplog + physint_lagged + pko,
                           data=repressmodeldata)
summary(repress_rebelv_physint)
tab_model(repress_rebelv_physint)

# without polity2 !!!!

repress_negsettle_physint_nopolity <- lm(csrepress.r ~ nego_settl + gov_v + rebel_v + gdplag + poplog + physint_lagged + pko,
                                data=repressmodeldata)
summary(repress_negsettle_physint_nopolity)
tab_model(repress_negsettle_physint_nopolity)

repress_govv_physint_nopolity <- lm(csrepress.r ~ gov_v + nego_settl + rebel_v + gdplag + poplog + physint_lagged + pko,
                           data=repressmodeldata)
summary(repress_govv_physint_nopolity)
tab_model(repress_govv_physint_nopolity)

repress_rebelv_physint_nopolity <- lm(csrepress.r ~ rebel_v + nego_settl + gov_v + gdplag + poplog + physint_lagged + pko,
                             data=repressmodeldata)
summary(repress_rebelv_physint_nopolity)
tab_model(repress_rebelv_physint_nopolity)

# PTS

repress_negsettle_ptsa <- lm(csrepress.r ~ nego_settl + polity2 + gdplag + poplog + pts_a_lagged + pko,
                                data=repressmodeldata)
summary(repress_negsettle_ptsa)
tab_model(repress_negsettle_ptsa)

repress_govv_ptsa <- lm(csrepress.r ~ gov_v + polity2 + gdplag + poplog + pts_a_lagged + pko,
                           data=repressmodeldata)
summary(repress_govv_ptsa)
tab_model(repress_govv_ptsa)

repress_rebelv_ptsa <- lm(csrepress.r ~ rebel_v + polity2 + gdplag + poplog + pts_a_lagged + pko,
                             data=repressmodeldata)
summary(repress_rebelv_ptsa)
tab_model(repress_rebelv_ptsa)

repress_negsettle_ptss <- lm(csrepress.r ~ nego_settl + polity2 + gdplag + poplog + pts_s_lagged + pko,
                             data=repressmodeldata)
summary(repress_negsettle_ptss)
tab_model(repress_negsettle_ptss)

# BAYESIAN IRT
repressBIRT_negsettle_physint <- lm(csrepress_BIRT ~ nego_settl + polity2 + gdplag + poplog + physint_lagged + pko,
                                data=repressmodeldata)
summary(repressBIRT_negsettle_physint)
tab_model(repressBIRT_negsettle_physint)
repressBIRT_govv_physint <- lm(csrepress_BIRT ~ gov_v + polity2 + gdplag + poplog + physint_lagged + pko,
                                data=repressmodeldata)
summary(repressBIRT_govv_physint)
tab_model(repressBIRT_govv_physint)
repressBIRT_rebelv_physint <- lm(csrepress_BIRT ~ rebel_v + polity2 + gdplag + poplog + physint_lagged + pko,
                                data=repressmodeldata)
summary(repressBIRT_rebelv_physint)
tab_model(repressBIRT_rebelv_physint)

# IRT without polity 2 !!!!
repressBIRT_negsettle_physint_nopolity <- lm(csrepress_BIRT ~ nego_settl + gdplag + poplog + physint_lagged + pko,
                                    data=repressmodeldata)
summary(repressBIRT_negsettle_physint_nopolity)
tab_model(repressBIRT_negsettle_physint_nopolity)
repressBIRT_govv_physint_nopolity <- lm(csrepress_BIRT ~ gov_v + gdplag + poplog + physint_lagged + pko,
                               data=repressmodeldata)
summary(repressBIRT_govv_physint_nopolity)
tab_model(repressBIRT_govv_physint_nopolity)
repressBIRT_rebelv_physint_nopolity <- lm(csrepress_BIRT ~ rebel_v + gdplag + poplog + physint_lagged + pko,
                                 data=repressmodeldata)
summary(repressBIRT_rebelv_physint_nopolity)
tab_model(repressBIRT_rebelv_physint_nopolity)

# Other checks
summary(lm(csrepress.r.2fac.numeric ~ nego_settl + polity2 + gdplag + poplog + physint_lagged + pko,
           data=repressmodeldata))

# attempt at ordered logit
# making ordinal repression as factor for logit instead of numeric for OLS

repressmodeldata$csrepress.r.factor <- as.factor(repressmodeldata$csrepress.r)

is.numeric(repressmodeldata$gdplag)

## Ordered Logits ##

orderedlogit_negsettle <- polr(as.factor(csrepress.r.factor)~ nego_settl + gov_v + rebel_v + polity2 + gdplag + poplog + physint_lagged + pko,
                               data=repressmodeldata, Hess=TRUE)
summary(orderedlogit_negsettle)

OL_negosettle_boxOdds <- cbind(coef(orderedlogit_negsettle),confint(orderedlogit_negsettle))
OL_negosettle_boxOdds <- as.data.frame(OL_negosettle_boxOdds)
colnames(OL_negosettle_boxOdds) <- c("boxOdds","boxCILow","boxCIHigh")
logit_negsettle_boxLabels <- c("Settlement", "Gov. Vic", "Rebel Vic", "Polity2", "GDP (t-1, log)", "Population (log)", "Physint (t-1)", "PKO")
OL_negosettle_boxOdds[, "yAxis"] <- length(logit_negsettle_boxLabels):1

plot_negsettle_odds <- ggplot(logit_negsettle_boxOdds, aes(x = boxOdds, y = yAxis))
plot_negsettle_odds + geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 1.5, color = "blue") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = yAxis, labels = logit_negsettle_boxLabels) +
  ylab("") +
  xlab("Odds ratio") +
  ggtitle("Odds Ratios for Negotiated Settlement on \nCS Repression Severity")

# -- #

orderedlogit_rebelv <- polr(as.factor(csrepress.r.factor)~ rebel_v + polity2 + gdplag + poplog + physint_lagged + pko,
                               data=repressmodeldata, Hess=TRUE)
summary(orderedlogit_rebelv)

logit_rebelv_boxOdds <- cbind(coef(orderedlogit_rebelv),confint(orderedlogit_rebelv))
logit_rebelv_boxOdds <- as.data.frame(logit_rebelv_boxOdds)
colnames(logit_rebelv_boxOdds) <- c("boxOdds","boxCILow","boxCIHigh")
logit_rebelv_boxLabels <- c("Rebel Vic.", "Polity2", "GDP (t-1, log)", "Population (log)", "Physint (t-1)", "PKO")
logit_rebelv_boxOdds[, "yAxis"] <- length(logit_rebelv_boxLabels):1

plot_rebelv_odds <- ggplot(logit_rebelv_boxOdds, aes(x = boxOdds, y = yAxis))
plot_rebelv_odds + geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 1.5, color = "blue") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = yAxis, labels = logit_rebelv_boxLabels) +
  ylab("") +
  xlab("Odds ratio") +
  ggtitle("Odds Ratios for Rebel Victory on \nCS Repression Severity")

# -- #

orderedlogit_govv <- polr(as.factor(csrepress.r.factor)~ gov_v + polity2 + gdplag + poplog + physint_lagged + pko,
                            data=repressmodeldata, Hess=TRUE)
summary(orderedlogit_govv)

logit_govv_boxOdds <- cbind(coef(orderedlogit_govv),confint(orderedlogit_govv))
logit_govv_boxOdds <- as.data.frame(logit_govv_boxOdds)
colnames(logit_govv_boxOdds) <- c("boxOdds","boxCILow","boxCIHigh")
logit_govv_boxLabels <- c("Government Vic.", "Polity2", "GDP (t-1, log)", "Population (log)", "Physint (t-1)", "PKO")
logit_govv_boxOdds[, "yAxis"] <- length(logit_rebelv_boxLabels):1

plot_govv_odds <- ggplot(logit_govv_boxOdds, aes(x = boxOdds, y = yAxis))
plot_govv_odds + geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 1.5, color = "blue") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = yAxis, labels = logit_govv_boxLabels) +
  ylab("") +
  xlab("Odds ratio") +
  ggtitle("Odds Ratios for Gov. Victory on \nCS Repression Severity")

# -- #

# confint.default(orderedlogit_negsettle)

#### Descriptives ####

# correlation for polity and repression 
CSrepress.polity <- aov(repressmodeldata$v2csreprss_ord~as.factor(repressmodeldata$polity2))
summary(CSrepress.polity)
TukeyHSD(CSrepress.polity)

 table(csrepression$csrepress.r)
table(vdem$v2csreprss_ord)
ggplot(data=subset(vdem, !is.na(v2csreprss_ord)), aes(year)) +
  geom_histogram(aes(fill = v2csreprss_ord), binwidth=1) + facet_wrap(~ v2csreprss_ord) +
  xlab("Year") +
  ylab("Count") +
  ggtitle("Count of Countries by CS Repression \nSeverity and Year") +
  labs(fill = "Severity")

worst_repression <- subset(repressmodeldata, csrepress.r == "Moderate",
                           select= c(country, csrepress.r, year))
barplot(table(repressmodeldata$csrepress.r))
boxplot(repressmodeldata$csrepress.r)

repressmodeldata$csrepress.r.withtitles <- factor(repressmodeldata$csrepress.r,
                                                  labels= c("None", "Weak", "Moderate", "Substantial", "Severe"),
                                                  ordered = TRUE)

ggplot(data=subset(repressmodeldata, !is.na(csrepress.r.withtitles)), aes(year)) +
  geom_histogram(aes(fill = csrepress.r.withtitles), binwidth=1) + facet_wrap(~ csrepress.r.withtitles) +
  xlab("Year") +
  ylab("Count") +
  ggtitle("Count of Countries by CSO Repression \nSeverity and Year") +
  labs( caption = "Note: 1948-2005, 69 countries") +
  theme(legend.position = "none")

repress_negsettle <- lm(csrepress.r ~ nego_settl, data=repressmodeldata)
summary(repress_negsettle)
tab_model(repress_negsettle)

repress_rebelv <- lm(csrepress.r ~ rebel_v, data=repressmodeldata)
summary(repress_rebelv)
tab_model(repress_rebelv)

repress_negsettle.ttest <- t.test(repressmodeldata$csrepress.r~repressmodeldata$nego_settl, alternative="two.sided", var.equal=F)
plot(repress_negsettle.ttest)

plot(repressmodeldata$nego_settl, repressmodeldata$csrepress.r)

source("SummarySE_script.R")
repress_negsettle_summary <- summarySEwithin(data=repressmodeldata, measurevar=c("csrepress.r"), 
                                             withinvars=c("nego_settl"), na.rm=T, conf.interval=.95)
repress_negsettle_summary
ggplot(data=subset(repress_negsettle_summary, !is.na(nego_settl)), aes(x=nego_settl, y=csrepress.r)) +
  geom_bar(position=position_dodge(), stat="identity", width=0.8) +
  geom_errorbar(position=position_dodge(0.2), width=0.2, aes(ymin=csrepress.r-ci, ymax=csrepress.r+ci)) +
  coord_cartesian(ylim=c(0,4)) +
  theme_bw() +
  xlab("Negotiated Settlement") +
  theme(text =element_text(size=12 )) +
  ylab("CS Repression (Ordinal)") +
  ggtitle("Civil Society Repression in Peace Spells \nAfter Civil War (by Settlement)")

repress_rebelv_summary <- summarySEwithin(data=repressmodeldata, measurevar=c("csrepress.r"), 
                                          withinvars=c("rebel_v"), na.rm=T, conf.interval=.95)
repress_rebelv_summary
ggplot(data=subset(repress_rebelv_summary, !is.na(rebel_v)), aes(x=rebel_v, y=csrepress.r)) +
  geom_bar(position=position_dodge(), stat="identity", width=0.8) +
  geom_errorbar(position=position_dodge(0.2), width=0.2, aes(ymin=csrepress.r-ci, ymax=csrepress.r+ci)) +
  coord_cartesian(ylim=c(0,4)) +
  theme_bw() +
  xlab("Rebel Victory") +
  theme(text =element_text(size=12 )) +
  ylab("CS Repression (Ordinal)") +
  ggtitle("Civil Society Repression in Peace Spells \nAfter Civil War (by Rebel Victory)")

repress_govv_summary <- summarySEwithin(data=repressmodeldata, measurevar=c("csrepress.r"), 
                                        withinvars=c("gov_v"), na.rm=T, conf.interval=.95)
repress_govv_summary
ggplot(data=subset(repress_govv_summary, !is.na(gov_v)), aes(x=gov_v, y=csrepress.r)) +
  geom_bar(position=position_dodge(), stat="identity", width=0.8) +
  geom_errorbar(position=position_dodge(0.2), width=0.2, aes(ymin=csrepress.r-ci, ymax=csrepress.r+ci)) +
  coord_cartesian(ylim=c(0,4)) +
  theme_bw() +
  xlab("Government Victory") +
  theme(text =element_text(size=12 )) +
  ylab("CS Repression (Ordinal)") +
  ggtitle("Civil Society Repression in Peace Spells \nAfter Civil War (by Government Victory)")

#### 5 year tests ####
# need to create a 5 year after conflict termination variable to subset data again
repressmodeldata_5yrlimit <- subset(csrepression, peacefailure != 1 | NA,
                                    select =c(ccode, year, csrepress.r, at_5, case_firstyear, polity2, logpop,
                                              physint_lagged, wardur, csrepress_BIRT, poplog,
                                              pts_s_lagged, pts_h_lagged, pts_a_lagged, physint, logbattledeath,
                                              v2csreprss, v2csreprss_ord, pko, nego_settl, rebel_v, gov_v, gdplag,
                                              pop, polity2, peace_agreement, battledeath, peacefailure))
View(repressmodeldata_5yrlimit)
table(repressmodeldata_5yrlimit$year)

for(i in 1:nrow(repressmodeldata_5yrlimit)){
  if(i==1){
    repressmodeldata_5yrlimit$yearcount[i]=1 # For first point
  }
  else
    if(repressmodeldata_5yrlimit$ccode[i]==repressmodeldata_5yrlimit$ccode[i-1]){
    repressmodeldata_5yrlimit$yearcount[i]=repressmodeldata_5yrlimit$yearcount[i-1]+1 # If old_col values are same
  }
  else{
    repressmodeldata_5yrlimit$yearcount[i]=1  # When we have a new old_col value
  }
} 

repressed_first5 <- subset(repressmodeldata_5yrlimit, yearcount < 5,
                           select =c(ccode, year, csrepress.r, at_5, case_firstyear, polity2, logpop, physint_lagged,
                                     wardur, csrepress_BIRT, poplog,
                                     pts_s_lagged, pts_h_lagged, pts_a_lagged, physint, logbattledeath,
                                     v2csreprss, v2csreprss_ord, pko, nego_settl, rebel_v, gov_v, gdplag,
                                     pop, polity2, peace_agreement, battledeath, peacefailure, yearcount))
summary(repressed_first5)

repressed_after5 <- subset(repressmodeldata_5yrlimit, yearcount > 5,
                           select =c(ccode, year, csrepress.r, at_5, case_firstyear, polity2, logpop, physint_lagged,
                                     wardur,csrepress_BIRT, poplog,
                                     pts_s_lagged, pts_h_lagged, pts_a_lagged, physint, logbattledeath,
                                     v2csreprss, v2csreprss_ord, pko, nego_settl, rebel_v, gov_v, gdplag,
                                     pop, polity2, peace_agreement, battledeath, peacefailure, yearcount))
summary(repressed_after5)

tab_model(lm(csrepress_BIRT ~ nego_settl + polity2 + gdplag + poplog + physint_lagged + pko,
           data=repressed_after5))


table(repressed_first5$csrepress.r)
table(repressed_after5$csrepress.r)
repressed_first5$csrepress.r <- as.numeric(repressed_first5$csrepress.r)
hist(repressed_first5$csrepress.r)
repressed_after5$csrepress.r <- as.numeric(repressed_after5$csrepress.r)
hist(repressed_after5$csrepress.r)
d <- density(repressmodeldata$csrepress.r, na.rm=TRUE)
plot(d)
repressed_first5$csrepress.r <- as.numeric(repressed_first5$csrepress.r)
d_f5 <- density(repressed_first5$csrepress_BIRT, na.rm=TRUE)
plot(d_f5)
repressed_after5$csrepress.r <- as.numeric(repressed_after5$csrepress.r)
d_a5 <- density(repressed_after5$csrepress_BIRT, na.rm=TRUE)
plot(d_a5)
