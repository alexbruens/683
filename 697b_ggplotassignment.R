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

table(repressmodeldata$csrepress.r.2fac)

# DV : csrepress.r
# IV : nego_settl , gov_v , rebel_v
# C  : polity2 + gdplag + poplog + physint_lagged + pko
# C2 : polity2 + gdplag + poplog + physint_lagged + pko
# C3 : polity2 + gdplag + poplog + physint_lagged + pko
# only if peacefailure != 1
# peacefailure = 0 when there is no peacefailure, then peacefailure =1 and the observations end

