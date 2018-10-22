library(readxl)
library(readr)
library(lme4) # this is the one to use for modeling
library(ggplot2)
library(dplyr)
library(tidyr)
library(psych)
library(gdata)
library(R.matlab)
library(xtable)
library(Hmisc)
library(foreign)
library(MASS)
library("lsmeans")
library(effects)
library(arm)
library(lme4)

setwd("~/Dropbox/USA/Pittsburgh/GitHub/dominance/snake_data_Pittsburgh_may2018")

load("snake_totP.Rda")

## the main variable of interest is trial, because we wanted to see increasing apple-stealing and rank-buying throughout time (i.e. the number of trials)

# models with dominance scale (ipip_total) for apple-stealing and rank-buying:

mappleA1_ipip2 <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ipip_total) + close.minus1 + win.minus1 + scale(oppRank)*scale(ipip_total) +  scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ipip2)
car::Anova(mappleA1_ipip2, type = 'III')

mrankA2_ipip1 <- lmer(rankChoice_wi_0 ~ scale(ipip_total)*scale(trial) + win + scale(rankStart) + scale(oppRank) + scale(scoreDelta) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA2_ipip1)
car::Anova(mrankA2_ipip1, type = 'III')

# With our preliminary sample, adding group differences (I am not sure)

mappleA1_ipip1 <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ipip_total)*group1_5 + close.minus1 + win.minus1 + scale(oppRank)  +  scale(score.minus1) + scale(rankEnd.minus1) + scale(age_snake) + gender.y + scale(education) + race + scale(gameExp) + scale(household_income_log) + scale(HRSD_no_suic) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ipip1)
car::Anova(mappleA1_ipip1, type = 'III')

plot(effect("scale(trial):scale(ipip_total):group1_5",mappleA1_ipip2), grid=TRUE)
vif.lme(mappleA1_ipip1)

mappleA1_ipip2 <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ipip_total) + scale(oppRank)*group1_5 + close.minus1 + win.minus1*group1_5 + scale(oppRank)*scale(ipip_total)  +  scale(score.minus1) + scale(rankEnd.minus1) + scale(age_snake) + gender.y + scale(education) + race + scale(gameExp) + scale(household_income_log) + scale(HRSD_no_suic) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ipip2)
car::Anova(mappleA1_ipip2, type = 'III')

anova(mappleA1_ipip1, mappleA1_ipip2, "II")

mrankA2_ipip1 <- lmer(rankChoice_wi_0 ~ scale(trial)*scale(ipip_total) + scale(trial)*group1_5 + win + scale(rankStart) + scale(oppRank) + scale(scoreDelta) + scale(rankChoice_wi_0.minus1) + scale(age_snake) + gender.y + scale(education) + race + scale(gameExp) + scale(household_income_log) + scale(HRSD_no_suic) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA2_ipip1)
car::Anova(mrankA2_ipip1, type = 'III')

## plots
#plot based on GLM
library(emmeans)

emmip(mappleA1_ipip1, group1_5*ipip_total ~ trial, at = list(ipip_total = c(18,34), trial = c(1,12,24)), CIs = TRUE, col = c("black", "black", "black", "black", "#c55a11", "#c55a11", "#c55a11", "#c55a11")) +
  theme_bw() +
  facet_wrap(~ group1_5) +
  scale_fill_manual(values = c("black", "black", "black", "black", "#c55a11", "#c55a11", "#c55a11", "#c55a11")) +
  scale_color_manual(values = c("black", "black", "black", "black", "#c55a11", "#c55a11", "#c55a11", "#c55a11"))

emmip(mrankA2_ipip1, ipip_total ~ trial, at = list(ipip_total = c(18,34), trial = c(1,12,24)), CIs = TRUE, col = c("black", "#c55a11")) +
  theme_bw() +
  scale_fill_manual(values = c("black", "#c55a11")) +
  scale_color_manual(values = c("black", "#c55a11"))
