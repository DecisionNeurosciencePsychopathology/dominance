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
library(effect)
library(arm)
library(emmeans)
library(corrplot)
library(data.table)
library(plyr)
library(car)

# clear environment
rm(list=ls())

######################################
## glossary to dataset variables ##
######################################
# gameExp: rated from 1 to 5, 1 being no/almost no experience at all (any game on computers, smartphones, tablets counts as experience)
# trial: number of rounds (max. 24)
# score_new: number of apples eaten on a given round/trial
# oppScore: opponent's score computed based on scoreDiff, below such as oppScore = score - scoreDiff (this variable is not displayed in the current version of the game)
# scoreDiff: rigged score differences between player and opponent
# win: a given round's/trial's outcome: 1 = victory, 0 = defeat (rigged, so same for all players!)
# close: based on scoreDiff, 1 = tight competition, 0 = big score difference between player and opponent (displayed during snake game for the last 10 seconds of the game)
# oppRank: opponents'ranks; predefined sequence that is the same for all players (displayed at the beginning of each round/trial) (warning: rank 1 = highest!)
# oppRank_inv: opponent's rank with inverted scale (200 = highest rank)
# appleChoice: player's choice of point stealing, i.e. how many apples to take away from opponent before playing. 1 = none, 2 = 1 apple, 3 = 2 apples, 4 = 5 apples, 5 = 10 apples.
# rankChoice: player's choice of buying a booster to increase rank at the end of a given round/trial. 1 = none, 2 = + 1 rank, 3 = + 2 ranks, 4 = + 3 ranks, 5 = + 5 ranks.
# appleChoice_b: between-subject mean point stealing choice
# rankChoice_b: between-subject mean rank buying choice
# appleChoice_wi_0: mean-centered within-subject point stealing
# rankChoice_wi_0: mean-centered within-subject rank buying
# rankStart: player's rank at the end of the round/trial before rank buying (warning: rank 1 = highest!)
# rankEnd: player's rank at the end of the round/trial (after rank buying) (warning: rank 1 = highest!)
# rankEnd_inv: player's rank at the end of the trial with an inverted scale.
# rankGain_initial: rank gain on a given trial before rank buying choice.
# rankGain_final: rank gain on a given trial after rank buying choice
# household_income and household_incomeF:  1 = a)/tLess than $25,000   2 = b)Dropbox\USA\Pittsburgh\GitHub\dominance\paper1\t$25,000 - $49,999 3 = c)\t$50,000 - $74,999 4= d)\t$75,000 - $99,999 5 = e)\t$100,000 - $149,999 6 = f)\t$150,000 or above
# dass21_depression_tot : DASS 21 depression subscale (raw scores multiplied by 2, according to scoring guidelines; Sample 1)
# HRSD : Hamilton Rating Scale for Depression (Sample 2)
# group_dep: 1 = depressed (>14 on the HRSD), 0 = non-depressed (Sample 2)
# bpni_TOTAL : Brief Pathological Narcissism Inventory - total score
# ffni_total : Five-Factor Narcissism Inventory - total score
# ipip_total : International Personality Item Pool - trait dominance scale (Sample 2)
# race (Sample 2) = ethnicity_simp (Sample 1)
# .minus1, .minus2 : lagged variables.

########################################


## set working directory (where you have the participants' output data files )
setwd("~GitHub/dominance/paper1/public_files")


## load data files
cobra_tot_sample1 <- read.csv("cobra_tot_sample1.csv")
cobra_tot_sample1_shrunk <- read.csv("cobra_tot_sample1_shrunk.csv")

cobra_tot_sample2 <- read.csv("cobra_tot_sample2.csv")
cobra_tot_sample2_shrunk <- read.csv("cobra_tot_sample2_shrunk.csv")

cobra_both <- read.csv("cobra_both.csv")


### Supplemental Table S1

#Vancouver (sample 1)
library(tableone)
names(cobra_tot_sample1_shrunk)
listVars <- c("age","gender","ethnicity_simp","household_incomeF","gameExp","dass21_depression_tot", "dass21dep_percentile","ffni_total")
catVars <- c("gender", "ethnicity_simp", "household_incomeF")
table1 <- CreateTableOne(vars = listVars, data = cobra_tot_sample1_shrunk, factorVars = catVars)
table1

#Pittsburgh (sample 2)
library(tableone)
names(cobra_tot_sample2_shrunk)
listVars <- c("age_snake","gender.y","race","household_income","gameExp","HRSD","hrsd_percentile","ffni_total", "ipip_total")
catVars <- c("gender.y", "race")
table1 <- CreateTableOne(vars = listVars, data = cobra_tot_sample2_shrunk, factorVars = catVars)
table1

### Correlation plots
library(corrplot)
library(data.table)

## correlation tables
#Pittsburgh (sample 2) output: 9.5x8
chars <- cobra_tot_sample2[,c('ffni_total','ipip_total','HRSD','trial','oppRank_inv','win','scoreDiff','rankEnd.minus1_inv', 'appleChoice', 'rankChoice', 'score_new')]
chars$win <- as.numeric(chars$win)
corrplot.mixed(cor(chars, method = "spearman", use = "na.or.complete"), lower.col = "black", number.cex = 1.1, tl.pos = 'lt', lower = 'number', upper = 'circle')

#Vancouver (sample 1) output: 9x8 
chars2 <- cobra_tot_sample1[,c('ffni_total','dass21_depression_tot','trial','oppRank_inv','win','scoreDiff','rankEnd.minus1_inv', 'appleChoice', 'rankChoice', 'score_new')]
chars2$win <- as.numeric(chars2$win)
corrplot.mixed(cor(chars2, method = "spearman", use = "na.or.complete"), lower.col = "black", number.cex = 1.1, tl.pos = 'lt', lower = 'number', upper = 'circle')

## histograms of FFNI total scores (supplemental figure S2) output: 11x5.5
ggplot(cobra_tot_sample1_shrunk, aes(x = ffni_total)) + geom_histogram(bins = 13) + geom_rug() + theme_bw()
ggplot(cobra_tot_sample2_shrunk, aes(x = ffni_total)) + geom_histogram(bins = 13) + geom_rug() + theme_bw()



#### Main analysis with linear mixed-effects models

### I. DESIGN VARIABLES

## Vancouver (sample 1)
mappleA2_V <- lmer(appleChoice_wi_0 ~ trial + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(score_new.minus1) + scale(201-rankEnd.minus1) + (1|ID),  data = cobra_tot_sample1, na.action = na.omit)
summary(mappleA2_V)
car::Anova(mappleA2_V, type = 'III')

mappleA2_V_sens <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(score_new.minus1) + scale(201-rankEnd.minus1)  + scale(age) + scale(household_income) + ethnicity_simp + gender + scale(gameExp) + scale(dass21_depression_tot) + (1|ID),  data = cobra_tot_sample1, na.action = na.omit)
summary(mappleA2_V_sens)
car::Anova(mappleA2_V_sens, type = 'III')

mrankA2_V <- lmer(rankChoice_wi_0 ~ scale(trial)*scale(oppRank_inv) + win + scale(score_new) + scale(201-rankStart) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = cobra_tot_sample1, na.action = na.omit)
summary(mrankA2_V)
car::Anova(mrankA2_V, type = 'III')

mrankA2_V_sens <- lmer(rankChoice_wi_0 ~ scale(trial)*scale(oppRank_inv) + win + scale(score_new) + scale(201-rankStart) + scale(rankChoice_wi_0.minus1) + scale(age) + scale(household_income) + ethnicity_simp + gender + scale(gameExp) + scale(dass21_depression_tot) + (1|ID),  data = cobra_tot_sample1, na.action = na.omit)
summary(mrankA2_V_sens)
car::Anova(mrankA2_V_sens, type = 'III')

## Pittsburgh (sample 2)
mappleA2_P <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(score_new.minus1) + scale(201-rankEnd.minus1) + (1|ID),  data = cobra_tot_sample2, na.action = na.omit)
summary(mappleA2_P)
car::Anova(mappleA2_P, type = 'III')

mappleA2_P_sens <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(score_new.minus1) + scale(201-rankEnd.minus1) + scale(age_snake) + gender.y + scale(education) + race + scale(gameExp) + scale(household_income_log) + scale(HRSD) + (1|ID),  data = cobra_tot_sample2, na.action = na.omit)
summary(mappleA2_P_sens)
car::Anova(mappleA2_P_sens, type = 'III')

mrankA0_P <- lmer(rankChoice_wi_0 ~ scale(trial)*scale(oppRank_inv) + win + scale(201-rankStart) + scale(score_new) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = cobra_tot_sample2, na.action = na.omit)
summary(mrankA0_P)
car::Anova(mrankA0_P, type = 'III')

mrankA0_P_sens <- lmer(rankChoice_wi_0 ~ scale(trial)*scale(oppRank_inv) + win + scale(201-rankStart) + scale(score_new) + scale(rankChoice_wi_0.minus1)  + scale(age_snake) + gender.y + scale(education) + race + scale(gameExp) + scale(household_income_log) + scale(HRSD) + (1|ID),  data = cobra_tot_sample2, na.action = na.omit)
summary(mrankA0_P_sens)
car::Anova(mrankA0_P_sens, type = 'III')

# model plots (figure 2), dimensions for manuscript: 12 x 3

library(sjPlot)
pSample1_design_apple <- plot_model(mappleA2_V_sens, type = "est", show.p = TRUE, show.values = TRUE, title = "Sample 1 - UNDERGRADUATES (N = 70)",
                              rm.terms = c("scale(age)", "scale(dass21_depression_tot)", "scale(gameExp)", "scale(household_income)", "ethnicity_simpblack", "ethnicity_simpother","ethnicity_simpasian", "gendermale"),
                              order.terms = c(1,4,5,7,3,2,6),
                              colors = c("#008600", "#c55a11"), show.legend = FALSE, value.offset = .3, value.size = 4, vline.color = 'grey80', grid.breaks = .1) + theme_bw()

pSample1_design_rank <- plot_model(mrankA2_V_sens, type = "est", show.p = TRUE, show.values = TRUE, title = "Sample 1 - UNDERGRADUATES (N = 70)",
                             rm.terms = c("scale(age)", "scale(dass21_depression_tot)", "scale(gameExp)", "scale(household_income)", "ethnicity_simpblack", "ethnicity_simpother","ethnicity_simpasian", "gendermale"),
                             order.terms = c(1,2,7,4,3,5,6), colors = c("#008600","#c55a11"), show.legend = FALSE, value.offset = .3, value.size = 4, vline.color = 'grey80', grid.breaks = .1) + theme_bw()


pSample2_design_apple <- plot_model(mappleA2_P_sens, show.p = TRUE, show.values = TRUE, title = "Sample 2 - ELDERLY (N = 85)",
                              rm.terms = c("scale(age_snake)", "scale(HRSD)", "scale(gameExp)", "scale(household_income_log)", "raceasian", "raceblack","raceother", "gender.yMALE", "scale(education)"),
                              order.terms = c(1,4,5,7,3,2,6), colors = c("#008600","#c55a11"), show.legend = FALSE, value.offset = .3, value.size = 4, vline.color = 'grey80', grid.breaks = .1) + theme_bw()

pSample2_design_rank <- plot_model(mrankA0_P_sens, type = "est", show.p = TRUE, show.values = TRUE, title = "Sample 2 - ELDERLY (N = 85)",
                             rm.terms = c("scale(age_snake)", "scale(HRSD)", "scale(gameExp)", "scale(household_income_log)", "raceasian", "raceother","raceblack", "gender.yMALE", "scale(education)"),
                             order.terms = c(1,2,7,5,3,4,6), colors = c( "#008600","#c55a11"), show.legend = FALSE, value.offset = .3, value.size = 4, vline.color = 'grey80', grid.breaks = .1) + theme_bw()

library(grid)
library(gridExtra)
grid.arrange(pSample2_design_apple,pSample2_design_rank,pSample1_design_apple,pSample1_design_rank,
             layout_matrix = matrix(c(3,1,4,2), ncol=2, byrow=TRUE))



### II. TRAIT DOMINANCE in Pittsburgh sample (sample 2) - best-fitting models

## point stealing 

mappleA1_ipip2 <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ipip_total) + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(ipip_total) +  scale(oppRank_inv)*scale(score_new.minus1) + scale(201-rankEnd.minus1) + (1|ID),  data = cobra_tot_sample2, na.action = na.omit)
summary(mappleA1_ipip2)
car::Anova(mappleA1_ipip2, type = 'III')

mappleA1_ipip2_sens <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ipip_total) + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(ipip_total) +  scale(oppRank_inv)*scale(score_new.minus1) + scale(201-rankEnd.minus1) + scale(age_snake) + gender.y + scale(education) + race + scale(gameExp) + scale(household_income_log) + scale(HRSD) + (1|ID),  data = cobra_tot_sample2, na.action = na.omit)
summary(mappleA1_ipip2_sens)
car::Anova(mappleA1_ipip2_sens, type = 'III')

## rank buying

mrankA2_ipip1 <- lmer(rankChoice_wi_0 ~ scale(ipip_total)*scale(trial) + win + scale(201-rankStart) + scale(oppRank_inv)*scale(trial) + scale(score_new) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = cobra_tot_sample2, na.action = na.omit)
summary(mrankA2_ipip1)
car::Anova(mrankA2_ipip1, type = 'III')

mrankA2_ipip1_sens <- lmer(rankChoice_wi_0 ~ scale(ipip_total)*scale(trial) + win + scale(201-rankStart) + scale(oppRank_inv)*scale(trial) + scale(score_new) + scale(rankChoice_wi_0.minus1) + scale(age_snake) + gender.y + scale(education) + race + scale(gameExp) + scale(household_income_log) + scale(HRSD) + (1|ID),  data = cobra_tot_sample2, na.action = na.omit)
summary(mrankA2_ipip1_sens)
car::Anova(mrankA2_ipip1_sens, type = 'III')


# plots for manuscript (Figures 3A and 4A)
a <- sd(cobra_tot_sample2_shrunk$ipip_total, na.rm = TRUE)
b <- mean(cobra_tot_sample2_shrunk$ipip_total, na.rm = TRUE)
b + a
b - a

library(emmeans)
#dominance*trial interactions for point stealing and rank buying (output dimension: 4 x 3) - Figure 3A
emmip(mappleA1_ipip2_sens, ipip_total ~ trial, at = list(ipip_total = c(18,35), trial = c(1,12,24)), CIs = TRUE, col = c("#008600", "#c55a11")) +
  scale_fill_manual(values = c("#008600", "#c55a11")) +
  scale_color_manual(values = c("#008600", "#c55a11"))

emmip(mrankA2_ipip1_sens, ipip_total ~ trial, at = list(ipip_total = c(18,35), trial = c(1,12,24)), CIs = TRUE, col = c("#008600", "#c55a11")) +
  scale_fill_manual(values = c("#008600", "#c55a11")) +
  scale_color_manual(values = c("#008600", "#c55a11"))

#dominance*opponent's rank (output dimension: 5 x 3) - Figure 4A
emmip(mappleA1_ipip2_sens, ipip_total ~ oppRank_inv, at = list(ipip_total = c(18,35), oppRank_inv = c(1,100,200)), CIs = TRUE, col = c("#008600", "#c55a11")) +
  theme_bw() +
  scale_fill_manual(values = c("#008600", "#c55a11")) +
  scale_color_manual(values = c("#008600", "#c55a11"))



### III. NARCISSISM - trial*narcissism interaction - best-fitting models

## Vancouver (Sample 1)

# point stealing
mappleA1_ffni1 <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ffni_total) + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(score_new.minus1) + scale(201-rankEnd.minus1) + (1|ID),  data = cobra_tot_sample1, na.action = na.omit)
summary(mappleA1_ffni1)
car::Anova(mappleA1_ffni1, type = 'III')

mappleA1_ffni1_sens <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ffni_total) + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(score_new.minus1) + scale(201-rankEnd.minus1) + scale(age) + scale(household_income) +  ethnicity_simp + gender + scale(gameExp) + scale(dass21_depression_tot) + (1|ID),  data = cobra_tot_sample1, na.action = na.omit)
summary(mappleA1_ffni1_sens)
car::Anova(mappleA1_ffni1_sens, type = 'III')

car::vif(mappleA1_ffni1_sens)

# rank buying
mrankA2_1_ffni1 <- lmer(rankChoice_wi_0 ~ scale(trial)*scale(ffni_total)*scale(oppRank_inv) + win + scale(score_new) + scale(201-rankStart) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = cobra_tot_sample1, na.action = na.omit)
summary(mrankA2_1_ffni1)
car::Anova(mrankA2_1_ffni1, type = 'III')

mrankA2_1_ffni1_sens <- lmer(rankChoice_wi_0 ~ scale(trial)*scale(ffni_total)*scale(oppRank_inv) + win + scale(score_new) + scale(201-rankStart) + scale(rankChoice_wi_0.minus1) + scale(age) + scale(household_income) +  ethnicity_simp + gender + scale(gameExp) + scale(dass21_depression_tot) + (1|ID),  data = cobra_tot_sample1, na.action = na.omit)
summary(mrankA2_1_ffni1_sens)
car::Anova(mrankA2_1_ffni1_sens, type = 'III')

car::vif(mrankA2_1_ffni1_sens)


## Pittsburgh (Sample 2)

# point stealing
mappleA1_ffni1_P <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ffni_total) + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(score_new.minus1) + scale(201-rankEnd.minus1) + (1|ID),  data = cobra_tot_sample2, na.action = na.omit)
summary(mappleA1_ffni1_P)
car::Anova(mappleA1_ffni1_P, type = 'III')

mappleA1_ffni1_sens_P <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ffni_total) + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(score_new.minus1) + scale(201-rankEnd.minus1) + scale(age_snake) + scale(household_income_log) + gender.y + scale(education) + race + scale(gameExp) + scale(HRSD) + (1|ID),  data = cobra_tot_sample2, na.action = na.omit)
summary(mappleA1_ffni1_sens_P)
car::Anova(mappleA1_ffni1_sens_P, type = 'III')

car::vif(mappleA1_ffni1_sens_P)

# rank buying
mrankA_ffni3_P <- lmer(rankChoice_wi_0 ~ scale(ffni_total)*scale(trial) + scale(201-rankStart) + scale(oppRank_inv)*scale(trial) + scale(score_new) + win + scale(rankChoice_wi_0.minus1) + (1|ID),  data = cobra_tot_sample2, na.action = na.omit)
summary(mrankA_ffni3_P)
car::Anova(mrankA_ffni3_P, type = 'III')

mrankA_ffni3_sens_P <- lmer(rankChoice_wi_0 ~ scale(ffni_total)*scale(trial)*scale(oppRank_inv) + scale(201-rankStart) + scale(score_new) + win + scale(rankChoice_wi_0.minus1) + scale(age_snake) + scale(household_income_log) + gender.y + scale(education) + race + scale(gameExp) + scale(HRSD) + (1|ID),  data = cobra_tot_sample2, na.action = na.omit)
summary(mrankA_ffni3_sens_P)
car::Anova(mrankA_ffni3_sens_P, type = 'III')

car::vif(mrankA_ffni3_sens_P)


## plots for manuscript (Figure 3B)
library(emmeans)

# Vancouver (sample 1)
a <- sd(cobra_tot_sample1_shrunk$ffni_total, na.rm = TRUE)
b <- mean(cobra_tot_sample1_shrunk$ffni_total, na.rm = TRUE)
b + a
b - a

emmip(mrankA2_1_ffni1_sens, ffni_total ~ trial, at = list(ffni_total = c(125,181), trial = c(1,12,24)), CIs = TRUE, col = c("#008600", "#c55a11")) +
  scale_fill_manual(values = c("#008600", "#c55a11")) +
  scale_color_manual(values = c("#008600", "#c55a11"))

#Pittsburgh (sample 2)
a <- sd(cobra_tot_sample2_shrunk$ffni_total, na.rm = TRUE)
b <- mean(cobra_tot_sample2_shrunk$ffni_total, na.rm = TRUE)
b + a
b - a

emmip(mappleA1_ffni1_sens_P, ffni_total ~ trial, at = list(ffni_total = c(109,160), trial = c(1,12,24)), CIs = TRUE, col = c("#008600", "#c55a11")) +
  scale_fill_manual(values = c("#008600", "#c55a11")) +
  scale_color_manual(values = c("#008600", "#c55a11"))

emmip(mrankA_ffni3_sens_P, ffni_total ~ trial, at = list(ffni_total = c(109,160), trial = c(1,12,24)), CIs = TRUE, col = c("#008600", "#c55a11")) +
  scale_fill_manual(values = c("#008600", "#c55a11")) +
  scale_color_manual(values = c("#008600", "#c55a11"))



### IV. DEPRESSION (best-fitting models)

## Vancouver (sample 1)

# point stealing
mappleA1_dep5 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(dass21_depression_tot)*scale(oppRank_inv) + scale(oppRank_inv)*scale(score_new.minus1) + scale(201-rankEnd.minus1) + (1|ID),  data = cobra_tot_sample1, na.action = na.omit)
summary(mappleA1_dep5)
car::Anova(mappleA1_dep5, type = 'III')

mappleA1_dep5_sens <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(dass21_depression_tot)*scale(oppRank_inv) + scale(oppRank_inv)*scale(score_new.minus1) + scale(201-rankEnd.minus1) + scale(age) + scale(household_income) + ethnicity_simp + gender + scale(gameExp) + (1|ID),  data = cobra_tot_sample1, na.action = na.omit)
summary(mappleA1_dep5_sens)
car::Anova(mappleA1_dep5_sens, type = 'III')

car::vif(mappleA1_dep5_sens)

## Pittsburgh (sample 2)

# point stealing
mappleA1_dep2 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(HRSD) + scale(oppRank_inv)*scale(score_new.minus1) + scale(201-rankEnd.minus1) + (1|ID),  data = cobra_tot_sample2, na.action = na.omit)
summary(mappleA1_dep2)
car::Anova(mappleA1_dep2, type = 'III')

mappleA1_dep2_sens <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(HRSD) + scale(oppRank_inv)*scale(score_new.minus1) + scale(201-rankEnd.minus1) + scale(age_snake) + scale(household_income_log) + gender.y + scale(education) + race + scale(gameExp) + (1|ID),  data = cobra_tot_sample2, na.action = na.omit)
summary(mappleA1_dep2_sens)
car::Anova(mappleA1_dep2_sens, type = 'III')

car::vif(mappleA1_dep2_sens)


#plots for manuscript (figure 4, panel B)
library(emmeans)

# Vancouver (Sample 1)
a <- sd(cobra_tot_sample1_shrunk$dass21_depression_tot, na.rm = TRUE)
b <- mean(cobra_tot_sample1_shrunk$dass21_depression_tot, na.rm = TRUE)
b + a
b - a

emmip(mappleA1_dep5_sens, dass21_depression_tot ~ oppRank_inv, at = list(dass21_depression_tot = c(1,18), oppRank_inv = c(1,100,200)), CIs = TRUE, col = c("#008600", "#c55a11")) +
  theme_bw() +
  scale_fill_manual(values = c("#008600", "#c55a11")) +
  scale_color_manual(values = c("#008600", "#c55a11"))

# Pittsburgh (Sample 2)
a <- sd(cobra_tot_sample2_shrunk$HRSD, na.rm = TRUE)
b <- mean(cobra_tot_sample2_shrunk$HRSD, na.rm = TRUE)
b + a
b - a

emmip(mappleA1_dep2_sens, HRSD ~ oppRank_inv, at = list(HRSD = c(9,22), oppRank_inv = c(1,100,200)), CIs = TRUE, col = c("#008600", "#c55a11")) +
  theme_bw() +
  scale_fill_manual(values = c("#008600", "#c55a11")) +
  scale_color_manual(values = c("#008600", "#c55a11"))



### V. POOLED ANALYSIS

# (i) design variables
mappleA2_both_sens <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(score_new.minus1) + scale(201-rankEnd.minus1)  + sample +  scale(age) + scale(household_income) + ethnicity_simp + gender + scale(education) + scale(gameExp) + scale(dep) + (1|ID),  data = cobra_both, na.action = na.omit)
summary(mappleA2_both_sens)
car::Anova(mappleA2_both_sens, type = 'III')

car::vif(mappleA2_both_sens)

mrankA2_both_sens <- lmer(rankChoice_wi_0 ~ scale(trial)*scale(oppRank_inv) + win + scale(score_new) + scale(201-rankStart) + scale(rankChoice_wi_0.minus1) + sample + scale(age) + scale(household_income) + ethnicity_simp + gender + scale(education) + scale(gameExp) + scale(dep) + (1|ID),  data = cobra_both, na.action = na.omit)
summary(mrankA2_both_sens)
car::Anova(mrankA2_both_sens, type = 'III')

car::vif(mrankA2_both_sens)

# interactions with sample
mappleA2_both_sens2 <- lmer(appleChoice_wi_0 ~ scale(trial)*sample + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(score_new.minus1)*sample + scale(201-rankEnd.minus1)*sample +  scale(age) + scale(household_income)*sample + ethnicity_simp + gender + scale(education) + scale(gameExp) + scale(dep)*sample + (1|ID),  data = cobra_both, na.action = na.omit)
summary(mappleA2_both_sens2)
car::Anova(mappleA2_both_sens2, type = 'III')

mappleA2_both_sens3 <- lmer(appleChoice_wi_0 ~ scale(trial)*sample + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(score_new.minus1) + scale(oppRank_inv)*sample + scale(oppRank_inv)*scale(201-rankEnd.minus1) +  scale(age) + scale(household_income) + ethnicity_simp + gender + scale(education) + scale(gameExp) + scale(dep) + (1|ID),  data = cobra_both, na.action = na.omit)
summary(mappleA2_both_sens3)
car::Anova(mappleA2_both_sens3, type = 'III')

mappleA2_both_sens4 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(score_new.minus1) + scale(oppRank_inv)*sample + scale(oppRank_inv)*scale(201-rankEnd.minus1) +  scale(age) + scale(household_income) + ethnicity_simp + gender + scale(education) + scale(gameExp) + scale(dep) + (1|ID),  data = cobra_both, na.action = na.omit)
summary(mappleA2_both_sens4)
car::Anova(mappleA2_both_sens4, type = 'III')

mrankA2_both_sens2 <- lmer(rankChoice_wi_0 ~ scale(trial)*scale(oppRank_inv)*sample + win + scale(score_new) + scale(201-rankStart)*sample + scale(rankChoice_wi_0.minus1) + scale(age) + scale(household_income)*sample + ethnicity_simp + gender + scale(education) + scale(gameExp) + scale(dep)*sample + (1|ID),  data = cobra_both, na.action = na.omit)
summary(mrankA2_both_sens2)
car::Anova(mrankA2_both_sens2, type = 'III')

mrankA2_both_sens3 <- lmer(rankChoice_wi_0 ~ scale(trial)*scale(oppRank_inv) + scale(trial)*sample + win + scale(score_new) + scale(rankStart_inv)*sample + scale(rankChoice_wi_0.minus1) + scale(age) + scale(household_income) + ethnicity_simp + gender + scale(education) + scale(gameExp) + scale(dep) + (1|ID),  data = cobra_both, na.action = na.omit)
summary(mrankA2_both_sens3)
car::Anova(mrankA2_both_sens3, type = 'III')


# (ii) with narcissistic scales
mappleA1_ffni1_sens_both <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ffni_total) + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(score_new.minus1) + scale(201-rankEnd.minus1) + sample + scale(age) + scale(household_income) +  ethnicity_simp + gender + scale(education) + scale(gameExp) + scale(dep) + (1|ID),  data = cobra_both, na.action = na.omit)
summary(mappleA1_ffni1_sens_both)
car::Anova(mappleA1_ffni1_sens_both, type = 'III')

car::vif(mappleA1_ffni1_sens_both)

mrankA2_1_ffni1_sens_both <- lmer(rankChoice_wi_0 ~ scale(trial)*scale(ffni_total)*scale(oppRank_inv) + win + scale(score_new) + scale(201-rankStart) + scale(rankChoice_wi_0.minus1) + sample + scale(age) + scale(household_income) +  ethnicity_simp + gender + scale(education) + scale(gameExp) + scale(dep) + (1|ID),  data = cobra_both, na.action = na.omit)
summary(mrankA2_1_ffni1_sens_both)
car::Anova(mrankA2_1_ffni1_sens_both, type = 'III')

car::vif(mrankA2_1_ffni1_sens_both)


# sensitivity analysis of narcissism with the BPNI scale
mappleA1_bpni1_sens_both <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(bpni_TOTAL) + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(score_new.minus1) + scale(201-rankEnd.minus1) + sample + scale(age) + scale(household_income) + ethnicity_simp + gender + scale(education) + scale(gameExp) + scale(dep) + (1|ID),  data = cobra_both, na.action = na.omit)
summary(mappleA1_bpni1_sens_both)
car::Anova(mappleA1_bpni1_sens_both, type = 'III')

car::vif(mappleA1_bpni1_sens_both)

mrankA2_1_bpni1_sens_both <- lmer(rankChoice_wi_0 ~ scale(trial)*scale(bpni_TOTAL) + scale(trial)*scale(oppRank_inv) + win + scale(score_new) + scale(201-rankStart) + scale(rankChoice_wi_0.minus1) + sample + scale(age) + scale(household_income) +  ethnicity_simp + gender + scale(education) + scale(gameExp) + scale(dep) + (1|ID),  data = cobra_both, na.action = na.omit)
summary(mrankA2_1_bpni1_sens_both)
car::Anova(mrankA2_1_bpni1_sens_both, type = 'III')

car::vif(mrankA2_1_bpni1_sens_both)


# interactions with sample
mappleA1_ffni1_sens_both2 <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ffni_total)*sample + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(score_new.minus1) + scale(201-rankEnd.minus1) + scale(age) + scale(household_income) +  ethnicity_simp + gender + scale(education) + scale(gameExp) + scale(dep) + (1|ID),  data = cobra_both, na.action = na.omit)
summary(mappleA1_ffni1_sens_both2)
car::Anova(mappleA1_ffni1_sens_both2, type = 'III')

mrankA2_1_ffni1_sens_both2 <- lmer(rankChoice_wi_0 ~ scale(trial)*scale(ffni_total)*scale(oppRank_inv)*sample + win + scale(score_new) + scale(201-rankStart) + scale(rankChoice_wi_0.minus1) + scale(age) + scale(household_income) +  ethnicity_simp + gender + scale(education) + scale(gameExp) + scale(dep) + (1|ID),  data = cobra_both, na.action = na.omit)
summary(mrankA2_1_ffni1_sens_both2)
car::Anova(mrankA2_1_ffni1_sens_both2, type = 'III')

mappleA1_bpni1_sens_both2 <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(bpni_TOTAL)*sample + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(score_new.minus1) + scale(201-rankEnd.minus1) + scale(age) + scale(household_income) + ethnicity_simp + gender + scale(education) + scale(gameExp) + scale(dep) + (1|ID),  data = cobra_both, na.action = na.omit)
summary(mappleA1_bpni1_sens_both2)
car::Anova(mappleA1_bpni1_sens_both2, type = 'III')

mrankA2_1_bpni1_sens_both2 <- lmer(rankChoice_wi_0 ~ scale(trial)*scale(bpni_TOTAL)*sample + scale(trial)*scale(oppRank_inv) + win + scale(score_new) + scale(201-rankStart) + scale(rankChoice_wi_0.minus1) + scale(age) + scale(household_income) +  ethnicity_simp + gender + scale(education) + scale(gameExp) + scale(dep) + (1|ID),  data = cobra_both, na.action = na.omit)
summary(mrankA2_1_bpni1_sens_both2)
car::Anova(mrankA2_1_bpni1_sens_both2, type = 'III')


# (iv) with depression
mappleA1_dep2_sens_both <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(dep) + scale(oppRank_inv)*scale(score_new.minus1) + scale(201-rankEnd.minus1) + sample + scale(age) + scale(household_income) +  ethnicity_simp + gender + scale(education) + scale(gameExp) + scale(dep) + (1|ID),  data = cobra_both, na.action = na.omit)
summary(mappleA1_dep2_sens_both)
car::Anova(mappleA1_dep2_sens_both, type = 'III')

car::vif(mappleA1_dep2_sens_both)

#interaction with sample
mappleA1_dep2_sens_both2 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(dep)*sample + scale(oppRank_inv)*scale(score_new.minus1) + scale(201-rankEnd.minus1) + scale(age) + scale(household_income) +  ethnicity_simp + gender + scale(education) + scale(gameExp) + scale(dep) + (1|ID),  data = cobra_both, na.action = na.omit)
summary(mappleA1_dep2_sens_both2)
car::Anova(mappleA1_dep2_sens_both2, type = 'III')


##############################
### SUPPLEMENTARY ANALYSES

## Replication of depression findings with categorical depression variable in Sample 2

mappleA1_GRdep1_sens <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank_inv)*group_dep + scale(oppRank_inv)*scale(score_new.minus1) + scale(201-rankEnd.minus1) + scale(age_snake) + scale(household_income_log) + gender.y + scale(education) + race + scale(gameExp) + (1|ID),  data = cobra_tot_sample2, na.action = na.omit)
summary(mappleA1_GRdep1_sens)
car::Anova(mappleA1_GRdep1_sens, type = 'III')

mappleA1_GRdep2_sens <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank_inv)*group_dep*scale(ffni_total) + scale(oppRank_inv)*scale(score_new.minus1) + scale(201-rankEnd.minus1) + scale(age_snake) + scale(household_income_log) + gender.y + scale(education) + race + scale(gameExp) + (1|ID),  data = cobra_tot_sample2, na.action = na.omit)
summary(mappleA1_GRdep2_sens)
car::Anova(mappleA1_GRdep2_sens, type = 'III')

car::vif(mappleA1_GRdep2_sens)


## Performance analyses

pm0allP <- lmer(score_new ~ scale(trial) + gender.y + scale(age_snake) + scale(oppRank_inv) + win.minus1 + scale(gameExp) + race + (1|ID), data = cobra_tot_sample2)
summary(pm0allP)
car::Anova(pm0allP,'3')

pm0allV <- lmer(score_new ~ scale(trial) + gender + scale(age) + scale(oppRank_inv) + win.minus1 + scale(gameExp) + ethnicity_simp + (1|ID), data = cobra_tot_sample1) 
summary(pm0allV)
car::Anova(pm0allV,'3')

car::vif(pm0allP)

#ipip (in Sample 2)
pm0allP_ipip1 <- lmer(score_new ~ scale(trial)*scale(ipip_total) + scale(oppRank_inv)*scale(ipip_total) + (1|ID), data = cobra_tot_sample2)
summary(pm0allP_ipip1)
car::Anova(pm0allP_ipip1,'3')

pm0allP_ipip2 <- lmer(score_new ~ scale(trial)*scale(ipip_total) + scale(oppRank_inv) + win.minus1 + gender.y + scale(age_snake) + scale(gameExp) + race + (1|ID), data = cobra_tot_sample2)
summary(pm0allP_ipip2)
car::Anova(pm0allP_ipip2,'3')

car::vif(pm0allP_ipip2)

#narcissism
pm0allV <- lmer(score_new ~ scale(trial)*scale(bpni_TOTAL) + gender + scale(age) + scale(oppRank) + win.minus1 + scale(gameExp) + scale(appleChoice) + ethnicity_simp + (1|ID), data = cobra_tot_sample1) 
summary(pm0allV)
car::Anova(pm0allV,'3')

pm0allP <- lmer(score_new ~ scale(trial)*scale(bpni_TOTAL) + gender.y + scale(age_snake) + scale(oppRank) + win.minus1 + scale(gameExp) + scale(appleChoice) + race + (1|ID), data = cobra_tot_sample2)
summary(pm0allP)
car::Anova(pm0allP,'3')

pm0allV <- lmer(score_new ~ scale(trial)*scale(ffni_total) + gender + scale(age) + scale(oppRank) + win.minus1 + scale(gameExp) + scale(appleChoice) + ethnicity_simp + (1|ID), data = cobra_tot_sample1) 
summary(pm0allV)
car::Anova(pm0allV,'3')

pm0allP <- lmer(score_new ~ scale(trial)*scale(ffni_total) + gender.y + scale(age_snake) + scale(oppRank) + win.minus1 + scale(gameExp) + scale(appleChoice) + race + (1|ID), data = cobra_tot_sample2)
summary(pm0allP)
car::Anova(pm0allP,'3')

#depression
pm0allV <- lmer(score_new ~ scale(trial)*scale(dass21_depression_tot) + gender + scale(age) + scale(oppRank) + win.minus1 + scale(gameExp) + scale(appleChoice) + ethnicity_simp + (1|ID), data = cobra_tot_sample1) 
summary(pm0allV)
car::Anova(pm0allV,'3')

pm0allP <- lmer(score_new ~ scale(trial)*scale(HRSD) + gender.y + scale(age_snake) + scale(oppRank) + win.minus1 + scale(gameExp) + scale(appleChoice) + race + (1|ID), data = cobra_tot_sample2)
summary(pm0allP)
car::Anova(pm0allP,'3')


# plots and model summaries for Supplement
library(sjPlot)
pP_score_new_all <- plot_model(pm0allP, type = "est", show.p = TRUE, show.values = TRUE, title = "Sample 2", colors = c( "#008600","#c55a11"), order.terms = c(1,4,5,3,2,6,7,8,9,10,11,12), value.offset = .3, value.size = 4, vline.color = 'grey80') + theme_bw()


pV_score_new_all <- plot_model(pm0allV, type = "est", show.p = TRUE, show.values = TRUE, title = "Sample 1", colors = c( "#008600","#c55a11"), order.terms = c(1,4,5,3,2,6,7,8,10,9,11,12), value.offset = .3, value.size = 4, vline.color = 'grey80') + theme_bw()

library(grid)
library(gridExtra)
grid.arrange(pV_score_new_all,pP_score_new_all,
             layout_matrix = matrix(c(1,2), ncol=2, byrow=TRUE))

# saved as 5x3
emmip(pm0allP_ipip2, ipip_total ~ trial, at = list(ipip_total = c(10,43), trial = c(1,12,24)), CIs = TRUE, col = c("#008600", "#c55a11")) +
  theme_bw() +
  scale_fill_manual(values = c("#008600", "#c55a11")) +
  scale_color_manual(values = c("#008600", "#c55a11"))

