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

# clear environment
rm(list=ls())

## function checking for colinearity:
#  for collinearity diagnostics
vif.lme <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)] }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v }

######################################
## glossary to snake game variables ##
######################################
# gender: 1 = male, 2 = female
# gameExp: rated from 1 to 5, 1 being no/almost no experience at all (any game on computers, smartphones, tablets counts as experience)
# rankEstim 1 and 2: final rank estimated by participants after the practice round (rankEstim1) and at the end of the game (rankEstim2) (warning: rank 1 = highest!)
# avatarChoice: participants' choice of 1 out of 4 avatars they are presented with
# consentChoice: 1 = agree to be listed in the ranking for subsequent players, 0 = does not agree
# snakeLevel: snake's speed established based on performance during practice phase. 1 = easy/slow, 2 = medium, 3 = difficult/fast
# excited, upset, scared, hostile, proud, irritable, alert, ashamed, nervous, determined: the 10 PANAS affects best related to competition (assessed both at baseline and when finishing the game)
# trial: number of rounds (max. 24)
# score: number of apples eaten on a given round/trial
# oppScore: opponent's score computed based on scoreDiff, below such as oppScore = score - scoreDiff (this variable is not displayed in the current version of the game)
# scoreDiff: rigged score differences between player and opponent
# win: a given round's/trial's outcome: 1 = victory, 0 = defeat (rigged, so same for all players!)
# close: based on scoreDiff, 1 = tight competition, 0 = big score difference between player and opponent (displayed during snake game for the last 10 seconds of the game)
# oppName: name sequence of opponents; predefined and the same for all players (displayed at the beginning of each round/trial)
# oppRank: opponents'ranks; predefined sequence that is the same for all players (displayed at the beginning of each round/trial) (warning: rank 1 = highest!)
# appleChoice: player's choice of how many apples to take away from opponent before playing. 1 = none, 2 = 1 apple, 3 = 2 apples, 4 = 5 apples, 5 = 10 apples.
# rankChoice: player's choice of buying a booster to increase rank at the end of a given round/trial. 1 = none, 2 = + 1 rank, 3 = + 2 ranks, 4 = + 3 ranks, 5 = + 5 ranks.
# rankStart: player's rank at the end of the round, but before boosters (warning: rank 1 = highest!)
# rankEnd: player's rank at the end of the round/trial (after boosters) (warning: rank 1 = highest!)
# mot1-8: motivation questions, assessed on a Lickert scale going from 1 = Strongly disagree to 5 = Strongly agree; 'I wanted to perform as well as I possibly could on the task.', 'Maximizing my personal record of apples eaten was important to me.', 'I wanted to perform better than everyone else on the task.', 'I did not want to perform more poorly than everyone else on the task.', 'Attaining the highest rank among all the competitors was important to me.', 'I wanted to take revenge on people who defeated me.', 'I wanted to avoid performing less than my best on the task.', 'I wanted to ensure that I win.'
# enjoyed: how much participant enjoyed playing on a scale from 1 to 10, 1 = not at all, 10 = extremely
# satisfied: how much participant is satisified with own performance  on a scale from 1 to 10, 1 = not at all, 10 = extremely
# fair: how much participant judged the opponents' behavior as fair
# credible: manipulation check, i.e. how much participant believed that he could control the outcome of the game on a scale from 1 to 10, 1 = not at all, 10 = extremely
########################################


## set working directory (where you have the participants' output data files )
setwd("~/Dropbox/USA/Pittsburgh/GitHub/dominance/snake_data_Pittsburgh_may2018")

# wd for Alex
#setwd("~/code//dominance")



## load datafile and skip to line 126 (use only if data has already been prepared previously, otherwise run lines 64 to 126) 
load("snake_totP.Rda")
load("snake_totP_shrunk.Rda")
snake_totP_HC <- snake_totP[snake_totP$group1_5 == '1',]
snake_totP_shrunk_HC <- snake_totP_shrunk[snake_totP_shrunk$group1_5 == '1',]
snake_totP_AT <- snake_totP[snake_totP$group1_5 == '5',]
snake_totP_shrunk_AT <- snake_totP_shrunk[snake_totP_shrunk$group1_5 == '5',]

# releveling group1_5 to avoid high vif scores.
snake_totP$group1_5 <- relevel(snake_totP$group1_5, ref = '5')


# within- and between-subject models ###########################################

###between-subject
# narcissistic scales as single predictors
mrankB0_bpni <- lm(rankChoice_b_logit ~ scale(bpni_TOTAL), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_bpni)
car::Anova(mrankB0_bpni,type = 'III')

mrankB0_bpni_V <- lm(rankChoice_b_logit ~ scale(bpni_VULNERABILITY), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_bpni_V)
car::Anova(mrankB0_bpni_V,type = 'III')

mrankB0_ffni_V <- lm(rankChoice_b_logit ~ scale(ffni_VULNERABLE_NARCISSISM), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_V)
car::Anova(mrankB0_ffni_V,type = 'III')

mrankB0_ffni_A <- lm(rankChoice_b_logit ~ scale(ffni_ANTAGONISM), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_A)
car::Anova(mrankB0_ffni_A,type = 'III')

mrankB0_bpni_G <- lm(rankChoice_b_logit ~ scale(bpni_GANDIOSITY), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_bpni_G)
car::Anova(mrankB0_bpni_G,type = 'III')

mrankB0_ffni_G <- lm(rankChoice_b_logit ~ scale(ffni_GRANDIOSE_NARCISSISM), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_G)
car::Anova(mrankB0_ffni_G,type = 'III')

mrankB0_ffni_E <- lm(rankChoice_b_logit ~ scale(ffni_AGENTIC_EXTRAVERSION), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_E)
car::Anova(mrankB0_ffni_E,type = 'III')

mrankB0_ffni_N <- lm(rankChoice_b_logit ~ scale(ffni_NARCISSISTIC_NEUROTICISM), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_N)
car::Anova(mrankB0_ffni_N,type = 'III')

mrankB0_ipip <- lm(rankChoice_b_logit ~ scale(ipip_total), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ipip)
car::Anova(mrankB0_ipip,type = 'III')

# narcissistic scales + demographics
mrankB0_bpni <- lm(rankChoice_b_logit ~ scale(bpni_TOTAL) + scale(delta_panas_angry) + scale(age) + scale(gameExp) + scale(education) + race + gender.y + scale(household_income_log), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_bpni)
car::Anova(mrankB0_bpni,type = 'III')

mrankB0_ffni <- lm(rankChoice_b_logit ~ scale(ffni_total) + scale(delta_panas_angry) + scale(age) + scale(gameExp) + scale(education) + race + gender.y + scale(household_income_log), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni)
car::Anova(mrankB0_ffni,type = 'III')

mrankB0_bpni_V <- lm(rankChoice_b_logit ~ scale(bpni_VULNERABILITY) + scale(delta_panas_angry) + scale(age) + scale(education) + race + gender.y + scale(gameExp) + scale(household_income_log), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_bpni_V)
car::Anova(mrankB0_bpni_V,type = 'III')

mrankB0_ffni_V <- lm(rankChoice_b_logit ~ scale(ffni_VULNERABLE_NARCISSISM) + scale(delta_panas_angry) + scale(age) + scale(education) + race + gender.y + scale(gameExp) + scale(household_income_log), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_V)
car::Anova(mrankB0_ffni_V,type = 'III')

mrankB0_ffni_N <- lm(rankChoice_b_logit ~ scale(ffni_NARCISSISTIC_NEUROTICISM) + scale(delta_panas_angry) + scale(age) + scale(education) + race + gender.y + scale(gameExp) + scale(household_income_log), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_N)
car::Anova(mrankB0_ffni_N,type = 'III')

mrankB0_ffni_A <- lm(rankChoice_b_logit ~ scale(ffni_ANTAGONISM) + scale(delta_panas_angry) + scale(age) + scale(education) + race + gender.y + scale(gameExp) + scale(household_income_log), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_A)
car::Anova(mrankB0_ffni_A,type = 'III')

mrankB0_bpni_G <- lm(rankChoice_b_logit ~ scale(bpni_GANDIOSITY) + scale(delta_panas_angry) + scale(age) + scale(education) + race + gender.y + scale(gameExp) + scale(household_income_log), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_bpni_G)
car::Anova(mrankB0_bpni_G,type = 'III')

mrankB0_ffni_G <- lm(rankChoice_b_logit ~ scale(ffni_GRANDIOSE_NARCISSISM) + scale(delta_panas_angry) + scale(age) + scale(education) + race + gender.y + scale(gameExp) + scale(household_income_log), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_G)
car::Anova(mrankB0_ffni_G,type = 'III')

mrankB0_ffni_E <- lm(rankChoice_b_logit ~ scale(ffni_AGENTIC_EXTRAVERSION) + scale(delta_panas_angry) + scale(age) + scale(education) + race + gender.y + scale(gameExp) + scale(household_income_log), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_E)
car::Anova(mrankB0_ffni_E,type = 'III')

mrankB0_ipip <- lm(rankChoice_b_logit ~ scale(ipip_total) + scale(delta_panas_angry) + scale(age) + scale(education) + race + gender.y + scale(gameExp) + scale(household_income_log), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ipip)
car::Anova(mrankB0_ipip,type = 'III')

# models with group
mrankB0_group <- lm(rankChoice_b_logit ~ group1_5 + scale(delta_panas_angry) + scale(age) + scale(education) + race + gender.y + scale(gameExp) + scale(household_income_log), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_group)
car::Anova(mrankB0_group,type = 'III')

mrankB0_group1 <- lm(rankChoice_b_logit ~ group1_5*scale(ipip_total) + scale(delta_panas_angry) + scale(age) + scale(education) + race + gender.y + scale(gameExp) + scale(household_income_log), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_group1)
car::Anova(mrankB0_group1,type = 'III')

mrankB0_group2 <- lm(rankChoice_b_logit ~ group1_5*scale(ffni_total) + scale(age) + gender.y + scale(education),  data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_group2)
car::Anova(mrankB0_group2, type = 'III')

mrankB0_group3 <- lm(rankChoice_b_logit ~ group1_5*scale(ffni_total) + scale(age) + gender.y + scale(education),  data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_group3)
car::Anova(mrankB0_group3, type = 'III')


## HC only
#panas angry*narcissism interaction
mrankB0_ffni <- lm(rankChoice_b_logit ~ scale(ffni_total)*scale(delta_panas_angry) + scale(age) + scale(gameExp) + scale(education) + race + gender.y + scale(household_income_log), data = snake_totP_shrunk_HC, na.action = na.omit)
summary(mrankB0_ffni)
car::Anova(mrankB0_ffni,type = 'III')

plot(effect("scale(ffni_total):scale(delta_panas_angry)",mrankB0_ffni), grid=TRUE)

mrankB0_ffni_G <- lm(rankChoice_b_logit ~ scale(ffni_GRANDIOSE_NARCISSISM)*scale(delta_panas_angry) + scale(age) + scale(education) + race + gender.y + scale(gameExp) + scale(household_income_log), data = snake_totP_shrunk_HC, na.action = na.omit)
summary(mrankB0_ffni_G)
car::Anova(mrankB0_ffni_G,type = 'III')

plot(effect("scale(ffni_GRANDIOSE_NARCISSISM):scale(delta_panas_angry)",mrankB0_ffni_G), grid=TRUE)

mrankB0_ffni_E <- lm(rankChoice_b_logit ~ scale(ffni_AGENTIC_EXTRAVERSION)*scale(delta_panas_angry) + scale(age) + scale(education) + race + gender.y + scale(gameExp) + scale(household_income_log), data = snake_totP_shrunk_HC, na.action = na.omit)
summary(mrankB0_ffni_E)
car::Anova(mrankB0_ffni_E,type = 'III')

plot(effect("scale(ffni_AGENTIC_EXTRAVERSION):scale(delta_panas_angry)",mrankB0_ffni_E), grid=TRUE)

mrankB0_ipip <- lm(rankChoice_b_logit ~ scale(ipip_total)*scale(delta_panas_angry) + scale(age) + scale(education) + race + gender.y + scale(gameExp) + scale(household_income_log), data = snake_totP_shrunk_HC, na.action = na.omit)
summary(mrankB0_ipip)
car::Anova(mrankB0_ipip,type = 'III')

plot(effect("scale(ipip_total):scale(delta_panas_angry)",mrankB0_ipip), grid=TRUE)

#marginal similar interaction for BPNI VULNERABILITY, FFNI ANTAGONISM
mrankB0_bpni <- lm(rankChoice_b_logit ~ scale(bpni_TOTAL)*scale(delta_panas_angry) + scale(age) + scale(gameExp) + scale(education) + race + gender.y + scale(household_income_log), data = snake_totP_shrunk_HC, na.action = na.omit)
summary(mrankB0_bpni)
car::Anova(mrankB0_bpni,type = 'III')

mrankB0_bpni_V <- lm(rankChoice_b_logit ~ scale(bpni_VULNERABILITY)*scale(delta_panas_angry) + scale(age) + scale(education) + race + gender.y + scale(gameExp) + scale(household_income_log), data = snake_totP_shrunk_HC, na.action = na.omit)
summary(mrankB0_bpni_V)
car::Anova(mrankB0_bpni_V,type = 'III')

mrankB0_ffni_A <- lm(rankChoice_b_logit ~ scale(ffni_ANTAGONISM)*scale(delta_panas_angry) + scale(age) + scale(education) + race + gender.y + scale(gameExp) + scale(household_income_log), data = snake_totP_shrunk_HC, na.action = na.omit)
summary(mrankB0_ffni_A)
car::Anova(mrankB0_ffni_A,type = 'III')

mrankB0_ffni_V <- lm(rankChoice_b_logit ~ scale(ffni_VULNERABLE_NARCISSISM)*scale(delta_panas_angry) + scale(age) + scale(education) + race + gender.y + scale(gameExp) + scale(household_income_log), data = snake_totP_shrunk_HC, na.action = na.omit)
summary(mrankB0_ffni_V)
car::Anova(mrankB0_ffni_V,type = 'III')

mrankB0_ffni_N <- lm(rankChoice_b_logit ~ scale(ffni_NARCISSISTIC_NEUROTICISM)*scale(delta_panas_angry) + scale(age) + scale(education) + race + gender.y + scale(gameExp) + scale(household_income_log), data = snake_totP_shrunk_HC, na.action = na.omit)
summary(mrankB0_ffni_N)
car::Anova(mrankB0_ffni_N,type = 'III')

mrankB0_bpni_G <- lm(rankChoice_b_logit ~ scale(bpni_GANDIOSITY)*scale(delta_panas_angry) + scale(age) + scale(education) + race + gender.y + scale(gameExp) + scale(household_income_log), data = snake_totP_shrunk_HC, na.action = na.omit)
summary(mrankB0_bpni_G)
car::Anova(mrankB0_bpni_G,type = 'III')



###########################################################################################
##################### within-subject means ################################################
###########################################################################################

# design variables only: no interactions that survives controlling for previous choices.
mrankA <- lmer(rankChoice_wi_0 ~ scale(trial) + win + scale(rankStart) + scale(oppRank) + scale(scoreDelta) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA)
car::Anova(mrankA, type = 'III')
vif.lme(mrankA)

## with narcissistic scales, without group

# bpni total
mrankA_bpni0 <- lmer(rankChoice_wi_0 ~ scale(bpni_TOTAL) + scale(trial) + scale(rankStart) + scale(oppRank) + scale(scoreDelta) +  win + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA_bpni0)
car:: Anova(mrankA_bpni0, type = 'III')

mrankA_bpni1 <- lmer(rankChoice_wi_0 ~ scale(trial) + scale(rankStart) + scale(oppRank) + scale(scoreDelta)*scale(bpni_TOTAL)*win + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA_bpni1)
car:: Anova(mrankA_bpni1, type = 'III')

anova(mrankA_bpni0, mrankA_bpni1)
vif.lme(mrankA_bpni1)
plot(effect("scale(scoreDelta):scale(bpni_TOTAL):win",mrankA_bpni1), grid=TRUE)
plot(effect("scale(scoreDelta):scale(bpni_TOTAL)",mrankA_bpni1), grid=TRUE)

#ffni
mrankA_ffni0 <- lmer(rankChoice_wi_0 ~ scale(ffni_total) + scale(trial) + scale(rankStart) + scale(oppRank) + scale(scoreDelta) +  win + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA_ffni0)
car::Anova(mrankA_ffni0, type = 'III')

mrankA_ffni1 <- lmer(rankChoice_wi_0 ~ scale(ffni_total)*scale(trial) + scale(rankStart) + scale(oppRank) + scale(scoreDelta) +  win + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA_ffni1)
car::Anova(mrankA_ffni1, type = 'III')

mrankA_ffni2 <- lmer(rankChoice_wi_0 ~ scale(trial) + scale(rankStart) + scale(oppRank) + scale(ffni_total)*scale(scoreDelta)*win + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA_ffni2)
car::Anova(mrankA_ffni2, type = 'III')

#best model for ffni
mrankA_ffni3 <- lmer(rankChoice_wi_0 ~ scale(ffni_total)*scale(trial) + scale(rankStart) + scale(oppRank) + scale(ffni_total)*scale(scoreDelta)*win + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA_ffni3)
car::Anova(mrankA_ffni3, type = 'III')

anova(mrankA2_ffni0,mrankA2_ffni1)
anova(mrankA2_ffni1,mrankA2_ffni2)
anova(mrankA2_ffni1,mrankA2_ffni3)
anova(mrankA2_ffni2,mrankA2_ffni3)
vif.lme(mrankA2_ffni3)

plot(effect("scale(ffni_total):scale(scoreDelta):win",mrankA_ffni3), grid=TRUE, x.var = 'scoreDelta')
plot(effect("scale(ffni_total):scale(trial)",mrankA_ffni3), grid=TRUE, x.var = 'trial')


#ipip
mrankA2_ipip <- lmer(rankChoice_wi_0 ~ scale(ipip_total) + scale(trial) + win + scale(rankStart) + scale(oppRank) + scale(scoreDelta) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA2_ipip)
car::Anova(mrankA2_ipip, type = 'III')

# best model for ipip
mrankA2_ipip1 <- lmer(rankChoice_wi_0 ~ scale(ipip_total)*scale(trial) + win + scale(rankStart) + scale(oppRank) + scale(scoreDelta) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA2_ipip1)
car::Anova(mrankA2_ipip1, type = 'III')

anova(mrankA2_ipip, mrankA2_ipip1)
plot(effect("scale(ipip_total):scale(trial)",mrankA2_ipip1), grid=TRUE, x.var = 'trial')
vif.lme(mrankA2_ipip1)

mrankA2_ipip2 <- lmer(rankChoice_wi_0 ~ scale(trial) + win + scale(oppRank) + scale(ipip_total)*scale(rankStart)*scale(scoreDelta) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA2_ipip2)
car::Anova(mrankA2_ipip2, type = 'III')

# group and design variables (no narcissistic scales)
mrankA_group <- lmer(rankChoice_wi_0 ~ group1_5 + scale(trial) + win + scale(rankStart) + scale(oppRank) + scale(scoreDelta) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA_group)
car::Anova(mrankA_group, type = 'III')
vif.lme(mrankA_group)

# best model with design and group
mrankA_group1 <- lmer(rankChoice_wi_0 ~ scale(trial) + win + scale(rankStart) + group1_5*scale(oppRank)*scale(scoreDelta) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA_group1)
car::Anova(mrankA_group1, type = 'III')
vif.lme(mrankA_group1)

anova(mrankA_group, mrankA_group1)

plot(effect("group1_5:scale(oppRank):scale(scoreDelta)",mrankA_group1), grid=TRUE)
plot(effect("scale(oppRank):group1_5",mrankA0_group1), grid=TRUE)

#sensitivity for best model with group
mrankA_group1s <- lmer(rankChoice_wi_0 ~ scale(trial) + win + scale(rankStart) + group1_5*scale(oppRank)*scale(scoreDelta) + scale(rankChoice_wi_0.minus1) + age + gender.y + scale(education) + race + scale(gameExp) + scale(household_income_log) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA_group1s)
car::Anova(mrankA_group1s, type = 'III')


mrankA0_group2 <- lmer(rankChoice_wi_0 ~ scale(trial)*scale(bpni_TOTAL)*group1_5 + win + scale(rankStart) + scale(score)*scale(oppRank)*group1_5 + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA0_group2)
car::Anova(mrankA0_group2, type = 'III')

plot(effect("scale(trial):scale(bpni_TOTAL):group1_5",mrankA0_group2), grid=TRUE)
plot(effect("group1_5:scale(score):scale(oppRank)",mrankA0_group2), grid=TRUE)

vif.lme(mrankA0_group2)

mrankA0_group3 <- lmer(rankChoice_wi_0 ~ scale(trial)*scale(bpni_TOTAL)*group1_5 + win + scale(rankStart) + scale(oppRank) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA0_group3)
car::Anova(mrankA0_group3, type = 'III')

vif.lme(mrankA0_group3)
plot(effect("group1_5:scale(oppRank)",mrankA0_group3), grid=TRUE)

anova(mrankA0_group2, mrankA0_group3)


# testing interactions with group and narcissistic scales (probably not useful)
mrankA_group1_bpni <- lmer(rankChoice_wi_0 ~ scale(trial) + win + scale(rankStart) + group1_5*scale(oppRank)*scale(bpni_TOTAL) + scale(scoreDelta)*scale(bpni_TOTAL) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA_group1_bpni)
car::Anova(mrankA_group1_bpni, type = 'III')

mrankA2_bpni0_group1 <- lmer(rankChoice_wi_0 ~ win + scale(trial)*scale(bpni_TOTAL)*group1_5 + scale(rankStart) + scale(oppRank) + scale(score) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA2_bpni0_group1)
car:: Anova(mrankA2_bpni0_group1, type = 'III')

vif.lme(mrankA2_bpni0_group1)
plot(effect("scale(trial):scale(bpni_TOTAL):group1_5",mrankA2_bpni0_group1), grid=TRUE)

mrankA2_bpni0_group2 <- lmer(rankChoice_wi_0 ~ win + scale(trial) + scale(rankStart) + scale(oppRank) + scale(score)*scale(bpni_TOTAL)*group1_5 + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA2_bpni0_group2)
car:: Anova(mrankA2_bpni0_group2, type = 'III')

mrankA2_bpni0_group3 <- lmer(rankChoice_wi_0 ~ win + scale(trial)*scale(bpni_TOTAL)*group1_5 + scale(rankStart) + scale(oppRank) + scale(score)*scale(bpni_TOTAL)*group1_5 + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA2_bpni0_group3)
car:: Anova(mrankA2_bpni0_group3, type = 'III')

anova(mrankA2_bpni0_group2, mrankA2_bpni0_group1)

#marginal 3-way interaction with group
mrankA2_ffni1_group <- lmer(rankChoice_wi_0 ~ win + scale(trial)*scale(ffni_total)*group1_5 + scale(rankStart) + scale(oppRank) + scale(score) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA2_ffni1_group)
car::Anova(mrankA2_ffni1_group, type = 'III')

plot(effect("scale(trial):scale(ffni_total):group1_5",mrankA2_ffni1_group), grid=TRUE)

##############################################
########### checking in HC only ##############
##############################################

snake_totP_HC <- snake_totP[snake_totP$group1_5 == '1',]


# Most reliable model.
mrankA <- lmer(rankChoice_wi_0 ~ scale(trial) + win + scale(rankStart) + scale(oppRank) + scale(scoreDelta) + rankChoice_wi_0.minus1 + (1|ID),  data = snake_totP_HC, na.action = na.omit)
summary(mrankA)
car::Anova(mrankA, type = 'III')

vif.lme(mrankA)


## adding narcissistic scales

mrankA1_bpni <- lmer(rankChoice_wi_0 ~ scale(bpni_TOTAL) + scale(trial) + win + scale(rankStart) + scale(oppRank) + scale(scoreDelta) + rankChoice_wi_0.minus1 + (1|ID),  data = snake_totP_HC, na.action = na.omit)
summary(mrankA1_bpni)
car::Anova(mrankA1_bpni, type = 'III')

mrankA1_bpni1 <- lmer(rankChoice_wi_0 ~ win + scale(trial)*scale(bpni_TOTAL)*scale(rankStart) + scale(oppRank) + scale(scoreDelta) + rankChoice_wi_0.minus1 + (1|ID),  data = snake_totP_HC, na.action = na.omit)
summary(mrankA1_bpni1)
car::Anova(mrankA1_bpni1, type = 'III')

#best model with BPNI
mrankA1_bpni2 <- lmer(rankChoice_wi_0 ~ scale(trial) + win*scale(bpni_TOTAL)*scale(rankStart) + scale(oppRank) + scale(scoreDelta) + rankChoice_wi_0.minus1 + (1|ID),  data = snake_totP_HC, na.action = na.omit)
summary(mrankA1_bpni2)
car::Anova(mrankA1_bpni2, type = 'III')

anova(mrankA1_bpni, mrankA1_bpni1)
anova(mrankA1_bpni, mrankA1_bpni2)
anova(mrankA1_bpni1, mrankA1_bpni2)

vif.lme(mrankA1_bpni2)
plot(effect("win:scale(bpni_TOTAL):scale(rankStart)",mrankA1_bpni2), grid=TRUE, x.var = 'rankStart')
plot(effect("win:scale(bpni_TOTAL)",mrankA1_bpni2), grid=TRUE, x.var = 'win')
plot(effect("scale(bpni_TOTAL):scale(rankStart)",mrankA1_bpni2), grid=TRUE, x.var = 'rankStart')
plot(effect("win:scale(rankStart)",mrankA1_bpni2), grid=TRUE, x.var = 'rankStart')


mrankA1_bpni3 <- lmer(rankChoice_wi_0 ~ scale(trial)*scale(bpni_TOTAL)*scale(rankStart) + win*scale(bpni_TOTAL)*scale(rankStart) + scale(oppRank) + scale(scoreDelta) + rankChoice_wi_0.minus1 + (1|ID),  data = snake_totP_HC, na.action = na.omit)
summary(mrankA1_bpni3)
car::Anova(mrankA1_bpni3, type = 'III')


#ffni
mrankA1_ffni <- lmer(rankChoice_wi_0 ~ scale(ffni_total) + scale(trial) + win + scale(rankStart) + scale(oppRank) + scale(scoreDelta) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP_HC, na.action = na.omit)
summary(mrankA1_ffni)
car::Anova(mrankA1_ffni, type = 'III')

#best model for ffni
mrankA1_ffni1 <- lmer(rankChoice_wi_0 ~ scale(trial) + scale(ffni_total)*win*scale(rankStart) + scale(oppRank) + scale(scoreDelta) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP_HC, na.action = na.omit)
summary(mrankA1_ffni1)
car::Anova(mrankA1_ffni1, type = 'III')

anova(mrankA1_ffni, mrankA1_ffni1)
vif.lme(mrankA1_ffni1)
plot(effect("scale(ffni_total):win:scale(rankStart)",mrankA1_ffni1), grid=TRUE, x.var = 'rankStart')
plot(effect("scale(ffni_total):win:scale(rankStart)",mrankA1_ffni1), grid=TRUE, x.var = 'rankStart')
plot(effect("scale(ffni_total):win:scale(rankStart)",mrankA1_ffni1), grid=TRUE, x.var = 'rankStart')

#ipip
mrankA1_ipip <- lmer(rankChoice_wi_0 ~ scale(ipip_total) + scale(trial) + win + scale(rankStart) + scale(oppRank) + scale(scoreDelta) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP_HC, na.action = na.omit)
summary(mrankA1_ipip)
car::Anova(mrankA1_ipip, type = 'III')

mrankA1_ipip1 <- lmer(rankChoice_wi_0 ~ scale(ipip_total)*scale(trial) + win + scale(ipip_total) + scale(rankStart) + scale(oppRank) + scale(scoreDelta) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP_HC, na.action = na.omit)
summary(mrankA1_ipip1)
car::Anova(mrankA1_ipip1, type = 'III')

vif.lme(mrankA1_ipip)

anova(mrankA1_ipip, mrankA1_ipip1)
anova(mrankA1_ipip1, mrankA1_ipip2)
anova(mrankA1_ipip1, mrankA1_ipip3)
anova(mrankA1_ipip2, mrankA1_ipip3)

plot(effect("scale(trial):scale(ipip_total)",mrankA1_ipip3), grid=TRUE)
plot(effect("scale(ipip_total):scale(score)",mrankA1_ipip3), grid=TRUE)


#####################################################################
############## depressive versus non-depressives ####################
#####################################################################

## design variables only
# group and design variables (no narcissistic scales)
mrankA_dep <- lmer(rankChoice_wi_0 ~ gp_dep + scale(trial) + win + scale(rankStart) + scale(oppRank) + scale(scoreDelta) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA_dep)
car::Anova(mrankA_dep, type = 'III')

vif.lme(mrankA_dep)

mrankA_dep1 <- lmer(rankChoice_wi_0 ~ scale(trial)*gp_dep + scale(rankStart) + scale(oppRank) + gp_dep*win*scale(scoreDelta) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA_dep1)
car::Anova(mrankA_dep1, type = 'III')

vif.lme(mrankA_dep1)
anova(mrankA_dep, mrankA_dep1)

plot(effect("gp_dep:win:scale(scoreDelta)",mrankA_dep1), grid=TRUE)

mrankA_dep2 <- lmer(rankChoice_wi_0 ~ scale(trial) + win + scale(rankStart) + gp_dep*scale(oppRank) + scale(scoreDelta) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA_dep2)
car::Anova(mrankA_dep2, type = 'III')

#sensitivity for best model with group
mrankA_dep1s <- lmer(rankChoice_wi_0 ~ scale(trial) + win + scale(rankStart) + gp_dep*scale(oppRank)*scale(scoreDelta) + scale(rankChoice_wi_0.minus1) + age + gender.y + scale(education) + race + scale(gameExp) + scale(household_income_log) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA_dep1s)
car::Anova(mrankA_dep1s, type = 'III')


## for neuroeconomics poster: narcissistic scales total scores, group dep.
mrankA <- lmer(rankChoice_wi_0 ~ scale(trial) + win + scale(rankStart) + scale(oppRank) + scale(scoreDelta) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA)
car::Anova(mrankA, type = 'III')
vif.lme(mrankA)

mrankAs <- lmer(rankChoice_wi_0 ~ scale(trial) + win + scale(rankStart) + scale(oppRank) + scale(scoreDelta) + scale(rankChoice_wi_0.minus1) + scale(age_snake) + gender.y + scale(education) + race + scale(gameExp) + scale(household_income_log) + scale(HRSD_no_suic) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankAs)
car::Anova(mrankAs, type = 'III')
vif.lme(mrankA)


#narcissistic finding: ffni total*trial interaction
mrankA_ffni_n <- lmer(rankChoice_wi_0 ~ scale(ffni_total)*scale(trial) + scale(rankStart) + scale(oppRank) + scale(ffni_total)*scale(scoreDelta)*win + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA_ffni_n)
car::Anova(mrankA_ffni_n, type = 'III')


# going  with simpler model, since no significant difference and other interaction not very informative.
snake_totP_NAfree <- snake_totP[!is.na(snake_totP$ffni_total),]

mrankA_ffni_n2 <- lmer(rankChoice_wi_0 ~ scale(ffni_total)*scale(trial) + scale(rankStart) + scale(oppRank) + scale(scoreDelta) + win + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA_ffni_n2)
car::Anova(mrankA_ffni_n2, type = 'III')

anova(mrankA_ffni_n, mrankA_ffni_n2)

vif.lme(mrankA_ffni_n)
vif.lme(mrankA_ffni_n2)

#image outputted in 6x3''
plot(effect("scale(ffni_total):scale(trial)",mrankA_ffni_n2, xlevels = list('ffni_total' = c(83,135,203))), grid=TRUE, x.var = 'trial', xlab = 'trial', ylab = 'within-subject mean\nrank-buying', main = 'FFNI')

#library(jtools)
#probe_interaction(mrankA_ffni_n2, pred = "scale(ffni_total)", modx = "scale(trial)", data = snake_totP_NAfree)

# x <- snake_totP_NAfree$ffni_total
# 
# snake_totP_NAfree$ffni_total_3group <-
#   case_when(x > mean(x)+sd(x) ~ "high",
#             x < mean(x)+sd(x) & x > mean(x)-sd(x) ~ "average",
#             x < mean(x)-sd(x) ~ "low")
# 
# count(snake_totP_NAfree, ffni_total_3group)
# 
# snake_totP_NAfree %>% 
#   ggplot() +
#   aes(x = trial, y = rankChoice_wi_0, group = ffni_total_3group, color = ffni_total_3group) +
#   geom_point(color = "grey", alpha = .7) +
#   geom_smooth(method = "lm")


#sensitivity
mrankA_ffni_n_s0 <- lmer(rankChoice_wi_0 ~ scale(ffni_total)*scale(trial) + scale(rankStart) + scale(oppRank) + scale(scoreDelta) + win + scale(rankChoice_wi_0.minus1) + scale(age_snake) + gender.y + scale(education) + race + scale(gameExp) + scale(household_income_log) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA_ffni_n_s0)
car::Anova(mrankA_ffni_n_s0, type = 'III')

mrankA_ffni_n_s1 <- lmer(rankChoice_wi_0 ~ scale(ffni_total)*scale(trial) + scale(rankStart) + scale(oppRank) + scale(scoreDelta) + win + scale(rankChoice_wi_0.minus1) + scale(age_snake) + gender.y + scale(education) + race + scale(gameExp) + scale(household_income_log) + scale(HRSD_no_suic) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA_ffni_n_s1)
car::Anova(mrankA_ffni_n_s1, type = 'III')
