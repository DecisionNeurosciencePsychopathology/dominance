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
setwd("~/Dropbox/USA/Pittsburgh/GitHub/dominance/snake_data_Vancouver")

# wd for Alex
#setwd("~/code//dominance")

# clear environment
rm(list=ls())


## load datafile and skip to line 126 (use only if data has already been prepared previously, otherwise run lines 64 to 126) 

load("snake_tot.Rda")
load("snake_tot_shrunk.Rda")




# within- and between-subject models ###########################################

###between-subject
# narcissistic scales as single predictors
mrankB0_bpni <- lm(rankChoice_b_logit ~ scale(bpni_TOTAL), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_bpni)
car::Anova(mrankB0_bpni,type = 'III')

mrankB0_ffni <- lm(rankChoice_b_logit ~ scale(bpni_TOTAL), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_ffni)
car::Anova(mrankB0_ffni,type = 'III')

mrankB0_bpni_V <- lm(rankChoice_b_logit ~ scale(bpni_VULNERABILITY), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_bpni_V)
car::Anova(mrankB0_bpni_V,type = 'III')

mrankB0_ffni_V <- lm(rankChoice_b_logit ~ scale(ffni_VULNERABLE_NARCISSISM), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_ffni_V)
car::Anova(mrankB0_ffni_V,type = 'III')

mrankB0_ffni_A <- lm(rankChoice_b_logit ~ scale(ffni_ANTAGONISM), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_ffni_A)
car::Anova(mrankB0_ffni_A,type = 'III')

mrankB0_bpni_G <- lm(rankChoice_b_logit ~ scale(bpni_GANDIOSITY), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_bpni_G)
car::Anova(mrankB0_bpni_G,type = 'III')

mrankB0_ffni_G <- lm(rankChoice_b_logit ~ scale(ffni_GRANDIOSE_NARCISSISM), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_ffni_G)
car::Anova(mrankB0_ffni_G,type = 'III')

mrankB0_ffni_E <- lm(rankChoice_b_logit ~ scale(ffni_AGENTIC_EXTRAVERSION), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_ffni_E)
car::Anova(mrankB0_ffni_E,type = 'III')

# narcissistic scales + demographics
mrankB0_bpni <- lm(rankChoice_b_logit ~ scale(bpni_TOTAL)*scale(delta_panas_angry) + scale(age) + scale(household_income) + ethnicity + gender + scale(gameExp), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_bpni)
car::Anova(mrankB0_bpni,type = 'III')

plot(effect("scale(bpni_TOTAL):scale(delta_panas_angry)",mrankB0_bpni), grid=TRUE)


mrankB0_ffni <- lm(rankChoice_b_logit ~ scale(ffni_total)*scale(delta_panas_angry) + scale(age) + scale(household_income) + ethnicity + gender + scale(gameExp), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_ffni)
car::Anova(mrankB0_ffni,type = 'III')

plot(effect("scale(ffni_total):scale(delta_panas_angry)",mrankB0_ffni), grid=TRUE)


mrankB0_bpni_V <- lm(rankChoice_b_logit ~ scale(bpni_VULNERABILITY)*scale(delta_panas_angry) + scale(age) + scale(household_income) + ethnicity + gender + scale(gameExp), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_bpni_V)
car::Anova(mrankB0_bpni_V,type = 'III')

mrankB0_ffni_V <- lm(rankChoice_b_logit ~ scale(ffni_VULNERABLE_NARCISSISM)*scale(delta_panas_angry) + scale(age) + scale(household_income) + ethnicity + gender + scale(gameExp), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_ffni_V)
car::Anova(mrankB0_ffni_V,type = 'III')

mrankB0_ffni_A <- lm(rankChoice_b_logit ~ scale(ffni_ANTAGONISM)*scale(delta_panas_angry) + scale(age) + scale(household_income) + ethnicity + gender + scale(gameExp), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_ffni_A)
car::Anova(mrankB0_ffni_A,type = 'III')

mrankB0_bpni_G <- lm(rankChoice_b_logit ~ scale(bpni_GANDIOSITY)*scale(delta_panas_angry) + scale(age) + scale(household_income) + ethnicity + gender + scale(gameExp), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_bpni_G)
car::Anova(mrankB0_bpni_G,type = 'III')

mrankB0_ffni_G <- lm(rankChoice_b_logit ~ scale(ffni_GRANDIOSE_NARCISSISM)*scale(delta_panas_angry) + scale(age) + scale(household_income) + ethnicity + gender + scale(gameExp), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_ffni_G)
car::Anova(mrankB0_ffni_G,type = 'III')

mrankB0_ffni_E <- lm(rankChoice_b_logit ~ scale(ffni_AGENTIC_EXTRAVERSION)*scale(delta_panas_angry) + scale(age) + scale(household_income) + ethnicity + gender + scale(gameExp), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_ffni_E)
car::Anova(mrankB0_ffni_E,type = 'III')

mrankB0_ffni_N <- lm(rankChoice_b_logit ~ scale(ffni_NARCISSISTIC_NEUROTICISM)*scale(delta_panas_angry) + scale(age) + scale(household_income) + ethnicity + gender + scale(gameExp), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_ffni_N)
car::Anova(mrankB0_ffni_N,type = 'III')


# narcissistic scales +  ratings
mrankB0_ffni <- lm(rankChoice_b_logit ~ scale(ffni_total)*scale(delta_panas_angry), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_ffni)
car::Anova(mrankB0_ffni,type = 'III')

mrankB0_bpni <- lm(rankChoice_b_logit ~ scale(bpni_TOTAL)*scale(delta_panas_angry), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_bpni)
car::Anova(mrankB0_bpni,type = 'III')

mrankB0_bpni_V <- lm(rankChoice_b_logit ~ scale(bpni_VULNERABILITY)*scale(delta_panas_angry), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_bpni_V)
car::Anova(mrankB0_bpni_V,type = 'III')

mrankB0_ffni_V <- lm(rankChoice_b_logit ~ scale(ffni_VULNERABLE_NARCISSISM)*scale(delta_panas_angry), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_ffni_V)
car::Anova(mrankB0_ffni_V,type = 'III')

mrankB0_ffni_A <- lm(rankChoice_b_logit ~ scale(ffni_ANTAGONISM)*scale(delta_panas_angry), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_ffni_A)
car::Anova(mrankB0_ffni_A,type = 'III')

mrankB0_bpni_G <- lm(rankChoice_b_logit ~ scale(bpni_GANDIOSITY)*scale(delta_panas_angry), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_bpni_G)
car::Anova(mrankB0_bpni_G,type = 'III')

mrankB0_ffni_G <- lm(rankChoice_b_logit ~ scale(ffni_GRANDIOSE_NARCISSISM)*scale(delta_panas_angry), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_ffni_G)
car::Anova(mrankB0_ffni_G,type = 'III')

mrankB0_ffni_E <- lm(rankChoice_b_logit ~ scale(ffni_AGENTIC_EXTRAVERSION)*scale(delta_panas_angry), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_ffni_E)
car::Anova(mrankB0_ffni_E,type = 'III')

plot(effect("scale(ffni_GRANDIOSE_NARCISSISM):scale(delta_panas_angry)",mrankB0_ffni_G), grid=TRUE)

mrankB0_ffni_N <- lm(rankChoice_b_logit ~ scale(ffni_NARCISSISTIC_NEUROTICISM)*scale(delta_panas_angry), data = snake_tot_shrunk, na.action = na.omit)
summary(mrankB0_ffni_N)
car::Anova(mrankB0_ffni_N,type = 'III')


###########################################################################################
##################### within-subject means ################################################
###########################################################################################

# design variables only
mrankA0 <- lmer(rankChoice_wi_0 ~ scale(trial) + win + close + scale(rankStart) + scale(oppRank) + scale(score) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA0)
car::Anova(mrankA0, type = 'III')

mrankA1 <- lmer(rankChoice_wi_0 ~ win + scale(trial)*close + scale(rankStart) + scale(oppRank) + scale(score) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA1)
car::Anova(mrankA1, type = 'III')

mrankA2 <- lmer(rankChoice_wi_0 ~ win + close + scale(rankStart) + scale(trial)*scale(oppRank) + scale(score) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2)
car::Anova(mrankA2, type = 'III')

mrankA3 <- lmer(rankChoice_wi_0 ~ scale(trial) + close + scale(rankStart) + win*scale(oppRank) + scale(score) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA3)
car::Anova(mrankA3, type = 'III')

mrankA4 <- lmer(rankChoice_wi_0 ~ close + scale(rankStart) + scale(trial)*win*scale(oppRank) + scale(score) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA4)
car::Anova(mrankA4, type = 'III')

mrankA5 <- lmer(rankChoice_wi_0 ~ scale(trial)*close + scale(rankStart) + scale(trial)*win*scale(oppRank) + scale(score) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA5)
car::Anova(mrankA5, type = 'III')

mrankA6 <- lmer(rankChoice_wi_0 ~ scale(trial)*close*scale(oppRank) + scale(rankStart) + scale(score) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA6)
car::Anova(mrankA6, type = 'III')

anova(mrankA1, mrankA6)


mrankA2_0 <- lmer(rankChoice_wi_0 ~ win + scale(scoreDelta) + scale(rankStart) + scale(trial)*scale(oppRank) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_0)
car::Anova(mrankA2_0, type = 'III')

AIC(mrankA1, mrankA2_0)

plot(effect("scale(trial):scale(oppRank)",mrankA2), grid=TRUE)

mrankA3 <- lmer(rankChoice_wi ~ scale(trial)*close +  win + scale(rankStart) + scale(trial)*scale(oppRank) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA3)
car::Anova(mrankA3, type = 'III')

vif.lme(mrankA3)
vif.lme(mrankA2_0)

anova(mrankA0, mrankA2)
anova(mrankA1, mrankA2)
anova(mrankA2, mrankA3)

mrankA4 <- lmer(rankChoice_wi ~ win + scale(rankStart) + scale(trial)*scale(oppRank)  + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA4)
car::Anova(mrankA4, type = 'III')

anova(mrankA2, mrankA4)

# best model with design variables only
mrankA2_1 <- lmer(rankChoice_wi_0 ~ win + scale(scoreDelta) + scale(rankStart) + scale(trial)*scale(oppRank) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_1)
car::Anova(mrankA2_1, type = 'III')

vif.lme(mrankA2_1)

anova(mrankA2_0, mrankA2_1)



## adding narcissistic scales
# bpni total: no correlations
mrankA2_bpni0 <- lmer(rankChoice_wi_0 ~ win + scale(scoreDelta) + scale(rankStart) + scale(trial)*scale(oppRank) + scale(bpni_TOTAL) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_bpni0)
car:: Anova(mrankA2_bpni0, type = 'III')

# best for bpni
mrankA2_1_bpni0 <- lmer(rankChoice_wi_0 ~ win + scale(scoreDelta) + scale(rankStart) + scale(trial)*scale(oppRank) + scale(rankChoice_wi_0.minus1) + scale(bpni_TOTAL) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_1_bpni0)
car:: Anova(mrankA2_1_bpni0, type = 'III')

#ffni
mrankA2_ffni0 <- lmer(rankChoice_wi_0 ~ win +  scale(scoreDelta) + scale(rankStart) + scale(trial)*scale(oppRank) + scale(ffni_total) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_ffni0)
car::Anova(mrankA2_ffni0, type = 'III')


mrankA2_ffni1 <- lmer(rankChoice_wi_0 ~ win + scale(scoreDelta) + scale(rankStart) + scale(trial)*scale(oppRank)*scale(ffni_total) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_ffni1)
car::Anova(mrankA2_ffni1, type = 'III')

anova(mrankA2_ffni0, mrankA2_ffni1)

# best model for ffni
mrankA2_1_ffni1 <- lmer(rankChoice_wi_0 ~ win + scale(scoreDelta) + scale(rankStart) + scale(trial)*scale(oppRank)*scale(ffni_total) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_1_ffni1)
car::Anova(mrankA2_1_ffni1, type = 'III')


mrankA2_ffni5 <- lmer(rankChoice_wi_0 ~ win + close + scale(rankStart) + scale(trial)*scale(oppRank)*scale(ffni_total) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_ffni5)
car::Anova(mrankA2_ffni5, type = 'III')

anova(mrankA2_ffni1, mrankA2_ffni5)

vif.lme(mrankA2_1_ffni1)

plot(effect("scale(trial):scale(ffni_total)",mrankA2_1_ffni1), grid=TRUE)
plot(effect("scale(oppRank):scale(ffni_total)",mrankA2_1_ffni1), grid=TRUE)
plot(effect("scale(trial):scale(oppRank):scale(ffni_total)",mrankA2_1_ffni1), grid=TRUE)




mrankA2_ffni2 <- lmer(rankChoice_wi_0 ~ win + close + scale(rankStart) + scale(trial)*scale(oppRank) + scale(trial)*scale(ffni_total) + scale(oppRank)*scale(ffni_total) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_ffni2)
car::Anova(mrankA2_ffni2, type = 'III')

anova(mrankA2_ffni1, mrankA2_ffni2)

mrankA2_ffni3 <- lmer(rankChoice_wi_0 ~ win + close + scale(rankStart) + scale(trial)*scale(oppRank) + scale(trial)*scale(ffni_total) + + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_ffni3)
car::Anova(mrankA2_ffni3, type = 'III')

anova(mrankA2_ffni1, mrankA2_ffni3)

plot(effect("scale(trial):scale(oppRank):scale(ffni_total)",mrankA2_ffni1), grid=TRUE)

mrankA2_ffni1_0 <- lmer(rankChoice_wi ~ win + scale(scoreDelta) + scale(rankStart) + scale(trial)*scale(oppRank)*scale(ffni_total) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_ffni1_0)
car::Anova(mrankA2_ffni1_0, type = 'III')

#sensitivity for ffni total score
mrankA2_1_ffni1_i <- lmer(rankChoice_wi_0 ~ win + scale(scoreDelta) + scale(rankStart) + scale(trial)*scale(oppRank)*scale(ffni_total) + scale(rankChoice_wi_0.minus1) +  scale(age) + scale(household_income) + ethnicity + gender + scale(gameExp) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_1_ffni1_i)
car::Anova(mrankA2_1_ffni1_i, type = 'III')

vif.lme(mrankA2_ffni1_i)

#ffni subscales: same interaction (but stronger) for grandiose narcissism and antagonism
mrankA2_1_ffni1_V <- lmer(rankChoice_wi_0 ~ win + scale(scoreDelta) + scale(rankStart) + scale(trial)*scale(oppRank)*scale(ffni_VULNERABLE_NARCISSISM) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_1_ffni1_V)
car::Anova(mrankA2_1_ffni1_V, type = 'III')

mrankA2_1_ffni1_G <- lmer(rankChoice_wi_0 ~ win + scale(scoreDelta) + scale(rankStart) + scale(trial)*scale(oppRank)*scale(ffni_GRANDIOSE_NARCISSISM) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_1_ffni1_G)
car::Anova(mrankA2_1_ffni1_G, type = 'III')

plot(effect("scale(trial):scale(oppRank):scale(ffni_GRANDIOSE_NARCISSISM)",mrankA2_ffni1_G), grid=TRUE)

mrankA2_1_ffni1_A <- lmer(rankChoice_wi_0 ~ win + scale(scoreDelta) + scale(rankStart) + scale(trial)*scale(oppRank)*scale(ffni_ANTAGONISM) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_1_ffni1_A)
car::Anova(mrankA2_1_ffni1_A, type = 'III')

plot(effect("scale(trial):scale(oppRank):scale(ffni_ANTAGONISM)",mrankA2_ffni1_A), grid=TRUE)

vif.lme(mrankA2_ffni1_Ai)

mrankA2_1_ffni1_E <- lmer(rankChoice_wi_0 ~ win + scale(scoreDelta) + scale(rankStart) + scale(trial)*scale(oppRank)*scale(ffni_AGENTIC_EXTRAVERSION) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_1_ffni1_E)
car::Anova(mrankA2_1_ffni1_E, type = 'III')

mrankA2_1_ffni1_N <- lmer(rankChoice_wi_0 ~ win + scale(scoreDelta) + scale(rankStart) + scale(trial)*scale(oppRank)*scale(ffni_NARCISSISTIC_NEUROTICISM) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_1_ffni1_N)
car::Anova(mrankA2_1_ffni1_N, type = 'III')

mrankA2_1_ffni1_N0 <- lmer(rankChoice_wi_0 ~ win + scale(scoreDelta) + scale(rankStart)*scale(ffni_NARCISSISTIC_NEUROTICISM) + scale(trial) + scale(oppRank) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_1_ffni1_N0)
car::Anova(mrankA2_1_ffni1_N0, type = 'III')

plot(effect("scale(rankStart):scale(ffni_NARCISSISTIC_NEUROTICISM)",mrankA2_1_ffni1_N0), grid=TRUE)

mrankA2_1_ffni1_V0 <- lmer(rankChoice_wi_0 ~ win + scale(scoreDelta) + scale(rankStart)*scale(ffni_VULNERABLE_NARCISSISM) + scale(trial) + scale(oppRank) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_1_ffni1_V0)
car::Anova(mrankA2_1_ffni1_V0, type = 'III')

#bpni subscales: no similar effect to ffni.
mrankA2_bpni0_V <- lmer(rankChoice_wi_0 ~ win + close + scale(rankStart) + scale(trial)*scale(oppRank)*scale(bpni_VULNERABILITY) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_bpni0_V)
car::Anova(mrankA2_bpni0_V, type = 'III')

mrankA2_bpni0_G <- lmer(rankChoice_wi_0 ~ win + close + scale(rankStart) + scale(trial)*scale(oppRank)*scale(bpni_GANDIOSITY) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_bpni0_G)
car::Anova(mrankA2_bpni0_G, type = 'III')


## for neuroeconomics poster: narcissistic scales total scores, group dep.
mrankA2_1 <- lmer(rankChoice_wi_0 ~ win + scale(scoreDelta) + scale(rankStart) + scale(trial)*scale(oppRank) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_1)
car::Anova(mrankA2_1, type = 'III')

mrankA2_1s <- lmer(rankChoice_wi_0 ~ win + scale(scoreDelta) + scale(rankStart) + scale(trial)*scale(oppRank) + scale(rankChoice_wi_0.minus1) 
                  + scale(age) + scale(household_income) + ethnicity + gender + scale(gameExp) + scale(dass21_depression)
                  + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_1s)
car::Anova(mrankA2_1s, type = 'III')


#narcissistic finding: ffni total*trial interaction
mrankA2_1_ffni_n <- lmer(rankChoice_wi_0 ~ win + scale(scoreDelta) + scale(rankStart) + scale(trial)*scale(ffni_total)*scale(oppRank) +  scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_1_ffni_n)
car::Anova(mrankA2_1_ffni_n, type = 'III')

#output as 6x3'' pdf
plot(effect("scale(trial):scale(ffni_total)",mrankA2_1_ffni_n, xlevels = list('ffni_total' = c(87,153,209))), grid=TRUE, x.var = 'trial', xlab = 'trial', ylab = 'within-subject mean\nrank-buying', main = 'FFNI')


mrankA2_1_ffni_n_s0 <- lmer(rankChoice_wi_0 ~ win + scale(scoreDelta) + scale(rankStart) + scale(trial)*scale(ffni_total)*scale(oppRank) +  scale(rankChoice_wi_0.minus1) +
                              scale(age) + scale(household_income) + ethnicity + gender + scale(gameExp) +
                            (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_1_ffni_n_s0)
car::Anova(mrankA2_1_ffni_n_s0, type = 'III')

mrankA2_1_ffni_n_s1 <- lmer(rankChoice_wi_0 ~ win + scale(scoreDelta) + scale(rankStart) + scale(trial)*scale(ffni_total)*scale(oppRank) +  scale(rankChoice_wi_0.minus1) +
                              scale(age) + scale(household_income) + ethnicity + gender + scale(gameExp) + scale(dass21_depression) +
                              (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_1_ffni_n_s1)
car::Anova(mrankA2_1_ffni_n_s1, type = 'III')

mrankA2_1_ffni_n_s2 <- lmer(rankChoice_wi_0 ~ win + scale(scoreDelta) + scale(rankStart) + scale(trial)*scale(ffni_total)*scale(oppRank) +  scale(rankChoice_wi_0.minus1) +
                              scale(age) + scale(household_income) + ethnicity + gender + scale(gameExp) + scale(dass21_depression) + scale(dass21_anxiety) + scale(dass21_stress) +
                              (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_1_ffni_n_s2)
car::Anova(mrankA2_1_ffni_n_s2, type = 'III')
