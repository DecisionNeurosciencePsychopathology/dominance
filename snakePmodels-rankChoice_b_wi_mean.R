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
setwd("~/Dropbox/USA/Pittsburgh/GitHub/dominance/snake_data_Pittsburgh_may2018")

# wd for Alex
#setwd("~/code//dominance")

# clear environment
rm(list=ls())


## load datafile and skip to line 126 (use only if data has already been prepared previously, otherwise run lines 64 to 126) 

load("snake_totP.Rda")
load("snake_totP_shrunk.Rda")




# within- and between-subject models ###########################################

###between-subject
# narcissistic scales as single predictors
mrankB0_bpni <- lm(rankChoice_b_logit ~ scale(bpni_TOTAL), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_bpni)
car::Anova(mrankB0_bpni,type = 'III')

mrankB0_ffni <- lm(rankChoice_b_logit ~ scale(bpni_TOTAL), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni)
car::Anova(mrankB0_ffni,type = 'III')

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

# demographics only
mrankB0 <- lm(rankChoice_b_logit ~ scale(delta_panas_angry) + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0)
car::Anova(mrankB0,type = 'III')

mrankB1 <- lm(rankChoice_b_logit ~ scale(delta_panas_angry) + scale(education) + race + gender.y + scale(age)*scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB1)
car::Anova(mrankB1,type = 'III')

plot(effect("scale(age):scale(gameExp)",mrankB1), grid=TRUE)
anova(mrankB0, mrankB1)

# narcissistic scales + demographics
# bpni: nothing much
mrankB0_bpni <- lm(rankChoice_b_logit ~ scale(bpni_TOTAL) + scale(delta_panas_angry) + scale(age)*scale(gameExp) + scale(education) + race + gender.y, data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_bpni)
car::Anova(mrankB0_bpni,type = 'III')

#ffni total*education interaction // agentic extraversion and ipip for appleChoice
mrankB0_ffni <- lm(rankChoice_b_logit ~ scale(delta_panas_angry) + scale(age) + scale(ffni_total)*scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni)
car::Anova(mrankB0_ffni,type = 'III')

plot(effect("scale(ffni_total):scale(education)",mrankB0_ffni), grid=TRUE)

mrankB0_bpni_V <- lm(rankChoice_b_logit ~ scale(bpni_VULNERABILITY) + scale(delta_panas_angry) + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_bpni_V)
car::Anova(mrankB0_bpni_V,type = 'III')

mrankB0_ffni_V <- lm(rankChoice_b_logit ~ scale(ffni_VULNERABLE_NARCISSISM) + scale(delta_panas_angry) + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_V)
car::Anova(mrankB0_ffni_V,type = 'III')

#ffni antagonism*gender interaction // agentic extraversion in appleChoice
mrankB0_ffni_A <- lm(rankChoice_b_logit ~ scale(delta_panas_angry) + scale(age) + scale(education) + race + scale(ffni_ANTAGONISM)*gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_A)
car::Anova(mrankB0_ffni_A,type = 'III')

plot(effect("scale(ffni_ANTAGONISM):gender.y",mrankB0_ffni_A), grid=TRUE)

mrankB0_bpni_G <- lm(rankChoice_b_logit ~ scale(bpni_GANDIOSITY) + scale(delta_panas_angry) + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_bpni_G)
car::Anova(mrankB0_bpni_G,type = 'III')

mrankB0_ffni_G <- lm(rankChoice_b_logit ~ scale(delta_panas_angry) + scale(age) + scale(education) + race + scale(ffni_GRANDIOSE_NARCISSISM)*gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_G)
car::Anova(mrankB0_ffni_G,type = 'III')

# agentic extraversion // for appleChoice + weird thing with race
mrankB0_ffni_E <- lm(rankChoice_b_logit ~ scale(delta_panas_angry) + scale(age) + scale(education) + scale(ffni_AGENTIC_EXTRAVERSION)*race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_E)
car::Anova(mrankB0_ffni_E,type = 'III')

mrankB0_ffni_E2 <- lm(rankChoice_b_logit ~ scale(delta_panas_angry) + scale(age) + scale(education) + race + scale(ffni_AGENTIC_EXTRAVERSION)*gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_E2)
car::Anova(mrankB0_ffni_E2,type = 'III')

anova(mrankB0_ffni_E, mrankB0_ffni_E2)

mrankB0_ffni_N <- lm(rankChoice_b_logit ~ scale(delta_panas_angry) + scale(ffni_NARCISSISTIC_NEUROTICISM)+ scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_N)
car::Anova(mrankB0_ffni_N,type = 'III')

mrankB0_ipip <- lm(rankChoice_b_logit ~ scale(delta_panas_angry) + scale(ipip_total) + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ipip)
car::Anova(mrankB0_ipip,type = 'III')

# ipip: interaction with education // appleChoice
mrankB0_ipip1 <- lm(rankChoice_b_logit ~ scale(delta_panas_angry) + scale(age) + scale(education)*scale(ipip_total) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ipip1)
car::Anova(mrankB0_ipip1,type = 'III')

anova(mrankB0_ipip, mrankB0_ipip1)

# narcissistic scales +  panas scared
mrankB0_ffni <- lm(rankChoice_b_logit ~ scale(ffni_total)*scale(delta_panas_scared) + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni)
car::Anova(mrankB0_ffni,type = 'III')

mrankB0_bpni <- lm(rankChoice_b_logit ~ scale(bpni_TOTAL)*scale(delta_panas_scared) + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_bpni)
car::Anova(mrankB0_bpni,type = 'III')

mrankB0_bpni_V <- lm(rankChoice_b_logit ~ scale(bpni_VULNERABILITY)*scale(delta_panas_scared) + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_bpni_V)
car::Anova(mrankB0_bpni_V,type = 'III')

mrankB0_ffni_V <- lm(rankChoice_b_logit ~ scale(ffni_VULNERABLE_NARCISSISM)*scale(delta_panas_scared) + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_V)
car::Anova(mrankB0_ffni_V,type = 'III')

mrankB0_ffni_A <- lm(rankChoice_b_logit ~ scale(ffni_ANTAGONISM)*scale(delta_panas_scared) + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_A)
car::Anova(mrankB0_ffni_A,type = 'III')

mrankB0_bpni_G <- lm(rankChoice_b_logit ~ scale(bpni_GANDIOSITY)*scale(delta_panas_scared) + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_bpni_G)
car::Anova(mrankB0_bpni_G,type = 'III')

mrankB0_ffni_G <- lm(rankChoice_b_logit ~ scale(ffni_GRANDIOSE_NARCISSISM)*scale(delta_panas_scared) + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_G)
car::Anova(mrankB0_ffni_G,type = 'III')

mrankB0_ffni_E <- lm(rankChoice_b_logit ~ scale(ffni_AGENTIC_EXTRAVERSION)*scale(delta_panas_scared) + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_E)
car::Anova(mrankB0_ffni_E,type = 'III')

mrankB0_ffni_N <- lm(rankChoice_b_logit ~ scale(ffni_NARCISSISTIC_NEUROTICISM)*scale(delta_panas_scared) + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_N)
car::Anova(mrankB0_ffni_N,type = 'III')

plot(effect("scale(ffni_NARCISSISTIC_NEUROTICISM):scale(delta_panas_scared)",mrankB0_ffni_N), grid=TRUE)

mrankB0_ipip <- lm(rankChoice_b_logit ~ scale(ipip_total)*scale(delta_panas_scared) + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ipip)
car::Anova(mrankB0_ipip,type = 'III')

# narcissistic scales + panans pos
mrankB0_ffni <- lm(rankChoice_b_logit ~ scale(ffni_total)*scale(delta_panas_pos) + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni)
car::Anova(mrankB0_ffni,type = 'III')

mrankB0_bpni <- lm(rankChoice_b_logit ~ scale(bpni_TOTAL)*scale(delta_panas_pos) + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_bpni)
car::Anova(mrankB0_bpni,type = 'III')

mrankB0_bpni_V <- lm(rankChoice_b_logit ~ scale(bpni_VULNERABILITY)*scale(delta_panas_pos) + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_bpni_V)
car::Anova(mrankB0_bpni_V,type = 'III')

mrankB0_ffni_V <- lm(rankChoice_b_logit ~ scale(ffni_VULNERABLE_NARCISSISM)*scale(delta_panas_pos) + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_V)
car::Anova(mrankB0_ffni_V,type = 'III')

mrankB0_ffni_A <- lm(rankChoice_b_logit ~ scale(ffni_ANTAGONISM)*scale(delta_panas_pos) + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_A)
car::Anova(mrankB0_ffni_A,type = 'III')

mrankB0_bpni_G <- lm(rankChoice_b_logit ~ scale(bpni_GANDIOSITY)*scale(delta_panas_pos) + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_bpni_G)
car::Anova(mrankB0_bpni_G,type = 'III')

mrankB0_ffni_G <- lm(rankChoice_b_logit ~ scale(ffni_GRANDIOSE_NARCISSISM)*scale(delta_panas_pos) + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_G)
car::Anova(mrankB0_ffni_G,type = 'III')

mrankB0_ffni_E <- lm(rankChoice_b_logit ~ scale(ffni_AGENTIC_EXTRAVERSION)*scale(delta_panas_pos) + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_E)
car::Anova(mrankB0_ffni_E,type = 'III')

mrankB0_ffni_N <- lm(rankChoice_b_logit ~ scale(ffni_NARCISSISTIC_NEUROTICISM)*scale(delta_panas_pos) + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_N)
car::Anova(mrankB0_ffni_N,type = 'III')

plot(effect("scale(ffni_NARCISSISTIC_NEUROTICISM):scale(delta_panas_pos)",mrankB0_ffni_N), grid=TRUE)

mrankB0_ipip <- lm(rankChoice_b_logit ~ scale(ipip_total)*scale(delta_panas_pos) + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ipip)
car::Anova(mrankB0_ipip,type = 'III')

# narcissistic scales + panans angry_b
mrankB0_bpni <- lm(rankChoice_b_logit ~ scale(bpni_TOTAL)*scale(delta_panas_angry_b) + group1_5 + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_bpni)
car::Anova(mrankB0_bpni,type = 'III')

mrankB0_ffni <- lm(rankChoice_b_logit ~ scale(ffni_total)*scale(delta_panas_angry_b) + group1_5 + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni)
car::Anova(mrankB0_ffni,type = 'III')

mrankB0_bpni_V <- lm(rankChoice_b_logit ~ scale(bpni_VULNERABILITY)*scale(delta_panas_angry_b) + group1_5 + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_bpni_V)
car::Anova(mrankB0_bpni_V,type = 'III')

mrankB0_ffni_V <- lm(rankChoice_b_logit ~ scale(ffni_VULNERABLE_NARCISSISM)*scale(delta_panas_angry_b) + group1_5 + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_V)
car::Anova(mrankB0_ffni_V,type = 'III')

mrankB0_ffni_A <- lm(rankChoice_b_logit ~ scale(ffni_ANTAGONISM)*scale(delta_panas_angry_b) + group1_5 + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_A)
car::Anova(mrankB0_ffni_A,type = 'III')

mrankB0_bpni_G <- lm(rankChoice_b_logit ~ scale(bpni_GANDIOSITY)*scale(delta_panas_angry_b) + group1_5 + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_bpni_G)
car::Anova(mrankB0_bpni_G,type = 'III')

mrankB0_ffni_G <- lm(rankChoice_b_logit ~ scale(ffni_GRANDIOSE_NARCISSISM)*scale(delta_panas_angry_b) + group1_5 + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_G)
car::Anova(mrankB0_ffni_G,type = 'III')

mrankB0_ffni_E <- lm(rankChoice_b_logit ~ scale(ffni_AGENTIC_EXTRAVERSION)*scale(delta_panas_angry_b) + group1_5 + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_E)
car::Anova(mrankB0_ffni_E,type = 'III')

mrankB0_ffni_N <- lm(rankChoice_b_logit ~ scale(ffni_NARCISSISTIC_NEUROTICISM) + scale(delta_panas_angry_b) + group1_5 + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ffni_N)
car::Anova(mrankB0_ffni_N,type = 'III')

plot(effect("scale(ffni_NARCISSISTIC_NEUROTICISM):scale(delta_panas_angry_b)",mrankB0_ffni_N), grid=TRUE)

#positive mean effect!
mrankB0_ipip <- lm(rankChoice_b_logit ~ scale(ipip_total)*scale(delta_panas_angry_b) + group1_5 + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_ipip)
car::Anova(mrankB0_ipip,type = 'III')

###########################################################################################
##################### within-subject means ################################################
###########################################################################################

# design variables only
mrankA <- lmer(rankChoice_wi_0 ~ scale(trial) + win + close + scale(rankStart) + scale(oppRank) + scale(score) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA)
car::Anova(mrankA, type = 'III')

mrankA0 <- lmer(rankChoice_wi_0 ~ scale(trial) + win + scale(rankStart) + scale(oppRank) + scale(score) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA0)
car::Anova(mrankA0, type = 'III')

mrankA1 <- lmer(rankChoice_wi_0 ~ win + scale(trial)*scale(rankStart) + scale(oppRank) + scale(score) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA1)
car::Anova(mrankA1, type = 'III')

anova(mrankA0, mrankA1)

#best model with design variables only
mrankA0_1 <- lmer(rankChoice_wi_0 ~ scale(trial) + win + scale(rankStart) + scale(oppRank) + scale(score) + rankChoice_wi_0.minus1 + group1_5 + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA0_1)
car::Anova(mrankA0_1, type = 'III')

mrankA1_1 <- lmer(rankChoice_wi_0 ~ win + scale(trial)*scale(rankStart) + scale(oppRank) + scale(score) + rankChoice_wi_0.minus1 + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA1_1)
car::Anova(mrankA1_1, type = 'III')


#resume here, also do _b with design variables only.
## adding narcissistic scales
# bpni total: no correlations
mrankA2_bpni0 <- lmer(rankChoice_wi_0 ~ win + scale(trial) + scale(rankStart) + scale(oppRank) + scale(score) + scale(bpni_TOTAL) + group1_5 + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA2_bpni0)
car:: Anova(mrankA2_bpni0, type = 'III')

# best for bpni
mrankA2_1_bpni0 <- lmer(rankChoice_wi_0 ~ win + scale(trial) + scale(rankStart) + scale(oppRank) + scale(score) + scale(bpni_TOTAL) + scale(rankChoice_wi_0.minus1) + group1_5 + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA2_1_bpni0)
car:: Anova(mrankA2_1_bpni0, type = 'III')

#ffni
mrankA2_ffni0 <- lmer(rankChoice_wi_0 ~ win + scale(trial)*scale(rankStart) + scale(oppRank) + scale(score) + scale(ffni_total) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA2_ffni0)
car::Anova(mrankA2_ffni0, type = 'III')

mrankA2_ffni1 <- lmer(rankChoice_wi_0 ~ win + scale(trial)*scale(ffni_total) + scale(rankStart) + scale(oppRank) + scale(score) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA2_ffni1)
car::Anova(mrankA2_ffni1, type = 'III')

plot(effect("scale(trial):scale(ffni_total)",mrankA2_ffni1), grid=TRUE)

mrankA2_1_ffni3 <- lmer(rankChoice_wi_0 ~ win + scale(trial)*scale(ffni_total) + scale(trial)*scale(rankStart) + scale(oppRank) + scale(score) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA2_1_ffni3)
car::Anova(mrankA2_1_ffni3, type = 'III')

#best model for ffni
mrankA2_1_ffni1 <- lmer(rankChoice_wi_0 ~ win + scale(trial)*scale(ffni_total) + scale(rankStart) + scale(oppRank) + scale(score) + scale(rankChoice_wi_0.minus1) + group1_5 + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA2_1_ffni1)
car::Anova(mrankA2_1_ffni1, type = 'III')

plot(effect("scale(trial):scale(ffni_total)",mrankA2_1_ffni1), grid=TRUE)

#ffni subscales: same interaction (but only marginal) for vulnerable narcissism and antagonism
#still have to try with group1_5
mrankA2_1_ffni1_V <- lmer(rankChoice_wi_0 ~ win + scale(trial)*scale(ffni_VULNERABLE_NARCISSISM) + scale(rankStart) + scale(oppRank) + scale(score) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA2_1_ffni1_V)
car::Anova(mrankA2_1_ffni1_V, type = 'III')

mrankA2_1_ffni1_G <- lmer(rankChoice_wi_0 ~ win + scale(trial)*scale(ffni_GRANDIOSE_NARCISSISM) + scale(rankStart) + scale(oppRank) + scale(score) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA2_1_ffni1_G)
car::Anova(mrankA2_1_ffni1_G, type = 'III')

mrankA2_1_ffni1_A <- lmer(rankChoice_wi_0 ~ win + scale(trial)*scale(ffni_ANTAGONISM) + scale(rankStart) + scale(oppRank) + scale(score) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA2_1_ffni1_A)
car::Anova(mrankA2_1_ffni1_A, type = 'III')

mrankA2_1_ffni1_E <- lmer(rankChoice_wi_0 ~ win + scale(trial)*scale(ffni_AGENTIC_EXTRAVERSION) + scale(rankStart) + scale(oppRank) + scale(score) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA2_1_ffni1_E)
car::Anova(mrankA2_1_ffni1_E, type = 'III')

mrankA2_1_ffni1_N <- lmer(rankChoice_wi_0 ~ win + scale(trial)*scale(ffni_NARCISSISTIC_NEUROTICISM) + scale(rankStart) + scale(oppRank) + scale(score) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA2_1_ffni1_N)
car::Anova(mrankA2_1_ffni1_N, type = 'III')

#bpni subscales: no similar effect to ffni.
mrankA2_1_bpni1_V <- lmer(rankChoice_wi_0 ~ win + scale(trial)*scale(bpni_VULNERABILITY) + scale(rankStart) + scale(oppRank) + scale(score) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA2_1_bpni1_V)
car::Anova(mrankA2_1_bpni1_V, type = 'III')

mrankA2_1_bpni1_G <- lmer(rankChoice_wi_0 ~ win + scale(trial)*scale(bpni_GANDIOSITY) + scale(rankStart) + scale(oppRank) + scale(score) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA2_1_bpni1_G)
car::Anova(mrankA2_1_bpni1_G, type = 'III')

#ipip
mrankA2_1_ipip0 <- lmer(rankChoice_wi_0 ~ scale(ipip_total) + win + scale(trial) + scale(rankStart) + scale(oppRank) + scale(score) + scale(rankChoice_wi_0.minus1) + group1_5 + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA2_1_ipip0)
car::Anova(mrankA2_1_ipip0, type = 'III')

#best model for ipip
mrankA2_1_ipip1 <- lmer(rankChoice_wi_0 ~ win + scale(trial)*scale(ipip_total) + scale(rankStart) + scale(oppRank) + scale(score) + scale(rankChoice_wi_0.minus1) + group1_5 + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA2_1_ipip1)
car::Anova(mrankA2_1_ipip1, type = 'III')

plot(effect("scale(trial):scale(ipip_total)",mrankA2_1_ipip1), grid=TRUE)

###############################################################################################################
################################################ group differences ############################################
###############################################################################################################

############################################# between-subject #################################################
# as single predictor
mrankB0_gp <- lm(rankChoice_b_logit ~ group1_5, data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_gp)
car::Anova(mrankB0_gp,type = 'III')

# with demographics: nothing much
mrankB0_gp0 <- lm(rankChoice_b_logit ~ group1_5 + scale(delta_panas_angry)+ scale(age) + scale(education) + race  + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_gp0)
car::Anova(mrankB0_gp0,type = 'III')

#with narcissistic scales
mrankB0_gp1 <- lm(rankChoice_b_logit ~ group1_5*scale(bpni_TOTAL) + scale(delta_panas_angry)+ scale(age) + scale(education) + race  + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_gp1)
car::Anova(mrankB0_gp1,type = 'III')

mrankB0_gp2 <- lm(rankChoice_b_logit ~ group1_5*scale(ffni_total) + scale(delta_panas_angry)+ scale(age) + scale(education) + race  + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_gp2)
car::Anova(mrankB0_gp2,type = 'III')

#margninal for ipip
mrankB0_gp3 <- lm(rankChoice_b_logit ~ group1_5*scale(ipip_total) + scale(delta_panas_angry)+ scale(age) + scale(education) + race  + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_gp3)
car::Anova(mrankB0_gp3,type = 'III')

plot(effect("group1_5:scale(ipip_total)",mrankB0_gp3), grid=TRUE)

mrankB0_gp4 <- lm(rankChoice_b_logit ~ group1_5*scale(ffni_GRANDIOSE_NARCISSISM) + scale(delta_panas_angry)+ scale(age) + scale(education) + race  + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_gp4)
car::Anova(mrankB0_gp4,type = 'III')

mrankB0_gp5 <- lm(rankChoice_b_logit ~ group1_5*scale(ffni_VULNERABLE_NARCISSISM) + scale(delta_panas_angry)+ scale(age) + scale(education) + race  + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_gp5)
car::Anova(mrankB0_gp5,type = 'III')

mrankB0_gp6 <- lm(rankChoice_b_logit ~ group1_5*scale(ffni_ANTAGONISM) + scale(delta_panas_angry)+ scale(age) + scale(education) + race  + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_gp6)
car::Anova(mrankB0_gp6,type = 'III')

mrankB0_gp7 <- lm(rankChoice_b_logit ~ group1_5*scale(ffni_AGENTIC_EXTRAVERSION) + scale(delta_panas_angry)+ scale(age) + scale(education) + race  + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_gp7)
car::Anova(mrankB0_gp7,type = 'III')

mrankB0_gp8 <- lm(rankChoice_b_logit ~ group1_5*scale(ffni_NARCISSISTIC_NEUROTICISM) + scale(delta_panas_angry)+ scale(age) + scale(education) + race  + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_gp8)
car::Anova(mrankB0_gp8,type = 'III')

mrankB0_gp9 <- lm(rankChoice_b_logit ~ group1_5*scale(bpni_GANDIOSITY) + scale(delta_panas_angry)+ scale(age) + scale(education) + race  + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_gp9)
car::Anova(mrankB0_gp9,type = 'III')

mrankB0_gp10 <- lm(rankChoice_b_logit ~ group1_5*scale(bpni_VULNERABILITY) + scale(delta_panas_angry)+ scale(age) + scale(education) + race  + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mrankB0_gp10)
car::Anova(mrankB0_gp10,type = 'III')

#within-subject: fo not focus on this one, possible that suicidal groups do not ingage in the task.
mrankA0_1_gp0 <- lmer(rankChoice_wi_0 ~ scale(trial) + win + scale(rankStart) + scale(oppRank) + scale(score) + rankChoice_wi_0.minus1 + group1_5 + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA0_1_gp0)
car::Anova(mrankA0_1_gp0, type = 'III')

mrankA0_1_gp1 <- lmer(rankChoice_wi_0 ~ scale(trial) + win + scale(rankStart) + scale(oppRank)*group1_5 + scale(score) + rankChoice_wi_0.minus1 + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA0_1_gp1)
car::Anova(mrankA0_1_gp1, type = 'III')

mrankA0_1_gp2 <- lmer(rankChoice_wi_0 ~ scale(trial) + win + scale(rankStart) + scale(score)*scale(oppRank)*group1_5 + rankChoice_wi_0.minus1 + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA0_1_gp2)
car::Anova(mrankA0_1_gp2, type = 'III')

vif.lme(mrankA0_1_gp2)
anova(mrankA0_1_gp1, mrankA0_1_gp2)

plot(effect("scale(score):scale(oppRank):group1_5",mrankA0_1_gp2), grid=TRUE)

mscoreA0_1_gp2 <- lmer(score ~ group1_5*scale(trial) + scale(age)*scale(trial) + scale(education)*scale(trial) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mscoreA0_1_gp2)
car::Anova(mscoreA0_1_gp2, type = 'III')

