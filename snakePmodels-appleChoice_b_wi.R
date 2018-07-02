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

# between-subject models
# narcissistic scales as single predictors
mappleB0_bpni <- lm(appleChoice_b_logit ~ scale(bpni_TOTAL), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_bpni)
car::Anova(mappleB0_bpni,type = 'III')

mappleB0_ffni <- lm(appleChoice_b_logit ~ scale(ffni_total), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ffni)
car::Anova(mappleB0_ffni,type = 'III')

mappleB0_bpni_V <- lm(appleChoice_b_logit ~ scale(bpni_VULNERABILITY), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_bpni_V)
car::Anova(mappleB0_bpni_V,type = 'III')

mappleB0_ffni_V <- lm(appleChoice_b_logit ~ scale(ffni_VULNERABLE_NARCISSISM), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ffni_V)
car::Anova(mappleB0_ffni_V,type = 'III')

mappleB0_ffni_A <- lm(appleChoice_b_logit ~ scale(ffni_ANTAGONISM), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ffni_A)
car::Anova(mappleB0_ffni_A,type = 'III')

mappleB0_bpni_G <- lm(appleChoice_b_logit ~ scale(bpni_GANDIOSITY), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_bpni_G)
car::Anova(mappleB0_bpni_G,type = 'III')

mappleB0_ffni_G <- lm(appleChoice_b_logit ~ scale(ffni_GRANDIOSE_NARCISSISM), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ffni_G)
car::Anova(mappleB0_ffni_G,type = 'III')

mappleB0_ffni_E <- lm(appleChoice_b_logit ~ scale(ffni_AGENTIC_EXTRAVERSION), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ffni_E)
car::Anova(mappleB0_ffni_E,type = 'III')

mappleB0_ffni_N <- lm(appleChoice_b_logit ~ scale(ffni_NARCISSISTIC_NEUROTICISM), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ffni_N)
car::Anova(mappleB0_ffni_N,type = 'III')

#ipip: almost significant main effect
mappleB0_ipip <- lm(appleChoice_b_logit ~ scale(ipip_total), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ipip)
car::Anova(mappleB0_ipip,type = 'III')

# demographics only: no significant interactions
mappleB0 <- lm(appleChoice_b_logit ~ scale(delta_panas_angry) + scale(age) + scale(education) + race  + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0)
car::Anova(mappleB0,type = 'III')

# narcissistic scales + demographics
mappleB0_bpni <- lm(appleChoice_b_logit ~ scale(bpni_TOTAL) + scale(delta_panas_angry)+ scale(age) + scale(education) + race  + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_bpni)
car::Anova(mappleB0_bpni,type = 'III')

# ffni_ significant interaction with education // ipip
mappleB0_ffni <- lm(appleChoice_b_logit ~ scale(delta_panas_angry) + scale(age) + scale(ffni_total)*scale(education) + race + gender.y + scale(gameExp) + scale(delta_panas_angry), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ffni)
car::Anova(mappleB0_ffni,type = 'III')

plot(effect("scale(ffni_total):scale(education)",mappleB0_ffni), grid=TRUE)

mappleB0_bpni_V <- lm(appleChoice_b_logit ~ scale(bpni_VULNERABILITY)*scale(delta_panas_angry)+ scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_bpni_V)
car::Anova(mappleB0_bpni_V,type = 'III')

#ffni vulnerability*education
mappleB0_ffni_V <- lm(appleChoice_b_logit ~ scale(delta_panas_angry)+ scale(age) + scale(ffni_VULNERABLE_NARCISSISM)*scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ffni_V)
car::Anova(mappleB0_ffni_V,type = 'III')

#same interaction but only marginal
mappleB0_ffni_A <- lm(appleChoice_b_logit ~ scale(delta_panas_angry)+ scale(age) + scale(ffni_ANTAGONISM)*scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ffni_A)
car::Anova(mappleB0_ffni_A,type = 'III')

mappleB0_bpni_G <- lm(appleChoice_b_logit ~ scale(bpni_GANDIOSITY) + scale(delta_panas_angry)+ scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_bpni_G)
car::Anova(mappleB0_bpni_G,type = 'III')

#same interaction but only marginal
mappleB0_ffni_G <- lm(appleChoice_b_logit ~ scale(delta_panas_angry) + scale(age) + scale(ffni_GRANDIOSE_NARCISSISM)*scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ffni_G)
car::Anova(mappleB0_ffni_G,type = 'III')

# different interaction: this time with gender!
mappleB0_ffni_E <- lm(appleChoice_b_logit ~ scale(delta_panas_angry) + scale(age) + scale(education) + race + scale(ffni_AGENTIC_EXTRAVERSION)*gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ffni_E)
car::Anova(mappleB0_ffni_E,type = 'III')

plot(effect("scale(ffni_AGENTIC_EXTRAVERSION):gender.y",mappleB0_ffni_E), grid=TRUE)

mappleB0_ffni_N <- lm(appleChoice_b_logit ~ scale(ffni_NARCISSISTIC_NEUROTICISM) + scale(delta_panas_angry)+ scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ffni_N)
car::Anova(mappleB0_ffni_N,type = 'III')

#ipip: interaction with education
mappleB0_ipip1 <- lm(appleChoice_b_logit ~  scale(delta_panas_angry)+ scale(age) + scale(ipip_total)*scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ipip1)
car::Anova(mappleB0_ipip1,type = 'III')

plot(effect("scale(ipip_total):scale(education)",mappleB0_ipip1), grid=TRUE)


#with the scared panas scale
mappleB0_bpni <- lm(appleChoice_b_logit ~ scale(bpni_TOTAL) + scale(delta_panas_angry) + group1_5 +scale(age) + scale(education) + race  + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_bpni)
car::Anova(mappleB0_bpni,type = 'III')

mappleB0_ffni <- lm(appleChoice_b_logit ~ scale(ffni_total) + scale(delta_panas_angry) + group1_5 + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ffni)
car::Anova(mappleB0_ffni,type = 'III')

mappleB0_bpni_V <- lm(appleChoice_b_logit ~ scale(bpni_VULNERABILITY) + scale(delta_panas_angry) + group1_5 + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_bpni_V)
car::Anova(mappleB0_bpni_V,type = 'III')

mappleB0_ffni_V <- lm(appleChoice_b_logit ~ scale(ffni_VULNERABLE_NARCISSISM) + scale(delta_panas_angry) + group1_5 + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ffni_V)
car::Anova(mappleB0_ffni_V,type = 'III')

mappleB0_ffni_A <- lm(appleChoice_b_logit ~ scale(ffni_ANTAGONISM) + scale(delta_panas_angry) + group1_5 + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ffni_A)
car::Anova(mappleB0_ffni_A,type = 'III')

mappleB0_bpni_G <- lm(appleChoice_b_logit ~ scale(bpni_GANDIOSITY) + scale(delta_panas_angry) + group1_5 + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_bpni_G)
car::Anova(mappleB0_bpni_G,type = 'III')

mappleB0_ffni_G <- lm(appleChoice_b_logit ~ scale(ffni_GRANDIOSE_NARCISSISM) + scale(delta_panas_angry) + group1_5 + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ffni_G)
car::Anova(mappleB0_ffni_G,type = 'III')

mappleB0_ffni_E <- lm(appleChoice_b_logit ~ scale(ffni_AGENTIC_EXTRAVERSION)*scale(delta_panas_angry) + group1_5 + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ffni_E)
car::Anova(mappleB0_ffni_E,type = 'III')

#ffni_N: scale with significant main effect for applChoice
mappleB0_ffni_N <- lm(appleChoice_b_logit ~ scale(ffni_NARCISSISTIC_NEUROTICISM) + scale(delta_panas_angry) + group1_5 + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ffni_N)
car::Anova(mappleB0_ffni_N,type = 'III')

mappleB0_ipip <- lm(appleChoice_b_logit ~ scale(ipip_total) + scale(delta_panas_angry) + group1_5 + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ipip)
car::Anova(mappleB0_ipip,type = 'III')

###########################################################################################
##################### within-subject means ################################################
###########################################################################################

# design variables 
mappleA1 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank) + scale(rankEnd.minus1) + scale(score.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1)
car::Anova(mappleA1, type = 'III')

vif.lme(mappleA1)

#best model: same as for Vancouver dataset
mappleA2 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + group1_5 + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA2)
car::Anova(mappleA2, type = 'III')

vif.lme(mappleA2)
anova(mappleA1, mappleA2)
plot(effect("scale(oppRank):scale(score.minus1)",mappleA2), grid=TRUE)
hist(snake_totP$score.minus1)

#trying to add appleChoice_wi_0.minus1, but the model gets less easily interpretable, and not necessary here so dropped it.
mappleA2_1 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + scale(appleChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA2_1)
car::Anova(mappleA2_1, type = 'III')

vif.lme(mappleA2_1)
plot(effect("scale(oppRank):scale(score.minus1)",mappleA2_1), grid=TRUE)

# adding narcissistic scales

# bpni: nothing much
mappleA1_bpni0 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank) + scale(bpni_TOTAL) + scale(score.minus1) + scale(rankEnd.minus1)  + group1_5 + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_bpni0)
car::Anova(mappleA1_bpni0, type = 'III')

# # best model
# mappleA1_bpni1 <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(bpni_TOTAL) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
# summary(mappleA1_bpni1)
# car::Anova(mappleA1_bpni1, type = 'III')
# 
# vif.lme(mappleA1_bpni1)

mappleA1_1_bpni1 <- lmer(appleChoice_wi_0 ~ scale(trial) + scale(bpni_TOTAL) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + scale(appleChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_1_bpni1)
car::Anova(mappleA1_1_bpni1, type = 'III')

# ffni: //bpni in Vancouver sample
mappleA1_ffni0 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + scale(ffni_total) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ffni0)
car::Anova(mappleA1_ffni0, type = 'III')

#ffni:best model
mappleA1_ffni1 <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ffni_total) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + *scale(ffni_total) + group1_5 + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ffni1)
car::Anova(mappleA1_ffni1, type = 'III')

anova(mappleA1_ffni0, mappleA1_ffni1)

plot(effect("scale(trial):scale(ffni_total)",mappleA1_ffni1), grid=TRUE)
vif.lme(mappleA1_ffni1)

mappleA1_ffni1_0_ii <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ffni_total) + close.minus1 + win.minus1 + scale(oppRank) +  scale(rankEnd.minus1) 
                         + scale(age) + scale(education) + race + gender.y + scale(gameExp)
                         + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ffni1_0_ii)
car::Anova(mappleA1_ffni1_0_ii, type = 'III')

# ipip
mappleA1_ipip0 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + scale(ipip_total) + group1_5 + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ipip0)
car::Anova(mappleA1_ipip0, type = 'III')

mappleA1_ipip1 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(oppRank)*scale(ipip_total) + scale(rankEnd.minus1) + group1_5 + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ipip1)
car::Anova(mappleA1_ipip1, type = 'III')

plot(effect("scale(oppRank):scale(ipip_total)",mappleA1_ipip1), grid=TRUE)
vif.lme(mappleA1_ipip1)
anova(mappleA1_ipip0, mappleA1_ipip1)

#ipip:best model
mappleA1_ipip2 <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ipip_total) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(oppRank)*scale(ipip_total) + scale(rankEnd.minus1) + group1_5 + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ipip2)
car::Anova(mappleA1_ipip2, type = 'III')

anova(mappleA1_ipip1, mappleA1_ipip2)
vif.lme(mappleA1_ipip2)
plot(effect("scale(trial):scale(ipip_total)",mappleA1_ipip2), grid=TRUE)
plot(effect("scale(ipip_total):scale(oppRank)",mappleA1_ipip2), grid=TRUE)

##have to add group1_5
# bpni subscales
mappleA1_bpni1V <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(bpni_VULNERABILITY) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_bpni1V)
car::Anova(mappleA1_bpni1V, type = 'III')

mappleA1_bpni1G <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(bpni_GANDIOSITY) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_bpni1G)
car::Anova(mappleA1_bpni1G, type = 'III')

# ffni subscales
mappleA1_ffni1V <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ffni_VULNERABLE_NARCISSISM) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ffni1V)
car::Anova(mappleA1_ffni1V, type = 'III')

mappleA1_ffni1G <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ffni_GRANDIOSE_NARCISSISM) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1)  + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ffni1G)
car::Anova(mappleA1_ffni1G, type = 'III')

mappleA1_ffni1E <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ffni_AGENTIC_EXTRAVERSION) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1)  + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ffni1E)
car::Anova(mappleA1_ffni1E, type = 'III')

mappleA1_ffni1A <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ffni_ANTAGONISM) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1)  + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ffni1A)
car::Anova(mappleA1_ffni1A, type = 'III')

mappleA1_ffni1N <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ffni_NARCISSISTIC_NEUROTICISM) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1)  + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ffni1N)
car::Anova(mappleA1_ffni1N, type = 'III')

###############################################################################################################
################################################ group differences ############################################
###############################################################################################################

############################################# between-subject################################################
# as single predictor
mappleB0_gp <- lm(appleChoice_b_logit ~ group1_5, data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_gp)
car::Anova(mappleB0_gp,type = 'III')

# with demographics: nothing much
mappleB0_gp0 <- lm(appleChoice_b_logit ~ group1_5 + scale(delta_panas_angry)+ scale(age) + scale(education) + race  + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_gp0)
car::Anova(mappleB0_gp0,type = 'III')

#with narcissistic scales
mappleB0_gp1 <- lm(appleChoice_b_logit ~ group1_5*scale(bpni_TOTAL) + scale(delta_panas_angry)+ scale(age) + scale(education) + race  + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_gp1)
car::Anova(mappleB0_gp1,type = 'III')

mappleB0_gp2 <- lm(appleChoice_b_logit ~ group1_5*scale(ffni_total) + scale(delta_panas_angry)+ scale(age) + scale(education) + race  + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_gp2)
car::Anova(mappleB0_gp2,type = 'III')

mappleB0_gp3 <- lm(appleChoice_b_logit ~ group1_5*scale(ipip_total) + scale(delta_panas_angry)+ scale(age) + scale(education) + race  + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_gp3)
car::Anova(mappleB0_gp3,type = 'III')

mappleB0_gp4 <- lm(appleChoice_b_logit ~ group1_5*scale(ffni_GRANDIOSE_NARCISSISM) + scale(delta_panas_angry)+ scale(age) + scale(education) + race  + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_gp4)
car::Anova(mappleB0_gp4,type = 'III')

mappleB0_gp5 <- lm(appleChoice_b_logit ~ group1_5*scale(ffni_VULNERABLE_NARCISSISM) + scale(delta_panas_angry)+ scale(age) + scale(education) + race  + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_gp5)
car::Anova(mappleB0_gp5,type = 'III')

mappleB0_gp6 <- lm(appleChoice_b_logit ~ group1_5*scale(ffni_ANTAGONISM) + scale(delta_panas_angry)+ scale(age) + scale(education) + race  + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_gp6)
car::Anova(mappleB0_gp6,type = 'III')

mappleB0_gp7 <- lm(appleChoice_b_logit ~ group1_5*scale(ffni_AGENTIC_EXTRAVERSION) + scale(delta_panas_angry)+ scale(age) + scale(education) + race  + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_gp7)
car::Anova(mappleB0_gp7,type = 'III')

mappleB0_gp8 <- lm(appleChoice_b_logit ~ group1_5*scale(ffni_NARCISSISTIC_NEUROTICISM) + scale(delta_panas_angry)+ scale(age) + scale(education) + race  + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_gp8)
car::Anova(mappleB0_gp8,type = 'III')

mappleB0_gp9 <- lm(appleChoice_b_logit ~ group1_5*scale(bpni_GANDIOSITY) + scale(delta_panas_angry)+ scale(age) + scale(education) + race  + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_gp9)
car::Anova(mappleB0_gp9,type = 'III')

mappleB0_gp10 <- lm(appleChoice_b_logit ~ group1_5*scale(bpni_VULNERABILITY) + scale(delta_panas_angry)+ scale(age) + scale(education) + race  + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_gp10)
car::Anova(mappleB0_gp10,type = 'III')


############################################ within-subject
mappleA2_gp0 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + group1_5 + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA2_gp0)
car::Anova(mappleA2_gp0, type = 'III')

mappleA2_gp1 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank)*group1_5 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA2_gp1)
car::Anova(mappleA2_gp1, type = 'III')

mappleA2_gp2 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1*group1_5 + scale(oppRank) + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA2_gp2)
car::Anova(mappleA2_gp2, type = 'III')

anova(mappleA2_gp1, mappleA2_gp2)

mappleA2_gp3 <- lmer(appleChoice_wi_0 ~ scale(oppRank)*scale(ipip_total) + scale(trial) + close.minus1 + win.minus1*group1_5 + scale(oppRank)*group1_5 + scale(oppRank) + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA2_gp3)
car::Anova(mappleA2_gp3, type = 'III')

plot(effect("group1_5:scale(oppRank)",mappleA2_gp3), grid=TRUE)
plot(effect("win.minus1:group1_5",mappleA2_gp3), grid=TRUE)
vif.lme(mappleA2_gp3)

anova(mappleA2_gp1, mappleA2_gp3)
anova(mappleA2_gp2, mappleA2_gp3)

