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
setwd("~/code//dominance")

# load datafile and skip to line 126 (use only if data has already been prepared previously, otherwise run lines 64 to 126) 
load("snake_totP.Rda")
load("snake_totP_shrunk.Rda")
snake_totP_HC <- snake_totP[snake_totP$group1_5 == '1',]
snake_totP_shrunk_HC <- snake_totP_shrunk[snake_totP_shrunk$group1_5 == '1',]
snake_totP_AT <- snake_totP[snake_totP$group1_5 == '5',]
snake_totP_shrunk_AT <- snake_totP_shrunk[snake_totP_shrunk$group1_5 == '5',]

# releveling group1_5 to avoid high vif scores.
snake_totP$group1_5 <- relevel(snake_totP$group1_5, ref = '5')

################################################################
#################   between-subject models  ####################
################################################################

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

# narcissistic scales + demographics
mappleB0_bpni <- lm(appleChoice_b_logit ~ scale(bpni_TOTAL) + scale(delta_panas_angry)+ scale(age) + scale(education) + race  + gender.y + scale(household_income_log) + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_bpni)
car::Anova(mappleB0_bpni,type = 'III')

mappleB0_ffni <- lm(appleChoice_b_logit ~ scale(ffni_total) + scale(delta_panas_angry) + scale(age) + scale(education) + race + gender.y + scale(household_income_log) + scale(gameExp) + scale(delta_panas_angry), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ffni)
car::Anova(mappleB0_ffni,type = 'III')

mappleB0_bpni_V <- lm(appleChoice_b_logit ~ scale(bpni_VULNERABILITY) + scale(delta_panas_angry)+ scale(age) + scale(education) + race + gender.y  + scale(household_income_log) + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_bpni_V)
car::Anova(mappleB0_bpni_V,type = 'III')

mappleB0_ffni_V <- lm(appleChoice_b_logit ~ scale(ffni_VULNERABLE_NARCISSISM)*scale(delta_panas_angry)+ scale(age) + scale(education) + race + gender.y  + scale(household_income_log) + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ffni_V)
car::Anova(mappleB0_ffni_V,type = 'III')

mappleB0_ffni_G <- lm(appleChoice_b_logit ~ scale(ffni_GRANDIOSE_NARCISSISM) + scale(delta_panas_angry)+ scale(age) + scale(education) + race + gender.y  + scale(household_income_log) + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ffni_G)
car::Anova(mappleB0_ffni_G,type = 'III')

mappleB0_ffni_N <- lm(appleChoice_b_logit ~ scale(ffni_NARCISSISTIC_NEUROTICISM)*scale(delta_panas_angry)+ scale(age) + scale(education) + race + gender.y  + scale(household_income_log) + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ffni_N)
car::Anova(mappleB0_ffni_N,type = 'III')

mappleB0_ffni_E <- lm(appleChoice_b_logit ~ scale(ffni_AGENTIC_EXTRAVERSION) + scale(delta_panas_angry)+ scale(age) + scale(education) + race + gender.y  + scale(household_income_log) + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ffni_E)
car::Anova(mappleB0_ffni_E,type = 'III')

mappleB0_ffni_A <- lm(appleChoice_b_logit ~ scale(ffni_ANTAGONISM) + scale(delta_panas_angry)+ scale(age) + scale(education) + race + gender.y + scale(household_income_log) + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ffni_A)
car::Anova(mappleB0_ffni_A,type = 'III')

mappleB0_bpni_G <- lm(appleChoice_b_logit ~ scale(bpni_GANDIOSITY)*scale(delta_panas_angry)+ scale(age) + scale(education) + race + gender.y + scale(household_income_log) + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_bpni_G)
car::Anova(mappleB0_bpni_G,type = 'III')

mappleB0_ipip <- lm(appleChoice_b_logit ~ scale(ipip_total) + scale(delta_panas_angry)+ scale(age) + scale(education) + race + gender.y + scale(household_income_log) + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ipip)
car::Anova(mappleB0_ipip,type = 'III')

#ffni_N: scale with significant main effect for applChoice without household income
mappleB0_ffni_N <- lm(appleChoice_b_logit ~ scale(ffni_NARCISSISTIC_NEUROTICISM) + scale(delta_panas_angry) + group1_5 + scale(age) + scale(education) + race + gender.y + scale(gameExp), data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0_ffni_N)
car::Anova(mappleB0_ffni_N,type = 'III')

# same model with group instead of narcissistic scales
mappleB0_group <- lm(appleChoice_b_logit ~ group1_5 + scale(delta_panas_angry)+ scale(age) + scale(education) + race + gender.y + scale(household_income_log) + scale(gameExp), data = snake_totP_shrunk_HCDC, na.action = na.omit)
summary(mappleB0_group)
car::Anova(mappleB0_group,type = 'III')

mappleB0 <- lm(appleChoice_b_logit ~ group1_5 + scale(delta_panas_angry) + scale(age) + gender.y + scale(education),  data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB0)
car::Anova(mappleB0, type = 'III')

mappleB1 <- lm(appleChoice_b_logit ~ group1_5*scale(ipip_total) + scale(age) + gender.y + scale(education),  data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB1)
car::Anova(mappleB1, type = 'III')

plot(effect("group1_5:scale(ipip_total)",mappleB1), grid=TRUE)

mappleB2 <- lm(appleChoice_b_logit ~ group1_5*scale(bpni_TOTAL) + scale(age) + gender.y + scale(education),  data = snake_totP_shrunk, na.action = na.omit)
summary(mappleB2)
car::Anova(mappleB2, type = 'III')


## group predicting narcissism
mbpni0 <- lm(mbpni_TOTAL ~ group1_5 + scale(age) + gender.y + scale(education),  data = snake_totP_shrunk, na.action = na.omit)
summary(mbpni0)
car::Anova(mbpni0, type = 'III')

ls_mbpni0 <- lsmeans(mbpni0,"group1_5")
plot(ls_mbpni0, horiz=F, ylab = "", xlab = "groups")
cld(ls_mbpni0, sort = FALSE)

mbpni1 <- lm(mbpni_GANDIOSITY ~ group1_5 + scale(age) + gender.y + scale(education),  data = snake_totP_shrunk, na.action = na.omit)
summary(mbpni1)
car::Anova(mbpni1, type = 'III')

ls_mbpni1 <- lsmeans(mbpni1,"group1_5")
plot(ls_mbpni1, horiz=F, ylab = "", xlab = "groups")
cld(ls_mbpni1, sort = FALSE)

mbpni2 <- lm(mbpni_VULNERABILITY ~ group1_5 + scale(age) + gender.y + scale(education),  data = snake_totP_shrunk, na.action = na.omit)
summary(mbpni2)
car::Anova(mbpni2, type = 'III')

ls_mbpni2 <- lsmeans(mbpni2,"group1_5")
plot(ls_mbpni2, horiz=F, ylab = "", xlab = "groups")
cld(ls_mbpni2, sort = FALSE)

mffni0 <- lm(ffni_total ~ group1_5 + scale(age) + gender.y + scale(education),  data = snake_totP_shrunk, na.action = na.omit)
summary(mffni0)
car::Anova(mffni0, type = 'III')

ls_mffni0 <- lsmeans(mffni0,"group1_5")
plot(ls_mffni0, horiz=F, ylab = "", xlab = "groups")
cld(ls_mffni0, sort = FALSE)

mffni1 <- lm(ffni_GRANDIOSE_NARCISSISM ~ group1_5 + scale(age) + gender.y + scale(education),  data = snake_totP_shrunk, na.action = na.omit)
summary(mffni1)
car::Anova(mffni1, type = 'III')

ls_mffni1 <- lsmeans(mffni1,"group1_5")
plot(ls_mffni1, horiz=F, ylab = "", xlab = "groups")
cld(ls_mffni1, sort = FALSE)

mffni2 <- lm(ffni_VULNERABLE_NARCISSISM ~ group1_5 + scale(age) + gender.y + scale(education),  data = snake_totP_shrunk, na.action = na.omit)
summary(mffni2)
car::Anova(mffni2, type = 'III')

ls_mffni2 <- lsmeans(mffni2,"group1_5")
plot(ls_mffni2, horiz=F, ylab = "", xlab = "groups")
cld(ls_mffni2, sort = FALSE)

mffni3 <- lm(ffni_ANTAGONISM ~ group1_5 + scale(age) + gender.y + scale(education),  data = snake_totP_shrunk, na.action = na.omit)
summary(mffni3)
car::Anova(mffni3, type = 'III')

ls_mffni3 <- lsmeans(mffni3,"group1_5")
plot(ls_mffni3, horiz=F, ylab = "", xlab = "groups")
cld(ls_mffni3, sort = FALSE)

mffni4 <- lm(ffni_AGENTIC_EXTRAVERSION ~ group1_5 + scale(age) + gender.y + scale(education),  data = snake_totP_shrunk, na.action = na.omit)
summary(mffni4)
car::Anova(mffni4, type = 'III')

ls_mffni4 <- lsmeans(mffni4,"group1_5")
plot(ls_mffni4, horiz=F, ylab = "", xlab = "groups")
cld(ls_mffni4, sort = FALSE)

mffni5 <- lm(ffni_NARCISSISTIC_NEUROTICISM ~ group1_5 + scale(age) + gender.y + scale(education),  data = snake_totP_shrunk, na.action = na.omit)
summary(mffni5)
car::Anova(mffni5, type = 'III')

ls_mffni5 <- lsmeans(mffni5,"group1_5")
plot(ls_mffni5, horiz=F, ylab = "", xlab = "groups")
cld(ls_mffni5, sort = FALSE)

mipip <- lm(ipip_total ~ group1_5 + scale(age) + gender.y + scale(education),  data = snake_totP_shrunk, na.action = na.omit)
summary(mipip)
car::Anova(mipip, type = 'III')

ls_mipip <- lsmeans(mipip,"group1_5")
plot(ls_mipip, horiz=F, ylab = "", xlab = "groups")
cld(ls_mipip, sort = FALSE)


## same with HC only 

mappleB0_bpni <- lm(appleChoice_b_logit ~ scale(bpni_TOTAL) + scale(delta_panas_angry)+ scale(age) + scale(education) + race  + gender.y + scale(household_income_log) + scale(gameExp), data = snake_totP_shrunk_HC, na.action = na.omit)
summary(mappleB0_bpni)
car::Anova(mappleB0_bpni,type = 'III')

mappleB0_ffni <- lm(appleChoice_b_logit ~ scale(ffni_total) + scale(delta_panas_angry) + scale(age) + scale(education) + race + gender.y + scale(household_income_log) + scale(gameExp) + scale(delta_panas_angry), data = snake_totP_shrunk_HC, na.action = na.omit)
summary(mappleB0_ffni)
car::Anova(mappleB0_ffni,type = 'III')

mappleB0_bpni_V <- lm(appleChoice_b_logit ~ scale(bpni_VULNERABILITY)*scale(delta_panas_angry)+ scale(age) + scale(education) + race + gender.y  + scale(household_income_log) + scale(gameExp), data = snake_totP_shrunk_HC, na.action = na.omit)
summary(mappleB0_bpni_V)
car::Anova(mappleB0_bpni_V,type = 'III')

mappleB0_ffni_V <- lm(appleChoice_b_logit ~ scale(ffni_VULNERABLE_NARCISSISM) + scale(delta_panas_angry)+ scale(age) + scale(education) + race + gender.y  + scale(household_income_log) + scale(gameExp), data = snake_totP_shrunk_HC, na.action = na.omit)
summary(mappleB0_ffni_V)
car::Anova(mappleB0_ffni_V,type = 'III')

mappleB0_ffni_G <- lm(appleChoice_b_logit ~ scale(ffni_GRANDIOSE_NARCISSISM)*scale(delta_panas_angry)+ scale(age) + scale(education) + race + gender.y  + scale(household_income_log) + scale(gameExp), data = snake_totP_shrunk_HC, na.action = na.omit)
summary(mappleB0_ffni_G)
car::Anova(mappleB0_ffni_G,type = 'III')

mappleB0_ffni_N <- lm(appleChoice_b_logit ~ scale(ffni_NARCISSISTIC_NEUROTICISM) + scale(delta_panas_angry)+ scale(age) + scale(education) + race + gender.y  + scale(household_income_log) + scale(gameExp), data = snake_totP_shrunk_HC, na.action = na.omit)
summary(mappleB0_ffni_N)
car::Anova(mappleB0_ffni_N,type = 'III')

mappleB0_ffni_E <- lm(appleChoice_b_logit ~ scale(ffni_AGENTIC_EXTRAVERSION)*scale(delta_panas_angry)+ scale(age) + scale(education) + race + gender.y  + scale(household_income_log) + scale(gameExp), data = snake_totP_shrunk_HC, na.action = na.omit)
summary(mappleB0_ffni_E)
car::Anova(mappleB0_ffni_E,type = 'III')

mappleB0_ffni_A <- lm(appleChoice_b_logit ~ scale(ffni_ANTAGONISM) + scale(delta_panas_angry)+ scale(age) + scale(education) + race + gender.y + scale(household_income_log) + scale(gameExp), data = snake_totP_shrunk_HC, na.action = na.omit)
summary(mappleB0_ffni_A)
car::Anova(mappleB0_ffni_A,type = 'III')

mappleB0_bpni_G <- lm(appleChoice_b_logit ~ scale(bpni_GANDIOSITY) + scale(delta_panas_angry)+ scale(age) + scale(education) + race + gender.y + scale(household_income_log) + scale(gameExp), data = snake_totP_shrunk_HC, na.action = na.omit)
summary(mappleB0_bpni_G)
car::Anova(mappleB0_bpni_G,type = 'III')

mappleB0_ipip <- lm(appleChoice_b_logit ~ scale(ipip_total) + scale(delta_panas_angry)+ scale(age) + scale(education) + race + gender.y + scale(household_income_log) + scale(gameExp), data = snake_totP_shrunk_HC, na.action = na.omit)
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
mappleA2 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1)+ (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA2)
car::Anova(mappleA2, type = 'III')

vif.lme(mappleA2)
anova(mappleA1, mappleA2)

plot(effect("scale(oppRank):scale(score.minus1)",mappleA2), grid=TRUE)
hist(snake_totP$score.minus1)


##with narcissistic scales, without study groups
#bpni total
mappleA1_1_bpni1 <- lmer(appleChoice_wi_0 ~ scale(bpni_TOTAL) + scale(trial) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_1_bpni1)
car::Anova(mappleA1_1_bpni1, type = 'III')

# ffni: //bpni in Vancouver sample
mappleA1_ffni0 <- lmer(appleChoice_wi_0 ~ scale(ffni_total) + scale(trial) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ffni0)
car::Anova(mappleA1_ffni0, type = 'III')

mappleA1_ffni1 <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ffni_total) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ffni1)
car::Anova(mappleA1_ffni1, type = 'III')

anova(mappleA1_ffni0, mappleA1_ffni1)
vif.lme(mappleA1_ffni1)
plot(effect("scale(trial):scale(ffni_total)",mappleA1_ffni1), grid=TRUE)

mappleA1_ipip <- lmer(appleChoice_wi_0 ~ scale(ipip_total) + scale(trial) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ipip)
car::Anova(mappleA1_ipip, type = 'III')

# ipip
mappleA1_ipip0 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + scale(ipip_total)+ (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ipip0)
car::Anova(mappleA1_ipip0, type = 'III')

mappleA1_ipip1 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(oppRank)*scale(ipip_total) + scale(rankEnd.minus1)+ (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ipip1)
car::Anova(mappleA1_ipip1, type = 'III')

plot(effect("scale(oppRank):scale(ipip_total)",mappleA1_ipip1), grid=TRUE)
vif.lme(mappleA1_ipip1)
anova(mappleA1_ipip0, mappleA1_ipip1)

#ipip:best model
mappleA1_ipip2 <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ipip_total) + close.minus1 + win.minus1 + scale(oppRank)*scale(ipip_total) +  scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ipip2)
car::Anova(mappleA1_ipip2, type = 'III')

anova(mappleA1_ipip1, mappleA1_ipip2)
vif.lme(mappleA1_ipip2)
plot(effect("scale(trial):scale(ipip_total)",mappleA1_ipip2), grid=TRUE)
plot(effect("scale(ipip_total):scale(oppRank)",mappleA1_ipip2), x.var = 'oppRank', grid=TRUE)


#with group, without narcissistic scales 
mappleA2_group0 <- lmer(appleChoice_wi_0 ~ group1_5 + scale(trial) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA2_group0)
car::Anova(mappleA2_group0, type = 'III')

mappleA2_group1 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1*group1_5 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA2_group1)
car::Anova(mappleA2_group1, type = 'III')

vif.lme(mappleA2_group1)
plot(effect("win.minus1:group1_5",mappleA2_group1), grid=TRUE, x.var = "win.minus1")

mappleA2_group2 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank)*group1_5 + scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA2_group2)
car::Anova(mappleA2_group2, type = 'III')

vif.lme(mappleA2_group2)
plot(effect("scale(oppRank):group1_5",mappleA2_group2), grid=TRUE)

anova(mappleA2_group1, mappleA2_group2)

mappleA2_group3 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1*group1_5 + scale(oppRank)*group1_5 + scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA2_group3)
car::Anova(mappleA2_group3, type = 'III')

vif.lme(mappleA2_group3)

mappleA2_group4 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1*scale(oppRank)*group1_5 + scale(oppRank) + scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA2_group4)
car::Anova(mappleA2_group4, type = 'III')

#best model with group and design variables
mappleA2_group5 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1*group1_5 + scale(oppRank)*group1_5 + scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA2_group5)
car::Anova(mappleA2_group5, type = 'III')

vif.lme(mappleA2_group5)
anova(mappleA2_group2, mappleA2_group5)
plot(effect("win.minus1:group1_5",mappleA2_group5), grid=TRUE, x.var = "win.minus1")
plot(effect("group1_5:scale(oppRank)",mappleA2_group5), grid=TRUE, x.var = "oppRank")

# sensitivity for the best model with group
mappleA2_group5s <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1*group1_5 + scale(oppRank)*group1_5 + scale(score.minus1) + scale(rankEnd.minus1) + age + gender.y + scale(education) + race + scale(gameExp) + scale(household_income_log) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA2_group5s)
car::Anova(mappleA2_group5s, type = 'III')


#group and narcissistic scales (not sure these models are useful)
#with narcissistic scales
mappleA3e_bpni1 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1*group1_5*scale(bpni_TOTAL) + scale(oppRank)*group1_5 + scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA3e_bpni1)
car::Anova(mappleA3e_bpni1, type = 'III')

# best model for group and BPNI
mappleA3e_bpni2 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1*group1_5 + scale(oppRank)*group1_5*scale(bpni_TOTAL) + scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA3e_bpni2)
car::Anova(mappleA3e_bpni2, type = 'III')

mappleA3e_bpni3 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1*group1_5*scale(oppRank)*scale(bpni_TOTAL) + scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA3e_bpni3)
car::Anova(mappleA3e_bpni3, type = 'III')

mappleA3e_bpni4 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1*group1_5*scale(bpni_TOTAL) + group1_5*scale(oppRank)*scale(bpni_TOTAL) + scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA3e_bpni4)
car::Anova(mappleA3e_bpni4, type = 'III')

anova(mappleA3e_bpni1, mappleA3e_bpni2)
anova(mappleA3e_bpni1, mappleA3e_bpni3)
anova(mappleA3e_bpni2, mappleA3e_bpni4)
anova(mappleA3e_bpni3, mappleA3e_bpni4)

vif.lme(mappleA3e_bpni2)
vif.lme(mappleA3e_bpni4)

plot(effect("win.minus1:group1_5:scale(bpni_TOTAL)",mappleA3e_bpni4), grid=TRUE, x.var = "bpni_TOTAL")
plot(effect("group1_5:scale(bpni_TOTAL):scale(oppRank)",mappleA3e_bpni2), grid=TRUE, x.var = "oppRank")

mappleA3e_ffni1 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1*group1_5*scale(ffni_total) + scale(oppRank)*group1_5 + scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA3e_ffni1)
car::Anova(mappleA3e_ffni1, type = 'III')

mappleA3e_ffni2 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1*group1_5 + scale(oppRank)*group1_5*scale(ffni_total) + scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA3e_ffni2)
car::Anova(mappleA3e_ffni2, type = 'III')

mappleA3e_ffni3 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1*group1_5*scale(oppRank)*scale(ffni_total) + scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA3e_ffni3)
car::Anova(mappleA3e_ffni3, type = 'III')

mappleA3e_ffni4 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1*group1_5*scale(ffni_total) + group1_5*scale(oppRank)*bpni_TOTAL + scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA3e_ffni4)
car::Anova(mappleA3e_ffni4, type = 'III')

anova(mappleA3e_bpni1, mappleA3e_bpni2)
anova(mappleA3e_bpni1, mappleA3e_bpni3)
anova(mappleA3e_bpni2, mappleA3e_bpni4)
anova(mappleA3e_bpni3, mappleA3e_bpni4)

vif.lme(mappleA3e_ffni2)
vif.lme(mappleA3e_ffni4)
plot(effect("group1_5:scale(oppRank):scale(ffni_total)",mappleA3e_ffni2), grid=TRUE, x.var = "oppRank")

# no similar interactions with IPIP
mappleA3e_ipip2 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1*group1_5 + scale(oppRank)*group1_5*scale(ipip_total) + scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA3e_ipip2)
car::Anova(mappleA3e_ipip2, type = 'III')


mappleA2 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1)+ (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA2)
car::Anova(mappleA2, type = 'III')

# bpni
mappleA3e_bpni <- lmer(appleChoice_wi_0 ~ scale(bpni_TOTAL) + scale(trial) + close.minus1 + win.minus1*group1_5 + scale(oppRank)*group1_5 + scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA3e_bpni)
car::Anova(mappleA3e_bpni, type = 'III')

mappleA3e_bpni1 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + scale(bpni_TOTAL)*win.minus1*group1_5 + scale(oppRank)*group1_5 + scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA3e_bpni1)
car::Anova(mappleA3e_bpni1, type = 'III')

anova(mappleA3e_bpni, mappleA3e_bpni1)
vif.lme(mappleA3e_bpni1)

#best model with BPNI and group
mappleA3e_bpni2 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1*group1_5 + scale(bpni_TOTAL)*scale(oppRank)*group1_5 + scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA3e_bpni2)
car::Anova(mappleA3e_bpni2, type = 'III')

anova(mappleA3e_bpni1, mappleA3e_bpni2)
vif.lme(mappleA3e_bpni2)
plot(effect("group1_5:scale(bpni_TOTAL):scale(oppRank)",mappleA3e_bpni2), grid=TRUE)

# ffni
mappleA3e_ffni <- lmer(appleChoice_wi_0 ~ scale(ffni_total) + scale(trial) + close.minus1 + win.minus1*group1_5 + scale(oppRank)*group1_5 + scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA3e_ffni)
car::Anova(mappleA3e_ffni, type = 'III')

mappleA3e_ffni1 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1*group1_5 + scale(ffni_total)*scale(oppRank)*group1_5 + scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA3e_ffni1)
car::Anova(mappleA3e_ffni1, type = 'III')

anova(mappleA3e_ffni, mappleA3e_ffni1)
vif.lme(mappleA3e_ffni1)
plot(effect("group1_5:scale(ffni_total):scale(oppRank)",mappleA3e_ffni1), grid=TRUE)

mappleA3e_ipip <- lmer(appleChoice_wi_0 ~ scale(ipip_total) + scale(trial) + close.minus1 + win.minus1*group1_5 + scale(oppRank)*group1_5 + scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA3e_ipip)
car::Anova(mappleA3e_ipip, type = 'III')


## checking in HC only

snake_totP_HC <- snake_totP[snake_totP$group1_5 == '1',]

#no interactions found with design variables only
mapple_HC <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank) + scale(score.minus1) + scale(rankEnd.minus1)+ (1|ID),  data = snake_totP_HC, na.action = na.omit)
summary(mapple_HC)
car::Anova(mapple_HC, type = 'III')

#adding narcissistic scales
#bpni: nothing much
mapple_HC_bpni <- lmer(appleChoice_wi_0 ~ scale(bpni_TOTAL) + scale(trial) + close.minus1 + win.minus1 + scale(oppRank) + scale(score.minus1) + scale(rankEnd.minus1)+ (1|ID),  data = snake_totP_HC, na.action = na.omit)
summary(mapple_HC_bpni)
car::Anova(mapple_HC_bpni, type = 'III')

mapple_HC_ffni <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(ffni_total) + scale(oppRank) + scale(score.minus1) + scale(rankEnd.minus1)+ (1|ID),  data = snake_totP_HC, na.action = na.omit)
summary(mapple_HC_ffni)
car::Anova(mapple_HC_ffni, type = 'III')

mapple_HC_ffni1 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank)*scale(ffni_total) + scale(score.minus1) + scale(rankEnd.minus1)+ (1|ID),  data = snake_totP_HC, na.action = na.omit)
summary(mapple_HC_ffni1)
car::Anova(mapple_HC_ffni1, type = 'III')

mapple_HC_ffni2 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank) + scale(score.minus1) + scale(rankEnd.minus1)*scale(ffni_total) + (1|ID),  data = snake_totP_HC, na.action = na.omit)
summary(mapple_HC_ffni2)
car::Anova(mapple_HC_ffni2, type = 'III')

mapple_HC_ffni3 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(score.minus1) + scale(oppRank)*scale(rankEnd.minus1)*scale(ffni_total) + (1|ID),  data = snake_totP_HC, na.action = na.omit)
summary(mapple_HC_ffni3)
car::Anova(mapple_HC_ffni3, type = 'III')


mapple_HC_ffni4 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank)*scale(ffni_total) + scale(score.minus1) + scale(rankEnd.minus1)*scale(ffni_total) + (1|ID),  data = snake_totP_HC, na.action = na.omit)
summary(mapple_HC_ffni4)
car::Anova(mapple_HC_ffni4, type = 'III')

anova(mapple_HC_ffni, mapple_HC_ffni1)
anova(mapple_HC_ffni1, mapple_HC_ffni2)
anova(mapple_HC_ffni2, mapple_HC_ffni4)

vif.lme(mapple_HC_ffni4)
plot(effect("scale(oppRank):scale(ffni_total)",mapple_HC_ffni4), grid=TRUE)
plot(effect("scale(ffni_total):scale(rankEnd.minus1)",mapple_HC_ffni4), grid=TRUE)

#best model with ffni
mapple_HC_ffni5 <- lmer(appleChoice_wi_0 ~ close.minus1 + win.minus1 + scale(trial)*scale(oppRank)*scale(ffni_total) + scale(score.minus1) + scale(rankEnd.minus1)*scale(ffni_total) + (1|ID),  data = snake_totP_HC, na.action = na.omit)
summary(mapple_HC_ffni5)
car::Anova(mapple_HC_ffni5, type = 'III')

vif.lme(mapple_HC_ffni5)
anova(mapple_HC_ffni4, mapple_HC_ffni5)
plot(effect("scale(trial):scale(oppRank):scale(ffni_total)",mapple_HC_ffni5), grid=TRUE)
plot(effect("scale(ffni_total):scale(rankEnd.minus1)",mapple_HC_ffni5), grid=TRUE)
plot(effect("scale(trial):scale(ffni_total)",mapple_HC_ffni5), grid=TRUE)
plot(effect("scale(oppRank):scale(ffni_total)",mapple_HC_ffni5), grid=TRUE)

mapple_HC_ipip <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(ipip_total) + scale(oppRank) + scale(score.minus1) + scale(rankEnd.minus1)+ (1|ID),  data = snake_totP_HC, na.action = na.omit)
summary(mapple_HC_ipip)
car::Anova(mapple_HC_ipip, type = 'III')

mapple_HC_ipip1 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + scale(oppRank) + win.minus1*scale(ipip_total)*scale(score.minus1) + scale(rankEnd.minus1)+ (1|ID),  data = snake_totP_HC, na.action = na.omit)
summary(mapple_HC_ipip1)
car::Anova(mapple_HC_ipip1, type = 'III')

vif.lme(mapple_HC_ipip1)
anova(mapple_HC_ipip, mapple_HC_ipip1)

#best model for ipip
mapple_HC_ipip2 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + scale(oppRank) + win.minus1*scale(ipip_total) + scale(score.minus1)*scale(ipip_total) + scale(rankEnd.minus1)+ (1|ID),  data = snake_totP_HC, na.action = na.omit)
summary(mapple_HC_ipip2)
car::Anova(mapple_HC_ipip2, type = 'III')

vif.lme(mapple_HC_ipip2)
anova(mapple_HC_ipip, mapple_HC_ipip2)
plot(effect("scale(ipip_total):scale(score.minus1)",mapple_HC_ipip2), grid=TRUE, x.var = 'score.minus1')
plot(effect("win.minus1:scale(ipip_total)",mapple_HC_ipip2), grid=TRUE, x.var = 'win.minus1')


mapple_HC_ipip3 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + scale(oppRank) + scale(score.minus1)*scale(ipip_total) + scale(rankEnd.minus1)+ (1|ID),  data = snake_totP_HC, na.action = na.omit)
summary(mapple_HC_ipip3)
car::Anova(mapple_HC_ipip3, type = 'III')

anova(mapple_HC_ipip2, mapple_HC_ipip3)
