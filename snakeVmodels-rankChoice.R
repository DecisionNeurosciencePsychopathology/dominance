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


library(lme4)
## Rank choice models
# multivariate model
mrank0 <- glmer(rankChoice_binary ~ scale(trial) + win + close + scale(score) + scale(rankStart) + scale(oppRank) + scale(scoreDiff) + rankChoice_binary.minus1 + appleChoice_binary + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank0)
car::Anova(mrank0)

mrank1 <- glmer(rankChoice_binary ~ scale(trial) + win + close + scale(score) + scale(rankStart) + rankChoice_binary.minus1 + appleChoice_binary + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank1)
car::Anova(mrank1)

mrank2 <- glmer(rankChoice_binary ~ scale(trial) + win + close + scale(scoreDelta) + scale(rankStart) + rankChoice_binary.minus1 + appleChoice_binary + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank2)
car::Anova(mrank2)

mrank3 <- glmer(rankChoice_binary ~ scale(trial) + win + scale(score) + scale(rankStart) + rankChoice_binary.minus1 + appleChoice_binary + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3)
car::Anova(mrank3)

anova(mrank1, mrank3)

mrank4 <- glmer(rankChoice_binary ~ scale(trial) + win + close + scale(score) + rankStart_binary + rankChoice_binary.minus1 + appleChoice_binary + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank4)
car::Anova(mrank4)


#checking for interactions in best-fitting model mrank3

mrank3i <- glmer(rankChoice_binary ~ win + scale(score) + scale(rankStart)*scale(trial) + rankChoice_binary.minus1 + appleChoice_binary + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3i)
car::Anova(mrank3i)

plot(effect("scale(rankStart):scale(trial)", mrank3i), grid=TRUE)

mrank3ii <- glmer(rankChoice_binary ~ win + scale(score) + scale(rankStart) + rankChoice_binary.minus1*scale(trial) + appleChoice_binary + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3ii)
car::Anova(mrank3ii)

plot(effect("rankChoice_binary.minus1:scale(trial)", mrank3ii), grid=TRUE)

mrank3iii <- glmer(rankChoice_binary ~ win + scale(score) + rankChoice_binary.minus1*scale(trial)*scale(rankStart) + appleChoice_binary + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
s <- getME(mrank3iii,c("theta","fixef"))
mrank3iii.2 <- update(mrank3iii, start=s, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))
summary(mrank3iii.2)
car::Anova(mrank3iii.2)

anova(mrank3ii, mrank3iii.2)

plot(effect("rankChoice_binary.minus1:scale(trial):scale(rankStart)", mrank3iii.2), grid=TRUE)

mrank3iv <- glmer(rankChoice_binary ~ scale(trial) + scale(score) + scale(rankStart) + rankChoice_binary.minus1*win + appleChoice_binary + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3iv)
car::Anova(mrank3iv)

plot(effect("rankChoice_binary.minus1:win", mrank3iv), grid=TRUE)

# best model: mrank3v
mrank3v <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(score) + scale(rankStart) + appleChoice_binary + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v)
car::Anova(mrank3v)

anova(mrank3iv, mrank3v)

mrank3vi <- glmer(rankChoice_binary ~ scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(score) + scale(rankStart) + appleChoice_binary + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3vi)
car::Anova(mrank3vi)

anova(mrank3v, mrank3vi)

#simpler model mrank3vii (as good as mrank3v) => kept this one
mrank3vii <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3vii)
car::Anova(mrank3vii)

mrank3viii <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial) + scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3viii)
car::Anova(mrank3viii)

anova(mrank3v, mrank3vii)
anova(mrank3vii, mrank3viii)

plot(effect("rankChoice_binary.minus1:win", mrank3vii), grid=TRUE)
plot(effect("rankChoice_binary.minus1:scale(trial):scale(rankStart)", mrank3vii), grid=TRUE)


# narcissistic scales in best-fitting model
#bpni: nothing much
mrank3v_bpni <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(bpni_TOTAL) + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_bpni)
car::Anova(mrank3v_bpni)

mrank3v_bpni1 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(bpni_TOTAL)*win + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_bpni1)
car::Anova(mrank3v_bpni1)

mrank3v_bpni2 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + appleChoice_binary + scale(bpni_TOTAL)*scale(rankStart) + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
s <- getME(mrank3v_bpni2,c("theta","fixef"))
mrank3v_bpni2.2 <- update(mrank3v_bpni2, start=s, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))
summary(mrank3v_bpni2.2)
car::Anova(mrank3v_bpni2.2)

mrank3v_bpni3 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win*scale(bpni_TOTAL) + scale(rankStart) + appleChoice_binary + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_bpni3)
car::Anova(mrank3v_bpni3)

mrank3v_bpni4 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(bpni_TOTAL)*rankChoice_binary.minus1 + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_bpni4)
car::Anova(mrank3v_bpni4)

mrank3v_bpni5 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(bpni_TOTAL)*scale(scoreDelta) + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_bpni5)
car::Anova(mrank3v_bpni5)

mrank3v_bpni6 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary*scale(bpni_TOTAL) + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_bpni6)
car::Anova(mrank3v_bpni6)


mrank3v_bpniV <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(bpni_VULNERABILITY) + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_bpniV)
car::Anova(mrank3v_bpniV)

mrank3v_bpniV1 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(bpni_VULNERABILITY)*win + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_bpniV1)
car::Anova(mrank3v_bpniV1)

mrank3v_bpniV2 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary*scale(bpni_VULNERABILITY) + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
s <- getME(mrank3v_bpniV2,c("theta","fixef"))
mrank3v_bpniV2.2 <- update(mrank3v_bpniV2, start=s, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))
summary(mrank3v_bpniV2.2)
car::Anova(mrank3v_bpniV2.2)

mrank3v_bpniV3 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win*scale(bpni_VULNERABILITY) + scale(rankStart) + appleChoice_binary + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_bpniV3)
car::Anova(mrank3v_bpniV3)

mrank3v_bpniV4 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + rankChoice_binary.minus1*scale(bpni_VULNERABILITY) + scale(rankStart) + appleChoice_binary + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_bpniV4)
car::Anova(mrank3v_bpniV4)

anova(mrank3v_bpniV, mrank3v_bpniV4)

plot(effect("rankChoice_binary.minus1:scale(bpni_VULNERABILITY)", mrank3v_bpniV4), grid=TRUE)


mrank3v_bpniG <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(bpni_GANDIOSITY) + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_bpniG)
car::Anova(mrank3v_bpniG)

mrank3v_bpniG1 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(bpni_GANDIOSITY)*win + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_bpniG1)
car::Anova(mrank3v_bpniG1)

mrank3v_bpniG2 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(bpni_GANDIOSITY)*scale(rankStart) + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_bpniG2)
car::Anova(mrank3v_bpniG2)

mrank3v_bpniG3 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + rankChoice_binary.minus1*scale(bpni_GANDIOSITY) + scale(rankStart) + appleChoice_binary + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
s <- getME(mrank3v_bpniG3,c("theta","fixef"))
mrank3v_bpniG3.2 <- update(mrank3v_bpniG3, start=s, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))
summary(mrank3v_bpniG3.2)
car::Anova(mrank3v_bpniG3.2)

mrank3v_bpniG4 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary*scale(bpni_GANDIOSITY) + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_bpniG4)
car::Anova(mrank3v_bpniG4)

#ffni: interaction with appleChoice of same round (mrank3v_ffni3): for total narcissism, grandiose narcissism, agentic extraversion, marginal for antagonism but not there for vulnerable (which is interacting with rankStart)
mrank3v_ffni <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(ffni_total) + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_ffni)
car::Anova(mrank3v_ffni)

mrank3v_ffni1 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(ffni_total)*win + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_ffni1)
car::Anova(mrank3v_ffni1)

mrank3v_ffni2 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(ffni_total)*win*appleChoice_binary + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
s <- getME(mrank3v_ffni2,c("theta","fixef"))
mrank3v_ffni2.2 <- update(mrank3v_ffni2, start=s, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))
summary(mrank3v_ffni2.2)
car::Anova(mrank3v_ffni2.2)

mrank3v_ffni3 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + scale(ffni_total)*appleChoice_binary + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_ffni3)
car::Anova(mrank3v_ffni3)

anova(mrank3v_ffni.3, mrank3v_ffni3)
anova(mrank3v_ffni1.3, mrank3v_ffni3)

mrank3v_ffni4 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(score) + scale(rankStart) + appleChoice_binary + scale(ffni_total)*scale(rankStart) + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_ffni4)
car::Anova(mrank3v_ffni4)

plot(effect("scale(ffni_total):appleChoice_binary", mrank3v_ffni3), grid=TRUE)

mrank3v_ffni3_sensitivity <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + scale(ffni_total)*appleChoice_binary + gameExp + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_ffni3_sensitivity)
car::Anova(mrank3v_ffni3_sensitivity)

mrank3v_ffni3_sensitivity2 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + scale(ffni_total)*appleChoice_binary + gameExp + scale(age) + gender + household_income + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
s <- getME(mrank3v_ffni3_sensitivity2,c("theta","fixef"))
mrank3v_ffni3_sensitivity2.2 <- update(mrank3v_ffni3_sensitivity2, start=s, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))
summary(mrank3v_ffni3_sensitivity2.2)
car::Anova(mrank3v_ffni3_sensitivity2.2)


mrank3v_ffni3G0 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(ffni_GRANDIOSE_NARCISSISM) + appleChoice_binary + scale(snakeLevel) + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_ffni3G0)
car::Anova(mrank3v_ffni3G0)

mrank3v_ffni3G <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(ffni_GRANDIOSE_NARCISSISM)*appleChoice_binary + gameExp + household_income + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_ffni3G)
car::Anova(mrank3v_ffni3G)

plot(effect("appleChoice_binary:scale(ffni_GRANDIOSE_NARCISSISM)", mrank3v_ffni3G), grid=TRUE)


mrank3v_ffni3G2 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(ffni_GRANDIOSE_NARCISSISM)*win + scale(snakeLevel) + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_ffni3G2)
car::Anova(mrank3v_ffni3G2)

mrank3v_ffni3V0 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(ffni_VULNERABLE_NARCISSISM) + appleChoice_binary + scale(snakeLevel) + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_ffni3V0)
car::Anova(mrank3v_ffni3V0)

mrank3v_ffni3V <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(ffni_VULNERABLE_NARCISSISM)*appleChoice_binary + gameExp + household_income + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
s <- getME(mrank3v_ffni3V,c("theta","fixef"))
mrank3v_ffni3V.2 <- update(mrank3v_ffni3V, start=s, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))
summary(mrank3v_ffni3V.2)
car::Anova(mrank3v_ffni3V.2)

mrank3v_ffni3V2 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(ffni_VULNERABLE_NARCISSISM)*scale(rankStart) + scale(snakeLevel) + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_ffni3V2)
car::Anova(mrank3v_ffni3V2)

plot(effect("scale(rankStart):scale(ffni_VULNERABLE_NARCISSISM)", mrank3v_ffni3V2), grid=TRUE)

mrank3v_ffni3V3 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(ffni_VULNERABLE_NARCISSISM)*scale(rankStart)*win + scale(snakeLevel) + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
s <- getME(mrank3v_ffni3V3,c("theta","fixef"))
mrank3v_ffni3V3.2 <- update(mrank3v_ffni3V3, start=s, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))
summary(mrank3v_ffni3V3.2)
car::Anova(mrank3v_ffni3V3.2)

mrank3v_ffni3V4 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(ffni_VULNERABLE_NARCISSISM)*rankStart_binary + scale(snakeLevel) + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_ffni3V4)
car::Anova(mrank3v_ffni3V4)

mrank3v_ffni3A0 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(ffni_ANTAGONISM) + appleChoice_binary + scale(snakeLevel) + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_ffni3A0)
car::Anova(mrank3v_ffni3A0)

mrank3v_ffni3A1 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + scale(ffni_ANTAGONISM)*appleChoice_binary + scale(snakeLevel) + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_ffni3A1)
car::Anova(mrank3v_ffni3A1)

mrank3v_ffni3A2 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(ffni_ANTAGONISM)*appleChoice_binary*scale(rankStart) + scale(snakeLevel) + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_ffni3A2)
car::Anova(mrank3v_ffni3A2)

mrank3v_ffni3A3 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(ffni_ANTAGONISM)*appleChoice_binary + gameExp + household_income + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_ffni3A3)
car::Anova(mrank3v_ffni3A3)

mrank3v_ffni3A4 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + scale(ffni_ANTAGONISM)*scale(rankStart) + scale(snakeLevel) + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_ffni3A4)
car::Anova(mrank3v_ffni3A4)


mrank3v_ffni3E0 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(ffni_AGENTIC_EXTRAVERSION) + appleChoice_binary + scale(snakeLevel) + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_ffni3E0)
car::Anova(mrank3v_ffni3E0)

mrank3v_ffni3E1 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + scale(ffni_AGENTIC_EXTRAVERSION)*appleChoice_binary + gameExp + household_income + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_ffni3E1)
car::Anova(mrank3v_ffni3E1)

plot(effect("appleChoice_binary:scale(ffni_AGENTIC_EXTRAVERSION)", mrank3v_ffni3E1), grid=TRUE)


mrank3v_ffni3E2 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(ffni_AGENTIC_EXTRAVERSION)*scale(rankStart) + scale(snakeLevel) + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_ffni3E2)
car::Anova(mrank3v_ffni3E2)

mrank3v_ffni3E3 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(ffni_AGENTIC_EXTRAVERSION)*appleChoice_binary*win + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_ffni3E3)
car::Anova(mrank3v_ffni3E3)

#pgsi (gambling)
mrank3v_pgsi <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(pgsi_total) + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
s <- getME(mrank3v_pgsi,c("theta","fixef"))
mrank3v_pgsi.2 <- update(mrank3v_pgsi, start=s, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))
summary(mrank3v_pgsi.2)
car::Anova(mrank3v_pgsi.2)

mrank3v_pgsi1 <- glmer(rankChoice_binary ~ rankChoice_binary.minus1*scale(trial)*scale(rankStart) + rankChoice_binary.minus1*win + scale(rankStart) + appleChoice_binary + scale(pgsi_total)*rankChoice_binary.minus1*win + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mrank3v_pgsi1)
car::Anova(mrank3v_pgsi1)

anova(mrank3v_pgsi, mrank3v_pgsi1)

plot(effect("rankChoice_binary.minus1:scale(pgsi_total):win", mrank3v_pgsi1), grid=TRUE)


##PITTSBUTGH MODELS##
mrank4.7 <- lmer(rankChoice ~ rankEstim2*satisfied*ipip + rankStart*trial + win + score*close + rankStart*rankChoice.minus1*rankChoice.minus2+ rankChoice.minus2*score + rankStart*appleChoice + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrank4.7)
car::Anova(mrank4.7)

anova(mrank2.7, mrank4.7)
# mrank 4.7 non-significantly better model

mrank5.7 <- lmer(rankChoice ~ rankEstim2*satisfied*ipip + rankStart*trial + score*close + rankStart*rankChoice.minus1*rankChoice.minus2+ rankChoice.minus2*score + rankStart*appleChoice + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrank5.7)
car::Anova(mrank5.7)

anova(mrank4.7, mrank5.7)
#mrank4.7 = better model

## BLOCK8 = BLOCKS 1 and 4
mrank1.8 <- lmer(rankChoice ~ ipip + rankStart*trial + win + score*close + rankStart*rankChoice.minus1*rankChoice.minus2+ rankChoice.minus2*score + rankStart*appleChoice + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrank1.8)
car::Anova(mrank1.8)
anova(mrank4.7, mrank1.8)

# spaghe