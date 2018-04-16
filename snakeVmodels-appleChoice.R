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


#building models by blocks
## BLOCK1: design variables
# correlation matrix for design variables


## Apple choice
# multivariate model
mapple1 <- lmer(appleChoice ~ trial + win.minus1 + scale(oppRank) + scoreDelta.minus1 + scale(rankEnd.minus1) + rankChoice.minus1 + appleChoice.minus1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple1)
car::Anova(mapple1)

mapple2 <- lmer(appleChoice ~ trial + win.minus1 + scale(oppRank) + scoreDelta.minus1 + rankChoice.minus1 + appleChoice.minus1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple2)
car::Anova(mapple2)

anova(mapple1, mapple2)

mapple3 <- lmer(appleChoice ~ trial + close.minus1 + win.minus1 + scale(oppRank) + scoreDiff.minus1 + scoreDelta.minus1 + scale(rankEnd.minus1) + rankChoice.minus1 + appleChoice.minus1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple3)
car::Anova(mapple3)

anova(mapple2, mapple3)

# binary models
mapple1 <- glmer(appleChoice_binary ~ trial + win.minus1 + scale(oppRank) + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + appleChoice_binary.minus1 + (1|ID),  data = snake_tot, family = binomial, na.action = na.omit)
summary(mapple1)
car::Anova(mapple1)

mapple3 <- glmer(appleChoice_binary ~ trial + close.minus1 + win.minus1 + scale(oppRank) + scale(scoreDiff.minus1) + scale(score.minus1) + scale(rankEnd.minus1) + rankChoice.minus1 + appleChoice.minus1 + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple3)
car::Anova(mapple3)

mapple4 <- glmer(appleChoice_binary ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank) + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + appleChoice_binary.minus1 + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4)
car::Anova(mapple4)

anova(mapple1, mapple4)

#checking for interactions in best binary model (mapple 4)
mapple4i <- glmer(appleChoice_binary ~ close.minus1*scale(trial) + win.minus1 + scale(oppRank) + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + appleChoice_binary.minus1 + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4i)
car::Anova(mapple4i)

anova(mapple4, mapple4i)

plot(effect("close.minus1:scale(trial)",mapple4i), grid=TRUE)

mapple4ii <- glmer(appleChoice_binary ~ scale(trial) + win.minus1 + scale(oppRank)*close.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + appleChoice_binary.minus1 + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4ii)
car::Anova(mapple4ii)

plot(effect("scale(oppRank):close.minus1",mapple4ii), grid=TRUE)

mapple4iv <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1 +  win.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + appleChoice_binary.minus1 + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4iv)
car::Anova(mapple4iv)

anova(mapple4ii, mapple4iv)

mapple4v <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1*appleChoice_binary.minus1 +  win.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4v)
car::Anova(mapple4v)

anova(mapple4i, mapple4v)

#best model with only design variables:
mapple4vi <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1*appleChoice_binary.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4vi)
car::Anova(mapple4vi)

anova(mapple4v, mapple4vi)
anova(mapple4i, mapple4vi)

plot(effect("close.minus1:appleChoice_binary.minus1:win.minus1",mapple4vi), grid=TRUE)
plot(effect("close.minus1:scale(oppRank):appleChoice_binary.minus1",mapple4vi), grid=TRUE)


vif.lme(mapple4vi)


mapple4vii <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4vii)
car::Anova(mapple4vii)

anova(mapple4vi, mapple4vii)


mapple4vii <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1*appleChoice_binary.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(rankEnd.minus1) + rankChoice_binary.minus1 + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4vii)
car::Anova(mapple4vii)

anova(mapple4vi, mapple4vii)

mapple4viii <- glmer(appleChoice_binary ~ scale(trial) + close.minus1  + scale(oppRank)*close.minus1*appleChoice_binary.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(rankEnd.minus1) + rankChoice_binary.minus1 + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4viii)
car::Anova(mapple4viii)

anova(mapple4vii, mapple4viii)

mapple4ix <- glmer(appleChoice_binary ~ close.minus1  + scale(oppRank)*close.minus1*appleChoice_binary.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(rankEnd.minus1) + rankChoice_binary.minus1 + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4ix)
car::Anova(mapple4ix)

anova(mapple4vii, mapple4ix)
#best models: mapple4vi (lowest AIC) OR mapple4ix (simplest)

## improving model by predcting appleChoice_delta and getting rid of previous choices
mappleA1 <- lmer(appleChoiceDelta ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank) + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA1)
car::Anova(mappleA1, type = 'III')

mappleA2 <- lmer(appleChoiceDelta ~ scale(trial)*close.minus1 + win.minus1 + scale(oppRank) + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA2)
car::Anova(mappleA2, type = 'III')

anova(mappleA1, mappleA2)

# best model with design variables only
mappleA3 <- lmer(appleChoiceDelta ~ scale(trial)*close.minus1 + win.minus1 + scale(oppRank) + scale(trial)*scale(scoreDelta.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA3)
car::Anova(mappleA3, type = 'III')

anova(mappleA2, mappleA3)
vif.lme(mappleA3)

plot(effect("scale(trial):scale(scoreDelta.minus1)",mappleA3), grid=TRUE)
plot(effect("scale(trial):close.minus1",mappleA3), grid=TRUE)

mappleA4 <- lmer(appleChoiceDelta ~ win.minus1 + scale(oppRank) + scale(trial)*close.minus1*scale(scoreDelta.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA4)
car::Anova(mappleA4, type = 'III')


##narcissistic scales
#bpni

mapple4vi_bpni <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1*appleChoice_binary.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + bpni_TOTAL + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4vi_bpni)
car::Anova(mapple4vi_bpni)

#best model
mapple4vi_bpni1 <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1*appleChoice_binary.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + bpni_TOTAL*win.minus1 + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4vi_bpni1)
car::Anova(mapple4vi_bpni1)

plot(effect("win.minus1:bpni_TOTAL",mapple4vi_bpni1), grid=TRUE)

vif.lme(mapple4vi_bpni1)


mapple4vi_bpni2 <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + bpni_TOTAL*win.minus1 + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4vi_bpni2)
car::Anova(mapple4vi_bpni2)

anova(mapple4vi_bpni1,mapple4vi_bpni2)

plot(effect("close.minus1:scale(oppRank)",mapple4vi_bpni2), grid=TRUE)

#sensitivtiy analysis for best BPNI model
mapple4vi_bpni1_sensitivity <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1*appleChoice_binary.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + scale(bpni_TOTAL)*win.minus1 + gameExp + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4vi_bpni1_sensitivity)
car::Anova(mapple4vi_bpni1_sensitivity)


plot(effect("win.minus1:bpni_TOTAL",mapple4vi_bpni1), grid=TRUE)

mapple4vi_bpni1G <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1*appleChoice_binary.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + scale(bpni_GANDIOSITY)*win.minus1 + gameExp + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4vi_bpni1G)
car::Anova(mapple4vi_bpni1G)

plot(effect("win.minus1:scale(bpni_GANDIOSITY)",mapple4vi_bpni1G), grid=TRUE)


vif.lme(mapple4vi_bpni1G)


mapple4vi_bpni1V <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1*appleChoice_binary.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + bpni_VULNERABILITY*win.minus1 + gameExp + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4vi_bpni1V)
car::Anova(mapple4vi_bpni1V)

plot(effect("win.minus1:bpni_VULNERABILITY",mapple4vi_bpni1V), grid=TRUE)


vif.lme(mapple4vi_bpni1V)


# ffni
mapple4vi_ffni <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1*appleChoice_binary.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + scale(ffni_total) + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4vi_ffni)
car::Anova(mapple4vi_ffni)

#best model
mapple4vi_ffni1 <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1*appleChoice_binary.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + scale(ffni_total)*win.minus1 + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4vi_ffni1)
car::Anova(mapple4vi_ffni1)

plot(effect("win.minus1:scale(ffni_total)",mapple4vi_ffni1), grid=TRUE)

vif.lme(mapple4vi_ffni1)


mapple4vi_ffni2 <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + scale(ffni_total)*win.minus1 + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4vi_ffni2)
car::Anova(mapple4vi_ffni2)

anova(mapple4vi_ffni1,mapple4vi_ffni2)

#sensitivtiy analysis for best FFNI model
mapple4vi_ffni1_sensitivity <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1*appleChoice_binary.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + scale(ffni_total)*win.minus1 + gameExp + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4vi_ffni1_sensitivity)
car::Anova(mapple4vi_ffni1_sensitivity)

mapple4vi_ffni2 <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1*appleChoice_binary.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + scale(ffni_total)*scale(oppRank) + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
s <- getME(mapple4vi_ffni2,c("theta","fixef"))
mapple4vi_ffni2.2 <- update(mapple4vi_ffni2, start=s, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))
ss <- getME(mapple4vi_ffni2.2,c("theta","fixef"))
mapple4vi_ffni2.3 <- update(mapple4vi_ffni2.2, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))

mapple4vi_ffni1G <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1*appleChoice_binary.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + scale(ffni_GRANDIOSE_NARCISSISM)*win.minus1 + gameExp + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4vi_ffni1G)
car::Anova(mapple4vi_ffni1G)

plot(effect("win.minus1:scale(ffni_GRANDIOSE_NARCISSISM)",mapple4vi_ffni1G), grid=TRUE)


vif.lme(mapple4vi_ffni1G)


mapple4vi_ffni1E <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1*appleChoice_binary.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + scale(ffni_AGENTIC_EXTRAVERSION)*win.minus1 + gameExp + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4vi_ffni1E)
car::Anova(mapple4vi_ffni1E)

plot(effect("win.minus1:scale(ffni_AGENTIC_EXTRAVERSION)",mapple4vi_ffni1E), grid=TRUE)

vif.lme(mapple4vi_ffni1E)


mapple4vi_ffni1V <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1*appleChoice_binary.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + scale(ffni_VULNERABLE_NARCISSISM)*win.minus1 + gameExp + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4vi_ffni1V)
car::Anova(mapple4vi_ffni1V)

plot(effect("win.minus1:scale(ffni_VULNERABLE_NARCISSISM)",mapple4vi_ffni1V), grid=TRUE)
plot(effect("win.minus1:scale(ffni_ANTAGONISM)",mapple4vi_ffni1A), grid=TRUE)


vif.lme(mapple4vi_ffni1V)


mapple4vi_ffniV2 <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1*appleChoice_binary.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + scale(ffni_VULNERABLE_NARCISSISM)*scale(oppRank) + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
s <- getME(mapple4vi_ffniV2,c("theta","fixef"))
mapple4vi_ffniV2.2 <- update(mapple4vi_ffniV2, start=s, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))
summary(mapple4vi_ffniV2.2)
car::Anova(mapple4vi_ffniV2.2)

mapple4vi_ffniV3 <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1*appleChoice_binary.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + scale(ffni_VULNERABLE_NARCISSISM)*scale(oppRank)*scale(trial) + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
s <- getME(mapple4vi_ffniV3,c("theta","fixef"))
mapple4vi_ffniV3.2 <- update(mapple4vi_ffniV3, start=s, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))
summary(mapple4vi_ffniV3.2)
car::Anova(mapple4vi_ffniV3.2)

mapple4vi_ffni1A <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1*appleChoice_binary.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + scale(ffni_ANTAGONISM)*win.minus1 + gameExp + scale(age) + gender + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4vi_ffni1A)
car::Anova(mapple4vi_ffni1A)


vif.lme(mapple4vi_ffni1A)


mapple4vi_ffniA1 <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1*appleChoice_binary.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + scale(ffni_ANTAGONISM)*win.minus1*scale(oppRank) + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
s <- getME(mapple4vi_ffniA1,c("theta","fixef"))
mapple4vi_ffniA1.2 <- update(mapple4vi_ffniA1, start=s, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))
ss <- getME(mapple4vi_ffniA1.2,c("theta","fixef"))
mapple4vi_ffniA1.3 <- update(mapple4vi_ffniA1.2, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))
sss <- getME(mapple4vi_ffniA1.3,c("theta","fixef"))
mapple4vi_ffniA1.4 <- update(mapple4vi_ffniA1.3, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))

mapple4vi_ffniA1 <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1*appleChoice_binary.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + scale(ffni_ANTAGONISM)*scale(oppRank) + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4vi_ffniA1)
car::Anova(mapple4vi_ffniA1)

#pgsi (gambling)
mapple4vi_pgsi <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1*appleChoice_binary.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + scale(pgsi_total) + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4vi_pgsi)
car::Anova(mapple4vi_pgsi)

mapple4vi_pgsi2 <- glmer(appleChoice_binary ~ scale(trial)*close.minus1  + scale(oppRank)*close.minus1*appleChoice_binary.minus1 + close.minus1*appleChoice_binary.minus1*win.minus1 + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + rankChoice_binary.minus1 + scale(pgsi_total)*win.minus1 + (1|ID),  data = snake_tot, family = binomial, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)), na.action = na.omit)
summary(mapple4vi_pgsi2)
car::Anova(mapple4vi_pgsi2)

anova(mapple4vi_pgsi, mapple4vi_pgsi2)


#best model with delta instead of absolute choices

##BPNI
mappleA3_bpni <- lmer(appleChoiceDelta ~ scale(trial)*close.minus1 + win.minus1 + scale(oppRank) + scale(trial)*scale(scoreDelta.minus1) + scale(rankEnd.minus1) + scale(bpni_TOTAL) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA3_bpni)
car::Anova(mappleA3_bpni, type = 'III')

mappleA3_bpni1 <- lmer(appleChoiceDelta ~ scale(trial)*close.minus1 + win.minus1*scale(bpni_TOTAL) + scale(oppRank) + scale(trial)*scale(scoreDelta.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA3_bpni1)
car::Anova(mappleA3_bpni1, type = 'III')

mappleA3_bpni2 <- lmer(appleChoiceDelta ~ scale(trial)*close.minus1 + win.minus1*scale(bpni_TOTAL) + scale(oppRank) + scale(trial) + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA3_bpni2)
car::Anova(mappleA3_bpni2, type = 'III')

anova(mappleA3_bpni1, mappleA3_bpni2)

mappleA3_bpni3 <- lmer(appleChoiceDelta ~ scale(trial)*close.minus1 + close.minus1*scale(bpni_TOTAL) + win.minus1 + scale(oppRank) + scale(trial)*scale(scoreDelta.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA3_bpni3)
car::Anova(mappleA3_bpni3, type = 'III')

anova(mappleA3_bpni1, mappleA3_bpni3)

mappleA3_bpni4 <- lmer(appleChoiceDelta ~ scale(trial)*close.minus1 + close.minus1*scale(bpni_TOTAL)*win.minus1 + scale(oppRank) + scale(trial)*scale(scoreDelta.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA3_bpni4)
car::Anova(mappleA3_bpni4, type = 'III')

anova(mappleA3_bpni3, mappleA3_bpni4)

mappleA3_bpni5 <- lmer(appleChoiceDelta ~ scale(trial)*close.minus1 + close.minus1*scale(bpni_TOTAL) + scale(bpni_TOTAL)*win.minus1 + scale(oppRank) + scale(scoreDelta.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA3_bpni5)
car::Anova(mappleA3_bpni5, type = 'III')

anova(mappleA3_bpni3, mappleA3_bpni5)
anova(mappleA3_bpni4, mappleA3_bpni5)

# best model with BPNI total score
mappleA3_bpni6 <- lmer(appleChoiceDelta ~ scale(trial)*close.minus1 + close.minus1*scale(bpni_TOTAL)*scale(scoreDelta.minus1) + scale(bpni_TOTAL)*win.minus1 + scale(oppRank) + scale(rankEnd.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA3_bpni6)
car::Anova(mappleA3_bpni6, type = 'III')

anova(mappleA3_bpni5, mappleA3_bpni6)
vif.lme(mappleA3_bpni6)

plot(effect("scale(bpni_TOTAL):win.minus1",mappleA3_bpni6), grid=TRUE)
plot(effect("close.minus1:scale(bpni_TOTAL):scale(scoreDelta.minus1)",mappleA3_bpni6), grid=TRUE)


mappleA3_bpni7 <- lmer(appleChoiceDelta ~ scale(trial)*close.minus1 + close.minus1*scale(bpni_TOTAL)*scale(scoreDelta.minus1)*win.minus1 + scale(oppRank) + scale(rankEnd.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA3_bpni7)
car::Anova(mappleA3_bpni7, type = 'III')

anova(mappleA3_bpni6, mappleA3_bpni7)

mappleA3_bpni8 <- lmer(appleChoiceDelta ~ scale(trial)*close.minus1 + scale(scoreDelta.minus1) + close.minus1*scale(bpni_TOTAL) + scale(bpni_TOTAL)*win.minus1 + scale(oppRank) + scale(rankEnd.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA3_bpni8)
car::Anova(mappleA3_bpni8, type = 'III')

anova(mappleA3_bpni6, mappleA3_bpni8)

#BPNI subscales
mappleA3_bpniG <- lmer(appleChoiceDelta ~ scale(trial)*close.minus1 + win.minus1 + scale(oppRank) + scale(trial)*scale(scoreDelta.minus1) + scale(rankEnd.minus1) + scale(bpni_GANDIOSITY) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA3_bpniG)
car::Anova(mappleA3_bpniG, type = 'III')

mappleA3_bpniG6 <- lmer(appleChoiceDelta ~ scale(trial)*close.minus1 + close.minus1*scale(bpni_GANDIOSITY)*scale(scoreDelta.minus1) + scale(bpni_GANDIOSITY)*win.minus1 + scale(oppRank) + scale(rankEnd.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA3_bpniG6)
car::Anova(mappleA3_bpniG6, type = 'III')

mappleA3_bpniG7 <- lmer(appleChoiceDelta ~ scale(trial)*close.minus1 + close.minus1*scale(bpni_GANDIOSITY) + scale(scoreDelta.minus1) + scale(bpni_GANDIOSITY)*win.minus1 + scale(oppRank) + scale(rankEnd.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA3_bpniG7)
car::Anova(mappleA3_bpniG7, type = 'III')

anova(mappleA3_bpniG6, mappleA3_bpniG7)

mappleA3_bpniV <- lmer(appleChoiceDelta ~ scale(trial)*close.minus1 + win.minus1 + scale(oppRank) + scale(trial)*scale(scoreDelta.minus1) + scale(rankEnd.minus1) + scale(bpni_VULNERABILITY) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA3_bpniV)
car::Anova(mappleA3_bpniV, type = 'III')

mappleA3_bpniV6 <- lmer(appleChoiceDelta ~ scale(trial)*close.minus1 + close.minus1*scale(bpni_GANDIOSITY)*scale(scoreDelta.minus1) + scale(bpni_VULNERABILITY)*win.minus1 + scale(oppRank) + scale(rankEnd.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA3_bpniV6)
car::Anova(mappleA3_bpniV6, type = 'III')


#FFNI
mappleA3_ffni <- lmer(appleChoiceDelta ~ scale(trial)*close.minus1 + win.minus1 + scale(oppRank) + scale(trial)*scale(scoreDelta.minus1) + scale(rankEnd.minus1) + scale(ffni_total) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA3_ffni)
car::Anova(mappleA3_ffni, type = 'III')

mappleA3_ffni6 <- lmer(appleChoiceDelta ~ scale(trial)*close.minus1 + close.minus1*scale(ffni_total)*scale(scoreDelta.minus1) + scale(ffni_total)*win.minus1 + scale(oppRank) + scale(rankEnd.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA3_ffni6)
car::Anova(mappleA3_ffni6, type = 'III')

mappleA3_ffni1 <- lmer(appleChoiceDelta ~ scale(trial)*close.minus1 + win.minus1 + scale(oppRank)*scale(ffni_total)*scale(trial) + scale(trial)*scale(scoreDelta.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA3_ffni1)
car::Anova(mappleA3_ffni1, type = 'III')

anova(mappleA3_ffni, mappleA3_ffni1)

