library(readxl)
library(readr)
library(lme4) # this is the one to use for modeling
library(ggplot2)
library(dplyr)
library(tidyr)
library(psych)
library(gdata)
source(file.path(getMainDir(), "Miscellaneous", "Global_Functions.R"))
library(R.matlab)
library(xtable)
library(Hmisc)
library(foreign)
library(MASS)
library("lsmeans")
library(effects)
library(arm)

setwd("C:/Users/a/Documents/GitHub/dominance")
# just for Alex
setwd("~/code//dominance")

# clears environment
rm(list=ls())

## starting from GitHub, you can load the following .Rda file (and jump the data preparation part to go directly to the plots and models)
load("sds1.Rda")
View(sds1)


# create .Rda files from excel datasets. Skip this part if starting from the GitHub folder where the files are already created
sds1 <- read_csv("Matlab/snake_data.xls")
View(sds1)

# format missing values
sds1[sds1=="NaN"] = NA

# change classes of categorical variables
summary(sds1)

sds1 <- transform(sds1, ID = as.factor(ID), n = as.factor(n), name = as.factor(name), gender = as.factor(gender), win = as.factor(win), close = as.factor(close), avatarChoice = as.factor(avatarChoice), consentChoice = as.factor(consentChoice))


# add additional useful variables & create lagged variables
sds1 <- sds1 %>% group_by(ID) %>% mutate(score.minus1 = lag(score, n=1, order_by=trial),
                                         score.minus2 = lag(score, n=2, order_by=trial),
                                         win.minus1 = lag(win, n=1, order_by = trial),
                                     win.minus2 = lag(win, n=2, order_by = trial),
                                     scoreDiff.minus1 = lag(scoreDiff, n=1, order_by = trial),
                                     scoreDiff.minus2 = lag(scoreDiff, n=2, order_by = trial),
                                     appleChoice.minus1 = lag(appleChoice, n=1, order_by=trial),
                                     appleChoice.minus2 = lag(appleChoice, n=2, order_by=trial),
                                     rankChoice.minus1 = lag(rankChoice, n=1, order_by=trial),
                                     rankChoice.minus2 = lag(rankChoice, n=2, order_by=trial),
                                     close.minus1 = lag(close, n=1, order_by=trial),
                                     close.minus2 = lag(close, n=2, order_by=trial))
                                
sds1$appleChoiceDelta <- sds1$appleChoice.minus1 - sds1$appleChoice
sds1$rankChoiceDelta <- sds1$rankChoice.minus1 - sds1$rankChoice
sds1$scoreDelta <- sds1$score.minus1 - sds1$score.minus2

sds1 <- sds1 %>% group_by(ID) %>% mutate(appleChoiceDelta.minus1 = lag(appleChoiceDelta, n=1, order_by=trial),
                                       appleChoiceDelta.minus2 = lag(appleChoiceDelta, n=2, order_by=trial),
                                       rankChoiceDelta.minus1 = lag(rankChoiceDelta, n=1, order_by=trial),
                                       rankChoiceDelta.minus2 = lag(rankChoiceDelta, n=2, order_by=trial),
                                       scoreDelta.minus1 = lag(scoreDelta, n=1, order_by=trial),
                                       scoreDelta.minus2 = lag(scoreDelta, n=2, order_by=trial))


save(sds1,file="sds1.Rda")

#Display dependant variables in histograms.
par(mfrow=c(1,1))
par(mfrow=c(2,4))


hist(sds1$age)
hist(sds1$gameExp)
hist(sds1$appleChoice,
         main = "Apple stealing",
         xlab = "apple choice",
         ylab = "trials")
hist(sds1$rankChoice,
         main = "Booster buying",
         xlab = "booster choice",
         ylab = "trials")

par(mfrow=c(1,1))
par(mfrow=c(2,4))

## apple choice
sink("appleChoice_model.txt",append=FALSE, split=FALSE)
sink()

mapple1 <- lmer(appleChoice ~ trial + win.minus1 + close.minus1 + oppRank + rankStart + gender + age + gameExp + rankEstim1 + scoreDelta.minus1 + rankChoice.minus1 + appleChoice.minus1 + (1|ID),  data = sds1, na.action = na.omit)
summary(mapple1)
print(mapple1, correlation = TRUE)
car::Anova(mapple1)

mapple2 <- lmer(appleChoice ~ win.minus1 + oppRank + gender + rankStart + gameExp + rankEstim1 + scoreDelta.minus1 + rankChoice.minus1 + appleChoice.minus1 + (1|ID),  data = sds1, na.action = na.omit)
summary(mapple2)
car::Anova(mapple2)

mapple3 <- lmer(appleChoice ~ oppRank + gameExp + rankEstim1 + rankChoice.minus1 + appleChoice.minus1*gender + (1|ID),  data = sds1, na.action = na.omit)
summary(mapple3)
car::Anova(mapple3)

mapple4 <- lmer(appleChoice ~ oppRank + gameExp + rankEstim1 + rankChoice.minus1 + appleChoice.minus1 + (1|ID),  data = sds1, na.action = na.omit)
summary(mapple4)
car::Anova(mapple4)


## plot opponentRank - sink choice minus1 interaction
#pdf("V4-2-oppRank-previousrankChoice.pdf", width=10, height=5)
#ls_mapple3p <- lsmeans(mapple3p,"oppRank", by = "rankChoice.minus1", cov.reduce = FALSE)
#plot(ls_mapple3p, type ~ appleChoice, horiz=F,ylab = "appleChoice", xlab = "oppRank")
#dev.off()


## booster choice

mboost1 <- lmer(rankChoice ~ trial + win + win.minus1 + close.minus1 + oppRank + rankStart + gender + age + gameExp + rankEstim1 + scoreDelta.minus1 + rankChoice.minus1 + appleChoice + (1|ID),  data = sds1, na.action = na.omit)
summary(mboost1)
car::Anova(mboost1)

mboost2 <- lmer(rankChoice ~ win + close.minus1 + rankStart + gender + gameExp + rankChoice.minus1*win + appleChoice + (1|ID),  data = sds1, na.action = na.omit)
summary(mboost2)
car::Anova(mboost2)

mboost3 <- lmer(rankChoice ~ rankStart + gender + gameExp + rankChoice.minus1 + appleChoice*win + (1|ID),  data = sds1, na.action = na.omit)
summary(mboost3)
car::Anova(mboost3)

mboost4 <- lmer(rankChoice ~ win + rankStart + gameExp + rankChoice.minus1*gender + win + appleChoice + win + (1|ID),  data = sds1, na.action = na.omit)
summary(mboost4)
car::Anova(mboost4)

mboost5 <- lmer(rankChoice ~ win + rankChoice.minus1*gender + win + appleChoice + win + (1|ID),  data = sds1, na.action = na.omit)
summary(mboost5)
car::Anova(mboost5)

# plot oppRank by win interaction
#pdf("V4-2-win-oppRank.pdf", width=10, height=5)
#ls_msink3e2 <- lsmeans(msink3e2,"oppRank", by = "win", cov.reduce = FALSE)
#plot(ls_msink3e2, type ~ rankChoice, horiz=F,ylab = "rankChoice", xlab = "oppRank")
#dev.off()
