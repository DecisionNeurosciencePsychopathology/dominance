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
load("gtn2.Rda")
View(gtn2)


# create .Rda files from excel datasets. Skip this part if starting from the GitHub folder where the files are already created
gtn2 <- read_excel("data3/dataNew4-2b.xlsx")
View(gtn2)

# format missing values
gtn2[gtn2=="NaN"] = NA

# change classes of categorical variables
summary(gtn2)

gtn2 <- transform(gtn2, ID = as.factor(ID), name = as.factor(name), gender = as.factor(gender), win = as.factor(win), close = as.factor(close))
gtn2 <- transform(gtn2, mot6 = as.numeric(mot6))

# add additional useful variables & create lagged variables
gtn2 <- gtn2 %>% group_by(ID) %>% mutate(scoreDiff.minus1 = lag(scoreDiff, n=1, order_by=trial),
                                       scoreDiff.minus2 = lag(scoreDiff, n=2, order_by=trial),
                                     win.minus1 = lag(win, n=1, order_by = trial),
                                     win.minus2 = lag(win, n=2, order_by = trial),
                                     scoreDiff.minus1 = lag(scoreDiff, n=1, order_by = trial),
                                     scoreDiff.minus2 = lag(scoreDiff, n=2, order_by = trial),
                                     appleChoice.minus1 = lag(appleChoice, n=1, order_by=trial),
                                     appleChoice.minus2 = lag(appleChoice, n=2, order_by=trial),
                                     sinkChoice.minus1 = lag(sinkChoice, n=1, order_by=trial),
                                     sinkChoice.minus2 = lag(sinkChoice, n=2, order_by=trial))
                                
gtn2 <- gtn2 %>% group_by(ID) %>% mutate(score.minus1 = lag(score, n=1, order_by=trial),
                                       score.minus2 = lag(score, n=2, order_by=trial))

gtn2$appleChoiceDelta <- gtn2$appleChoice.minus1 - gtn2$appleChoice
gtn2$sinkChoiceDelta <- gtn2$sinkChoice.minus1 - gtn2$sinkChoice
gtn2$scoreDelta <- gtn2$score.minus1 - gtn2$score.minus2

gtn2 <- gtn2 %>% group_by(ID) %>% mutate(appleChoiceDelta.minus1 = lag(appleChoiceDelta, n=1, order_by=trial),
                                       appleChoiceDelta.minus2 = lag(appleChoiceDelta, n=2, order_by=trial),
                                       sinkChoiceDelta.minus1 = lag(sinkChoiceDelta, n=1, order_by=trial),
                                       sinkChoiceDelta.minus2 = lag(sinkChoiceDelta, n=2, order_by=trial),
                                       scoreDelta.minus1 = lag(scoreDelta, n=1, order_by=trial),
                                       scoreDelta.minus2 = lag(scoreDelta, n=2, order_by=trial),
                                       close.minus1 = lag(close, n=1, order_by=trial),
                                       close.minus2 = lag(close, n=2, order_by=trial))


save(gtn2,file="gtn2.Rda")

#Display dependant variables in histograms.
par(mfrow=c(1,1))
par(mfrow=c(2,4))

hist(gtn2$appleChoice,
         main = "Apple buying",
         xlab = "apple choice",
         ylab = "trials")
hist(gtn2$sinkChoice,
         main = "Opponent sinking",
         xlab = "sink choice",
         ylab = "trials")

par(mfrow=c(1,1))
par(mfrow=c(2,4))

## model for opponent choice
sink("appleChoice_model.txt",append=FALSE, split=FALSE)

# best-fitting models for apple choice (mapples3i)
mapple3i <- lmer(appleChoice ~ trial + win.minus1 + rankStart + (1|ID),  data = gtn2, na.action = na.omit)
summary(mapple3i)
car::Anova(mapple3i)

sink()

mapple3p <- lmer(appleChoice ~ win.minus1 + rankStart + oppRank*sinkChoice.minus1*trial + (1|ID),  data = gtn2, na.action = na.omit)
summary(mapple3p)
car::Anova(mapple3p)


mapple3p2 <- lmer(appleChoice ~ win.minus1 + rankStart + oppRank*sinkChoice.minus1 + trial + (1|ID),  data = gtn2, na.action = na.omit)
summary(mapple3p2)
car::Anova(mapple3p2)

anova(mapple3i, mapple3p2)



mapple3q <- lmer(appleChoice ~ win.minus1 + rankStart + appleChoice.minus1*oppRank + (1|ID),  data = gtn2, na.action = na.omit)
summary(mapple3q)
car::Anova(mapple3q)

mapple3n <- lmer(appleChoice ~ appleChoice.minus1*trial + oppRank*sinkChoice.minus1*trial + (1|ID),  data = gtn2, na.action = na.omit)
summary(mapple3n)
car::Anova(mapple3n)

anova(mapple3n, mapple3m)

mapple3m <- lmer(appleChoice ~ appleChoice.minus1*trial + appleChoice.minus1*rankStart + appleChoice.minus1*oppRank + oppRank*sinkChoice.minus1 + (1|ID),  data = gtn2, na.action = na.omit)
summary(mapple3m)
car::Anova(mapple3m)

mapple3o <- lmer(appleChoice ~ win.minus1 + rankStart + appleChoice.minus1*trial + oppRank*sinkChoice.minus1*trial + (1|ID),  data = gtn2, na.action = na.omit)
summary(mapple3o)
car::Anova(mapple3o)


anova(mapple3b,mapple3c)

mapple3j <- lmer(appleChoice ~ appleChoice.minus1 + trial + win.minus1 + close.minus1 + rankStart + oppRank + sinkChoice.minus1 + (1|ID),  data = gtn2, na.action = na.omit)
summary(mapple3j)
car::Anova(mapple3j)

mapple3k <- lmer(appleChoice ~ appleChoice.minus1*trial + appleChoice.minus1*win.minus1 + trial*win.minus1 + appleChoice.minus1*rankStart + trial*rankStart + win.minus1*rankStart + appleChoice.minus1*oppRank + trial*oppRank + win.minus1*oppRank + rankStart*oppRank + appleChoice.minus1*sinkChoice.minus1 + trial*sinkChoice.minus1 + win.minus1*sinkChoice.minus1 + rankStart*sinkChoice.minus1 + oppRank*sinkChoice.minus1 + (1|ID),  data = gtn2, na.action = na.omit)
summary(mapple3k)
car::Anova(mapple3k)

mapple3l <- lmer(appleChoice ~ appleChoice.minus1*trial + appleChoice.minus1*rankStart + trial*rankStart + win.minus1*rankStart + appleChoice.minus1*oppRank + trial*oppRank + oppRank*sinkChoice.minus1 + (1|ID),  data = gtn2, na.action = na.omit)
summary(mapple3l)
car::Anova(mapple3l)

mapple3m <- lmer(appleChoice ~ appleChoice.minus1*trial + appleChoice.minus1*rankStart + appleChoice.minus1*oppRank + oppRank*sinkChoice.minus1 + (1|ID),  data = gtn2, na.action = na.omit)
summary(mapple3m)
car::Anova(mapple3m)

anova(mapple3l, mapple3m)

mapple3n <- lmer(appleChoice ~ appleChoice.minus1*trial + oppRank*sinkChoice.minus1*trial + (1|ID),  data = gtn2, na.action = na.omit)
summary(mapple3n)
car::Anova(mapple3n)

anova(mapple3n, mapple3m)



mapple4 <- lmer(appleChoice ~ rankStart + appleChoice.minus1 + win.minus1 + trial + oppRank + close.minus1 + score.minus1  + (1|ID),  data = gtn2, na.action = na.omit)
summary(mapple4)
car::Anova(mapple4)

## plot opponentRank - sink choice minus1 interaction
pdf("V4-2-oppRank-previoussinkChoice.pdf", width=10, height=5)
ls_mapple3p <- lsmeans(mapple3p,"oppRank", by = "sinkChoice.minus1", cov.reduce = FALSE)
plot(ls_mapple3p, type ~ appleChoice, horiz=F,ylab = "appleChoice", xlab = "oppRank")
dev.off()


## model for sinking choice
msink3b <- lmer(sinkChoice ~ oppRank + win + appleChoice + trial + rankEnd + sinkChoice.minus1 +(1|ID),  data = gtn2, na.action = na.omit)
summary(msink3b)
car::Anova(msink3b)

msink3c <- lmer(sinkChoice ~ oppRank*win + win*trial + trial*appleChoice + oppRank*appleChoice + win*appleChoice + oppRank*trial + oppRank*rankEnd + win*rankEnd + appleChoice*rankEnd + oppRank*sinkChoice.minus1 + win*sinkChoice.minus1 + appleChoice*sinkChoice.minus1 + rankEnd*sinkChoice.minus1 + (1|ID),  data = gtn2, na.action = na.omit)
summary(msink3c)
car::Anova(msink3c)

msink3d <- lmer(sinkChoice ~ oppRank*win + win*trial + trial*appleChoice + oppRank*sinkChoice.minus1 + win*sinkChoice.minus1 + (1|ID),  data = gtn2, na.action = na.omit)
summary(msink3d)
car::Anova(msink3d)

msink3d2 <- lmer(sinkChoice ~ oppRank*win + win*trial + appleChoice + oppRank*sinkChoice.minus1 + win*sinkChoice.minus1 + (1|ID),  data = gtn2, na.action = na.omit)
summary(msink3d2)
car::Anova(msink3d2)

anova(msink3c, msink3d)

msink3e <- lmer(sinkChoice ~ oppRank*win + win*trial + oppRank*sinkChoice.minus1 + (1|ID),  data = gtn2, na.action = na.omit)
summary(msink3e)
car::Anova(msink3e)

anova(msink3e, msink3d)

msink3f <- lmer(sinkChoice ~ oppRank*win*trial + oppRank*sinkChoice.minus1 + (1|ID),  data = gtn2, na.action = na.omit)
summary(msink3f)
car::Anova(msink3f)

msink3h <- lmer(sinkChoice ~ oppRank*win + win*trial + oppRank*sinkChoice.minus1 + sinkChoice.minus2 + (1|ID),  data = gtn2, na.action = na.omit)
summary(msink3e)
car::Anova(msink3e)

msink3g2 <- lmer(sinkChoice ~ appleChoice*sinkChoice.minus1 + oppRank*win*sinkChoice.minus1 + win*trial + (1|ID),  data = gtn2, na.action = na.omit)
summary(msink3g2)
car::Anova(msink3g2)

sink("sinkChoice_model.txt",append=FALSE, split=FALSE)
#best-fitting models

msink3e2 <- lmer(sinkChoice ~ oppRank*win + win*trial + appleChoice*sinkChoice.minus1 + oppRank*sinkChoice.minus1 + (1|ID),  data = gtn2, na.action = na.omit)
summary(msink3e2)
car::Anova(msink3e2)

msink3g3 <- lmer(sinkChoice ~ appleChoice*sinkChoice.minus1 + oppRank*win + win*sinkChoice.minus1 + sinkChoice.minus1*oppRank + win*trial + (1|ID),  data = gtn2, na.action = na.omit)
summary(msink3g3)
car::Anova(msink3g3)

anova(msink3e2, msink3g3)

sink()

# plot oppRank by win interaction
pdf("V4-2-win-oppRank.pdf", width=10, height=5)
ls_msink3e2 <- lsmeans(msink3e2,"oppRank", by = "win", cov.reduce = FALSE)
plot(ls_msink3e2, type ~ sinkChoice, horiz=F,ylab = "sinkChoice", xlab = "oppRank")
dev.off()

# plot trial by win interaction
pdf("V4-2-win-trial.pdf", width=10, height=5)
ls_msink3e1 <- lsmeans(msink3e2,"trial", by = "win", cov.reduce = FALSE)
plot(ls_msink3e1, type ~ sinkChoice, horiz=F,ylab = "sinkChoice", xlab = "trial")
dev.off()

# plot oppRank by sinkChoice.minus1 interaction
pdf("V4-2-oppRank-previoussinkChoice.pdf", width=10, height=5)
ls_msink3e3 <- lsmeans(msink3e2,"oppRank", by = "sinkChoice.minus1", cov.reduce = FALSE)
plot(ls_msink3e3, type ~ sinkChoice, horiz=F,ylab = "sinkChoice", xlab = "oppRank")
dev.off()

# plot appleChoice by sinkChoice.minus1 interaction
pdf("V4-2-appleChoice-previoussinkChoice.pdf", width=10, height=5)
ls_msink3e4 <- lsmeans(msink3e2,"appleChoice", by = "sinkChoice.minus1", cov.reduce = FALSE)
plot(ls_msink3e4, type ~ sinkChoice, horiz=F,ylab = "sinkChoice", xlab = "appleChoice")
dev.off()

# plot win by rankStart interaction
#pdf("rankStartF.pdf", width=10, height=5)
#plot(effect("win:rankStart",mrank6), grid=TRUE)
#dev.off()

