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
load("gtn.Rda")
View(gtn)

# create .Rda files from excel datasets. Skip this part if starting from the GitHub folder where the files are already created
gtn <- read_excel("data3/dataNew.xlsx")
View(gtn)

# format missing values
gtn[gtn=="NA"] = NA

# change classes of categorical variables
summary(gtn)

gtn <- transform(gtn, ID = as.factor(ID), name = as.factor(name), gender = as.factor(gender), win = as.factor(win), close = as.factor(close))
gtn <- transform(gtn, mot6 = as.numeric(mot6), mot8 = as.numeric(mot8))

# add additional useful variables & create lagged variables
gtn <- gtn %>% group_by(ID) %>% mutate(scoreDiff.minus1 = lag(scoreDiff, n=1, order_by=trial),
                                       scoreDiff.minus2 = lag(scoreDiff, n=2, order_by=trial),
                                     rankDelta.minus1 = lag(rankDelta, n=1, order_by = trial),
                                     rankDelta.minus2 = lag(rankDelta, n=2, order_by = trial),
                                     win.minus1 = lag(win, n=1, order_by = trial),
                                     win.minus2 = lag(win, n=2, order_by = trial),
                                     scoreDiff.minus1 = lag(scoreDiff, n=1, order_by = trial),
                                     scoreDiff.minus2 = lag(scoreDiff, n=2, order_by = trial),
                                     oppChoice.minus1 = lag(oppChoice, n=1, order_by=trial),
                                     oppChoice.minus2 = lag(oppChoice, n=2, order_by=trial),
                                     rankChoice.minus1 = lag(rankChoice, n=1, order_by=trial),
                                     rankChoice.minus2 = lag(rankChoice, n=2, order_by=trial))
                                
gtn <- gtn %>% group_by(ID) %>% mutate(score.minus1 = lag(score, n=1, order_by=trial),
                                       score.minus2 = lag(score, n=2, order_by=trial))

gtn$oppChoiceDelta <- gtn$oppChoice.minus1 - gtn$oppChoice
gtn$rankChoiceDelta <- gtn$rankChoice.minus1 - gtn$rankChoice
gtn$scoreDelta <- gtn$score.minus1 - gtn$score.minus2

gtn <- gtn %>% group_by(ID) %>% mutate(oppChoiceDelta.minus1 = lag(oppChoiceDelta, n=1, order_by=trial),
                                       oppChoiceDelta.minus2 = lag(oppChoiceDelta, n=2, order_by=trial),
                                       rankChoiceDelta.minus1 = lag(rankChoiceDelta, n=1, order_by=trial),
                                       rankChoiceDelta.minus2 = lag(rankChoiceDelta, n=2, order_by=trial),
                                       scoreDelta.minus1 = lag(scoreDelta, n=1, order_by=trial),
                                       scoreDelta.minus2 = lag(scoreDelta, n=2, order_by=trial),
                                       close.minus1 = lag(close, n=1, order_by=trial),
                                       close.minus2 = lag(close, n=2, order_by=trial))


save(gtn,file="gtn.Rda")

#Display dependant variables in histograms.
par(mfrow=c(1,1))
par(mfrow=c(2,4))

hist(gtn$oppChoice,
         main = "Opponent choice",
         xlab = "trials",
         ylab = "opp choice")
hist(gtn$rankChoice,
         main = "Booster choice",
         xlab = "trials",
         ylab = "rank choice")
barchart(gtn$oppChoice[gtn$gender == 1],
         main = "Opponent choice - gender M",
         xlab = "trials",
         ylab = "opp choice")
barchart(gtn$oppChoice[gtn$gender == 2],
         main = "Opponent choice - gender F",
         xlab = "trials",
         ylab = "opp choice")
barchart(gtn$rankChoice[gtn$gender == 1],
         main = "Rank choice - gender M",
         xlab = "trials",
         ylab = "rank choice")
barchart(gtn$rankChoice[gtn$gender == 2],
         main = "Rank choice - gender F",
         xlab = "trials",
         ylab = "rank choice")
barchart(gtn$oppChoice[gtn$gameExp <= 4],
         main = "Opponent choice - game-exp 1 to 4",
         xlab = "trials",
         ylab = "opp choice")
barchart(gtn$oppChoice[gtn$gameExp <= 5],
         main = "Opponent choice - game-exp 5",
         xlab = "trials",
         ylab = "opp choice")
barchart(gtn$oppChoice[gtn$satisfied < 5],
         main = "low satisfaction with own performance < 5",
         xlab = "trials",
         ylab = "opp choice")
barchart(gtn$oppChoice[gtn$satisfied >= 5],
         main = "high satisfaction with own performance >= 5",
         xlab = "trials",
         ylab = "opp choice")
barchart(gtn$rankChoice[gtn$satisfied < 5],
         main = "low satisfaction with own performance < 5",
         xlab = "trials",
         ylab = "rank choice")
barchart(gtn$rankChoice[gtn$satisfied >= 5],
         main = "high satisfaction with own performance >= 5",
         xlab = "trials",
         ylab = "rank choice")

par(mfrow=c(1,1))
par(mfrow=c(2,4))

## model for opponent choice
mopp1 <- lmer(oppChoice ~ oppChoice.minus1 + win.minus1 + close.minus1 + trial + score.minus1 + rankChoice.minus1 + (1|ID),  data = gtn, na.action = na.omit)
summary(mopp1)
car::Anova(mopp1)

mopp2 <- lmer(oppChoice ~ oppChoice.minus1 + win.minus1*close.minus1 + trial + score.minus1 + rankChoice.minus1 + (1|ID),  data = gtn, na.action = na.omit)
summary(mopp2)
car::Anova(mopp2)

mopp3 <- lmer(oppChoice ~ oppChoice.minus1 + win.minus1 + satisfied + trial + enjoyed + rankChoice.minus1 + (1|ID),  data = gtn, na.action = na.omit)
summary(mopp3)
car::Anova(mopp3)

anova(mopp3,mopp4)

sink("oppChoice_model.txt",append=FALSE, split=FALSE)
mopp4 <- lmer(oppChoice ~ oppChoice.minus1 + win.minus1 + trial + rankChoice.minus1 + (1|ID),  data = gtn, na.action = na.omit)
summary(mopp4)
car::Anova(mopp4)
sink()

mopp4b <- lmer(oppChoice ~ oppChoice.minus1 + win.minus1 + trial + rankStart + rankChoice.minus1 + (1|ID),  data = gtn, na.action = na.omit)
summary(mopp4b)
car::Anova(mopp4b)

mopp5 <- lmer(oppChoice ~ oppChoice.minus1 + win.minus1 + trial*rankChoice.minus1 + (1|ID),  data = gtn, na.action = na.omit)
summary(mopp5)
car::Anova(mopp5)

mopp6 <- lmer(oppChoice ~ oppChoice.minus1 + win.minus1 + rankChoice.minus1 + rankStart + trial + (1|ID),  data = gtn, na.action = na.omit)
summary(mopp6)
car::Anova(mopp6)

anova(mopp4,mopp5)

moppD1 <- lmer(oppChoiceDelta ~ oppChoice.minus1 + win.minus1 + close.minus1 + trial + score.minus1 + rankChoice.minus1 + (1|ID),  data = gtn, na.action = na.omit)
summary(moppD1)
car::Anova(moppD1)

## model for rank choice
mrank1 <- lmer(rankChoice ~ oppChoice + win + close + trial + rankChoice.minus1 + (1|ID),  data = gtn, na.action = na.omit)
summary(mrank1)
car::Anova(mrank1)

mrank2 <- lmer(rankChoice ~ oppChoice + win + close + trial*rankChoice.minus1 + (1|ID),  data = gtn, na.action = na.omit)
summary(mrank2)
car::Anova(mrank2)

mrank3b <- lmer(rankChoice ~ oppChoice + win*close + trial + rankChoice.minus1 + (1|ID),  data = gtn, na.action = na.omit)
summary(mrank3b)
car::Anova(mrank3b)

mrank3 <- lmer(rankChoice ~ win*close + trial + rankChoice.minus1 + (1|ID),  data = gtn, na.action = na.omit)
summary(mrank3)
car::Anova(mrank3)

mrank4 <- lmer(rankChoice ~ win*close + trial + rankChoice.minus1 + rankStart + (1|ID),  data = gtn, na.action = na.omit)
summary(mrank4)
car::Anova(mrank4)

anova(mrank3, mrank4)

mrank5 <- lmer(rankChoice ~ win*close + rankChoice.minus1 + rankStart*trial + (1|ID),  data = gtn, na.action = na.omit)
summary(mrank5)
car::Anova(mrank5)

anova(mrank4, mrank5)

# best-fitting models: mrank6 & mrank6b, depending on whether we look to the AIC or BIC.


sink("Version4-1-rankChoice_model.txt",append=FALSE, split=FALSE)
mrank6 <- lmer(rankChoice ~ trial + rankChoice.minus1 + close*win + rankStart*win + (1|ID),  data = gtn, na.action = na.omit)
summary(mrank6)
car::Anova(mrank6)

sink()

mrank6b <- lmer(rankChoice ~ trial + rankChoice.minus1 + rankStart*win + (1|ID),  data = gtn, na.action = na.omit)
summary(mrank6b)
car::Anova(mrank6b)

anova(mrank6, mrank6b)

sink()

mrank9f <- lmer(rankChoice ~ trial + rankChoice.minus1*win + rankStart*win + (1|ID),  data = gtn, na.action = na.omit)
summary(mrank9f)
car::Anova(mrank9f)

mrank9e <- lmer(rankChoice ~ trial + rankStart + rankChoice.minus1*win + (1|ID),  data = gtn, na.action = na.omit)
summary(mrank9e)
car::Anova(mrank9e)

mrank7 <- lmer(rankChoice ~ trial + rankChoice.minus1 + close*rankStart*win + (1|ID),  data = gtn, na.action = na.omit)
summary(mrank7)
car::Anova(mrank7)

anova(mrank6, mrank7)

mrank8 <- lmer(rankChoice ~ trial + rankChoice.minus1*rankStart + close*win + rankStart*win + (1|ID),  data = gtn, na.action = na.omit)
summary(mrank8)
car::Anova(mrank8)

mrank9 <- lmer(rankChoice ~ trial + win*rankStart + oppRank + close + oppChoice + score + rankChoice.minus1 + (1|ID),  data = gtn, na.action = na.omit)
summary(mrank9)
car::Anova(mrank9)

mrank9b <- lmer(rankChoice ~ trial + win*rankStart + rankChoice.minus1 + (1|ID),  data = gtn, na.action = na.omit)
summary(mrank9b)
car::Anova(mrank9b)

mrank9c <- lmer(rankChoice ~ trial*rankStart + trial*win + trial*rankChoice.minus1 + win*rankStart + rankChoice.minus1*rankStart + rankChoice.minus1*win + (1|ID),  data = gtn, na.action = na.omit)
summary(mrank9c)
car::Anova(mrank9c)

mrank9d <- lmer(rankChoice ~ trial + rankChoice.minus1*rankStart + rankChoice.minus1*win + (1|ID),  data = gtn, na.action = na.omit)
summary(mrank9d)
car::Anova(mrank9d)

mrank9e <- lmer(rankChoice ~ trial + rankStart + rankChoice.minus1*win + (1|ID),  data = gtn, na.action = na.omit)
summary(mrank9e)
car::Anova(mrank9e)

anova(mrank9e, mrank9d)
anova(mrank9e, mrank6)


mrank9f <- lmer(rankChoice ~ trial + rankChoice.minus1*win + close*win + rankStart*win + (1|ID),  data = gtn, na.action = na.omit)
summary(mrank9f)
car::Anova(mrank9f)

mrank9i <- lmer(rankChoice ~ trial + rankChoice.minus1*win + win*close*rankStart + (1|ID),  data = gtn, na.action = na.omit)
summary(mrank9i)
car::Anova(mrank9i)


# plot win by close interaction
pdf("closeF.pdf", width=10, height=5)
ls_mrank6 <- lsmeans(mrank6,"win", by = "close")
plot(ls_mrank6, type ~ rankChoice, horiz=F,ylab = "rankChoice", xlab = "win")
dev.off()

# plot win by rankStart interaction
pdf("rankStartF.pdf", width=10, height=5)
plot(effect("win:rankStart",mrank6), grid=TRUE)
dev.off()

pdf("rankStartF2.pdf", width=10, height=5)
ls_mrank61 <- lsmeans(mrank6,"rankStart", by = "win", cov.reduce = FALSE)
plot(ls_mrank61, type ~ rankChoice, horiz=F,ylab = "rankChoice", xlab = "rankStart")
dev.off()