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

setwd("C:/Users/a/Documents/GitHub/dominance")

## create .Rda files from excel datasets. Skip this part if starting from the GitHub folder where the files are already created

gt1 <- read_excel("data3/gt1.xlsx")
View(gt1)
gt2 <- read_excel("data3/gt2.xlsx")
View(gt2)

# in case there are missing values in the dataset (not the case here)
# gt1[gt1=="NaN"] = NA
# gt2[gt2=="NaN"] = NA

# change classes of categorical variables
summary(gt1)
summary(gt2)

gt1 <- transform(gt1, ID = as.factor(ID), name = as.factor(name), country = as.factor(country), gender = as.factor(gender))
gt2 <- transform(gt2, ID = as.factor(ID), name = as.factor(name), country = as.factor(country), gender = as.factor(gender))
#gt1 <- transform(gt1, age = as.numeric(age))

# add additional useful variables & create lagged variables
gt1$scoreDiff <- gt1$score - gt1$oppScore;
gt2$scoreDiff <- gt2$score - gt2$oppScore;

gt1 <- gt1 %>% group_by(ID) %>% mutate(scoreDiff.minus1 = lag(scoreDiff, n=1, order_by=trial),
                                       scoreDiff.minus2 = lag(scoreDiff, n=2, order_by=trial),
                                       score.minus1 = lag(score, n=1, order_by=trial),
                                       score.minus2 = lag(score, n=2, order_by=trial),
                                     rankDelta.minus1 = lag(rankDelta, n=1, order_by = trial),
                                     rankDelta.minus2 = lag(rankDelta, n=2, order_by = trial),
                                     win.minus1 = lag(win, n=1, order_by = trial),
                                     win.minus2 = lag(win, n=2, order_by = trial),
                                     money.minus1 = lag(money, n=1, order_by = trial),
                                     money.minus2 = lag(money, n=2, order_by = trial),
                                     choice.minus1 = lag(choice, n=1, order_by=trial),
                                     choice.minus2 = lag(choice, n=2, order_by=trial),
                                     ugChoice.minus1 = lag(ugChoice, n=1, order_by=trial),
                                     ugChoice.minus2 = lag(ugChoice, n=2, order_by=trial))

gt2 <- gt2 %>% group_by(ID) %>% mutate(scoreDiff.minus1 = lag(scoreDiff, n=1, order_by=trial),
                                       scoreDiff.minus2 = lag(scoreDiff, n=2, order_by=trial),
                                       score.minus1 = lag(score, n=1, order_by=trial),
                                       score.minus2 = lag(score, n=2, order_by=trial),
                                       rankDelta.minus1 = lag(rankDelta, n=1, order_by = trial),
                                       rankDelta.minus2 = lag(rankDelta, n=2, order_by = trial),
                                       win.minus1 = lag(win, n=1, order_by = trial),
                                       win.minus2 = lag(win, n=2, order_by = trial),
                                       money.minus1 = lag(money, n=1, order_by = trial),
                                       money.minus2 = lag(money, n=2, order_by = trial),
                                       choice.minus1 = lag(choice, n=1, order_by=trial),
                                       choice.minus2 = lag(choice, n=2, order_by=trial),
                                       ugChoice.minus1 = lag(ugChoice, n=1, order_by=trial),
                                       ugChoice.minus2 = lag(ugChoice, n=2, order_by=trial))


gt1$choiceDelta <- gt1$choice.minus1 - gt1$choice
gt1$ugDelta <- gt1$ugChoice.minus1 - gt1$ugChoice

gt1 <- gt1 %>% group_by(ID) %>% mutate(choiceDelta.minus1 = lag(choiceDelta, n=1, order_by=trial),
                                       choiceDelta.minus2 = lag(choiceDelta, n=2, order_by=trial),
                                       ugDelta.minus1 = lag(ugDelta, n=1, order_by=trial),
                                       ugDelta.minus2 = lag(ugDelta, n=2, order_by=trial))


gt2$choiceDelta <- gt2$choice.minus1 - gt2$choice
gt2$ugDelta <- gt2$ugChoice.minus1 - gt2$ugChoice

gt2 <- gt2 %>% group_by(ID) %>% mutate(choiceDelta.minus1 = lag(choiceDelta, n=1, order_by=trial),
                                       choiceDelta.minus2 = lag(choiceDelta, n=2, order_by=trial),
                                       ugDelta.minus1 = lag(ugDelta, n=1, order_by=trial),
                                       ugDelta.minus2 = lag(ugDelta, n=2, order_by=trial))


save(gt1,file="gt1.Rda")
save(gt2,file="gt2.Rda")


## if starting from GitHub, you can simply load the .Rda files (no need for previous steps)
load("gt1.Rda")
View(gt1)
load("gt2.Rda")
View(gt2)


#Display dependant variables in histograms.
hist(gt1$choice)
hist(gt2$choice)
hist(gt1$ugChoice)
hist(gt2$ugChoice)
hist(gt1$choiceDelta)
hist(gt2$choiceDelta)
hist(gt1$ugDelta)
hist(gt2$ugDelta)

##gt1

m1 <- lmer(choiceDelta ~ choice.minus1 + (1|ID),  data = gt1,na.action = na.omit)
summary(m1)
car::Anova(m1)

m1bis <- lmer(choiceDelta ~ choice.minus1 + choice.minus2 + (1|ID),  data = gt1,na.action = na.omit)
summary(m1bis)
car::Anova(m1bis)

m2 <- lmer(choiceDelta ~ choice.minus1 + win.minus1 + trial + (1|ID),  data = gt1,na.action = na.omit)
summary(m2)
car::Anova(m2)

anova(m1,m2)

m3 <- lmer(choiceDelta ~ choice.minus1 + money.minus1 + trial + (1|ID),  data = gt1,na.action = na.omit)
summary(m3)
car::Anova(m3)

anova(m3,m2)

m4 <- lmer(choiceDelta ~ choice.minus1 + win.minus1 + rankDelta.minus1 + trial + (1|ID),  data = gt1,na.action = na.omit)
summary(m4)
car::Anova(m4)

m5 <- lmer(choiceDelta ~ choice.minus1 + win.minus1 + + ugDelta.minus1 + trial + (1|ID), data = gt1,na.action = na.omit)
summary(m5)
car::Anova(m5)

anova(m5,m1)

um1 <- lmer(ugDelta ~ win + (1|ID), data = gt1,na.action = na.omit)
summary(um1)
car::Anova(um1)

um2 <- lmer(ugDelta ~ win + choiceDelta + trial + (1|ID), data = gt1,na.action = na.omit)
summary(um2)
car::Anova(um2)

um3 <- lmer(ugDelta ~ money + choiceDelta + trial + (1|ID), data = gt1,na.action = na.omit)
summary(um3)
car::Anova(um3)

um4 <- lmer(ugDelta ~ win + choiceDelta + ugDelta.minus1 + trial + (1|ID), data = gt1,na.action = na.omit)
summary(um4)
car::Anova(um4)

um5 <- lmer(ugDelta ~ win + choiceDelta + ugDelta.minus1 + + ugDelta.minus2 + trial + (1|ID), data = gt1,na.action = na.omit)
summary(um5)
car::Anova(um5)


##gt2

m1.gt2 <- lmer(choiceDelta ~ choice.minus1 + (1|ID),  data = gt2,na.action = na.omit)
summary(m1.gt2)
car::Anova(m1.gt2)

m1bis.gt2 <- lmer(choiceDelta ~ choice.minus1 + choice.minus2 + (1|ID),  data = gt2,na.action = na.omit)
summary(m1bis.gt2)
car::Anova(m1bis.gt2)

m2.gt2 <- lmer(choiceDelta ~ choice.minus1 + win.minus1 + trial + (1|ID),  data = gt2,na.action = na.omit)
summary(m2.gt2)
car::Anova(m2.gt2)

anova(m1,m2)

m3.gt2 <- lmer(choiceDelta ~ choice.minus1 + money.minus1 + trial + (1|ID),  data = gt2,na.action = na.omit)
summary(m3.gt2)
car::Anova(m3.gt2)

m4.gt2 <- lmer(choiceDelta ~ choice.minus1 + win.minus1 + rankDelta.minus1 + (1|ID),  data = gt2,na.action = na.omit)
summary(m4.gt2)
car::Anova(m4.gt2)

m5.gt2 <- lmer(choiceDelta ~ choice.minus1 + win.minus1 + + ugDelta.minus1 + trial + (1|ID), data = gt2,na.action = na.omit)
summary(m5.gt2)
car::Anova(m5.gt2)

anova(m5.gt2,m2.gt2)

um1.gt2 <- lmer(ugDelta ~ win + (1|ID), data = gt2,na.action = na.omit)
summary(um1.gt2)
car::Anova(um1.gt2)

um2.gt2 <- lmer(ugDelta ~ win + choiceDelta + trial + (1|ID), data = gt2,na.action = na.omit)
summary(um2.gt2)
car::Anova(um2.gt2)

um3.gt2 <- lmer(ugDelta ~ money + choiceDelta + trial + (1|ID), data = gt2,na.action = na.omit)
summary(um3.gt2)
car::Anova(um3.gt2)

um4.gt2 <- lmer(ugDelta ~ win + choiceDelta + ugDelta.minus1 + trial + (1|ID), data = gt2,na.action = na.omit)
summary(um4.gt2)
car::Anova(um4.gt2)

um5.gt2 <- lmer(ugDelta ~ win + choiceDelta + ugDelta.minus1 + + ugDelta.minus2 + trial + (1|ID), data = gt2,na.action = na.omit)
summary(um5.gt2)
car::Anova(um5.gt2)

anova(um4.gt2, um5.gt2)

## create smaller datasets with only first-experience games.
sgt1 <- gt1[which(gt2$invOrder==0),];
View(sgt1)
sgt2 <- gt2[which(gt2$invOrder==1),];
View(sgt2)

## see if difference in best fitting models in smaller datasets
m5.sgt1 <- lmer(choiceDelta ~ choice.minus1 + win.minus1 + + ugDelta.minus1 + trial + (1|ID), data = sgt1,na.action = na.omit)
summary(m5.sgt1)
car::Anova(m5.sgt1)

m5.sgt2 <- lmer(choiceDelta ~ choice.minus1 + win.minus1 + + ugDelta.minus1 + trial + (1|ID), data = sgt2,na.action = na.omit)
summary(m5.sgt2)
car::Anova(m5.sgt2)

um5.sgt1 <- lmer(ugDelta ~ win + choiceDelta + ugDelta.minus1 + + ugDelta.minus2 + trial + (1|ID), data = sgt1,na.action = na.omit)
summary(um5.sgt1)
car::Anova(um5.sgt1)

um5.sgt2 <- lmer(ugDelta ~ win + choiceDelta + ugDelta.minus1 + + ugDelta.minus2 + trial + (1|ID), data = sgt2,na.action = na.omit)
summary(um5.sgt2)
car::Anova(um5.sgt2)
