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
# rankStart: player's rank at the beginnig of the round/trial (warning: rank 1 = highest!)
# rankEnd: player's rank at the end of the round/trial (after boosters) (warning: rank 1 = highest!)
# mot1-8: motivation questions, assessed on a Lickert scale going from 1 = Strongly disagree to 5 = Strongly agree; 'I wanted to perform as well as I possibly could on the task.', 'Maximizing my personal record of apples eaten was important to me.', 'I wanted to perform better than everyone else on the task.', 'I did not want to perform more poorly than everyone else on the task.', 'Attaining the highest rank among all the competitors was important to me.', 'I wanted to take revenge on people who defeated me.', 'I wanted to avoid performing less than my best on the task.', 'I wanted to ensure that I win.'
# enjoyed: how much participant enjoyed playing on a scale from 1 to 10, 1 = not at all, 10 = extremely
# satisfied: how much participant is satisified with own performance  on a scale from 1 to 10, 1 = not at all, 10 = extremely
# fair: how much participant judged the opponents' behavior as fair
# credible: manipulation check, i.e. how much participant believed that he could control the outcome of the game on a scale from 1 to 10, 1 = not at all, 10 = extremely
########################################


## set working directory (where you have the participants' output data files )
setwd("/home/anna/Dropbox/USA/Pittsburgh/GitHub/dominance/snake_data")

# wd for Alex
#setwd("~/code//dominance")

# clear environment
rm(list=ls())


## load datafile and skip to line 126 (use only if data has already been prepared previously, otherwise run lines 64 to 126) 
load("snake_ds.Rda")
View(snake_ds)


## create .Rda file by combining the participants' output files. Skip this part until line 101 if the dataset has already been created)
#the following command reads a table with the participants'ids, which are also their output files' names (the files are named [id].csv); this file should be in the working directory
participants <- read_excel("snake_game_participants2.xlsx")

participants <- transform(participants, id = as.factor(id))
participants_full <-matrix(nrow = nrow(participants), ncol = 1)
prov <- matrix

for (i in 1:nrow(participants))
{
  participants_full[i,1] <- paste(participants[i,1], ".csv", sep="")
}

snake_ds <- read.csv(participants_full[1,1], header = TRUE, sep = ';')
snake_ds[1:nrow(snake_ds)-1, (ncol(snake_ds)-22):ncol(snake_ds)] = snake_ds[nrow(snake_ds), (ncol(snake_ds)-22):ncol(snake_ds)]
snake_ds$n <- 1


for (i in 2:nrow(participants))
{
  prov <- read.csv(participants_full[i,1], header = TRUE, sep = ';')
  prov[1:nrow(prov)-1, (ncol(prov)-22):ncol(prov)] = prov[nrow(prov), (ncol(prov)-22):ncol(prov)]
  prov$n = i
  snake_ds <- bind_rows(snake_ds, prov, .id = NULL)
}

View(snake_ds)

save(snake_ds, file="snake_ds.Rda")

# format missing values
snake_ds[snake_ds=="NaN"] = NA

# change classes of categorical variables
summary(snake_ds)

snake_ds <- transform(snake_ds, ID = as.factor(ID), n = as.factor(n), name = as.factor(name), gender = as.factor(gender), win = as.factor(win), close = as.factor(close), avatarChoice = as.factor(avatarChoice), consentChoice = as.factor(consentChoice))

# add additional useful variables & create lagged variables
snake_ds <- snake_ds %>% group_by(ID) %>% mutate(score.minus1 = lag(score, n=1, order_by=trial),
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
                                
snake_ds$appleChoiceDelta <- snake_ds$appleChoice.minus1 - snake_ds$appleChoice
snake_ds$rankChoiceDelta <- snake_ds$rankChoice.minus1 - snake_ds$rankChoice
snake_ds$scoreDelta <- snake_ds$score.minus1 - snake_ds$score.minus2

snake_ds <- snake_ds %>% group_by(ID) %>% mutate(appleChoiceDelta.minus1 = lag(appleChoiceDelta, n=1, order_by=trial),
                                       appleChoiceDelta.minus2 = lag(appleChoiceDelta, n=2, order_by=trial),
                                       rankChoiceDelta.minus1 = lag(rankChoiceDelta, n=1, order_by=trial),
                                       rankChoiceDelta.minus2 = lag(rankChoiceDelta, n=2, order_by=trial),
                                       scoreDelta.minus1 = lag(scoreDelta, n=1, order_by=trial),
                                       scoreDelta.minus2 = lag(scoreDelta, n=2, order_by=trial))

save(snake_ds,file="snake_ds.Rda")

# distribution.
par(mfrow=c(2,4))
summary(snake_ds$gender)
hist(snake_ds$age)
hist(snake_ds$gameExp)
hist(snake_ds$appleChoice,
         main = "Apple stealing",
         xlab = "apple choice",
         ylab = "trials")
hist(snake_ds$rankChoice,
         main = "Booster buying",
         xlab = "booster choice",
         ylab = "trials")


## linear mixed-effect models for apple choice

mapple1 <- lmer(appleChoice ~ trial + win.minus1 + win.minus2 + close.minus1 + oppRank + rankStart + gender + age + gameExp + rankEstim1 + scoreDelta.minus1 + rankChoice.minus1 + appleChoice.minus1 + enjoyed + satisfied + fair + credible + (1|ID),  data = snake_ds, na.action = na.omit)
summary(mapple1)
car::Anova(mapple1)

mapple2 <- lmer(appleChoice ~ trial + win.minus1 + win.minus2 + close.minus1 + oppRank + gender + age + rankEstim1 + scoreDelta.minus1 + rankChoice.minus1 + appleChoice.minus1 + enjoyed + satisfied + fair + (1|ID),  data = snake_ds, na.action = na.omit)
summary(mapple2)
car::Anova(mapple2)

anova(mapple1, mapple2)

mapple3 <- lmer(appleChoice ~ trial + win.minus2 + oppRank + gender + rankEstim1 + scoreDelta.minus1 + rankChoice.minus1 + appleChoice.minus1 + enjoyed + satisfied + fair + (1|ID),  data = snake_ds, na.action = na.omit)
summary(mapple3)
car::Anova(mapple3)

anova(mapple2, mapple3)

#to print output in .txt file
sink("appleChoice_model.txt",append=FALSE, split=FALSE)

# preferred model: mapple4
mapple4 <- lmer(appleChoice ~ trial + oppRank + gender + rankEstim1*win.minus1 + scoreDelta.minus1 + rankChoice.minus1 + appleChoice.minus1*win.minus1 + fair + (1|ID),  data = snake_ds, na.action = na.omit)
summary(mapple4)
car::Anova(mapple4)

#to stop prinitng output
sink()

anova(mapple3, mapple4)

mapple5 <- lmer(appleChoice ~ trial + oppRank + gender + rankEstim1*win.minus1 + scoreDelta.minus1 + rankChoice.minus1 + appleChoice.minus1*win.minus1*trial + fair + (1|ID),  data = snake_ds, na.action = na.omit)
summary(mapple5)
car::Anova(mapple4)

anova(mapple4, mapple5)

mapple6 <- lmer(appleChoice ~ trial + oppRank + gender + rankEstim1*win.minus1 + scoreDelta.minus1 + rankChoice.minus1 + appleChoice.minus1*trial + fair + (1|ID),  data = snake_ds, na.action = na.omit)
summary(mapple6)
car::Anova(mapple6)

anova(mapple4, mapple6)

mapple7 <- lmer(appleChoice ~ trial + oppRank + gender + scoreDelta.minus1 + rankChoice.minus1 + rankEstim1*appleChoice.minus1*win.minus1 + fair + (1|ID),  data = snake_ds, na.action = na.omit)
summary(mapple7)
car::Anova(mapple7)

anova(mapple4, mapple7)

# plot interactions
plot(effect("win.minus1:appleChoice.minus1",mapple4), grid=TRUE)

plot(effect("rankEstim1:win.minus1",mapple4), grid=TRUE)


## linear mixed-effect models for booster choice
mboost1 <- lmer(rankChoice ~ trial + win + win.minus1 + close + oppRank + rankStart + gender + age + gameExp + rankEstim1 + scoreDelta + rankChoice.minus1 + rankChoice.minus2 + appleChoice + enjoyed + satisfied + fair + credible + (1|ID),  data = snake_ds, na.action = na.omit) 
summary(mboost1)
car::Anova(mboost1)

mboost2 <- lmer(rankChoice ~ trial + win + close + oppRank + rankStart + scoreDelta + rankChoice.minus1 + rankChoice.minus2 + appleChoice + satisfied + (1|ID),  data = snake_ds, na.action = na.omit) 
summary(mboost2)
car::Anova(mboost2)

anova(mboost1, mboost2)

mboost3 <- lmer(rankChoice ~ trial + win + close + rankChoice.minus1 + rankChoice.minus2 + appleChoice + satisfied + (1|ID),  data = snake_ds, na.action = na.omit) 
summary(mboost3)
car::Anova(mboost3)

anova(mboost2, mboost3)

mboost4 <- lmer(rankChoice ~ win*rankChoice.minus1 + close + win*rankChoice.minus2 + win*appleChoice + satisfied + (1|ID),  data = snake_ds, na.action = na.omit) 
summary(mboost4)
car::Anova(mboost4)

anova(mboost3, mboost4)

sink("boosterChoice_model.txt",append=FALSE, split=FALSE)

#preferred model
mboost5 <- lmer(rankChoice ~ win*rankChoice.minus1 + rankChoice.minus2 + win*appleChoice + satisfied + (1|ID),  data = snake_ds, na.action = na.omit) 
summary(mboost5)
car::Anova(mboost5)

sink()

anova(mboost4, mboost5)

# plot interactions (commented because bugging)
#pdf("V4-2-win-oppRank.pdf", width=10, height=5)
plot(effect("win:rankChoice.minus1",mboost5), grid=TRUE)
plot(effect("win:appleChoice",mboost5), grid=TRUE)
#dev.off()

##############################
### narcissism measures ######
##############################

# preparing and adding the dataset with demographic info and narcissistic questionnaires (hamilton still to be added)
snake_suppl <- read_excel("snake_suppl2.xlsx")
View(snake_suppl)
summary(snake_suppl)

snake_suppl <- transform(snake_suppl, ID = as.factor(ID), comment0 = as.factor(comment0), comment = as.factor(comment), gender = as.factor(gender), group1_7 = as.factor(group1_7), group1_5 = as.factor(group1_5), race = as.factor(race), ethnicity = as.factor(ethnicity), marital_status = as.factor(marital_status))
snake_suppl <- transform(snake_suppl, cDate = as.Date(cDate), dob = as.Date(dob), baseline_consent_date = as.factor(baseline_consent_date), snake_date = as.factor(snake_date))

snake_suppl$ffni <- rowSums(snake_suppl[, 18:77], na.rm = FALSE)
snake_suppl$ipip <- rowSums(snake_suppl[, 78], (6-snake_suppl[, 79]),  na.rm = FALSE)
snake_suppl$inv.ipip2 <- 6-snake_suppl[, 79]
snake_suppl$ipip <- rowSums(snake_suppl[, c("ipip1", "inv.ipip2", "ipip3", "ipip4", "ipip5", "ipip6", "ipip7", "ipip8", "ipip9", "ipip10", "ipip11")], na.rm = FALSE)

snake_tot <- left_join(snake_ds, snake_suppl, by=c("ID"))
View(snake_tot)

save(snake_tot, file="snake_tot.Rda")
load("snake_tot.Rda")

#distribution
summary(snake_tot$group1_5)
hist(snake_tot$ipip)
hist(snake_tot$ffni)
hist(snake_tot$bpni_grandiose)
hist(snake_tot$bpni_vulnerable)
hist(log(snake_tot$bpni_grandiose))
hist(log(snake_tot$bpni_vulnerable))

###adding narcissistic measures to models (use log for BPNI?)

##apple choice
mapple4_tot <- lmer(appleChoice ~ trial + oppRank + gender.y + rankEstim1*win.minus1 + scoreDelta.minus1 + rankChoice.minus1 + appleChoice.minus1*win.minus1 + fair + ipip + ffni + bpni_grandiose + bpni_vulnerable + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple4_tot)
car::Anova(mapple4_tot)

#best model for now, but more thorough analysis required
mapple4_tot2 <- lmer(appleChoice ~ trial + oppRank + gender.y + rankEstim1*win.minus1 + scoreDelta.minus1 + rankChoice.minus1 + appleChoice.minus1*win.minus1 + fair + ipip*oppRank + ffni*oppRank + bpni_grandiose*oppRank + bpni_vulnerable*oppRank + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple4_tot2)
car::Anova(mapple4_tot2)

mapple4_tot3 <- lmer(appleChoice ~ trial + oppRank + gender.y + rankEstim1*win.minus1 + scoreDelta.minus1 + rankChoice.minus1 + appleChoice.minus1*win.minus1 + fair + ipip*win.minus1 + ffni*win.minus1 + bpni_grandiose*win.minus1 + bpni_vulnerable*win.minus1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple4_tot3)
car::Anova(mapple4_tot3)

#interactions
plot(effect("oppRank:ipip",mapple4_tot2), grid=TRUE)
plot(effect("oppRank:ffni",mapple4_tot2), grid=TRUE)
plot(effect("oppRank:bpni_grandiose",mapple4_tot2), grid=TRUE)
plot(effect("oppRank:bpni_vulnerable",mapple4_tot2), grid=TRUE)
plot(effect("win.minus1:bpni_vulnerable",mapple4_tot3), grid=TRUE)


#rank choice
mboost5_tot <- lmer(rankChoice ~ win*rankChoice.minus1 + rankChoice.minus2 + win*appleChoice + satisfied + ipip + ffni + bpni_grandiose + bpni_vulnerable + (1|ID),  data = snake_tot, na.action = na.omit) 
summary(mboost5_tot)
car::Anova(mboost5_tot)

#best model of these three, but more thorough analysis required
mboost5_tot2 <- lmer(rankChoice ~ win*rankChoice.minus1 + rankChoice.minus2 + win*appleChoice + satisfied + ipip*win + ffni*win + bpni_grandiose*win + bpni_vulnerable*win + (1|ID),  data = snake_tot, na.action = na.omit) 
summary(mboost5_tot2)
car::Anova(mboost5_tot2)

mboost5_tot3 <- lmer(rankChoice ~ win*rankChoice.minus1 + rankChoice.minus2 + win*appleChoice + satisfied + ipip*rankStart + ffni*rankStart + bpni_grandiose*rankStart + bpni_vulnerable*rankStart + (1|ID),  data = snake_tot, na.action = na.omit) 
summary(mboost5_tot3)
car::Anova(mboost5_tot3)

mboost5_tot4 <- lmer(rankChoice ~ win*rankChoice.minus1 + rankChoice.minus2 + win*appleChoice + satisfied + ipip*trial + ffni*trial + bpni_grandiose*trial + bpni_vulnerable*trial + (1|ID),  data = snake_tot, na.action = na.omit) 
summary(mboost5_tot4)
car::Anova(mboost5_tot4)

#interactions
plot(effect("win:bpni_grandiose",mboost5_tot2), grid=TRUE)
plot(effect("win:bpni_vulnerable",mboost5_tot2), grid=TRUE)
