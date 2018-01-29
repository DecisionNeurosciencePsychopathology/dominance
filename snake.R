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
setwd("~/Dropbox/USA/Pittsburgh/GitHub/dominance/snake_data")

# wd for Alex
#setwd("~/code//dominance")

# clear environment
rm(list=ls())


## load datafile and skip to line 126 (use only if data has already been prepared previously, otherwise run lines 64 to 126) 
load("snake_ds.Rda")
View(snake_ds)
load("snake_tot.Rda")

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

# missingness
library(mice)
md.pattern(snake_ds)

library(VIM)
snake_ds_aggr = aggr(snake_ds, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(snake_ds), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))


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


# preparing and adding the dataset with demographic info and narcissistic questionnaires (hamilton still to be added)
snake_suppl <- read_excel("snake_suppl2.xlsx")
View(snake_suppl)
summary(snake_suppl)

snake_suppl <- transform(snake_suppl, ID = as.factor(ID), comment0 = as.factor(comment0), comment = as.factor(comment), gender = as.factor(gender), group1_7 = as.factor(group1_7), group1_5 = as.factor(group1_5), race = as.factor(race), ethnicity = as.factor(ethnicity), marital_status = as.factor(marital_status))
snake_suppl <- transform(snake_suppl, cDate = as.Date(cDate), dob = as.Date(dob), baseline_consent_date = as.factor(baseline_consent_date), snake_date = as.factor(snake_date))

snake_suppl$ffni <- rowSums(snake_suppl[, 18:77], na.rm = FALSE)
snake_suppl$inv.ipip2 <- 6-snake_suppl[, 79]
snake_suppl$ipip <- rowSums(snake_suppl[, c("ipip1", "inv.ipip2", "ipip3", "ipip4", "ipip5", "ipip6", "ipip7", "ipip8", "ipip9", "ipip10", "ipip11")], na.rm = FALSE)

save(snake_suppl,file="snake_suppl.Rda")

library(corrplot)
load("snake_suppl.Rda")
chars <- snake_suppl[,c(6,15,89,90,91,92,94)]
cormat <- corr.test(chars)

chars <- snake_suppl[,c(89,90,91,92,94)]
cormat <- corr.test(chars)

snake_healthy <- snake_suppl[snake_suppl$group1_5 == "1",]
View(snake_healthy)

chars <- snake_healthy[,c(89,90,91,92,94)]
cormat <- corr.test(chars)

snake_tot <- left_join(snake_ds, snake_suppl, by=c("ID"))
View(snake_tot)

save(snake_tot, file="snake_tot.Rda")
load("snake_tot.Rda")

#distribution
summary(snake_tot$group1_5)

par(mfrow=c(4,2))

hist(snake_suppl$ipip, xlab = "whole sample (23 HC, 3 DC, 8 Ideators, 8 attempters)", main = "IPIP-Dominance Scale", density = 25)
hist(snake_healthy$ipip, xlab = "23 healthy controls", main = "IPIP-Domincance scale", density = 25)

hist(snake_suppl$ffni, xlab = "whole sample (23 HC, 3 DC, 8 Ideators, 8 attempters)", main = "FFNI", density = 25)
hist(snake_healthy$ffni, xlab = "23 healthy controls", main = "FFNI", density = 25)

hist(snake_suppl$bpni_grandiose, xlab = "whole sample (23 HC, 3 DC, 8 Ideators, 8 attempters)", main = "BPNI grandiose subscale", density = 25)
hist(snake_healthy$bpni_grandiose, xlab = "23 healthy controls", main = "BPNI grandiose subscale", density = 25)

hist(snake_suppl$bpni_vulnerable, xlab = "whole sample (23 HC, 3 DC, 8 Ideators, 8 attempters)", main = "BPNI vulnerable subscale", density = 25)
hist(snake_healthy$bpni_vulnerable, xlab = "23 healthy controls", main = "BPNI vulnerable subscale", density = 25)



hist(snake_tot$ffni)
hist(snake_tot$bpni_grandiose)
hist(snake_tot$bpni_vulnerable)
hist(snake_tot$bpni_total)
hist(snake_tot$rankChoice, xlab = "Rank Choice", main = "Distribution of Rank (booster) choice", density = 25)
hist(snake_tot$appleChoice, xlab = "Apple Choice", main = "Distribution of Apple stealing choice", density = 25)
table(snake_tot$group1_5)
552/24
72/24
192/24

#building models by blocks
## BLOCK1: design variables
# correlation matrix for design variables
chars <- snake_tot[,c(20,21,22,23,27,28,29)]
cormat <- corr.test(chars)
names(snake_tot)


# univariate models
uv_apple <- lmer(appleChoice ~ trial + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ win.minus1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ win.minus2 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ close.minus1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ oppRank + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ rankStart + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ scoreDelta.minus1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ scoreDiff.minus1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ score.minus1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ rankChoice.minus1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ appleChoice.minus1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

# multivariate model
mapple1 <- lmer(appleChoice ~ trial + win.minus1 + close.minus1 + oppRank + rankStart + scoreDelta.minus1 + score.minus1 + rankChoice.minus1 + appleChoice.minus1 + (1|ID),  data = snake_ds, na.action = na.omit)
summary(mapple1)
car::Anova(mapple1)

mapple2 <- lmer(appleChoice ~ trial + oppRank + scoreDelta.minus1 + score.minus1 + rankChoice.minus1 + appleChoice.minus1 + (1|ID),  data = snake_ds, na.action = na.omit)
summary(mapple2)
car::Anova(mapple2)

anova(mapple1, mapple2)

mapple3 <- lmer(appleChoice ~ trial + win.minus1 + oppRank + scoreDelta.minus1 + rankChoice.minus1 + appleChoice.minus1 + (1|ID),  data = snake_ds, na.action = na.omit)
summary(mapple3)
car::Anova(mapple3)

anova(mapple1, mapple3)

mapple4 <- lmer(appleChoice ~ trial + win.minus1 + oppRank + scoreDiff.minus1 + scoreDelta.minus1 + rankChoice.minus1 + appleChoice.minus1 + (1|ID),  data = snake_ds, na.action = na.omit)
summary(mapple4)
car::Anova(mapple4)

anova(mapple3, mapple4)

# checking for interactions
## no significant interactions with win.minus1 (all possibilities checked)
mapple5 <- lmer(appleChoice ~ trial*win.minus1 + oppRank + scoreDelta.minus1 + rankChoice.minus1 + appleChoice.minus1 + (1|ID),  data = snake_ds, na.action = na.omit)
summary(mapple5)
car::Anova(mapple5)

## no significant interactions with trial (all possibilities checked)
mapple6 <- lmer(appleChoice ~ oppRank + scoreDelta.minus1 + rankChoice.minus1 + appleChoice.minus1*trial + win.minus1 + (1|ID),  data = snake_ds, na.action = na.omit)
summary(mapple6)
car::Anova(mapple6)

## no significant interactions with oppRank (all possibilities checked)
mapple7 <- lmer(appleChoice ~ scoreDelta.minus1 + rankChoice.minus1 + appleChoice.minus1*oppRank + trial + win.minus1 + (1|ID),  data = snake_ds, na.action = na.omit)
summary(mapple7)
car::Anova(mapple7)

## no significant interactions with scoreDelta.minus1 (all possibilities checked)
mapple8 <- lmer(appleChoice ~  rankChoice.minus1 + appleChoice.minus1*scoreDelta.minus1 + oppRank + trial + win.minus1 + (1|ID),  data = snake_ds, na.action = na.omit)
summary(mapple8)
car::Anova(mapple8)

## significant interactions between rankChoice.minus1 and appleChoice.minus1
mapple9 <- lmer(appleChoice ~  rankChoice.minus1*appleChoice.minus1 + scoreDelta.minus1 + oppRank + trial + win.minus1 + (appleChoice.minus1 + trial |ID),  data = snake_tot, na.action = na.omit)
summary(mapple9)
car::Anova(mapple9)

anova(mapple3, mapple9)

mapple9b <- glmer(appleChoice>1 ~  I(rankChoice.minus1>1)+I(appleChoice.minus1>1) + scoreDelta.minus1 + oppRank + trial + win.minus1 + (1|ID),  family = 'binomial', data = snake_ds, na.action = na.omit)
summary(mapple9b)


# best model mapple9; plot interaction
plot(effect("rankChoice.minus1:appleChoice.minus1",mapple9), grid=TRUE)

##BLOCK2: demographics (age, gender education, snake_level, group1_5)
# univariate models
names(snake_tot)

uv_apple <- lmer(appleChoice ~ age + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ gender.y + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ education + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ snakeLevel + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ gameExp + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ group1_5 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)
lsm.apple_group <- lsmeans(uv_apple, "group1_5")
contrast(lsm.apple_group, method = "pairwise", adjust ="tukey")
plot(lsm.apple_group, type ~ snake_tot$group1_5, horiz=F, ylab = "appleChoice", xlab = "Group")

#multivariate model
mapple1.2 <- lmer(appleChoice ~ group1_5 + gameExp + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple1.2)
car::Anova(mapple1.2)

anova(mapple1.2, uv_apple)

mapple2.2 <- lmer(appleChoice ~ group1_5*gameExp + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple2.2)
car::Anova(mapple2.2)

mapple3.2 <- lmer(appleChoice ~ group1_5 + gameExp + age + education + gender.y + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple3.2)
car::Anova(mapple3.2)

anova(mapple1.2, mapple3.2)
#not able to compute but 3.2 better based on residuals

mapple4.2 <- lmer(appleChoice ~ group1_5 + gameExp + age + gender.y + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple4.2)
car::Anova(mapple4.2)

#no significative difference between mapple1.2 and 4.2
anova(mapple1.2, mapple4.2)

## BLOCK3: emotional scales
snake_tot$panas_pos1 <- rowSums(snake_tot[, c(10,14,16,19)], na.rm = FALSE)
snake_tot$panas_pos2 <- rowSums(snake_tot[, c(32,36,38,41)], na.rm = FALSE)
snake_tot$panas_scared1 <- rowSums(snake_tot[, c(11,12,17,18)], na.rm = FALSE)
snake_tot$panas_scared2 <- rowSums(snake_tot[, c(33,34,39,40)], na.rm = FALSE)
snake_tot$panas_angry1 <- rowSums(snake_tot[, c(13,15)], na.rm = FALSE)
snake_tot$panas_angry2 <- rowSums(snake_tot[, c(35,37)], na.rm = FALSE)

snake_tot$delta_rankEstim <-snake_tot$rankEstim2-snake_tot$rankEstim1

snake_tot$delta_panas_angry <-snake_tot$panas_angry2-snake_tot$panas_angry1
snake_tot$delta_panas_scared <-snake_tot$panas_scared2-snake_tot$panas_scared1 
snake_tot$delta_panas_pos <-snake_tot$panas_pos2-snake_tot$panas_pos1

chars <- snake_tot[,c(6,54,170,171,172,173,174,175,176,50,51,52,53)]
cormat <- corr.test(chars)
names(snake_tot)

#univariate models
uv_apple <- lmer(appleChoice ~ rankEstim1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ rankEstim2 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ delta_rankEstim + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ satisfied + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ fair + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ enjoyed + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ credible + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ panas_pos1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ panas_pos2 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ panas_scared1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ panas_scared2 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ panas_angry1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ panas_angry2 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ delta_panas_angry + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ delta_panas_scared + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ delta_panas_pos + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

# multivariate model
mapple1.3 <- lmer(appleChoice ~ rankEstim1 + rankEstim2 + satisfied + fair + enjoyed + panas_pos1 + panas_pos2 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple1.3)
car::Anova(mapple1.3)

mapple2.3 <- lmer(appleChoice ~ rankEstim1 + rankEstim2 + fair + panas_pos1 + panas_pos2 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple2.3)
car::Anova(mapple2.3)

anova(mapple1.3, mapple2.3)

mapple3.3 <- lmer(appleChoice ~ rankEstim1 + fair + panas_pos2 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple3.3)
car::Anova(mapple3.3)

anova(mapple2.3, mapple3.3)

mapple4.3 <- lmer(appleChoice ~ fair + panas_pos2 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple4.3)
car::Anova(mapple4.3)

anova(mapple3.3, mapple4.3)

mapple4.3 <- lmer(appleChoice ~ fair*panas_pos2 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple4.3)
car::Anova(mapple4.3)

#best model: mapple 4.3

## BLOCK 4: narcissistic scales
uv_apple <- lmer(appleChoice ~ bpni_total + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ bpni_vulnerable + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ bpni_grandiose + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ ffni + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

uv_apple <- lmer(appleChoice ~ ipip + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_apple)
car::Anova(uv_apple)

mapple1.4 <- lmer(appleChoice ~ bpni_vulnerable + ipip + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple1.4)
car::Anova(mapple1.4)

anova(uv_apple, mapple1.4)

## linear mixed-effect models for apple choice (preliminary)
## BLOCKS 5 = 3 and 4
mapple1.5 <- lmer(appleChoice ~ ipip + fair + panas_pos2 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple1.5)
car::Anova(mapple1.5)

mapple2.5 <- lmer(appleChoice ~ ipip*fair + panas_pos2 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple2.5)
car::Anova(mapple2.5)

mapple3.5 <- lmer(appleChoice ~ fair + ipip*panas_pos2 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple3.5)
car::Anova(mapple3.5)

anova(mapple1.5, mapple3.5)
#best model: mapple 3.5

plot(effect("ipip:panas_pos2",mapple3.5), grid=TRUE)

mapple4.5 <- lmer(appleChoice ~ fair*panas_pos2 + ipip + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple4.5)
car::Anova(mapple4.5)

## BLOCK 6 = 5 and 2
mapple1.6 <- lmer(appleChoice ~ fair + ipip*panas_pos2 + group1_5 + gameExp + age + gender.y + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple1.6)
car::Anova(mapple1.6)

mapple2.6 <- lmer(appleChoice ~ ipip*panas_pos2 + group1_5 + gameExp + age + gender.y + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple2.6)
car::Anova(mapple2.6)

anova(mapple1.6, mapple2.6)

mapple3.6 <- lmer(appleChoice ~ ipip*panas_pos2 + gameExp + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple3.6)
car::Anova(mapple3.6)

anova(mapple2.6, mapple3.6)

mapple4.6 <- lmer(appleChoice ~ ipip*panas_pos2*gameExp + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple4.6)
car::Anova(mapple4.6)

anova(mapple4.6, mapple3.6)
#best model mapple 3.6

## Preliminary final model (BLOCK7 = BLOCKS 1 and 6)
mapple1.7 <- lmer(appleChoice ~ ipip*panas_pos2 + gameExp + rankChoice.minus1*appleChoice.minus1 + scoreDelta.minus1 + oppRank + trial + win.minus1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple1.7)
car::Anova(mapple1.7)

mapple2.7 <- lmer(appleChoice ~ ipip*panas_pos2 + gameExp + rankChoice.minus1*appleChoice.minus1 + scoreDelta.minus1 + oppRank + trial + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple2.7)
car::Anova(mapple2.7)

anova(mapple1.7, mapple2.7)

#checking for interactions:
#interactions between gameExp and opponentRank
mapple3.7 <- lmer(appleChoice ~ ipip*panas_pos2 + rankChoice.minus1*appleChoice.minus1 + scoreDelta.minus1 + gameExp*oppRank + trial + win.minus1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple3.7)
car::Anova(mapple3.7)

mapple4.7 <- lmer(appleChoice ~ ipip*panas_pos2*appleChoice.minus1 + ipip*panas_pos2*rankChoice.minus1 + scoreDelta.minus1 + oppRank + trial + win.minus1 + gameExp + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple4.7)
car::Anova(mapple4.7)

anova(mapple3.7, mapple4.7)

mapple5.7 <- lmer(appleChoice ~ ipip*panas_pos2*appleChoice.minus1 + ipip*panas_pos2*rankChoice.minus1 + ipip*oppRank + scoreDelta.minus1 + trial + win.minus1 + gameExp + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple5.7)
car::Anova(mapple5.7)

anova(mapple4.7, mapple5.7)
#best model mapple5.7

mapple6.7 <- lmer(appleChoice ~ ipip*panas_pos2*appleChoice.minus1 + ipip*panas_pos2*rankChoice.minus1 + ipip*oppRank + scoreDelta.minus1 + trial + win.minus1 + oppRank*gameExp + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple6.7)
car::Anova(mapple6.7)

mapple7.7 <- lmer(appleChoice ~ ipip*panas_pos2*appleChoice.minus1 + ipip*panas_pos2*rankChoice.minus1 + ipip*oppRank + scoreDelta.minus1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple7.7)
car::Anova(mapple7.7)
anova(mapple5.7, mapple7.7)

mapple8.7 <- lmer(appleChoice ~ ipip*panas_pos2 + appleChoice.minus1 + panas_pos2*rankChoice.minus1 + ipip*oppRank + scoreDelta.minus1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple8.7)
car::Anova(mapple8.7)

anova(mapple7.7, mapple8.7)
#mapple 7.7 best model

plot(effect("ipip:oppRank",mapple7.7), grid=TRUE)

mapple9.7 <- lmer(appleChoice ~ ipip*panas_pos2*appleChoice.minus1 + ipip*panas_pos2*rankChoice.minus1 + ipip*oppRank + scoreDelta.minus1 + appleChoice.minus1*win.minus1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple9.7)
car::Anova(mapple9.7)

anova(mapple7.7, mapple9.7)
#mapple 9.7 marginally better
plot(effect("appleChoice.minus1:win.minus1",mapple9.7), grid=TRUE)

mapple10.7 <- lmer(appleChoice ~ appleChoice.minus1 + rankChoice.minus1 + ipip*oppRank + scoreDelta.minus1 + appleChoice.minus1*win.minus1*trial + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mapple10.7)
car::Anova(mapple10.7)

snake_tot$oppRanks <- scale(snake_tot$oppRank)
snake_tot$trials <- scale(snake_tot$trial)

mapple10.7 <- lmer(appleChoice ~ appleChoice.minus1 + rankChoice.minus1 + ipip*oppRank +  scoreDelta.minus1 + trial + (trial|ID),  data = snake_tot, na.action = na.omit)
summary(mapple10.7)
car::Anova(mapple10.7)

plot(effect("ipip:oppRank",mapple10.7, grid=TRUE, x.var = "oppRank"))



mapple10.7b <- glmer(appleChoice>1 ~ I(appleChoice.minus1>1) + I(rankChoice.minus1>1) + group1_5 + oppRanks*ipip +  scoreDelta.minus1 + trials + (1|ID),  family = 'binomial',data = snake_tot, na.action = na.omit)
summary(mapple10.7b)
car::Anova(mapple10.7b)

plot(effect("oppRanks:ipip",mapple10.7b), grid=TRUE)


lsm <- lsmeans(mapple10.7b, "oppRanks", by = "ipip", at = list(oppRanks = c(1,200), ipip = c(15,35)))
plot(lsm, horiz = F)
anova(mapple9.7, mapple10.7)
#best model apple 10.7
#to print output in .txt file
#sink("appleChoice_model.txt",append=FALSE, split=FALSE)
#to stop prinitng output
#sink()

##########################################################
##################### booster choice #####################
##########################################################

# univariate models
uv_rank <- lmer(rankChoice ~ trial + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ win + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ win.minus1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ close + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ oppRank + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ rankStart + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ scoreDelta + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ scoreDiff + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ score + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ rankChoice.minus1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ rankChoice.minus2 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ appleChoice + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

# multivariate model
mrank1 <- lmer(rankChoice ~ trial + win + close + rankStart + scoreDiff + score + rankChoice.minus1 + rankChoice.minus2 + appleChoice + (1|ID),  data = snake_ds, na.action = na.omit)
summary(mrank1)
car::Anova(mrank1)

mrank2 <- lmer(rankChoice ~ rankStart*trial + win + close + scoreDiff + score + rankChoice.minus1 + rankChoice.minus2 + appleChoice + (1|ID),  data = snake_ds, na.action = na.omit)
summary(mrank2)
car::Anova(mrank2)

anova(mrank1, mrank2)
plot(effect("rankStart:trial",mrank2), grid=TRUE)


mrank3 <- lmer(rankChoice ~ rankStart*trial + win + close + scoreDiff + score + rankStart*rankChoice.minus1 + rankChoice.minus2 + appleChoice + (1|ID),  data = snake_ds, na.action = na.omit)
summary(mrank3)
car::Anova(mrank3)

anova(mrank2, mrank3)
plot(effect("rankStart:rankChoice.minus1",mrank2), grid=TRUE)

mrank4 <- lmer(rankChoice ~ rankStart*trial + win + close + scoreDiff + score + rankStart*rankChoice.minus1 + rankChoice.minus2 + rankStart*appleChoice + (1|ID),  data = snake_ds, na.action = na.omit)
summary(mrank4)
car::Anova(mrank4)

anova(mrank3, mrank4)
plot(effect("rankStart:appleChoice",mrank4), grid=TRUE)

mrank5 <- lmer(rankChoice ~ rankStart*trial + win + scoreDiff + score*close + rankStart*rankChoice.minus1 + rankChoice.minus2 + rankStart*appleChoice + (1|ID),  data = snake_ds, na.action = na.omit)
summary(mrank5)
car::Anova(mrank5)

anova(mrank4, mrank5)
plot(effect("score:close",mrank5), grid=TRUE)

mrank6 <- lmer(rankChoice ~ rankStart*trial + win + score*close + rankStart*rankChoice.minus1 + rankChoice.minus2*rankChoice.minus1 + rankStart*appleChoice + (1|ID),  data = snake_ds, na.action = na.omit)
summary(mrank6)
car::Anova(mrank6)

anova(mrank5, mrank6)
plot(effect("rankChoice.minus1:rankChoice.minus2",mrank6), grid=TRUE)

mrank7 <- lmer(rankChoice ~ rankStart*trial + win + score*close + rankStart*rankChoice.minus1 + rankChoice.minus2*rankChoice.minus1+ rankChoice.minus2*score + rankStart*appleChoice + (1|ID),  data = snake_ds, na.action = na.omit)
summary(mrank7)
car::Anova(mrank7)

anova(mrank6, mrank7)
plot(effect("score:rankChoice.minus2",mrank7), grid=TRUE)

mrank8 <- lmer(rankChoice ~ rankStart*trial + score*close + rankStart*rankChoice.minus1 + rankChoice.minus2*rankChoice.minus1+ rankChoice.minus2*score*win + rankStart*appleChoice + (1|ID),  data = snake_ds, na.action = na.omit)
summary(mrank8)
car::Anova(mrank8)

anova(mrank7, mrank8)
# mrank7 = better model

mrank9 <- lmer(rankChoice ~ rankStart*trial + win + score*close + oppRank + rankChoice.minus1 + appleChoice + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrank9)
car::Anova(mrank9, type = 'III')

snake_tot$rankStarts <- scale(snake_tot$rankStart)
snake_tot$scores <- scale(snake_tot$score)
snake_tot$trials <- scale(snake_tot$trial)
snake_tot$oppRanks <- scale(snake_tot$oppRank)

save(snake_tot, file="snake_tot.Rda")


mrank9b <- glmer(rankChoice>1 ~ rankStarts + trials + win + scores*close + oppRanks + I(rankChoice.minus1>1) + I(appleChoice>1) + (1|ID),  family = binomial(link = "logit"), data = snake_tot, na.action = na.omit)
summary(mrank9b)
car::Anova(mrank9b, type = 'III')

mrank9b1 <- glmer(rankChoice>1 ~ rankStarts + score*close +  I(rankChoice.minus1>1) + I(appleChoice>1) + (1|ID),  nAGQ = 0, family = binomial(link = "logit"), data = snake_tot, na.action = na.omit)
summary(mrank9b1)
anova(mrank9b1, mrank9b)
car::Anova(mrank9b1, type = 'III')


snake_tot$rankChoice.zero <- snake_tot$rankChoice - 1

library(pscl)
mrank9c <- hurdle(rankChoice.zero ~ rankStart + trial + win + score*close + oppRank + rankChoice.minus1 + appleChoice + (1:ID),  data = snake_tot, na.action = na.omit, dist = 'negbin', zero.dist = 'negbin', link = 'logit')
summary(mrank9c)

anova(mrank9c, mrank9d)


mrank9d <- hurdle(rankChoice.zero ~ rankStart + trial + win + oppRank + rankChoice.minus1 + appleChoice + (1:ID) |trial + rankChoice.minus1 + rankStart + win + (1:ID),  data = snake_tot, na.action = na.omit, dist = 'negbin', zero.dist = 'geometric', link = 'logit')
summary(mrank9d)

t <- stargazer(mrank9c, mrank9d, type = "text")


# mrank9 + best model

plot(effect("rankStart:trial",mrank9), grid=TRUE)
plot(effect("scores:close",mrank9b), grid=TRUE)

chars <- snake_tot[,c(20,21,22,23,25,27,28,29)]
chars$close <- as.numeric(chars$close)
cormat <- corr.test(chars, method = "spearman")
names(snake_tot)


mrank10 <- lmer(rankChoice ~ rankStart*trial + score*close + rankStart*rankChoice.minus1*rankChoice.minus2+ rankChoice.minus2*score + rankStart*appleChoice + (1|ID),  data = snake_ds, na.action = na.omit)
summary(mrank10)
car::Anova(mrank10)

anova(mrank9, mrank10)

# dichotomize boost choice
snake_tot$boost <- snake_tot$rankChoice>1

mrankBi <- glmer(rankChoice>1 ~ rankStarts + trials + win + scores*close + oppRanks + I(rankChoice.minus1>1) + I(appleChoice>1) + (1|ID),  family = 'binomial', data = snake_tot, na.action = na.omit)
summary(mrankBi)


##BLOCK2: demographics (age, gender education, snake_level, group1_5)
# univariate models
names(snake_tot)

uv_rank <- lmer(rankChoice ~ age + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ gender.y + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ education + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ snakeLevel + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ gameExp + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ group1_5 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ group1_7 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)
lsm.rank_group <- lsmeans(uv_rank, "group1_7")
contrast(lsm.rank_group, method = "pairwise", adjust ="tukey")
plot(lsm.rank_group, type ~ snake_tot$group1_7, horiz=F, ylab = "rankChoice", xlab = "Group")

#multivariate model
mrank1.2 <- lmer(rankChoice ~ group1_5 + age + gender.y + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrank1.2)
car::Anova(mrank1.2)

## BLOCK3: emotional scales
#univariate models
uv_rank <- lmer(rankChoice ~ rankEstim1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ rankEstim2 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ delta_rankEstim + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ satisfied + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ fair + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ enjoyed + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ credible + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ panas_pos1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ panas_pos2 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ panas_scared1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ panas_scared2 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ panas_angry1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ panas_angry2 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ delta_panas_angry + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ delta_panas_scared + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ delta_panas_pos + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

# multivariate model
mrank1.3 <- lmer(rankChoice ~ rankEstim2 + satisfied + fair + panas_scared1 + panas_scared2 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrank1.3)
car::Anova(mrank1.3)

mrank2.3 <- lmer(rankChoice ~ rankEstim2 + satisfied + panas_scared1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrank2.3)
car::Anova(mrank2.3)

anova(mrank1.3, mrank2.3)

mrank3.3 <- lmer(rankChoice ~ rankEstim2 + satisfied + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrank3.3)
car::Anova(mrank3.3)

anova(mrank2.3, mrank3.3)
#mrank3.3 = best model

mrank4.3 <- lmer(rankChoice ~ rankEstim2*satisfied + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrank4.3)
car::Anova(mrank4.3)

## BLOCK 4: narcissistic scales
uv_rank <- lmer(rankChoice ~ bpni_total + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ bpni_vulnerable + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ bpni_grandiose + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ ffni + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

uv_rank <- lmer(rankChoice ~ ipip + (1|ID),  data = snake_tot, na.action = na.omit)
summary(uv_rank)
car::Anova(uv_rank)

# multivariate models
mrank1.4 <- lmer(rankChoice ~ bpni_vulnerable + ipip + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrank1.4)
car::Anova(mrank1.4)

anova(uv_rank, mrank1.4)

## linear mixed-effect models for apple choice (preliminary)
## BLOCKS 5 = 3 and 4
mrank1.5 <- lmer(rankChoice ~ ipip + rankEstim2 + satisfied + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrank1.5)
car::Anova(mrank1.5)

mrank2.5 <- lmer(rankChoice ~ ipip + rankEstim2 + satisfied + panas_scared1 + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrank2.5)
car::Anova(mrank2.5)

mrank3.5 <- lmer(rankChoice ~ ipip + rankEstim2*satisfied + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrank3.5)
car::Anova(mrank3.5)

anova(mrank1.5, mrank3.5)
#marginally best model: mrank 3.5

plot(effect("rankEstim2:satisfied",mrank3.5), grid=TRUE)

## BLOCK 6 = 5 and 2
mrank1.6 <- lmer(rankChoice ~ rankEstim2*satisfied + ipip + group1_5 + gameExp + age + gender.y + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrank1.6)
car::Anova(mrank1.6)


mrank2.6 <- lmer(rankChoice ~ rankEstim2*satisfied + ipip + group1_5 + gameExp + age + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrank2.6)
car::Anova(mrank2.6)

anova(mrank1.6, mrank2.6)

#best model mrank 2.6

## BLOCK7 = BLOCKS 1 and 6
mrank1.7 <- lmer(rankChoice ~ rankEstim2*satisfied + ipip + group1_5 + gameExp + age + rankStart*trial + win + score*close + rankStart*rankChoice.minus1*rankChoice.minus2+ rankChoice.minus2*score + rankStart*appleChoice + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrank1.7)
car::Anova(mrank1.7)

mrank2.7 <- lmer(rankChoice ~ rankEstim2*satisfied*ipip + group1_5 + gameExp + age + rankStart*trial + win + score*close + rankStart*rankChoice.minus1*rankChoice.minus2+ rankChoice.minus2*score + rankStart*appleChoice + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrank2.7)
car::Anova(mrank2.7)

anova(mrank1.7, mrank2.7)

mrank3.7 <- lmer(rankChoice ~ rankEstim2*satisfied + group1_5 + gameExp + age + rankStart*trial*ipip + win + score*close + rankStart*rankChoice.minus1*rankChoice.minus2+ rankChoice.minus2*score + rankStart*appleChoice + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrank3.7)
car::Anova(mrank3.7)

anova(mrank2.7, mrank3.7)
# mrank2.7 better model

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