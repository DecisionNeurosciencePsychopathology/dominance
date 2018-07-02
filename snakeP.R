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
# rankStart: player's rank at the end of the round/trial before booster choice (warning: rank 1 = highest!)
# rankEnd: player's rank at the end of the round/trial (after boosters) (warning: rank 1 = highest!)
# mot1-8: motivation questions, assessed on a Lickert scale going from 1 = Strongly disagree to 5 = Strongly agree; 'I wanted to perform as well as I possibly could on the task.', 'Maximizing my personal record of apples eaten was important to me.', 'I wanted to perform better than everyone else on the task.', 'I did not want to perform more poorly than everyone else on the task.', 'Attaining the highest rank among all the competitors was important to me.', 'I wanted to take revenge on people who defeated me.', 'I wanted to avoid performing less than my best on the task.', 'I wanted to ensure that I win.'
# enjoyed: how much participant enjoyed playing on a scale from 1 to 10, 1 = not at all, 10 = extremely
# satisfied: how much participant is satisified with own performance  on a scale from 1 to 10, 1 = not at all, 10 = extremely
# fair: how much participant judged the opponents' behavior as fair
# credible: manipulation check, i.e. how much participant believed that he could control the outcome of the game on a scale from 1 to 10, 1 = not at all, 10 = extremely

# household_income:  1 = a)\tLess than $25,000   2 = b)\t$25,000 - $49,999 3 = c)\t$50,000 - $74,999 4= d)\t$75,000 - $99,999 5 = e)\t$100,000 - $149,999 6 = f)\t$150,000 or above
########################################


## set working directory (where you have the participants' output data files )
setwd("~/Dropbox/USA/Pittsburgh/GitHub/dominance/snake_data_Pittsburgh_may2018")

# wd for Alex
#setwd("~/code//dominance")

# clear environment
rm(list=ls())


## load datafile and skip to line 126 (use only if data has already been prepared previously, otherwise run lines 64 to 126) 
#load("snake_totP.Rda")
#load("snake_totP_shrunk.Rda")

## create .Rda file by combining the participants' output files. Skip this part until line 101 if the dataset has already been created)
#the following command reads a table with the participants'ids, which are also their output files' names (the files are named [id].csv); this file should be in the working directory
participants <- read_excel("snake_data_Pittsburgh.xlsx")

participants <- transform(participants, ID = as.factor(ID))
participants_full <-matrix(nrow = nrow(participants), ncol = 1)
#prov <- matrix

for (i in 1:nrow(participants))
{
  participants_full[i,1] <- paste(participants[i,1], ".csv", sep="")
}

snake_ds <- read.csv(participants_full[1,1], header = TRUE, sep = ';')
snake_ds[1:nrow(snake_ds)-1, (ncol(snake_ds)-22):ncol(snake_ds)] = snake_ds[nrow(snake_ds), (ncol(snake_ds)-22):ncol(snake_ds)]
snake_ds$n <- 1
snake_ds$name <- as.factor(snake_ds$name)

for (i in 2:nrow(participants))
{
  if(participants_full[i,1] %in% list.files())
  {prov <- read.csv(participants_full[i,1], header = TRUE, sep = ';')
  if(ncol(prov) > 1)
    {prov[1:nrow(prov)-1, (ncol(prov)-22):ncol(prov)] = prov[nrow(prov), (ncol(prov)-22):ncol(prov)]}
  else
    {prov <- read.csv(participants_full[i,1], header = TRUE, sep = ',')
    prov[1:nrow(prov)-1, (ncol(prov)-22):ncol(prov)] = prov[nrow(prov), (ncol(prov)-22):ncol(prov)]}
  prov$n = i
  prov$name <- as.factor(prov$name)
  snake_ds <- bind_rows(snake_ds, prov, .id = NULL)
  }
}

summary(snake_ds)

table(snake_ds$n)
snake_ds$name <- as.factor(snake_ds$name)
snake_ds$ID <- as.factor(snake_ds$ID)
save(snake_ds, file="snake_ds.Rda")

# format missing values
#snake_ds[snake_ds=="NaN"] = NA

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
                                
snake_ds$appleChoiceDelta <- snake_ds$appleChoice - snake_ds$appleChoice.minus1
snake_ds$rankChoiceDelta <- snake_ds$rankChoice - snake_ds$rankChoice.minus1
snake_ds$scoreDelta <- snake_ds$score - snake_ds$score.minus1

snake_ds <- snake_ds %>% group_by(ID) %>% mutate(appleChoiceDelta.minus1 = lag(appleChoiceDelta, n=1, order_by=trial),
                                       appleChoiceDelta.minus2 = lag(appleChoiceDelta, n=2, order_by=trial),
                                       rankChoiceDelta.minus1 = lag(rankChoiceDelta, n=1, order_by=trial),
                                       rankChoiceDelta.minus2 = lag(rankChoiceDelta, n=2, order_by=trial),
                                       scoreDelta.minus1 = lag(scoreDelta, n=1, order_by=trial),
                                       scoreDelta.minus2 = lag(scoreDelta, n=2, order_by=trial),
                                       rankEnd.minus1 = lag(rankEnd, n=1, order_by=trial),
                                       rankEnd.minus2 = lag(rankEnd, n=2, order_by=trial),
                                       rankStart.minus1 = lag(rankStart, n=1, order_by=trial),
                                       rankStart.minus2 = lag(rankStart, n=2, order_by=trial))

snake_ds$rankGain_initial <- snake_ds$rankStart - snake_ds$rankEnd.minus1
snake_ds$rankGain_final <- snake_ds$rankEnd - snake_ds$rankEnd.minus1


# preparing and adding the dataset with demographic info and narcissistic questionnaires (hamilton still to be added)
snake_suppl <- read_excel("snake_data_Pittsburgh.xlsx")

summary(snake_suppl)
names(snake_suppl)
snake_suppl <- transform(snake_suppl, ID = as.factor(ID), group1_5 = as.factor(group1_5), group1_7 = as.factor(group1_7), gender = as.factor(gender), ethnicity = as.factor(ethnicity), race = as.factor(race), marital_status = as.factor(marital_status))
snake_suppl <- transform(snake_suppl, snake_date = as.Date(snake_date), dob = as.Date(dob))

save(snake_suppl,file="snake_suppl.Rda")

#reverse-keyed items
snake_suppl$ffni_q19r <- 6 -snake_suppl$ffni_q19
snake_suppl$ffni_q27r <- 6 -snake_suppl$ffni_q27
snake_suppl$ffni_q38r <- 6 -snake_suppl$ffni_q38

#ffni facets
snake_suppl$ffni_acclaim_seeking <- rowSums(snake_suppl[,c("ffni_q1", "ffni_q16", "ffni_q31", "ffni_q46")], na.rm = FALSE)
snake_suppl$ffni_arrogance <- rowSums(snake_suppl[,c("ffni_q2", "ffni_q17", "ffni_q32", "ffni_q47")], na.rm = FALSE)
snake_suppl$ffni_authoritativeness <- rowSums(snake_suppl[,c("ffni_q3", "ffni_q18", "ffni_q33", "ffni_q48")], na.rm = FALSE)
snake_suppl$ffni_distrust <- rowSums(snake_suppl[,c("ffni_q4", "ffni_q19r", "ffni_q34", "ffni_q49")], na.rm = FALSE)
snake_suppl$ffni_entitlement <- rowSums(snake_suppl[,c("ffni_q5", "ffni_q20", "ffni_q35", "ffni_q50")], na.rm = FALSE)
snake_suppl$ffni_exhibitionism <- rowSums(snake_suppl[,c("ffni_q6", "ffni_q21", "ffni_q36", "ffni_q51")], na.rm = FALSE)
snake_suppl$ffni_exploitativeness <- rowSums(snake_suppl[,c("ffni_q7", "ffni_q22", "ffni_q37", "ffni_q52")], na.rm = FALSE)
snake_suppl$ffni_grandiose_fantasies <- rowSums(snake_suppl[,c("ffni_q8", "ffni_q23", "ffni_q38r", "ffni_q53")], na.rm = FALSE)
snake_suppl$ffni_indifference <- rowSums(snake_suppl[,c("ffni_q9", "ffni_q24", "ffni_q39", "ffni_q54")], na.rm = FALSE)
snake_suppl$ffni_lack_of_empathy <- rowSums(snake_suppl[,c("ffni_q10", "ffni_q25", "ffni_q40", "ffni_q55")], na.rm = FALSE)
snake_suppl$ffni_manipulativeness <- rowSums(snake_suppl[,c("ffni_q11", "ffni_q26", "ffni_q41", "ffni_q56")], na.rm = FALSE)
snake_suppl$ffni_need_for_admiration <- rowSums(snake_suppl[,c("ffni_q12", "ffni_q27r", "ffni_q42", "ffni_q57")], na.rm = FALSE)
snake_suppl$ffni_reactive_anger <- rowSums(snake_suppl[,c("ffni_q13", "ffni_q28", "ffni_q43", "ffni_q58")], na.rm = FALSE)
snake_suppl$ffni_shame <- rowSums(snake_suppl[,c("ffni_q14", "ffni_q29", "ffni_q44", "ffni_q59")], na.rm = FALSE)
snake_suppl$ffni_thrill_seeking <- rowSums(snake_suppl[,c("ffni_q15", "ffni_q30", "ffni_q45", "ffni_q60")], na.rm = FALSE)

snake_suppl$ffni_indifference_r <- 24 - snake_suppl$ffni_indifference 

#checking ffni main score and subscores that are already in the dataset
#snake_suppl$ffni_total2 <- rowSums(snake_suppl[,c("ffni_indifference", "ffni_exhibitionism", "ffni_authoritativeness", "ffni_grandiose_fantasies", "ffni_manipulativeness", "ffni_exploitativeness", "ffni_entitlement", "ffni_lack_of_empathy", "ffni_arrogance", "ffni_acclaim_seeking", "ffni_thrill_seeking","ffni_reactive_anger", "ffni_shame", "ffni_need_for_admiration", "ffni_distrust")], na.rm = FALSE)
#snake_suppl$ffni_VULNERABLE_NARCISSISM2 <- rowSums(snake_suppl[,c("ffni_reactive_anger", "ffni_shame", "ffni_need_for_admiration", "ffni_distrust")], na.rm = FALSE)
#snake_suppl$ffni_GRANDIOSE_NARCISSISM2 <- rowSums(snake_suppl[,c("ffni_indifference", "ffni_exhibitionism", "ffni_authoritativeness", "ffni_grandiose_fantasies", "ffni_manipulativeness", "ffni_exploitativeness", "ffni_entitlement", "ffni_lack_of_empathy", "ffni_arrogance", "ffni_acclaim_seeking", "ffni_thrill_seeking")], na.rm = FALSE)

#three-factor model based on Miller et al. 2014
snake_suppl$ffni_ANTAGONISM <- rowSums(snake_suppl[,c("ffni_manipulativeness", "ffni_exploitativeness", "ffni_entitlement", "ffni_lack_of_empathy", "ffni_arrogance", "ffni_reactive_anger", "ffni_distrust", "ffni_thrill_seeking")], na.rm = FALSE)
snake_suppl$ffni_AGENTIC_EXTRAVERSION <- rowSums(snake_suppl[,c("ffni_acclaim_seeking", "ffni_authoritativeness", "ffni_grandiose_fantasies", "ffni_exhibitionism")], na.rm = FALSE)
snake_suppl$ffni_NARCISSISTIC_NEUROTICISM <- rowSums(snake_suppl[,c("ffni_shame", "ffni_indifference_r", "ffni_need_for_admiration")], na.rm = FALSE)

#checkig for missingness in narcissistic scales
snake_suppl$ID[is.na(snake_suppl$ffni_total)]
snake_suppl$ID[is.na(snake_suppl$bpni_TOTAL)]
snake_suppl$ID[is.na(snake_suppl$ipip_total)]

summary(snake_ds)
snake_totP <- left_join(snake_ds, snake_suppl, by=c("ID"))
summary(snake_totP)
snake_totP$ID <- as.factor(snake_totP$ID)

#set up binary outcome variables (not used in current analysis)
# snake_totP$appleChoice_binary <- NA 
# snake_totP$appleChoice_binary[snake_totP$appleChoice == 1] <- 0 
# snake_totP$appleChoice_binary[snake_totP$appleChoice > 1] <- 1 
# snake_totP$appleChoice_binary <- as.factor(snake_totP$appleChoice_binary)
# 
# snake_totP$rankChoice_binary <- NA 
# snake_totP$rankChoice_binary[snake_totP$rankChoice == 1] <- 0 
# snake_totP$rankChoice_binary[snake_totP$rankChoice > 1] <- 1 
# snake_totP$rankChoice_binary <- as.factor(snake_totP$rankChoice_binary)
# 
# snake_totP$rankEnd_binary <- NA
# snake_totP$rankEnd_binary[snake_totP$rankEnd > 100] <- 0
# snake_totP$rankEnd_binary[snake_totP$rankEnd < 101] <- 1
# snake_totP$rankEnd_binary <- as.factor(snake_totP$rankEnd_binary)
# 
# snake_totP$rankStart_binary <- NA
# snake_totP$rankStart_binary[snake_totP$rankStart > 100] <- 0
# snake_totP$rankStart_binary[snake_totP$rankStart < 101] <- 1
# snake_totP$rankStart_binary <- as.factor(snake_totP$rankStart_binary)

# snake_totP <- snake_totP %>% group_by(ID) %>% mutate(rankChoice_binary.minus1 = lag(rankChoice_binary, n=1, order_by=trial),
#                                                  rankChoice_binary.minus2 = lag(rankChoice_binary, n=2, order_by=trial),
#                                                  appleChoice_binary.minus1 = lag(appleChoice_binary, n=1, order_by = trial),
#                                                  appleChoice_binary.minus2 = lag(appleChoice_binary, n=2, order_by = trial),
#                                                  rankStart_binary.minus1 = lag(rankStart_binary, n=1, order_by = trial),
#                                                  rankStart_binary.minus2 = lag(rankStart_binary, n=2, order_by = trial),
#                                                  rankEnd_binary.minus1 = lag(rankEnd_binary, n=1, order_by = trial),
#                                                  rankEnd_binary.minus2 = lag(rankEnd_binary, n=2, order_by = trial))
                                                 
snake_totP$panas_pos1 <- rowSums(snake_totP[, c(10,14,16,19)], na.rm = FALSE)
snake_totP$panas_pos2 <- rowSums(snake_totP[, c(32,36,38,41)], na.rm = FALSE)
snake_totP$panas_scared1 <- rowSums(snake_totP[, c(11,12,17,18)], na.rm = FALSE)
snake_totP$panas_scared2 <- rowSums(snake_totP[, c(33,34,39,40)], na.rm = FALSE)
snake_totP$panas_angry1 <- rowSums(snake_totP[, c(13,15)], na.rm = FALSE)
snake_totP$panas_angry2 <- rowSums(snake_totP[, c(35,37)], na.rm = FALSE)

snake_totP$panas_angry1b <- rowSums(snake_totP[, c(11,13,15)], na.rm = FALSE)
snake_totP$panas_angry2b <- rowSums(snake_totP[, c(33,35,37)], na.rm = FALSE)

snake_totP$delta_rankEstim <-snake_totP$rankEstim2-snake_totP$rankEstim1

snake_totP$delta_panas_angry <-snake_totP$panas_angry2-snake_totP$panas_angry1
snake_totP$delta_panas_scared <-snake_totP$panas_scared2-snake_totP$panas_scared1 
snake_totP$delta_panas_pos <-snake_totP$panas_pos2-snake_totP$panas_pos1

snake_totP$delta_panas_angry_b <-snake_totP$panas_angry2b-snake_totP$panas_angry1b

#adding within- and between-subject means:
scale_this <- function(x) as.vector(scale(x))


snake_totP = snake_totP %>% group_by(ID) %>% 
  mutate(
    appleChoice_wi = scale_this(appleChoice),
    appleChoice_b = mean(appleChoice, na.rm = TRUE),
    rankChoice_wi = scale_this(rankChoice),
    rankChoice_b = mean(rankChoice, na.rm = TRUE)
  ) %>% ungroup()



# …_wi — within-subject
# …_b — between-subject

snake_totP$appleChoice_b_logit <- psych::logit(snake_totP$appleChoice_b/6.25)
snake_totP$rankChoice_b_logit <- psych::logit(snake_totP$rankChoice_b/6.25)

snake_totP$appleChoice_wi_0 <- snake_totP$appleChoice_wi
snake_totP$appleChoice_wi_0[is.nan(snake_totP$appleChoice_wi_0)] <- 0

snake_totP$rankChoice_wi_0 <- snake_totP$rankChoice_wi
snake_totP$rankChoice_wi_0[is.nan(snake_totP$rankChoice_wi_0)] <- 0


snake_totP <- snake_totP %>% group_by(ID) %>% mutate(rankChoice_wi_0.minus1 = lag(rankChoice_wi_0, n=1, order_by=trial),
                                                   rankChoice_wi_0.minus2 = lag(rankChoice_wi_0, n=2, order_by=trial),
                                                   appleChoice_wi_0.minus1 = lag(appleChoice_wi_0, n=1, order_by = trial),
                                                   appleChoice_wi_0.minus2 = lag(appleChoice_wi_0, n=2, order_by = trial))


#saving processed dataset
save(snake_totP, file="snake_totP.Rda")

# creating a shrunk dataset for between-subject mean analysis.
library(data.table)

drops <- c("score","oppScore","scoreDiff","win","close","oppName","oppRank","rankStart","rankEnd","score.minus1", "score.minus2", "win.minus1", "win.minus2", "scoreDiff.minus1", "scoreDiff.minus2", "appleChoice.minus1", "appleChoice.minus2", "rankChoice.minus1", "rankChoice.minus2", "close.minus1", "close.minus2", "appleChoiceDelta", "rankChoiceDelta", "scoreDelta", "appleChoiceDelta.minus1",
           "appleChoiceDelta.minus2", "rankChoiceDelta.minus1", "rankChoiceDelta.minus2", "scoreDelta.minus1", "scoreDelta.minus2", "rankEnd.minus1", "rankEnd.minus2", "rankStart.minus1", "rankStart.minus2", "rankGain_initial", "rankGain_final", "appleChoice_binary", "rankChoice_binary", "rankEnd_binary", "rankStart_binary", "rankChoice_binary.minus1", "rankChoice_binary.minus2", "appleChoice_binary.minus1",
           "appleChoice_binary.minus2", "rankStart_binary.minus1", "rankStart_binary.minus2", "rankEnd_binary.minus1", "rankEnd_binary.minus2", "appleChoice", "rankChoice", "appleChoice_wi", "rankChoice_wi", "rankChoice_wi_0.minus1", "rankChoice_wi_0.minus2", "appleChoice_wi_0.minus1", "appleChoice_wi_0.minus2")
snake_totP_shrunk <- snake_totP[ , !(names(snake_totP) %in% drops)]
snake_totP_shrunk <- as.data.table(snake_totP_shrunk)
snake_totP_shrunk <-dcast.data.table(snake_totP_shrunk, ... ~ trial, value.var = c("appleChoice_wi_0", "rankChoice_wi_0"))
View(snake_totP_shrunk)

save(snake_totP_shrunk, file="snake_totP_shrunk.Rda")

# missingness
library(mice)
md.pattern(snake_totP)

library(VIM)
snake_totP_aggr = aggr(snake_totP_shrunk, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(snake_ds), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
snake_suppl$bpni_TOTAL[snake_totP$ID=='64']

# distribution.
par(mfrow=c(2,3))
summary(snake_totP$gender)
hist(snake_totP$age)
hist(snake_totP$gameExp)
hist(snake_totP$appleChoice,
     main = "Apple stealing",
     xlab = "apple choice",
     ylab = "trials")

hist(snake_totP$rankChoice,
     main = "Paying for rank",
     xlab = "rank choice",
     ylab = "trials")

hist(snake_totP$appleChoiceDelta,
     main = "Delta Apple stealing",
     xlab = "apple choice - previous apple choice",
     ylab = "trials")

hist(snake_totP$rankChoiceDelta,
     main = "Delta Booster buying",
     xlab = "rank choice - previous rank choice",
     ylab = "trials")

hist(snake_totP$appleChoice_b,
     main = "Between-subject mean",
     xlab = "mean appleChoice",
     ylab = "trials")

hist(snake_totP$rankChoice_b,
     main = "Between-subject mean",
     xlab = "mean rankChoice",
     ylab = "trials")

hist(snake_totP_shrunk$appleChoice_b_logit,
     main = "Between-subject mean (logit)",
     xlab = "mean appleChoice",
     ylab = "trials")

hist(snake_totP_shrunk$rankChoice_b_logit,
     main = "Between-subject mean (logit)",
     xlab = "mean rankChoice",
     ylab = "trials")

hist(snake_totP$appleChoice_wi_0,
     main = "Within-subject mean",
     xlab = "mean appleChoice",
     ylab = "trials")

hist(snake_totP$rankChoice_wi_0,
     main = "Within-subject mean",
     xlab = "mean rankChoice",
     ylab = "trials")


hist(snake_suppl$ffni_total, xlab = "scores", main = "FFNI TOTAL", density = 25)
hist(snake_suppl$ffni_GRANDIOSE_NARCISSISM, xlab = "scores", main = "GRANDIOSE N.", density = 25)
hist(snake_suppl$ffni_VULNERABLE_NARCISSISM, xlab = "scores", main = "VULNERABLE N.", density = 25)
hist(snake_suppl$ffni_AGENTIC_EXTRAVERSION, xlab = "scores", main = "AGENTIC E.", density = 25)
hist(snake_suppl$ffni_NARCISSISTIC_NEUROTICISM, xlab = "scores", main = "N. NEUROTICISM", density = 25)
hist(snake_suppl$ffni_ANTAGONISM, xlab = "scores", main = "ANTAGONISM", density = 25)
hist(snake_suppl$bpni_TOTAL, xlab = "scores", main = "BPNI TOTAL", density = 20)
hist(snake_suppl$bpni_GANDIOSITY, xlab = "scores", main = "GRANDIOSITY", density = 20)
hist(snake_suppl$bpni_VULNERABILITY, xlab = "scores", main = "VULNERABILITY", density = 20)
hist(snake_totP$ipip_total, xlab = "sample)", main = "IPIP TOTAL", density = 25)


library(tableone)
#Create a variable list which we want in Table 1
listVars <- c("age","gender.y","ethnicity","education","group1_5","group1_7","ipip_total","bpni_GANDIOSITY","bpni_VULNERABILITY","bpni_TOTAL","ffni_GRANDIOSE_NARCISSISM", "ffni_VULNERABLE_NARCISSISM","ffni_ANTAGONISM","ffni_AGENTIC_EXTRAVERSION","ffni_NARCISSISTIC_NEUROTICISM")

#Define categorical variables
catVars <- c("gender.y", "ethnicity", "group1_5", "group1_7")

#Total Population
table1 <- CreateTableOne(vars = listVars, data = snake_totP_shrunk, factorVars = catVars)
table1


library(compareGroups)
chars <- snake_totP_shrunk[,c("age","gender.y","race","education","group1_5","group1_7","ipip_total","bpni_GANDIOSITY","bpni_VULNERABILITY","bpni_TOTAL","ffni_GRANDIOSE_NARCISSISM", "ffni_VULNERABLE_NARCISSISM","ffni_ANTAGONISM","ffni_AGENTIC_EXTRAVERSION","ffni_NARCISSISTIC_NEUROTICISM")]
# describe.by(chars,group = df$group_early_no_break)
c <- compareGroups(chars,snake_totP_shrunk$group1_5, show.descr = TRUE)
tc <- createTable(c, hide.no = 0, digits = 1, show.p.mul = TRUE)
tc
export2html(tc, "Table1.html")

par(mfrow=c(1,1))

library(corrplot)
library(data.table)
cor(snake_totP_shrunk$appleChoice_b, snake_totP_shrunk$rankChoice_b, method = 'spearman')

#cormat <- corr.test(chars, method = "spearman")
names(snake_totP)
chars <- snake_totP[,c(170,173,171,172,156,157,158,193:195)]
#setnames(chars,1:8,c("FFNI total", "FFNI grand", "FFNI vuln", "FFNI antag", "FFNI ag.ex.", "BPNI total", "BPNI grand", "BPNI vuln"))

corrplot(cor(chars, method = "spearman", use = "na.or.complete"))
corrplot.mixed(cor(chars, method = "spearman", use = "na.or.complete"), lower.col = "black", number.cex = 1.1)

#design variables
chars2 <- snake_totP[,c(20,23,27,77,81)]
#setnames(chars,1:8,c("trial", "FFNI grand", "FFNI vuln", "FFNI antag", "FFNI ag.ex.", "BPNI total", "BPNI grand", "BPNI vuln"))
corrplot(cor(chars2, method = "spearman", use = "na.or.complete"), method = "number")
corrplot.mixed(cor(chars2, method = "spearman", use = "na.or.complete"), lower.col = "black", number.cex = 1.1)

# correlation matrix for subject-dependent variables
chars3 <- snake_totP[,c(21,70,30,31,77,170,173,156)]
corrplot.mixed(cor(chars3, method = "spearman", use = "na.or.complete"), lower.col = "black", number.cex = 1.1)

#other measures of psychopathology
#chars4 <- snake_totP[,c(104,105,106,103,116,102)]
#corrplot.mixed(cor(chars4, method = "spearman", use = "na.or.complete"), lower.col = "black", number.cex = 1.1)

chars5 <- snake_totP[,c(203:205,170,173,156)]
corrplot.mixed(cor(chars5, method = "spearman", use = "na.or.complete"), lower.col = "black", number.cex = 1.1)

