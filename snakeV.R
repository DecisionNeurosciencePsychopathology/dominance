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
########################################


## set working directory (where you have the participants' output data files )
setwd("~/Dropbox/USA/Pittsburgh/GitHub/dominance/snake_data_Vancouver")

# wd for Alex
#setwd("~/code//dominance")

# clear environment
rm(list=ls())


## load datafile and skip to line 126 (use only if data has already been prepared previously, otherwise run lines 64 to 126) 
load("snake_tot.Rda")

## create .Rda file by combining the participants' output files. Skip this part until line 101 if the dataset has already been created)
#the following command reads a table with the participants'ids, which are also their output files' names (the files are named [id].csv); this file should be in the working directory
participants <- read_excel("snake_gameV_participants1.xlsx")

participants <- transform(participants, id = as.factor(id))
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
  prov <- read.csv(participants_full[i,1], header = TRUE, sep = ';')
  prov[1:nrow(prov)-1, (ncol(prov)-22):ncol(prov)] = prov[nrow(prov), (ncol(prov)-22):ncol(prov)]
  prov$n = i
  prov$name <- as.factor(prov$name)
  snake_ds <- bind_rows(snake_ds, prov, .id = NULL)
}

table(snake_ds$n)
snake_ds$ID[snake_ds$n==52] <- '52'
snake_ds$name <- as.factor(snake_ds$name)
snake_ds$ID <- as.factor(snake_ds$ID)
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
snake_suppl <- read_excel("snake_gameV_demogdata_cleaned.xlsx")
snake_suppl2 <- read_excel("snake_gameV_demogdata_cleaned2.xlsx")

summary(snake_suppl)

snake_suppl <- transform(snake_suppl, ID = as.factor(ID), ethnicity = as.factor(ethnicity), ethnicity_other = as.factor(ethnicity_other), english_first_language = as.factor(english_first_language), alcohol_use = as.factor(alcohol_use), drug_use = as.factor(drug_use), psych_treatment = as.factor(psych_treatment), household_income = as.factor(household_income), financially_supporting_myself = as.factor(financially_supporting_myself), financially_supporting_parents = as.factor(financially_supporting_parents), financially_supporting_scholarships = as.factor(financially_supporting_scholarships), financially_supporting_studentloan = as.factor(financially_supporting_studentloan), financially_supporting_other = as.factor(financially_supporting_other))
#snake_suppl <- transform(snake_suppl, cDate = as.Date(cDate), dob = as.Date(dob), baseline_consent_date = as.factor(baseline_consent_date), snake_date = as.factor(snake_date))

save(snake_suppl,file="snake_suppl.Rda")

summary(snake_ds)
snake_tot <- left_join(snake_ds, snake_suppl, by=c("ID"))
summary(snake_tot)
snake_tot$ID <- as.factor(snake_tot$ID)

snake_tot$appleChoice_binary <- NA 
snake_tot$appleChoice_binary[snake_tot$appleChoice == 1] <- 0 
snake_tot$appleChoice_binary[snake_tot$appleChoice > 1] <- 1 
snake_tot$appleChoice_binary <- as.factor(snake_tot$appleChoice_binary)

snake_tot$rankChoice_binary <- NA 
snake_tot$rankChoice_binary[snake_tot$rankChoice == 1] <- 0 
snake_tot$rankChoice_binary[snake_tot$rankChoice > 1] <- 1 
snake_tot$rankChoice_binary <- as.factor(snake_tot$rankChoice_binary)

snake_tot$rankEnd_binary <- NA
snake_tot$rankEnd_binary[snake_tot$rankEnd > 100] <- 0
snake_tot$rankEnd_binary[snake_tot$rankEnd < 101] <- 1
snake_tot$rankEnd_binary <- as.factor(snake_tot$rankEnd_binary)

snake_tot$rankStart_binary <- NA
snake_tot$rankStart_binary[snake_tot$rankStart > 100] <- 0
snake_tot$rankStart_binary[snake_tot$rankStart < 101] <- 1
snake_tot$rankStart_binary <- as.factor(snake_tot$rankStart_binary)



snake_tot <- snake_tot %>% group_by(ID) %>% mutate(rankChoice_binary.minus1 = lag(rankChoice_binary, n=1, order_by=trial),
                                                 rankChoice_binary.minus2 = lag(rankChoice_binary, n=2, order_by=trial),
                                                 appleChoice_binary.minus1 = lag(appleChoice_binary, n=1, order_by = trial),
                                                 appleChoice_binary.minus2 = lag(appleChoice_binary, n=2, order_by = trial),
                                                 rankStart_binary.minus1 = lag(rankStart_binary, n=1, order_by = trial),
                                                 rankStart_binary.minus2 = lag(rankStart_binary, n=2, order_by = trial),
                                                 rankEnd_binary.minus1 = lag(rankEnd_binary, n=1, order_by = trial),
                                                 rankEnd_binary.minus2 = lag(rankEnd_binary, n=2, order_by = trial))
                                                 

#emotional scales
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


#saving processed dataset
save(snake_tot, file="snake_tot.Rda")

table(snake_tot$ID)
table(snake_tot$appleChoice_binary)
table(snake_tot$rankChoice_binary)

# missingness
library(mice)
md.pattern(snake_tot)

library(VIM)
snake_tot_aggr = aggr(snake_tot, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(snake_ds), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
snake_suppl$bpni_TOTAL[snake_tot$ID=='64']

# distribution.
par(mfrow=c(1,1))
summary(snake_tot$gender)
hist(snake_tot$age)
hist(snake_tot$gameExp)
hist(snake_tot$appleChoice,
     main = "Apple stealing",
     xlab = "apple choice",
     ylab = "trials")

hist(snake_tot$rankChoice,
     main = "Booster buying",
     xlab = "booster choice",
     ylab = "trials")
barchart(snake_tot$appleChoice_binary)
barchart(snake_tot$rankChoice_binary)

hist(snake_tot$appleChoiceDelta,
     main = "Delta Apple stealing",
     xlab = "diff. apple choice - previous apple choice",
     ylab = "trials")

hist(snake_tot$rankChoiceDelta,
     main = "Delta Booster buying",
     xlab = "diff. booster choice - previous booster choice",
     ylab = "trials")



hist(snake_suppl$ffni_total, xlab = "scores", main = "FFNI TOTAL", density = 25)
hist(snake_suppl$ffni_GRANDIOSE_NARCISSISM, xlab = "scores", main = "GRANDIOSE N.", density = 25)
hist(snake_suppl$ffni_VULNERABLE_NARCISSISM, xlab = "scores", main = "VULNERABLE N.", density = 25)
hist(snake_suppl$ffni_AGENTIC_EXTRAVERSION, xlab = "scores", main = "AGENTIC E.", density = 25)
hist(snake_suppl$ffni_ANTAGONISM, xlab = "scores", main = "ANTAGONISM", density = 25)
hist(snake_suppl$bpni_TOTAL, xlab = "scores", main = "BPNI", density = 20)
hist(snake_suppl$bpni_GANDIOSITY, xlab = "scores", main = "GRANDIOSITY", density = 20)
hist(snake_suppl$bpni_VULNERABILITY, xlab = "scores", main = "VULNERABILITY", density = 20)

hist(snake_tot$pgsi_total, xlab = "sample)", main = "PGSI", density = 25)
hist(snake_tot$dass21_stress, xlab = "sample)", main = "DASS21-stress", density = 25)
hist(snake_tot$dass21_anxiety, xlab = "sample)", main = "DASS21-anxiety", density = 25)
hist(snake_tot$dass21_depression, xlab = "sample)", main = "DASS21-depression", density = 25)



library(tableone)
#Create a variable list which we want in Table 1
listVars <- c("age","gender","ethnicity","english_first_language","household_income","psych_treatment","dass21_stress","dass21_anxiety","dass21_depression","alcohol_use","drug_use","pgsi_total","bpni_GANDIOSITY","bpni_VULNERABILITY","bpni_TOTAL","ffni_GRANDIOSE_NARCISSISM", "ffni_VULNERABLE_NARCISSISM","ffni_ANTAGONISM","ffni_AGENTIC_EXTRAVERSION")

#Define categorical variables
catVars <- c("gender", "ethnicity", "household_income", "english_first_language","alcohol_use", "drug_use", "psych_treatment")

#Total Population
table1 <- CreateTableOne(vars = listVars, data = snake_suppl2, factorVars = catVars)
table1


library(corrplot)
library(data.table)
#cormat <- corr.test(chars, method = "spearman")
names(snake_suppl)
chars <- snake_suppl[,c(21,18,17,19,20,35,29,34)]
setnames(chars,1:8,c("FFNI total", "FFNI grand", "FFNI vuln", "FFNI antag", "FFNI ag.ex.", "BPNI total", "BPNI grand", "BPNI vuln"))
summary(chars)
corrplot(cor(chars, method = "spearman", use = "na.or.complete"), method = "number")
corrplot.mixed(cor(chars, method = "spearman", use = "na.or.complete"), lower.col = "black", number.cex = 1.1)

#design variables
chars2 <- snake_tot[,c(20,23,27,77,81)]
#setnames(chars,1:8,c("trial", "FFNI grand", "FFNI vuln", "FFNI antag", "FFNI ag.ex.", "BPNI total", "BPNI grand", "BPNI vuln"))
corrplot(cor(chars2, method = "spearman", use = "na.or.complete"), method = "number")
corrplot.mixed(cor(chars2, method = "spearman", use = "na.or.complete"), lower.col = "black", number.cex = 1.1)

# correlation matrix for subject-dependent variables
chars3 <- snake_tot[,c(21,70,30,62,31,64)]
corrplot.mixed(cor(chars3, method = "spearman", use = "na.or.complete"), lower.col = "black", number.cex = 1.1)

#other measures of psychopathology
chars4 <- snake_tot[,c(104,105,106,103,116,102)]
corrplot.mixed(cor(chars4, method = "spearman", use = "na.or.complete"), lower.col = "black", number.cex = 1.1)
