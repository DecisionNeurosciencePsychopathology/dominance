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
setwd("~/Dropbox/USA/Pittsburgh/GitHub/dominance/snake_data_Vancouver")

# wd for Alex
#setwd("~/code//dominance")

# clear environment
rm(list=ls())


## load datafile and skip to line 126 (use only if data has already been prepared previously, otherwise run lines 64 to 126) 
load("snake_tot.Rda")
load("snake_tot_shrunk.Rda")

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

summary(snake_suppl)

snake_suppl <- transform(snake_suppl, ID = as.factor(ID), ethnicity = as.factor(ethnicity), ethnicity_other = as.factor(ethnicity_other), english_first_language = as.factor(english_first_language), alcohol_use = as.factor(alcohol_use), drug_use = as.factor(drug_use), psych_treatment = as.factor(psych_treatment), household_income = as.factor(household_income), financially_supporting_myself = as.factor(financially_supporting_myself), financially_supporting_parents = as.factor(financially_supporting_parents), financially_supporting_scholarships = as.factor(financially_supporting_scholarships), financially_supporting_studentloan = as.factor(financially_supporting_studentloan), financially_supporting_other = as.factor(financially_supporting_other))
#snake_suppl <- transform(snake_suppl, cDate = as.Date(cDate), dob = as.Date(dob), baseline_consent_date = as.factor(baseline_consent_date), snake_date = as.factor(snake_date))
snake_suppl$household_income <- as.numeric(snake_suppl$household_income)

save(snake_suppl,file="snake_suppl.Rda")

summary(snake_ds)
snake_tot <- left_join(snake_ds, snake_suppl, by=c("ID"))
summary(snake_tot)
snake_tot$ID <- as.factor(snake_tot$ID)

#computing narcissistic neuroticism
snake_tot$ffni_indifference_rev <- 24-snake_tot$ffni_indiference
snake_tot$ffni_NARCISSISTIC_NEUROTICISM <- rowSums(snake_tot[,c('ffni_shame', 'ffni_indifference_rev', 'ffni_need_for_admiration')], na.rm = FALSE)

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


# #add person_level means
# person_level_mean_apple <- aggregate(appleChoice ~ ID, data = snake_tot, mean)
# #View(person_level_mean_apple)
# person_level_mean_apple$ID <- as.factor(person_level_mean_apple$ID)
# 
# person_level_mean_rank <- aggregate(rankChoice ~ ID, data = snake_tot, mean)
# #View(person_level_mean_rank)
# person_level_mean_rank$ID <- as.factor(person_level_mean_rank$ID)
# 
# colnames(person_level_mean_apple)[2] <- "appleChoice_mean"
# colnames(person_level_mean_rank)[2] <- "rankChoice_mean"
# 
# snake_tot <- left_join(snake_tot, person_level_mean_apple, by=c("ID"))
# snake_tot <- left_join(snake_tot, person_level_mean_rank, by=c("ID"))
# 
# summary(snake_tot)
# View(snake_tot$appleChoice_mean)
# View(snake_tot$rankChoice_mean)
# View(snake_tot$ID)

scale_this <- function(x) as.vector(scale(x))


snake_tot = snake_tot %>% group_by(ID) %>% 
  mutate(
    appleChoice_wi = scale_this(appleChoice),
    appleChoice_b = mean(appleChoice, na.rm = TRUE),
    rankChoice_wi = scale_this(rankChoice),
    rankChoice_b = mean(rankChoice, na.rm = TRUE)
  ) %>% ungroup()



# …_wi — within-subject
# …_b — between-subject

snake_tot$appleChoice_b_logit <- psych::logit(snake_tot$appleChoice_b/6.25)
snake_tot$rankChoice_b_logit <- psych::logit(snake_tot$rankChoice_b/6.25)

snake_tot$appleChoice_wi_0 <- snake_tot$appleChoice_wi
snake_tot$appleChoice_wi_0[is.nan(snake_tot$appleChoice_wi_0)] <- 0

snake_tot$rankChoice_wi_0 <- snake_tot$rankChoice_wi
snake_tot$rankChoice_wi_0[is.nan(snake_tot$rankChoice_wi_0)] <- 0


snake_tot <- snake_tot %>% group_by(ID) %>% mutate(rankChoice_wi_0.minus1 = lag(rankChoice_wi_0, n=1, order_by=trial),
                                                   rankChoice_wi_0.minus2 = lag(rankChoice_wi_0, n=2, order_by=trial),
                                                   appleChoice_wi_0.minus1 = lag(appleChoice_wi_0, n=1, order_by = trial),
                                                   appleChoice_wi_0.minus2 = lag(appleChoice_wi_0, n=2, order_by = trial))
                                                 
#saving processed dataset
save(snake_tot, file="snake_tot.Rda")

# creating a shrunk dataset for between-subject mean analysis.
library(data.table)

drops <- c("score","oppScore","scoreDiff","win","close","oppName","oppRank","rankStart","rankEnd","score.minus1", "score.minus2", "win.minus1", "win.minus2", "scoreDiff.minus1", "scoreDiff.minus2", "appleChoice.minus1", "appleChoice.minus2", "rankChoice.minus1", "rankChoice.minus2", "close.minus1", "close.minus2", "appleChoiceDelta", "rankChoiceDelta", "scoreDelta", "appleChoiceDelta.minus1",
           "appleChoiceDelta.minus2", "rankChoiceDelta.minus1", "rankChoiceDelta.minus2", "scoreDelta.minus1", "scoreDelta.minus2", "rankEnd.minus1", "rankEnd.minus2", "rankStart.minus1", "rankStart.minus2", "rankGain_initial", "rankGain_final", "appleChoice_binary", "rankChoice_binary", "rankEnd_binary", "rankStart_binary", "rankChoice_binary.minus1", "rankChoice_binary.minus2", "appleChoice_binary.minus1",
           "appleChoice_binary.minus2", "rankStart_binary.minus1", "rankStart_binary.minus2", "rankEnd_binary.minus1", "rankEnd_binary.minus2", "appleChoice_wi", "rankChoice_wi", "appleChoice_wi_0", "rankChoice_wi_0", "appleChoice_wi_0.minus1", "appleChoice_wi_0.minus2", "rankChoice_wi_0.minus1", "rankChoice_wi_0.minus2")
snake_tot_shrunk <- snake_tot[ , !(names(snake_tot) %in% drops)]
snake_tot_shrunk <- as.data.table(snake_tot_shrunk)
snake_tot_shrunk <-dcast.data.table(snake_tot_shrunk, ... ~ trial, value.var = c("appleChoice", "rankChoice"))
View(snake_tot_shrunk)

save(snake_tot_shrunk, file="snake_tot_shrunk.Rda")

# 
# #creating a dataset with demographic variables and mean scores, and without repeated measures
# snake_suppl2 <- read_excel("snake_gameV_demogdata_cleaned2.xlsx")
# snake_suppl2$ID <- as.factor(snake_suppl2$ID)
# 
# snake_suppl_mean <- left_join(snake_suppl2, person_level_mean_apple, by=c("ID"))
# snake_suppl_mean$ID <- as.factor(snake_suppl_mean$ID)
# snake_suppl_mean <- left_join(snake_suppl_mean, person_level_mean_rank, by=c("ID"))
# #View(snake_suppl_mean)
# #table(snake_suppl_mean$ID)
# 
# snake_suppl_mean$appleChoice_mean_logit <- psych::logit(snake_suppl_mean$appleChoice_mean/6.25)
# hist(snake_suppl_mean$appleChoice_mean_logit)
# 
# snake_suppl_mean$rankChoice_mean_logit <- psych::logit(snake_suppl_mean$rankChoice_mean/6.25)
# hist(snake_suppl_mean$rankChoice_mean_logit)
# 
# save(snake_suppl_mean, file="snake_suppl_mean.Rda")
# 
# table(snake_tot$ID)
# table(snake_tot$appleChoice_binary)
# table(snake_tot$rankChoice_binary)

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
     main = "Apple Choice",
     xlab = "choice",
     ylab = "trials")

hist(snake_tot$rankChoice,
     main = "Rank Choice",
     xlab = "choice",
     ylab = "trials")
barchart(snake_tot$appleChoice_binary)
barchart(snake_tot$rankChoice_binary)

hist(snake_tot$appleChoiceDelta,
     main = "Delta Apple stealing",
     xlab = "apple choice - previous apple choice",
     ylab = "trials")

hist(snake_tot$rankChoiceDelta,
     main = "Delta Booster buying",
     xlab = "rank choice - previous rank choice",
     ylab = "trials")

hist(snake_tot$appleChoice_b,
     main = "Between-subject mean",
     xlab = "mean appleChoice",
     ylab = "trials")

hist(snake_tot$rankChoice_b,
     main = "Between-subject mean",
     xlab = "mean rankChoice",
     ylab = "trials")

hist(snake_tot_shrunk$appleChoice_b_logit,
     main = "Between-subject mean (logit)",
     xlab = "mean appleChoice",
     ylab = "trials")

hist(snake_tot_shrunk$rankChoice_b_logit,
     main = "Between-subject mean (logit)",
     xlab = "mean rankChoice",
     ylab = "trials")

hist(snake_tot$appleChoice_wi_0,
     main = "Within-subject mean",
     xlab = "mean appleChoice",
     ylab = "trials")

hist(snake_tot$rankChoice_wi_0,
     main = "Within-subject mean",
     xlab = "mean rankChoice",
     ylab = "trials")


hist(snake_suppl$ffni_total, xlab = "scores", main = "FFNI TOTAL", density = 25)
hist(snake_suppl$ffni_GRANDIOSE_NARCISSISM, xlab = "scores", main = "GRANDIOSE N.", density = 25)
hist(snake_suppl$ffni_VULNERABLE_NARCISSISM, xlab = "scores", main = "VULNERABLE N.", density = 25)
hist(snake_suppl$ffni_AGENTIC_EXTRAVERSION, xlab = "scores", main = "AGENTIC E.", density = 25)
hist(snake_suppl$ffni_ANTAGONISM, xlab = "scores", main = "ANTAGONISM", density = 25)
hist(snake_suppl$bpni_TOTAL, xlab = "scores", main = "BPNI", density = 20)
hist(snake_suppl$bpni_GANDIOSITY, xlab = "scores", main = "GRANDIOSITY", density = 20)
hist(snake_suppl$bpni_VULNERABILITY, xlab = "scores", main = "VULNERABILITY", density = 20)

hist(snake_tot_shrunk$pgsi_total, xlab = "sample)", main = "PGSI", density = 25)
hist(snake_tot_shrunk$dass21_stress, xlab = "sample)", main = "DASS21-stress", density = 25)
hist(snake_tot_shrunk$dass21_anxiety, xlab = "sample)", main = "DASS21-anxiety", density = 25)
hist(snake_tot_shrunk$dass21_depression, xlab = "sample)", main = "DASS21-depression", density = 25)

hist(snake_tot$appleChoice[snake_tot$win == '1'])
hist(snake_tot$rankChoice[snake_tot$win == '1'])
hist(snake_tot$appleChoice[snake_tot$win == '0'])
hist(snake_tot$rankChoice[snake_tot$win == '0'])

hist(snake_tot$appleChoice_wi_0)
hist(snake_tot$rankChoice_wi_0)
hist(snake_tot_shrunk$appleChoice_b_logit)
hist(snake_tot_shrunk$rankChoice_b_logit)


papple <- ggplot(snake_tot,aes(trial,appleChoice)) + geom_line() + facet_wrap(~ID)
prank <- ggplot(snake_tot,aes(trial,rankChoice)) + geom_line() + facet_wrap(~ID)

library(tableone)
#Create a variable list which we want in Table 1
listVars <- c("age","gender","ethnicity","english_first_language","household_income","psych_treatment","dass21_stress","dass21_anxiety","dass21_depression","alcohol_use","drug_use","pgsi_total","bpni_GANDIOSITY","bpni_VULNERABILITY","bpni_TOTAL","ffni_GRANDIOSE_NARCISSISM", "ffni_VULNERABLE_NARCISSISM","ffni_ANTAGONISM","ffni_AGENTIC_EXTRAVERSION")

#Define categorical variables
catVars <- c("gender", "ethnicity", "household_income", "english_first_language","alcohol_use", "drug_use", "psych_treatment")

#Total Population
table1 <- CreateTableOne(vars = listVars, data = snake_suppl2, factorVars = catVars)
table1

#Create a variable list which we want in Table 1
library(compareGroups)
chars <- snake_tot_shrunk[,c("age","gender","ethnicity","household_income","bpni_GANDIOSITY","bpni_VULNERABILITY","bpni_TOTAL","ffni_GRANDIOSE_NARCISSISM", "ffni_VULNERABLE_NARCISSISM","ffni_ANTAGONISM","ffni_AGENTIC_EXTRAVERSION","ffni_NARCISSISTIC_NEUROTICISM","ffni_VULNERABLE_NARCISSISM","ffni_total")]
# describe.by(chars,group = df$group_early_no_break)
c <- compareGroups(chars, show.descr = TRUE)
tc <- createTable(c, hide.no = 0, digits = 1, show.p.mul = TRUE)
tc
export2html(tc, "Table1.html")


library(corrplot)
library(data.table)
cor(snake_tot_shrunk$appleChoice_b, snake_tot_shrunk$rankChoice_b, method = 'spearman')

#cormat <- corr.test(chars, method = "spearman")
names(snake_tot_shrunk)
chars <- snake_tot_shrunk[,c(63,60,59,61,62,98,77,71,76)]
setnames(chars,1:8,c("FFNI total", "FFNI grand", "FFNI vuln", "FFNI antag", "FFNI ag.ex.", "FFNI n.neur.", "BPNI total", "BPNI grand", "BPNI vuln"))
summary(chars)
corrplot(cor(chars, method = "spearman", use = "na.or.complete"))
corrplot.mixed(cor(chars, method = "spearman", use = "na.or.complete"), lower.col = "black", number.cex = 1.1)

names(snake_tot_shrunk)
chars <- snake_tot_shrunk[,c(63,60,59,77,71,76)]
setnames(chars,1:8,c("FFNI total", "FFNI grand", "FFNI vuln", "BPNI total", "BPNI grand", "BPNI vuln"))
summary(chars)
corrplot(cor(chars, method = "spearman", use = "na.or.complete"))
corrplot.mixed(cor(chars, method = "spearman", use = "na.or.complete"), lower.col = "black", number.cex = 1.1)

#design variables
chars2 <- snake_tot[,c(20,23,27,81)]
#setnames(chars,1:8,c("trial", "FFNI grand", "FFNI vuln", "FFNI antag", "FFNI ag.ex.", "BPNI total", "BPNI grand", "BPNI vuln"))
corrplot(cor(chars2, method = "spearman", use = "na.or.complete"), method = "number")
corrplot.mixed(cor(chars2, method = "spearman", use = "na.or.complete"), lower.col = "black", number.cex = 1.1)

# correlation matrix for subject-dependent variables
chars3 <- snake_tot[,c(21,70,30,31,77,116,102)]
corrplot.mixed(cor(chars3, method = "spearman", use = "na.or.complete"), lower.col = "black", number.cex = 1.1)

#other measures of psychopathology
chars4 <- snake_tot[,c(104,105,106,103,116,102)]
corrplot.mixed(cor(chars4, method = "spearman", use = "na.or.complete"), lower.col = "black", number.cex = 1.1)

chars5 <- snake_tot[,c(155:157,116,102)]
corrplot.mixed(cor(chars5, method = "spearman", use = "na.or.complete"), lower.col = "black", number.cex = 1.1)

#check the correlation with rankChoiceDelta and rankChoice_binary
chars_weird <- snake_tot[,c(69, 31, 116, 115, 110, 102, 98, 99, 100, 101)]
corrplot(cor(chars_weird, method = "spearman", use = "na.or.complete"), method = "number", number.cex = 1.1)

chars_weird2 <- snake_tot[,c(68, 30, 116, 115, 110, 102, 98, 99, 100, 101)]
corrplot.mixed(cor(chars_weird2, method = "spearman", use = "na.or.complete"), lower.col = "black", number.cex = 1.1)

cor(chars_weird2, method = "spearman", use = "complete.obs")
corr.test(chars_weird2, method = "spearman", use = "complete.obs")
corr.test(chars_weird, method = "spearman", use = "complete.obs")

##########################################################################################################################################
##PLOT FOR NEUROCON:
setwd("~/code/dominance/snake_data_Vancouver")
load("snake_tot.Rda")
load("snake_tot_shrunk.Rda")

setwd("~/code/dominance/snake_data_Pittsburgh_may2018")
load("snake_totP.Rda")
load("snake_totP_shrunk.Rda")

setwd("~/code/GitHub/dominance/neurocon2018")

#Best model with design variables:
#-Vancouver
# mappleA2_V <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(201-oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
# summary(mappleA2_V)
# car::Anova(mappleA2_V, type = 'III')
# 
# library(sjPlot)
# pV_design_apple <- plot_model(mappleA2_V, type = "est", show.p = TRUE, show.values = TRUE, title = "Sample 2 - undergrads - stealing", order.terms = c(1,4,5,7,3,2,6), colors = c("#008600", "#c55a11"), show.legend = FALSE)
# 
# mrankA2_V <- lmer(rankChoice_wi_0 ~ win + scale(scoreDelta) + scale(rankStart) + scale(trial)*scale(201-oppRank) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
# summary(mrankA2_V)
# car::Anova(mrankA2_V, type = 'III')
# 
# library(sjPlot)
# pV_design_rank <- plot_model(mrankA2_V, type = "est", show.p = TRUE, show.values = TRUE, title = "Sample 2 - undergrads - rank-buying", order.terms = c(4,5,7,2,3,1,6), colors = c("#008600","#c55a11"), show.legend = FALSE)

stats::cor(as.numeric(snake_totP_shrunk$ipip_total), as.numeric(snake_totP_shrunk$bpni_TOTAL), method = 'spearman', use = "pairwise.complete.obs")
stats::cor(as.numeric(snake_totP_shrunk$ipip_total), as.numeric(snake_totP_shrunk$ffni_total), method = 'spearman', use = "pairwise.complete.obs")

#sensitivity
library(plyr)
snake_tot$ethnicity_simp <- revalue(snake_tot$ethnicity, c("Asian"='asian',"Black or African Canadian"='black',"Other"='other',"White/Caucasian"='caucas'))

mappleA2_V <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(201-oppRank)*scale(score.minus1) + scale(rankEnd.minus1)  + scale(age) + scale(household_income) + ethnicity_simp + gender + scale(gameExp) + scale(dass21_depression) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA2_V)
car::Anova(mappleA2_V, type = 'III')

mrankA2_V <- lmer(rankChoice_wi_0 ~ win + scale(scoreDelta) + scale(rankStart) + scale(trial)*scale(201-oppRank) + scale(rankChoice_wi_0.minus1) + scale(age) + scale(household_income) + ethnicity_simp + gender + scale(gameExp) + scale(dass21_depression) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_V)
car::Anova(mrankA2_V, type = 'III')

library(sjPlot)
pV_design_apple <- plot_model(mappleA2_V, type = "est", show.p = TRUE, show.values = TRUE, title = "Sample 2 - undergrads - stealing",
                              rm.terms = c("scale(age)", "scale(dass21_depression)", "scale(gameExp)", "scale(household_income)", "ethnicity_simpblack", "ethnicity_simpother","ethnicity_simpcaucas", "gender2"),
                              order.terms = c(1,4,5,7,3,2,6),
                              colors = c("#008600", "#c55a11"), show.legend = FALSE) + theme_bw()

library(sjPlot)
pV_design_rank <- plot_model(mrankA2_V, type = "est", show.p = TRUE, show.values = TRUE, title = "Sample 2 - undergrads - rank-buying",
                             rm.terms = c("scale(age)", "scale(dass21_depression)", "scale(gameExp)", "scale(household_income)", "ethnicity_simpblack", "ethnicity_simpother","ethnicity_simpcaucas", "gender2"),
                             order.terms = c(4,5,7,2,3,1,6), colors = c("#008600","#c55a11"), show.legend = FALSE) + theme_bw()

#-Pittsburgh
#best model: same as for Vancouver dataset
# mappleA2_P <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(201-oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
# summary(mappleA2_P)
# car::Anova(mappleA2_P, type = 'III')
# 
# library(sjPlot)
# pP_design_apple <- plot_model(mappleA2_P, show.p = TRUE, show.values = TRUE, title = "Sample 1 - elderly - stealing", order.terms = c(1,4,5,7,3,2,6), colors = c("#008600","#c55a11"))
# 
# mrankA0_P <- lmer(rankChoice_wi_0 ~ scale(trial)*scale(201-oppRank) + win + scale(rankStart) + scale(scoreDelta) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
# summary(mrankA0_P)
# car::Anova(mrankA0_P, type = 'III')
# 
# library(sjPlot)
# pP_design_rank <- plot_model(mrankA0_P, type = "est", show.p = TRUE, show.values = TRUE, title = "Sample 1 - elderly- rank-buying", order.terms = c(1,2,7,5,4,3,6), colors = c( "#008600","#c55a11"))

#sensitivity:
mappleA2_P <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(201-oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + scale(age_snake) + gender.y + scale(education) + race + scale(gameExp) + scale(household_income_log) + scale(HRSD_no_suic) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA2_P)
car::Anova(mappleA2_P, type = 'III')

mrankA0_P <- lmer(rankChoice_wi_0 ~ scale(trial)*scale(201-oppRank) + win + scale(rankStart) + scale(scoreDelta) + scale(rankChoice_wi_0.minus1)  + scale(age_snake) + gender.y + scale(education) + race + scale(gameExp) + scale(household_income_log) + scale(HRSD_no_suic) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA0_P)
car::Anova(mrankA0_P, type = 'III')

pP_design_apple <- plot_model(mappleA2_P, show.p = TRUE, show.values = TRUE, title = "Sample 1 - elderly - stealing",
                              rm.terms = c("scale(age_snake)", "scale(HRSD_no_suic)", "scale(gameExp)", "scale(household_income_log)", "raceASIAN", "raceMORE THAN ONE RACE","raceWHITE", "gender.yMALE", "scale(education)"),
                              order.terms = c(1,4,5,7,3,2,6), colors = c("#008600","#c55a11")) + theme_bw()
pP_design_rank <- plot_model(mrankA0_P, type = "est", show.p = TRUE, show.values = TRUE, title = "Sample 1 - elderly- rank-buying",
                             rm.terms = c("scale(age_snake)", "scale(HRSD_no_suic)", "scale(gameExp)", "scale(household_income_log)", "raceASIAN", "raceMORE THAN ONE RACE","raceWHITE", "gender.yMALE", "scale(education)"),
                             order.terms = c(1,2,7,5,4,3,6), colors = c( "#008600","#c55a11")) + theme_bw()


library(grid)
library(gridExtra)
grid.arrange(pP_design_apple,pP_design_rank,pV_design_apple,pV_design_rank,
             layout_matrix = matrix(c(3,1,4,2), ncol=2, byrow=TRUE))



# trial*narcissism interaction
#Vancouver
mappleA1_bpni1 <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(bpni_TOTAL) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA1_bpni1)
car::Anova(mappleA1_bpni1, type = 'III')

mappleA1_bpni1_lm <- lm(appleChoice_wi_0 ~ scale(trial)*scale(bpni_TOTAL) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1),  data = snake_tot, na.action = na.omit)
summary(mappleA1_bpni1_lm)
car::Anova(mappleA1_bpni1_lm, type = 'III')

mrankA2_1_ffni1 <- lmer(rankChoice_wi_0 ~ win + scale(scoreDelta) + scale(rankStart) + scale(trial)*scale(oppRank)*scale(ffni_total) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2_1_ffni1)
car::Anova(mrankA2_1_ffni1, type = 'III')

mrankA2_1_ffni1_lm <- lm(rankChoice_wi_0 ~ win + scale(scoreDelta) + scale(rankStart) + scale(trial)*scale(oppRank)*scale(ffni_total) + scale(rankChoice_wi_0.minus1),  data = snake_tot, na.action = na.omit)
summary(mrankA2_1_ffni1_lm)
car::Anova(mrankA2_1_ffni1_lm, type = 'III')

library("rsm")
pV_narc_apple <- persp(mappleA1_bpni1_lm, scale(bpni_TOTAL) ~ scale(trial), zlab = "appleChoice_wi_0", col = terrain.colors(50), contours = "colors")
# image(mappleA1_bpni1_lm, scale(bpni_TOTAL) ~ scale(trial), col = rainbow(10, s = 1, v = 0.6, start = 0.1, end = 0.4))
# contour(mappleA1_bpni1_lm, scale(bpni_TOTAL) ~ scale(trial))

pV_narc_rank <- persp(mrankA2_1_ffni1_lm, scale(ffni_total) ~ scale(trial), zlab = "rankChoice_wi_0", col = terrain.colors(50), contours = "colors")

#Pittsburgh
mappleA1_ffni1 <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ffni_total) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ffni1)
car::Anova(mappleA1_ffni1, type = 'III')

mappleA1_ffni1_lm <- lm(appleChoice_wi_0 ~ scale(trial)*scale(ffni_total) + close.minus1 + win.minus1 + scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ffni1_lm)
car::Anova(mappleA1_ffni1_lm, type = 'III')

mrankA_ffni3 <- lmer(rankChoice_wi_0 ~ scale(ffni_total)*scale(trial) + scale(rankStart) + scale(oppRank) + scale(ffni_total)*scale(scoreDelta)*win + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA_ffni3)
car::Anova(mrankA_ffni3, type = 'III')

mrankA_ffni3_lm <- lm(rankChoice_wi_0 ~ scale(ffni_total)*scale(trial) + scale(rankStart) + scale(oppRank) + scale(ffni_total)*scale(scoreDelta)*win + scale(rankChoice_wi_0.minus1),  data = snake_totP, na.action = na.omit)
summary(mrankA_ffni3_lm)
car::Anova(mrankA_ffni3_lm, type = 'III')

pP_narc_apple <- persp(mappleA1_ffni1_lm, scale(ffni_total) ~ scale(trial), zlab = "appleChoice_wi_0", col = terrain.colors(50), contours = "colors")
pP_narc_rank <- persp(mrankA_ffni3_lm, scale(ffni_total) ~ scale(trial), zlab = "rankChoice_wi_0", col = terrain.colors(50), contours = "colors")

#Pittsburgh IPIP
mappleA1_ipip2 <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ipip_total) + close.minus1 + win.minus1 + scale(oppRank)*scale(ipip_total) +  scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ipip2)
car::Anova(mappleA1_ipip2, type = 'III')

mappleA1_ipip2_lm <- lm(appleChoice_wi_0 ~ scale(trial)*scale(ipip_total) + close.minus1 + win.minus1 + scale(oppRank)*scale(ipip_total) +  scale(oppRank)*scale(score.minus1) + scale(rankEnd.minus1),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ipip2_lm)
car::Anova(mappleA1_ipip2_lm, type = 'III')

mrankA2_ipip1 <- lmer(rankChoice_wi_0 ~ scale(ipip_total)*scale(trial) + win + scale(rankStart) + scale(oppRank) + scale(scoreDelta) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mrankA2_ipip1)
car::Anova(mrankA2_ipip1, type = 'III')

mrankA2_ipip1_lm <- lm(rankChoice_wi_0 ~ scale(ipip_total)*scale(trial) + win + scale(rankStart) + scale(oppRank) + scale(scoreDelta) + scale(rankChoice_wi_0.minus1),  data = snake_totP, na.action = na.omit)
summary(mrankA2_ipip1_lm)
car::Anova(mrankA2_ipip1_lm, type = 'III')

pP_ipip_apple <- persp(mappleA1_ipip2_lm, scale(ipip_total) ~ scale(trial), zlab = "appleChoice_wi_0", col = terrain.colors(50), contours = "colors")
pP_ipip_rank <- persp(mrankA2_ipip1_lm, scale(ipip_total) ~ scale(trial), zlab = "rankChoice_wi_0", col = terrain.colors(50), contours = "colors")

library(grid)
library(gridExtra)
grid.arrange(pV_narc_apple,pV_narc_rank,pP_narc_apple,pP_narc_rank,pP_ipip_apple,pP_ipip_rank,
             layout_matrix = matrix(c(1,3,5,2,4,6), ncol=3, byrow=TRUE))

# plots for depression and the interesting effect with ipip
mappleA1_ipip2 <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ipip_total) + close.minus1 + win.minus1 + scale(201-oppRank)*scale(ipip_total) +  scale(201-oppRank)*scale(score.minus1) + scale(rankEnd.minus1) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ipip2)
car::Anova(mappleA1_ipip2, type = 'III')

#best model with ipip and group_dep
snake_totP$current_rank1 <- 201-snake_totP$rankEnd.minus1
mappleA1_ipip2_dep <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ipip_total) + close.minus1 + win.minus1 + scale(201-oppRank)*scale(ipip_total) + gp_dep*scale(201-oppRank) + scale(score.minus1) + scale(current_rank1) + scale(age_snake) + gender.y + scale(education) + race + scale(gameExp) + scale(household_income_log) + (1|ID),  data = snake_totP, na.action = na.omit)
summary(mappleA1_ipip2_dep)
car::Anova(mappleA1_ipip2_dep, type = 'III')

library(emmeans)
em_mappleA1_ipip2_dep <- emmeans(mappleA1_ipip2_dep,"oppRank", by = "gp_dep", at = list(oppRank = c(1,100,200)))
plot(em_mappleA1_ipip2_dep,horiz = F)
em_mappleA1_ipip2_ipip <- emmeans(mappleA1_ipip2_dep,"oppRank", by = "ipip_total", 
                                  at = list(oppRank = c(1,100,200), ipip_total = c(18,26,34)))
plot(em_mappleA1_ipip2_ipip,horiz = F)


ls_mappleA1_ipip2_dep <- lsmeans(mappleA1_ipip2_dep,"oppRank", by = "gp_dep")
CLD_ipip_dep <- cld(ls_mappleA1_ipip2_dep)

pP_dep_ipip <- plot_model(mappleA1_ipip2_dep, type = "est", show.p = TRUE, show.values = TRUE, title = "Sample 2 - elderly IPIP, depression", colors = c( "#008600","#c55a11"), sort.est = TRUE, rm.terms = c("scale(age_snake)", "scale(gameExp)", "scale(household_income_log)", "raceASIAN", "raceMORE THAN ONE RACE","raceWHITE", "gender.yMALE", "scale(education)", "scale(score.minus1)", "close.minus11", "scale(current_rank1)", "win.minus11"), order.terms = c(1,2,5,3,6,4,7)) + theme_bw()

#plot based on GLM
ee <- Effect(c("ipip_total","oppRank", "gp_dep"), mappleA1_ipip2_dep)
ee <- data.frame(ee)
ee$ipip_level <- cut(ee$ipip_total, 3, include.lowest = TRUE, labels = c("low dominance\n(scores 11-22)", "medium dominance\n(scores 23-33)", "high dominance\n(scores34-44)"))
dplyr::count(ee,ipip_level)

ggplot(data = ee, aes(201-oppRank, fit, colour = ipip_level, fill = ipip_level)) +
  geom_line() +
  geom_ribbon(data = ee, colour= NA, alpha = 0.1, aes(ymin = lower, ymax = upper)) +
  facet_wrap(~gp_dep) +
  theme_bw() +
  scale_fill_manual(values = c("#008600", "black", "#c55a11")) +
  scale_color_manual(values = c("#008600", "black", "#c55a11"))


#plot(effect("scale(ipip_total):scale(201-oppRank)",mappleA1_ipip2, xlevels = list('ipip_total' = c(11,26,44))), grid=TRUE, x.var = 'oppRank', xlab = 'oppRank', ylab = 'within-subject mean\nstealing', main = 'IPIP-DS')

snake_totP_no_NA_ipip <- snake_totP[is.na(snake_totP$ipip_total)==FALSE,] 
snake_totP_no_NA_ipip$ipip_level <- cut(snake_totP_no_NA_ipip$ipip_total, 3, include.lowest = TRUE, labels = c("low dominance\n(scores 11-22)", "medium dominance\n(scores 23-33)", "high dominance\n(scores34-44)"))
dplyr::count(snake_totP_no_NA_ipip,ipip_level)

snake_totP_no_NA_ipip$hrsd_level <- cut(snake_totP_no_NA_ipip$HRSD_no_suic, 3, include.lowest = TRUE)
dplyr::count(snake_totP_no_NA_ipip,hrsd_level)

library(ggplot2)
ggplot(snake_totP_no_NA_ipip,aes(201-oppRank, appleChoice_wi_0, colour = factor(ipip_level))) +
  geom_smooth(method = 'loess', span = 4, na.rm = TRUE) + facet_wrap(~gp_dep) +
  scale_fill_manual(values = c("#008600", "black", "#c55a11")) +
  theme_bw()


##models with score

# examine performance (finally, why didn't we look before?)

## good simple model
hist(snake_totP$score)
table(snake_totP$gp_dep)
table(snake_totP$group1_5)

pm0_dep <- lmer(score ~ scale(trial)*gp_dep  + (1|ID), data = snake_totP)
summary(pm0_dep)
car::Anova(pm0_dep,'3')

vif.lme(pm0_dep)
plot(effect("scale(trial):gp_dep ",pm1_dep), grid=TRUE)




pm1_dep <- lmer(score ~ scale(trial)*gp_dep + win*gp_dep  + (1|ID), data = snake_totP)
summary(pm1_dep)
car::Anova(pm1_dep,'3')

vif.lme(pm0s2_dep)

pm2_dep <- lmer(score ~ scale(trial)*gp_dep + win*gp_dep + scale(oppRank)*gp_dep + (1|ID), data = snake_totP)
summary(pm2_dep)
car::Anova(pm2_dep,'3')

vif.lme(pm2_dep)



## good "saturated" model
pm3 <- lmer(score ~ scale(trial)*gp_dep + win*gp_dep + oppRank*gp_dep + appleChoice*gp_dep +  (1|ID), data = snake_totP)
summary(pm3)
car::Anova(pm3,'3')

pm4 <- lmer(score ~ scale(trial)*gp_dep + win*gp_dep + oppRank*gp_dep + appleChoice*gp_dep +
              + scale(trial)*scale(age) + win*scale(age) + oppRank*scale(age) + appleChoice*scale(age) +
              +  gender.x +
              (1|ID), data = snake_totP)
summary(pm4)
car::Anova(pm4,'3')


#additional score models
pm0s_dep <- lmer(score ~ scale(trial)*gp_dep + gender.x*gp_dep + scale(age_snake)*gp_dep + scale(gameExp)*gp_dep  + (1|ID), data = snake_totP)
summary(pm0s_dep)
car::Anova(pm0s_dep,'3')

vif.lme(pm0s_dep)

pm0s1_dep <- lmer(score ~ scale(trial)*gp_dep + gender.x*scale(age_snake) + scale(age_snake)*gp_dep + scale(gameExp)*scale(age_snake)  + (1|ID), data = snake_totP)
summary(pm0s1_dep)
car::Anova(pm0s1_dep,'3')

vif.lme(pm0s1_dep)

anova(pm0s1_dep, pm0s_dep)

pm0s2_dep <- lmer(score ~ scale(trial)*gp_dep + scale(oppRank)*gp_dep + gender.x + scale(age_snake) + scale(gameExp) + win.minus1 + scale(scoreDelta.minus1) + scale(appleChoice) +
                    scale(trial)*scale(age_snake) + scale(oppRank)*scale(age_snake) + gender.x*scale(age_snake) + scale(gameExp)*scale(age_snake) + win.minus1*scale(age_snake) + scale(scoreDelta.minus1)*scale(age_snake) + scale(appleChoice)*scale(age_snake) +
                    scale(trial)*gender.x + scale(oppRank)*gender.x + scale(gameExp)*gender.x + win.minus1*gender.x + scale(scoreDelta.minus1)*gender.x + scale(appleChoice)*gender.x +
                    scale(trial)*scale(gameExp) + scale(oppRank)*scale(gameExp) + win.minus1*scale(gameExp) + scale(scoreDelta.minus1)*scale(gameExp) + scale(appleChoice)*scale(gameExp) + (1|ID), data = snake_totP)

summary(pm0s2_dep)
car::Anova(pm0s2_dep,'3')

vif.lme(pm0s2_dep)

#good model from Anna's models
pm0_ipipP <- lmer(score ~ scale(trial)*scale(ipip_total)+ (1|ID), data = snake_totP)
summary(pm0_ipipP)
car::Anova(pm0_ipipP,'3')

plot(effect("scale(trial):scale(ipip_total)",pm0_depP), grid=TRUE)

pm0_bpniP <- lmer(score ~ scale(trial)*scale(bpni_TOTAL)+ (1|ID), data = snake_totP)
summary(pm0_bpniP)
car::Anova(pm0_bpniP,'3')

pm0_ffniP <- lmer(score ~ scale(trial)*scale(ffni_total)+ (1|ID), data = snake_totP)
summary(pm0_ffniP)
car::Anova(pm0_ffniP,'3')


pm0_depP <- lmer(score ~ scale(trial)*gp_dep  + (1|ID), data = snake_totP)
summary(pm0_depP)
car::Anova(pm0_depP,'3')

pm0s2c_dep <- lmer(score ~ scale(trial)*gp_dep + scale(oppRank)*gp_dep + gender.x + scale(age_snake) + scale(gameExp) + win.minus1 + scale(appleChoice) +
                     scale(appleChoice)*scale(age_snake) + (1|ID), data = snake_totP)

summary(pm0s2c_dep)
car::Anova(pm0s2c_dep,'3')

plot(effect("gp_dep:scale(oppRank)",pm0s2c_dep), grid=TRUE)
plot(effect("scale(age_snake):scale(appleChoice)",pm0s2c_dep), grid=TRUE)

vif.lme(pm0s2c_dep)
anova(pm0s2c_dep, pm3)


pm0s2c_dep_narc <- lmer(score ~ scale(trial)*gp_dep + scale(oppRank)*gp_dep + gender.x + scale(age_snake) + scale(gameExp) + win.minus1 + scale(appleChoice) +
                          scale(appleChoice)*scale(age_snake) +
                          scale(bpni_TOTAL) + (1|ID), data = snake_totP)

summary(pm0s2c_dep_narc)
car::Anova(pm0s2c_dep_narc,'3')

#Vancouver
##no effect of narcissistic scales on score
pm0_narcV <- lmer(score ~ scale(trial)*scale(ffni_total) + gender + scale(age) + ethnicity_simp+ scale(gameExp) + (1|ID), data = snake_tot)
summary(pm0_narcV)
car::Anova(pm0_narcV,'3')


pm0all_depV <- lmer(score ~ scale(trial) + gender + scale(age) + scale(oppRank) + win.minus1 + scale(gameExp) + scale(appleChoice) + (1|ID), data = snake_tot)
summary(pm0all_depV)
car::Anova(pm0all_depV,'3')

vif.lme(pm0all_depV)

pm0s2_depV <- lmer(score ~ scale(trial)*gender*scale(age) + scale(oppRank) win.minus1 + scale(gameExp) + scale(appleChoice) + (1|ID), data = snake_tot)
summary(pm0s2_depV)
car::Anova(pm0s2_depV,'3')

plot(effect("gender:scale(age):scale(trial)",pm0s2_depV), grid=TRUE)

vif.lme(pm0s2_depV)

##model plots for neurocon2018
pm0_depP <- lmer(score ~ scale(trial)*gp_dep + gender.x + scale(age_snake) + race + scale(gameExp) + (1|ID), data = snake_totP)
summary(pm0_depP)
car::Anova(pm0_depP,'3')

pP_dep_engage <- plot_model(pm0_depP, type = "est", show.p = TRUE, show.values = TRUE, title = "Sample 2 - score predicted by group", colors = c( "#008600","#c55a11"), terms = c("scale(trial)","gp_dep1", "scale(trial):gp_dep1"), order.terms = c(1,2,3)  ,sort.est = TRUE) + theme_bw()


pm0_depP_ipip <- lmer(score ~ scale(trial)*gp_dep + scale(trial)*scale(ipip_total) + gender.x + scale(age_snake) + race + scale(gameExp) + (1|ID), data = snake_totP_no_NA_ipip)
summary(pm0_depP_ipip)
car::Anova(pm0_depP_ipip,'3')

pP_dep_ipip_engage <- plot_model(pm0_depP_ipip, type = "est", show.p = TRUE, show.values = TRUE, title = "Sample 2 - score predicted by group and ipip", colors = c( "#008600","#c55a11"), terms = c("scale(trial)", "scale(ipip_total)", "scale(trial):scale(ipip_total)","gp_dep1", "scale(trial):gp_dep1"), order.terms = c(1,2,4,3,5)  ,sort.est = TRUE) + theme_bw()

library(ggplot2)
ggplot(snake_totP_no_NA_ipip,aes(scale(trial), score, colour = factor(ipip_level))) +
  geom_smooth(method = 'loess', span = 4, na.rm = TRUE) + facet_wrap(~gp_dep) +
  scale_fill_manual(values = c("#008600", "black", "#c55a11")) +
  theme_bw()


pm0_ipipP <- lmer(score ~ scale(trial)*scale(ipip_total) + gp_dep + gender.x + scale(age_snake) + race + scale(gameExp) + (1|ID), data = snake_totP)
summary(pm0_ipipP)
car::Anova(pm0_ipipP,'3')

pP_ipip_engage <- plot_model(pm0_ipipP, type = "est", show.p = TRUE, show.values = TRUE, title = "Sample 2 - score predicted by IPIP-DS scores", colors = c("#c55a11"), terms = c("scale(trial)","scale(ipip_total)", "scale(trial):scale(ipip_total)"), order.terms = c(1,2,3)  ,sort.est = TRUE) + theme_bw()


snake_totP$race <- relevel(snake_totP$race, ref = "MORE THAN ONE RACE")

pm0allP <- lmer(score ~ scale(trial) + gender.x + scale(age_snake) + scale(oppRank) + win.minus1 + scale(gameExp) + scale(appleChoice) + race + (1|ID), data = snake_totP)
summary(pm0allP)
car::Anova(pm0allP,'3')

pP_score_all <- plot_model(pm0allP, type = "est", show.p = TRUE, show.values = TRUE, title = "Sample 2 - model predicting score", colors = c( "#008600","#c55a11"), rm.terms = c("raceWHITE", "raceASIAN", "raceAFRICAN AMERICAN"), order.terms = c(7,6,1,4,3,5,2)) + theme_bw()


snake_tot$ethnicity_simp <- relevel(snake_tot$ethnicity_simp, ref = "other")

pm0allV <- lmer(score ~ scale(trial) + gender + scale(age) + scale(oppRank) + win.minus1 + scale(gameExp) + scale(appleChoice) + ethnicity_simp + (1|ID), data = snake_tot)
summary(pm0allV)
car::Anova(pm0allV,'3')

pV_score_all <- plot_model(pm0allV, type = "est", show.p = TRUE, show.values = TRUE, title = "Sample 1 - model predicting score", colors = c( "#008600","#c55a11"), rm.terms = c("ethnicity_simpcaucas", "ethnicity_simpasian", "ethnicity_simpblack"), sort.est = TRUE) + theme_bw()

library(grid)
library(gridExtra)
grid.arrange(pV_score_all,pP_score_all,
             layout_matrix = matrix(c(1,2), ncol=2, byrow=TRUE))


#group differences in narcissism and dominance
snake_totP_shrunk$gp_dep <- "0"
snake_totP_shrunk$gp_dep[snake_totP_shrunk$group1_5 != "1"] <- "1"
table(snake_totP_shrunk$gp_dep)
table(snake_totP_shrunk$group1_5)

mbpni0 <- lm(bpni_TOTAL ~ gp_dep + scale(age) + gender.x + scale(education) + race,  data = snake_totP_shrunk, na.action = na.omit)
summary(mbpni0)
car::Anova(mbpni0, type = 'III')
ls_bpni0 <- lsmeans(mbpni0,"gp_dep")
plot(ls_bpni0, horiz=F, ylab = "", xlab = "groups")
pairs(ls_bpni0)
CLD_bpni <- cld(ls_bpni0, sort = FALSE)

mffni0 <- lm(ffni_total ~ gp_dep + scale(age) + gender.x + scale(education) + race,  data = snake_totP_shrunk, na.action = na.omit)
summary(mffni0)
car::Anova(mffni0, type = 'III')
ls_ffni0 <- lsmeans(mffni0,"gp_dep")
plot(ls_ffni0, horiz=F, ylab = "", xlab = "groups")
CLD_ffni <- cld(ls_ffni0, sort = FALSE)

mipip0 <- lm(ipip_total ~ gp_dep + scale(age) + gender.x + scale(education) + race,  data = snake_totP_shrunk, na.action = na.omit)
summary(mipip0)
car::Anova(mipip0, type = 'III')
ls_ipip0 <- lsmeans(mipip0,"gp_dep")
plot(ls_ipip0, horiz=F, ylab = "", xlab = "groups")
CLD_ipip <- cld(ls_ipip0, sort = FALSE)

pd = position_dodge(0.8)
p_bpni <- ggplot(CLD_bpni, aes(x     = gp_dep, y = lsmean, fill = .group, label = .group)) +
  geom_point(shape  = 15, size   = 3, colour = c("#008600", "#c55a11"), position = pd) +
  geom_errorbar(aes(ymin  =  lower.CL, ymax  =  upper.CL), width =  0.2, size  =  0.9, colour = c("#008600", "#c55a11"), position = pd) +
  theme_bw() +
  scale_x_discrete(labels=c("Healthy group (N = 25)","Depressed group (N = 60)")) +
  ylab("BPNI scores") +
  xlab("Study groups") +
  ggtitle("Group differences in narcissism and dominance") +
  theme(axis.title.y   = element_text(size = 16), axis.title.x   = element_blank(), axis.text.x = element_blank(), axis.text.y = element_text(siz=12), plot.caption = element_text(hjust = 0),
  legend.title = element_blank(), legend.text = element_blank(),
  strip.text.x = element_blank(),
  plot.title = element_text(size = 18))
  

p_ffni <- ggplot(CLD_ffni, aes(x     = gp_dep, y = lsmean, fill = .group, label = .group)) +
  geom_point(shape  = 15, size   = 3, colour = c("#008600", "#c55a11"), position = pd) +
  geom_errorbar(aes(ymin  =  lower.CL, ymax  =  upper.CL), width =  0.2, size  =  0.9, colour = c("#008600", "#c55a11"), position = pd) +
  theme_bw() +
  scale_x_discrete(labels=c("Healthy group (N = 25)","Depressed group (N = 60)")) +
  ylab("FFNI scores") +
  xlab("Study groups") +
  theme(axis.title.y   = element_text(size = 16), axis.title.x   = element_blank(), axis.text.x = element_blank(), axis.text.y = element_text(siz=12), plot.caption = element_text(hjust = 0),
        legend.title = element_blank(), legend.text = element_blank(),
        strip.text.x = element_text(size=14),
        plot.title = element_blank())

p_ipip <- ggplot(CLD_ipip, aes(x     = gp_dep, y = lsmean, fill = .group, label = .group)) +
  geom_point(shape  = 15, size   = 3, colour = c("#008600", "#c55a11"), position = pd) +
  geom_errorbar(aes(ymin  =  lower.CL, ymax  =  upper.CL), width =  0.2, size  =  0.9, colour = c("#008600", "#c55a11"), position = pd) +
  theme_bw() +
  scale_x_discrete(labels=c("Healthy group (N = 25)","Depressed group (N = 60)")) +
  ylab("IPIP-DS scores") +
  xlab("Study groups") +
    theme(axis.title   = element_text(size = 16), axis.text    = element_text(size=12), plot.caption = element_text(hjust = 0),
        legend.title = element_blank(), legend.text = element_blank(),
        strip.text.x = element_blank(),
        plot.title = element_blank())

library(grid)
library(gridExtra)
grid.arrange(p_bpni,p_ffni,p_ipip,
             layout_matrix = matrix(c(1,2,3), ncol=1, byrow=TRUE))
