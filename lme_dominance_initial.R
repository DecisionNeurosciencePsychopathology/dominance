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

load("~/Google Drive/skinner/projects_analyses/SocialRankingParadigm/P.Rda")
df <- P



# df[df=="NaN"] = NA
# df$steering <- df$Stearing
summary(df)
df$score_diff <- df$score - df$oppscore;
df$trial <- df$round
df$ID <- df$player
df$win <- as.factor(df$win)
df$win.minus1 <- as.factor(df$win.minus1)
df$win.minus2 <- as.factor(df$win.minus2)
df$elevator <- as.factor(df$elevator)
df$elevator.minus1 <- as.factor(df$elevator.minus1)
df$elevator.minus2 <- as.factor(df$elevator.minus2)
# df$rank_change <- df$Rank_Tminus1 - df$Rank_T_start;
# df$resp_new <- 4 - df$Response;
df <- df %>% group_by(ID) %>% mutate(score_diff_lag = lag(score_diff, n=1, order_by=trial),
                                     player_rank_lag1 = lag(playerRank, n=1, order_by = trial),
                                     player_rank_lag2 = lag(playerRank, n=2, order_by = trial))
df$score_diff_lag[df$trial==1] = NA

df$choice_diff = -df$oppChoice.minus1 + df$oppChoice

df$rank_diff = df$player_rank_lag1 - df$playerRank

# df$steering[is.na(df$steering)] = 0;
#
#
# sdf <- df[df$any_steer==TRUE,]
# df <- read_excel("~/Google Drive/skinner/projects_analyses/dominance/n=8_pilot.xlsx")
# View(df)
#
#

hist(df$oppChoice)
hist(df$ugChoice)

hist(df$choice_diff)

sdf <- df[(df$ID!=7 & df$ID!=8),]

m1 <- lmer(choice_diff ~ oppChoice.minus1 + (1|ID),  data = sdf,na.action = na.omit)
summary(m1)
car::Anova(m1)

m2 <- lmer(choice_diff ~ oppChoice.minus1 + win.minus1 + elevator.minus1 + playerRank*trial + (1|ID),  data = sdf,na.action = na.omit)
summary(m2)
car::Anova(m2)
anova(m2,m1)

rg2 <- lsmeans(m2,"playerRank", by = "trial", at = list(trial = c(5,  10,  15), playerRank = c(90, 105, 130)))
plot(rg2, type ~ choice_diff, horiz=F, ylab = "Change in challenging behavior", xlab = "Current rank")


# m3 <- lmer(choice_diff ~ oppChoice.minus1 + win.minus1 + elevator.minus1 + playerRank*trial + rank_diff + (1|ID),  data = sdf,na.action = na.omit)
# summary(m3)
# car::Anova(m3)
# anova(m2,m3)



um1 <- lmer(ugChoice ~ ugChoice.minus1 + playerRank + win.minus1 + score.minus1 + score.minus2 + elevator + (1|ID),  data = sdf,na.action = na.omit)
summary(um1)
car::Anova(um1)


m1 <- lmer(Response ~ Trial + steering + any_steer + (1|ID),  data = df,na.action = na.omit)
anova(m1)
rg1 <- lsmeans(m1,"steering", at = list(steering = c(-2,  0,  2)))
plot(rg1, type ~ Response, horiz=F, ylab = "Choice of rank", xlab = "steering")


m2 <- lmer(Response ~ Trial + resplag*steering + any_steer + (1|ID),  data = df,na.action = na.omit)
summary(m2)
car::Anova(m2)
anova(m1,m2)
rg2 <- lsmeans(m2,"resplag", by = "steering", at = list(steering = c(-2,  0,  2), resplag = c(1,2,3, 4)))
plot(rg2, type ~ resplag, horiz=F, ylab = "Choice of rank", xlab = "Previous response")

m3 <- lmer(Response ~ Trial + resplag*steering + any_steer + (1|ID),  data = df,na.action = na.omit)
summary(m3)
car::Anova(m3)
anova(m2,m3)

m4 <- lmer(Response ~ resplag*steering + any_steer + WinLose_Tminus1 + (1|ID),  data = df,na.action = na.omit)
summary(m4)
car::Anova(m4)
anova(m3,m4)


um1 <- lmer(sqrt(UG_choice_T) ~ UG_choice_Tminus1 + UG_choice_Tminus2 + Trial + WinLose_T + rank_change + (1|ID),  data = df,na.action = na.omit)
summary(um1)
car::Anova(um1)
rg1 <- lsmeans(m1,"steering", at = list(steering = c(-2,  0,  2)))
plot(rg1, type ~ Response, horiz=F, ylab = "Choice of rank", xlab = "steering")

sum1 <- lmer((UG_choice_T) ~ UG_choice_Tminus1 + UG_choice_Tminus2 + Trial + WinLose_T + rank_change + (1|ID),  data = sdf,na.action = na.omit)
summary(sum1)
car::Anova(sum1)

sum2 <- lmer((UG_choice_T) ~  Trial + Money_T + rank_change + (1|ID),  data = sdf,na.action = na.omit)
summary(sum2)
car::Anova(sum2)
anova(sum1, sum2)




gm1 <- glm.nb(resp_new ~ Trial + steering + (1|ID),  data = df,na.action = na.omit)
anova(gm1)

gm2 <- glm.nb(resp_new ~ Trial + steering + resplag + (1|ID),  data = df,na.action = na.omit)
summary(gm2)
anova(gm1, gm2)



