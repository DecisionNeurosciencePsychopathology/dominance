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

#  for collinearity diagnostics
vif.lme <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)] }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v }


setwd("~/Dropbox/USA/Pittsburgh/GitHub/dominance/snake_data_Vancouver")
# wd for Alex
setwd("~/code/dominance/snake_data_Vancouver")

load("snake_tot.Rda")
load("snake_tot_shrunk.Rda")

setwd("~/Dropbox/USA/Pittsburgh/GitHub/dominance/snake_data_Pittsburgh_may2018")
# wd for Alex
setwd("~/code/dominance/snake_data_Pittsburgh_may2018")

load("snake_totP.Rda")
load("snake_totP_shrunk.Rda")

setwd("~/Dropbox/USA/Pittsburgh/GitHub/dominance")
# wd for Alex
setwd("~/code/dominance")


# score models

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

vif.lme(pm3)
anova(pm2_dep, pm3)

pm4 <- lmer(score ~ scale(trial)*gp_dep + win*gp_dep + scale(oppRank)*gp_dep + scale(appleChoice)*gp_dep +
              + scale(trial)*scale(age) + win*scale(age) + scale(oppRank)*scale(age) + scale(appleChoice)*scale(age) +
              +  gender.x +
              (1|ID), data = snake_totP)
summary(pm4)
car::Anova(pm4,'3')

vif.lme(pm4)
anova(pm3, pm4)

pm5 <- lmer(score ~ scale(trial) + win + scale(oppRank)*gp_dep + 
              gender.x +
              (1|ID), data = snake_totP)
summary(pm5)
car::Anova(pm5,'3')

vif.lme(pm5)
anova(pm4, pm5)


library(effects)
plot(effect("scale(oppRank):gp_dep",pm5), grid=TRUE)


pm5V <- lmer(score ~ scale(trial) + win + scale(oppRank)*scale(dass21_depression) + 
              gender +
              (1|ID), data = snake_tot)
summary(pm5V)
car::Anova(pm5V,'3')

vif.lme(pm5V)
anova(pm4, pm5V)

pm4V <- lmer(score ~ scale(trial)*scale(age) + win*scale(age) + scale(oppRank)*gender + scale(appleChoice)*scale(age) +
              (1|ID), data = snake_tot)
summary(pm4V)
car::Anova(pm4V,'3')

# narcissism and dominance
pm5ipip <- lmer(score ~ scale(trial)*scale(ipip_total) + win*scale(ipip_total) + scale(oppRank)*gp_dep + scale(oppRank)*scale(ipip_total) + 
              gender.x*scale(ipip_total) +
              (1|ID), data = snake_totP)
summary(pm5ipip)
car::Anova(pm5ipip,'3')

vif.lme(pm5ipip)
anova(pm4, pm5ipip)


pm5bpni <- lmer(score ~ scale(trial)*scale(bpni_TOTAL) + win*scale(bpni_TOTAL) + scale(oppRank)*gp_dep + scale(oppRank)*scale(bpni_TOTAL) + 
                  gender.x*scale(bpni_TOTAL) +
                  (1|ID), data = snake_totP)
summary(pm5bpni)
car::Anova(pm5bpni,'3')

vif.lme(pm5bpni)
anova(pm4, pm5bpni)

plot(effect("scale(bpni_TOTAL):win",pm5bpni), grid=TRUE)

pm5bpniV <- lmer(score ~ scale(trial)*scale(bpni_VULNERABILITY) + win*scale(bpni_VULNERABILITY) + scale(oppRank)*gp_dep + scale(oppRank)*scale(bpni_VULNERABILITY) + 
                  gender.x*scale(bpni_VULNERABILITY) +
                  (1|ID), data = snake_totP)
summary(pm5bpniV)
car::Anova(pm5bpniV,'3')

pm5bpniG <- lmer(score ~ scale(trial)*scale(bpni_GANDIOSITY) + win*scale(bpni_GANDIOSITY) + scale(oppRank)*gp_dep + scale(oppRank)*scale(bpni_GANDIOSITY) + 
                   gender.x*scale(bpni_GANDIOSITY) +
                   (1|ID), data = snake_totP)
summary(pm5bpniG)
car::Anova(pm5bpniG,'3')


pm5ffni <- lmer(score ~ scale(trial)*scale(ffni_total) + win*scale(ffni_total) + scale(oppRank)*gp_dep + scale(oppRank)*scale(ffni_total) + 
                  gender.x*scale(ffni_total) +
                  (1|ID), data = snake_totP)
summary(pm5ffni)
car::Anova(pm5ffni,'3')

vif.lme(pm5ffni)
anova(pm4, pm5ffni)

pm5ffniV <- lmer(score ~ scale(trial)*scale(ffni_VULNERABLE_NARCISSISM) + win*scale(ffni_VULNERABLE_NARCISSISM) + scale(oppRank)*gp_dep + scale(oppRank)*scale(ffni_VULNERABLE_NARCISSISM) + 
                   gender.x*scale(ffni_VULNERABLE_NARCISSISM) +
                   (1|ID), data = snake_totP)
summary(pm5ffniV)
car::Anova(pm5ffniV,'3')

pm5ffniG <- lmer(score ~ scale(trial)*scale(ffni_GRANDIOSE_NARCISSISM) + win*scale(ffni_GRANDIOSE_NARCISSISM) + scale(oppRank)*gp_dep + scale(oppRank)*scale(ffni_GRANDIOSE_NARCISSISM) + 
                   gender.x*scale(ffni_GRANDIOSE_NARCISSISM) +
                   (1|ID), data = snake_totP)
summary(pm5ffniG)
car::Anova(pm5ffniG,'3')

#Vancouver-narcissism
pm5bpni <- lmer(score ~ scale(trial)*scale(bpni_TOTAL) + win*scale(bpni_TOTAL) + scale(oppRank)*scale(bpni_TOTAL) + 
                  gender*scale(bpni_TOTAL) +
                  (1|ID), data = snake_tot)
summary(pm5bpni)
car::Anova(pm5bpni,'3')

vif.lme(pm5bpni)
anova(pm4, pm5bpni)

pm5ffni <- lmer(score ~ scale(trial)*scale(ffni_total) + win*scale(ffni_total) + scale(oppRank)*scale(ffni_total) + 
                  gender*scale(ffni_total) +
                  (1|ID), data = snake_tot)
summary(pm5ffni)
car::Anova(pm5ffni,'3')

vif.lme(pm5ffni)
anova(pm4, pm5ffni)

#additional score models
pm0s_dep <- lmer(score ~ scale(trial)*gp_dep + gender.x*gp_dep + scale(age_snake)*gp_dep + scale(gameExp)*gp_dep  + (1|ID), data = snake_totP)
summary(pm0s_dep)
car::Anova(pm0s_dep,'3')

vif.lme(pm0s_dep)
anova(pm4, pm0s_dep)

pm0s1_dep <- lmer(score ~ scale(trial)*gp_dep + gender.x*scale(age_snake) + scale(age_snake)*gp_dep + scale(gameExp)*scale(age_snake)  + (1|ID), data = snake_totP)
summary(pm0s1_dep)
car::Anova(pm0s1_dep,'3')

vif.lme(pm0s1_dep)

anova(pm0s1_dep, pm4)

pm0s2_dep <- lmer(score ~ scale(trial)*gp_dep + scale(oppRank)*gp_dep + scale(trial)*scale(age_snake) + scale(oppRank)*scale(age_snake) + gender.x*scale(age_snake) + scale(gameExp)*scale(age_snake) +
                    win.minus1*scale(age_snake) + scale(scoreDelta.minus1)*scale(age_snake) + scale(appleChoice)*scale(age_snake) +
                    scale(trial)*gender.x + scale(oppRank)*gender.x + scale(gameExp)*gender.x + win.minus1*gender.x + scale(scoreDelta.minus1)*gender.x + scale(appleChoice)*gender.x +
                    scale(trial)*scale(gameExp) + scale(oppRank)*scale(gameExp) + win.minus1*scale(gameExp) + scale(scoreDelta.minus1)*scale(gameExp) + scale(appleChoice)*scale(gameExp) + (1|ID), data = snake_totP)

summary(pm0s2_dep)
car::Anova(pm0s2_dep,'3')

vif.lme(pm0s2_dep)
anova(pm4, pm0s2_dep)

#good model from Anna's models
pm0_ipipP <- lmer(score ~ scale(trial)*scale(ipip_total)+ (1|ID), data = snake_totP)
summary(pm0_ipipP)
car::Anova(pm0_ipipP,'3')

library(effects)
plot(effect("scale(trial):scale(ipip_total)",pm0_ipipP), grid=TRUE)

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
