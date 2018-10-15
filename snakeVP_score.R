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

pm4 <- lmer(score ~ scale(trial)*gp_dep + win*gp_dep + oppRank*gp_dep + appleChoice*gp_dep +
              + scale(trial)*scale(age) + win*scale(age) + oppRank*scale(age) + appleChoice*scale(age) +
              +  gender.x +
              (1|ID), data = snake_totP)
summary(pm4)
car::Anova(pm4,'3')

vif.lme(pm4)
anova(pm3, pm4)

library(effects)
plot(effect("appleChoice:scale(age)",pm4), grid=TRUE)


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
