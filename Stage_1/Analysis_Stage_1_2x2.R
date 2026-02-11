############################################################################
# PACKAGES & FUNCTIONS
############################################################################
library("easypackages")
libraries(
  "lme4", "tidyverse", "tidyselect", "optimx", "emmeans", "car",
  "effects", "agridat", "ggplot2", "ghibli", "ggdist", "see"
)

source("./functions/diagnostic_fcns.r")
source("./functions/glmm_stability.r")
source("./functions/boot_glmm.r")

############################################################################
# R SETTINGS
############################################################################
options(scipen = 999)
load("Stage_1.RData")

############################################################################
# PREPARE DATAFRAME FOR MODEL FITTING
############################################################################
xx.fe.re <- fe.re.tab(
  fe.model =
    "correct_choice_nr ~ Receiver_Evidence*Evidence*Trial_nr",
  re = "(1|Chimp)", data = xxdata
)
xx.fe.re$summary
t.data <- xx.fe.re$data
str(t.data)

# Center dummy variables
t.data$Receiver_Evidence.Partner.code <-
  t.data$Receiver_Evidence.Partner - mean(t.data$Receiver_Evidence.Partner)
t.data$Evidence.None.code <-
  t.data$Evidence.None - mean(t.data$Evidence.None)
t.data$z.Trial <-
  scale(t.data$Trial_nr)

tapply(
  xxdata$correct_choice_nr,
  list(xxdata$Condition, as.factor(xxdata$Trial_nr)), mean, na.rm = TRUE
)

############################################################################
# FITTING THE MODEL AS PREREGISTERED
############################################################################
contr <-
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000000))

full_exp1 <- glmer(correct_choice_nr ~
                     Receiver_Evidence * Evidence * z.Trial +
                     (1 + Receiver_Evidence.Partner.code * Evidence.None.code * z.Trial | Chimp),
                   data = t.data, control = contr, family = binomial(link = "logit"))

red_exp1 <- glmer(correct_choice_nr ~
                     (Receiver_Evidence + Evidence + z.Trial)^2 +
                    (1 + Receiver_Evidence.Partner.code * Evidence.None.code * z.Trial | Chimp),
                  data = t.data, control = contr, family = binomial(link = "logit"))

main_exp1 <- glmer(correct_choice_nr ~
                     Receiver_Evidence + Evidence + z.Trial +
                     (1 + Receiver_Evidence.Partner.code * Evidence.None.code * z.Trial | Chimp),
                   data = t.data, control = contr, family = binomial(link = "logit"))

null_exp1 <- glmer(correct_choice_nr ~
                     1 +
                     (1 + Receiver_Evidence.Partner.code * Evidence.None.code * z.Trial | Chimp),
                   data = t.data, control = contr, family = binomial(link = "logit"))

summary(full_exp1)$varcor
ranef.diagn.plot(full_exp1)
round(summary(full_exp1)$coefficients, 3) # massive SDs

############################################################################
# CHECKING ASSUMPTIONS
############################################################################
overdisp.test(full_exp1)
library(car)
vif(main_exp1)
# Checking model stability
m.stab.b <-
  glmm.model.stab(model.res = full_exp1, contr = contr, use = c("Chimp"))
m.stab.b$detailed$warnings
as.data.frame(round(m.stab.b$summary[, -1], 3))
m.stab.plot(round(m.stab.b$summary[, -1], 3))
# Model stablility only with models that did not give a warning message
m.stab.b$detailed <- m.stab.b$detailed %>%
  filter(warnings == "none")
cbind(
  apply(m.stab.b$detailed, 2, min),
  apply(m.stab.b$detailed, 2, max)
)

############################################################################
# MODEL COMPARISONS
############################################################################
round(anova(full_exp1, null_exp1, test = "Chisq"), 3)
round(drop1(full_exp1, test = "Chisq"), 3)
round(drop1(red_exp1, test = "Chisq"), 3)
round(drop1(main_exp1, test = "Chisq"), 3)

## First peek at effects
plot(effect("Receiver_Evidence:Evidence", red_exp1), type = "response")

# Coefficients of the full_exp1 model
round(summary(full_exp1)$coefficients, 3)

# Get estimated marginal means (EMMs) for the 'Condition' factor
emm <- emmeans(red_exp1, ~ Receiver_Evidence:Evidence, type = "response")
pairs(emm)
pairs(emm, adjust = "holm")

############################################################################
# BOOTSTRAPS
############################################################################
## Bootstraps of full_exp2 model
# The bootstrap has already been run and is saved in the image
boot.full_exp1 <- boot.glmm.pred(
  model.res = full_exp1, excl.warnings = T,
  nboots = 10, para = F, level = 0.95,
  use = c("Receiver_Evidence", "Receiver_Evidence", "z.Trial")
)

round(boot.full_exp1$ci.estimates, 3)
m.stab.plot(round(boot.full_exp1$ci.estimates, 3))
boot.full_exp1$ci.predicted

boot.red_exp1 <- boot.glmm.pred(
  model.res = red_exp1, excl.warnings = T,
  nboots = 10, para = F, level = 0.95,
  use = c("Receiver_Evidence", "Receiver_Evidence")
)

round(boot.red_exp1$ci.estimates, 3)
m.stab.plot(round(boot.red_exp1$ci.estimates, 3))
boot.red_exp1$ci.predicted

boot.main_exp1 <- boot.glmm.pred(
  model.res = main_exp1, excl.warnings = T,
  nboots = 10, para = F, level = 0.95,
  use = c("Receiver_Evidence", "Receiver_Evidence")
)

round(boot.main_exp1$ci.estimates, 3)
m.stab.plot(round(boot.main_exp1$ci.estimates, 3))
boot.main_exp1$ci.predicted

#load("Stage_1.RData")


