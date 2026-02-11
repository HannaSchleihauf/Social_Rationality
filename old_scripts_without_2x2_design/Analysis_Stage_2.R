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

xxdata <- data_long_filtered

############################################################################
# PREPARE DATAFRAME FOR MODEL FITTING
############################################################################
xx.fe.re <- fe.re.tab(
  fe.model =
    "subject_changed ~ Condition.New*Trial",
  re = "(1|Chimp)", data = xxdata
)
xx.fe.re$summary
t.data <- xx.fe.re$data
str(t.data)

# Center dummy variables
t.data$Condition.New.Partner.code <-
  t.data$Condition.New.Partner - mean(t.data$Condition.New.Partner)
t.data$Condition.New.Subject.code <-
  t.data$Condition.New.Subject - mean(t.data$Condition.New.Subject)
t.data$Condition.New.SubjectPartner.code <-
  t.data$Condition.New.SubjectPartner - mean(t.data$Condition.New.SubjectPartner)
t.data$z.Trial <-
  scale(t.data$Trial)

tapply(
  t.data$subject_changed,
  list(t.data$Condition.New, as.factor(t.data$Trial)), mean, na.rm = TRUE
)

############################################################################
# FITTING THE MODEL AS PREREGISTERED
############################################################################
contr <-
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000000))

full_exp2 <- glmer(subject_changed ~
                     Condition.New * z.Trial +
                     (1 + (Condition.New.Partner.code + Condition.New.Subject.code + Condition.New.SubjectPartner.code) * z.Trial | Chimp),
                   data = t.data, control = contr, family = binomial(link = "logit"))

main_exp2 <- glmer(subject_changed ~
                     Condition.New + z.Trial +
                     (1 + (Condition.New.Partner.code + Condition.New.Subject.code + Condition.New.SubjectPartner.code) * z.Trial | Chimp),
                   data = t.data, control = contr, family = binomial(link = "logit"))


null_exp2 <- glmer(subject_changed ~
                     1 +
                     (1 + (Condition.New.Partner.code + Condition.New.Subject.code + Condition.New.SubjectPartner.code) * z.Trial | Chimp),
                   data = t.data, control = contr, family = binomial(link = "logit"))


summary(full_exp2)$varcor
ranef.diagn.plot(full_exp2)
round(summary(full_exp2)$coefficients, 3) # massive SDs

############################################################################
# CHECKING ASSUMPTIONS
############################################################################
overdisp.test(full_exp2)
library(car)
vif(main_exp2)
# Checking model stability
m.stab.b <-
  glmm.model.stab(model.res = full_exp2, contr = contr, use = c("Chimp"))
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
round(anova(full_exp2, null_exp2, test = "Chisq"), 3)
round(drop1(full_exp2, test = "Chisq"), 3)
round(drop1(main_exp2, test = "Chisq"), 3)

## First peek at effects
plot(effect("Condition.New", main_exp2), type = "response")

# Coefficients of the full_exp2 model
round(summary(full_exp2)$coefficients, 3)

# Get estimated marginal means (EMMs) for the 'Condition' factor
emm <- emmeans(main_exp2, ~ Condition.New, type = "response")
pairs(emm, adjust = "none")

############################################################################
# BOOTSTRAPS
############################################################################
## Bootstraps of full_exp2 model
# The bootstrap has already been run and is saved in the image
boot.full_exp2 <- boot.glmm.pred(
  model.res = full_exp2, excl.warnings = T,
  nboots = 1000, para = F, level = 0.95,
  use = c("Condition", "z.Trial")
)

round(boot.full_exp2$ci.estimates, 3)
m.stab.plot(round(boot.full_exp2$ci.estimates, 3))
boot.full_exp2$ci.predicted

boot.main_exp2 <- boot.glmm.pred(
  model.res = main_exp2, excl.warnings = T,
  nboots = 1000, para = F, level = 0.95,
  use = c("Condition")
)

round(boot.main_exp2$ci.estimates, 3)
m.stab.plot(round(boot.main_exp2$ci.estimates, 3))
boot.main_exp2$ci.predicted

load("old_scripts_without_2x2_design/Stage_2.RData")
