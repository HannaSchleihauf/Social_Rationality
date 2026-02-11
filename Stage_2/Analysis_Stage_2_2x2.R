############################################################################
# PACKAGES & FUNCTIONS
############################################################################
install.packages("optimix")
library("easypackages")
libraries(
  "lme4", "tidyverse", "emmeans", "car",
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
    "subject_changed ~ Subject_Evidence*Partner_Evidence*Trial",
  re = "(1|Chimp)", other.vars = c("Condition.New", "Condition"), data = xxdata
)
xx.fe.re$summary
t.data <- xx.fe.re$data
str(t.data)

# Center dummy variables
t.data$Subject_Evidence.Strong.code <-
  t.data$Subject_Evidence.Strong - mean(t.data$Subject_Evidence.Strong)
t.data$Partner_Evidence.Strong.code <-
  t.data$Partner_Evidence.Strong - mean(t.data$Partner_Evidence.Strong)
t.data$z.Trial <-
  scale(t.data$Trial)

tapply(
  t.data$subject_changed,
  list(t.data$Condition, as.factor(t.data$Trial)), mean, na.rm = TRUE
)

############################################################################
# FITTING THE MODEL
############################################################################
contr <-
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000000))

full_exp2 <- glmer(subject_changed ~
                     Subject_Evidence*Partner_Evidence * z.Trial +
                     (1 + (Subject_Evidence.Strong.code * Partner_Evidence.Strong.code) * z.Trial || Chimp),
                   data = t.data, control = contr, family = binomial(link = "logit"))

red_exp2 <- glmer(subject_changed ~
                     (Subject_Evidence * Partner_Evidence) + z.Trial +
                     (1 + (Subject_Evidence.Strong.code * Partner_Evidence.Strong.code) * z.Trial || Chimp),
                   data = t.data, control = contr, family = binomial(link = "logit"))

main_exp2 <- glmer(subject_changed ~
                     (Subject_Evidence + Partner_Evidence + z.Trial) +
                     (1 + (Subject_Evidence.Strong.code * Partner_Evidence.Strong.code) * z.Trial | Chimp),
                   data = t.data, control = contr, family = binomial(link = "logit"))


null_exp2 <- glmer(subject_changed ~
                     1 +
                     (1 + (Subject_Evidence.Strong.code * Partner_Evidence.Strong.code) * z.Trial | Chimp),
                   data = t.data, control = contr, family = binomial(link = "logit"))


ranef.diagn.plot(full_exp2)
round(summary(red_exp2)$coefficients, 3) # massive SDs

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
round(drop1(red_exp2, test = "Chisq"), 3)
round(drop1(main_exp2, test = "Chisq"), 3)

## First peek at effects
plot(effect("Subject_Evidence:Partner_Evidence", red_exp2), type = "response")
plot(effect(c("Subject_Evidence"), main_exp2), type = "response")
plot(effect(c("Partner_Evidence"), main_exp2), type = "response")

# Coefficients of the full_exp2 model
round(summary(full_exp2)$coefficients, 3)

# Get estimated marginal means (EMMs) for the 'Condition' factor
emm <- emmeans(red_exp2, ~ Subject_Evidence*Partner_Evidence, type = "response")
pairs(emm, adjust = "none")

pairs(emmeans(emm, ~ Subject_Evidence | Partner_Evidence), adjust = "none")
pairs(emmeans(emm, ~ Partner_Evidence | Subject_Evidence), adjust = "none")

emm_cells <- emmeans(main_exp2, ~ Partner_Evidence * Subject_Evidence, type = "response")
pairs(emm_cells, adjust = "bonferroni")


############################################################################
# BOOTSTRAPS
############################################################################
## Bootstraps of full_exp2 model
# The bootstrap has already been run and is saved in the image
boot.red_exp2 <- boot.glmm.pred(
  model.res = red_exp2, excl.warnings = T,
  nboots = 1000, para = T, level = 0.95, n.cores=c("all"),
  use = c("Subject_Evidence", "Partner_Evidence")
)
load("./boot.red_exp2.RData")
round(boot.red_exp2$ci.estimates, 3)
m.stab.plot(round(boot.red_exp2$ci.estimates, 3))
boot.red_exp2$ci.predicted

boot.main_exp2 <- boot.glmm.pred(
  model.res = main_exp2, excl.warnings = T,
  nboots = 1000, para = T, level = 0.95, n.cores=c("all"),
  use = c("Subject_Evidence", "Partner_Evidence")
)
load("./boot.main_exp2.RData")
round(boot.main_exp2$ci.estimates, 3)
m.stab.plot(round(boot.main_exp2$ci.estimates, 3))
boot.main_exp2$ci.predicted

load("./Stage_2_2x2_other_laptop.RData")


boot.main_exp2$all.boots
