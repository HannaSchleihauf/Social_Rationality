
# Get a quick blot of finding

library("easypackages")
libraries(
  "lme4", "tidyverse", "tidyselect", "optimx", "emmeans", "car",
  "effects", "agridat", "ggplot2", "ghibli", "ggdist", "see", "gghalves",
  "ggthemes", "cowplot", "viridis", "directlabels", "ggsci", "geomtextpath",
  "geomtextpath"
)
source("/Users/Schle008/Dropbox/Research/my_workshops/linear_models/Linear Models Course/Functions/paired_data_functions.r")

xxdata <- data_long_filtered

# Remove the "Trial_" part and keep only the number
xxdata$Trial_nr <- as.numeric(gsub("Trial_", "", xxdata$Trial_New))

boot.red_exp2$ci.predicted$Condition <- paste(boot.red_exp2$ci.predicted$Subject_Evidence, boot.red_exp2$ci.predicted$Partner_Evidence, sep = "_")
boot.red_exp2$ci.predicted <- boot.red_exp2$ci.predicted %>%
  mutate(
    Condition = factor(
      Condition,
      levels = c("None_None", "None_Strong", "Strong_None",
                 "Strong_Strong")
    )
  )

levels(as.factor(xxdata$Condition))

xxdata$Condition <-
  factor(xxdata$Condition,
         levels = c("Subject -, Partner -", "Subject -, Partner +", "Subject +, Partner -", "Subject +, Partner +")
  )

xxdata$subject_changed.nr <-
  as.numeric(xxdata$subject_changed)

xxdata.agg <- xxdata %>%
  group_by(Condition) %>%
  summarise(
    mean.condition = mean(subject_changed.nr, na.rm = TRUE),  # Calculate mean
    sd.condition = sd(subject_changed.nr, na.rm = TRUE)      # Calculate standard deviation
  ) %>%
  # Create the upper and lower error bars, constrained between 0 and 1
  mutate(
    upper.limit = pmin(mean.condition + sd.condition, 1),  # Ensure the upper limit does not exceed 1
    lower.limit = pmax(mean.condition - sd.condition, 0)   # Ensure the lower limit does not go below 0
  )

#aggregate on chimp level
xxdata.agg.Chimp <- xxdata %>%
  group_by(Chimp, Condition) %>%
  summarise(mean.resp = mean(subject_changed.nr, na.rm = TRUE))

# move points for plot
xxdata.agg.Chimp <- xxdata.agg.Chimp %>%
  group_by(Condition, mean.resp) %>%
  mutate(trial.per.cell = row_number()) %>%
  ungroup() %>%
  mutate(Condition2 = as.numeric(as.factor(Condition)) -
           0.08 - (trial.per.cell * 0.04))

# ggplot
plot_Condition <-
  ggplot() +
  geom_point(
    data = xxdata.agg.Chimp,
    aes(x = Condition2, y = mean.resp, fill = Condition),
    size = 2.5, alpha = .8, shape = 21, color = "black"
  ) +
  geom_line(
    data = xxdata.agg.Chimp,
    aes(x = Condition2, y = mean.resp, group = Chimp),
    color = "gray", lty = 1, alpha = .7
  ) +
  geom_errorbar(
    data = boot.red_exp2$ci.predicted,
    aes(
      x = as.numeric(Condition),
      y = fitted,
      ymin = lower.cl, ymax = upper.cl,
      color = Condition
    ),
    width = 0.1, size = 1
  ) +
  geom_point(
    data = boot.red_exp2$ci.predicted,
    aes(
      x = as.numeric(Condition), y = fitted,
      color = Condition
    ), size = 3.5
  ) +
  scale_x_discrete(
    limits = c("None", "Partner", "Subject", "SubjectPartner"),
    name = "evidence combination",
    labels = c("Subject -, Partner -", "Subject -, Partner +", "Subject +, Partner -", "Subject +, Partner +")
  ) +
  scale_y_continuous(
    name = "belief revision",
    labels = scales::percent
  ) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0.95, NA)) +
  geom_violinhalf(
    data = xxdata.agg.Chimp,
    aes(x = as.numeric(Condition), y = mean.resp, fill = Condition),
    position = position_nudge(x = 0.1),
    alpha = .8
  ) +
  scale_fill_viridis_d(begin = .25, end = .9, option = "magma") +
  scale_color_viridis_d(begin = .25, end = .9, option = "magma") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
    strip.text.x = element_text(size = 13),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.title.y.left = element_text(vjust = 3),
    plot.title = element_text(color = "black", size = 15, face = "bold"),
    axis.title.x = element_text(
      margin = margin(t = 10, r = 0, b = 0, l = 0)
    ), legend.position = "none"
  )
plot_Condition
