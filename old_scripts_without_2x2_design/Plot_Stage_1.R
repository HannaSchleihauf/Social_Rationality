
# Get a quick blot of finding
load("old_scripts_without_2x2_design/Stage_1.RData")

library("easypackages")
libraries(
  "lme4", "tidyverse", "tidyselect", "optimx", "emmeans", "car",
  "effects", "agridat", "ggplot2", "ghibli", "ggdist", "see", "gghalves",
  "ggthemes", "cowplot", "viridis", "directlabels", "ggsci", "geomtextpath",
  "geomtextpath"
)
source("/Users/Schle008/Dropbox/Research/my_workshops/linear_models/Linear Models Course/Functions/paired_data_functions.r")

xxdata.agg <- xxdata %>%
  group_by(Condition) %>%
  summarise(
    mean.condition = mean(correct_choice_nr, na.rm = TRUE),  # Calculate mean
    sd.condition = sd(correct_choice_nr, na.rm = TRUE)      # Calculate standard deviation
  ) %>%
  # Create the upper and lower error bars, constrained between 0 and 1
  mutate(
    upper.limit = pmin(mean.condition + sd.condition, 1),  # Ensure the upper limit does not exceed 1
    lower.limit = pmax(mean.condition - sd.condition, 0)   # Ensure the lower limit does not go below 0
  )

#aggregate on chimp level
xxdata.agg.Chimp <- xxdata %>%
  group_by(Chimp, Condition) %>%
  summarise(mean.resp = mean(correct_choice_nr, na.rm = TRUE))

#Recorder all factors
boot.main_exp1$ci.predicted$Condition <- factor(
  boot.main_exp1$ci.predicted$Condition,
  levels = c(
    "Subject No Evidence",
    "Subject Evidence",
    "Partner No Evidence",
    "Partner Evidence"
  )
)

xxdata.agg.Chimp$Condition <- factor(
  xxdata.agg.Chimp$Condition,
  levels = c(
    "Subject No Evidence",
    "Subject Evidence",
    "Partner No Evidence",
    "Partner Evidence"
  )
)

xxdata.agg$Condition <- factor(
  xxdata.agg$Condition,
  levels = c(
    "Subject No Evidence",
    "Subject Evidence",
    "Partner No Evidence",
    "Partner Evidence"
  )
)




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
  geom_line(
    data = xxdata.agg.Chimp,
    aes(x = Condition2, y = mean.resp, group = Chimp),
    color = "gray", lty = 1, alpha = .7
  ) +
  geom_point(
    data = xxdata.agg.Chimp,
    aes(x = Condition2, y = mean.resp, fill = Condition),
    size = 2.5, alpha = .8, shape = 21, color = "black"
  ) +
  geom_errorbar(
    data = boot.main_exp1$ci.predicted,
    aes(
      x = as.numeric(Condition),
      y = fitted,
      ymin = lower.cl, ymax = upper.cl,
      color = Condition
    ),
    width = 0.1, size = 1
  ) +
  geom_point(
    data = boot.main_exp1$ci.predicted,
    aes(
      x = as.numeric(Condition), y = fitted,
      color = Condition
    ), size = 3.5
  ) +
  scale_x_discrete(
    limits = c("Subject No Evidence", "Subject Evidence", "Partner No Evidence", "Partner Evidence"),
    name = "type of evidence",
    labels = c("Subject No Evidence", "Subject Evidence", "Partner No Evidence", "Partner Evidence")
  ) +
  scale_y_continuous(
    name = "followed evidence",
    labels = scales::percent
  ) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0.95, NA)) +
  geom_violinhalf(
    data = xxdata.agg.Chimp,
    aes(x = Condition, y = mean.resp, fill = Condition),
    position = position_nudge(x = 0.1),
    alpha = .8
  ) +
  geom_hline(
    yintercept = 0.5,
    linetype   = "dashed",
    color      = "grey50",
    linewidth  = 0.4
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
