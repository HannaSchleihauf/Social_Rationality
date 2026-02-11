# Get a quick plot of finding

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

# ---- BOOT data: create condition factor ----
boot.main_exp2$ci.predicted$Condition <- paste(
  boot.main_exp2$ci.predicted$Subject_Evidence,
  boot.main_exp2$ci.predicted$Partner_Evidence,
  sep = "_"
)

boot.main_exp2$ci.predicted <- boot.main_exp2$ci.predicted %>%
  mutate(
    Condition = factor(
      Condition,
      levels = c("None_None", "None_Strong", "Strong_None", "Strong_Strong")
    )
  )

# ---- MAIN data: condition factor ----
xxdata$Condition <- factor(
  xxdata$Condition,
  levels = c("Subject -, Partner -", "Subject -, Partner +",
             "Subject +, Partner -", "Subject +, Partner +")
)

xxdata$subject_changed.nr <- as.numeric(xxdata$subject_changed)

# Which conditions should stay visible?
keep_cond <- c("Subject -, Partner +", "Subject +, Partner -")

# Aggregate on chimp level
xxdata.agg.Chimp <- xxdata %>%
  group_by(Chimp, Condition) %>%
  summarise(mean.resp = mean(subject_changed.nr, na.rm = TRUE), .groups = "drop")

# move points for plot (KEEP ALL 4 conditions so spacing stays identical)
xxdata.agg.Chimp <- xxdata.agg.Chimp %>%
  group_by(Condition, mean.resp) %>%
  mutate(trial.per.cell = row_number()) %>%
  ungroup() %>%
  mutate(
    Condition_num = as.numeric(as.factor(Condition)),
    Condition2 = Condition_num - 0.08 - (trial.per.cell * 0.04),
    show_alpha = ifelse(Condition %in% keep_cond, 0.8, 0)   # outer -> invisible
  )

# For lines: ONLY draw inner conditions (so no grey lines to outer conditions)
xxdata.agg.Chimp_line <- xxdata.agg.Chimp %>%
  filter(Condition %in% keep_cond)

# Boot: create numeric x and alpha for visibility
boot.main_exp2$ci.predicted <- boot.main_exp2$ci.predicted %>%
  mutate(
    # map to x = 1..4
    x = as.numeric(Condition),
    # visible only for middle two conditions
    show_alpha = ifelse(Condition %in% c("None_Strong", "Strong_None"), 1, 0)
  )

# ggplot
plot_Condition <-
  ggplot() +
  geom_line(
    data = xxdata.agg.Chimp_line,
    aes(x = Condition2, y = mean.resp, group = Chimp),
    color = "gray", lty = 1, alpha = .7
  ) +
  geom_point(
    data = xxdata.agg.Chimp,
    aes(x = Condition2, y = mean.resp, fill = Condition, alpha = show_alpha),
    size = 2.5, shape = 21, color = "black"
  ) +
  geom_errorbar(
    data = boot.main_exp2$ci.predicted,
    aes(
      x = x,
      y = fitted,
      ymin = lower.cl, ymax = upper.cl,
      color = Condition,
      alpha = show_alpha
    ),
    width = 0.1, size = 1
  ) +
  geom_point(
    data = boot.main_exp2$ci.predicted,
    aes(x = x, y = fitted, color = Condition, alpha = show_alpha),
    size = 3.5
  ) +
  # NOTE: x is numeric, so use continuous scale with fixed breaks (keeps empty slots!)
  scale_x_discrete(
    limits = c("None", "Partner", "Subject", "SubjectPartner"),
    name = "evidence combination",
    labels = c("Subject None\nPartner None", "Subject None\nPartner Strong", "Subject Strong\nPartner None", "Subject Strong\nPartner Strong")
  ) +
  scale_y_continuous(
    name = "belief revision",
    labels = scales::percent
  ) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0.95, NA)) +
  geom_violinhalf(
    data = xxdata.agg.Chimp,
    aes(x = Condition_num, y = mean.resp, fill = Condition, alpha = show_alpha),
    position = position_nudge(x = 0.1)
  ) +
  scale_alpha_identity(guide = "none") +   # use alpha values as-is (0 or 0.8)
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
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
    legend.position = "none"
  )


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
