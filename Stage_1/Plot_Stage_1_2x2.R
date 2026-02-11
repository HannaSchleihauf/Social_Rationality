
# Get packages
pkgs <- c(
  "lme4","tidyverse","tidyselect","optimx","emmeans","car",
  "effects","agridat","ggplot2","ghibli","ggdist","see",
  "ggthemes","cowplot","viridis","directlabels","ggsci","geomtextpath"
)

# install only missing
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install)

# load
invisible(lapply(pkgs, library, character.only = TRUE))

# Make a new variable so that the order of the factor is correct
boot.main_exp1$ci.predicted$Condition <- paste(boot.main_exp1$ci.predicted$Receiver_Evidence, boot.main_exp1$ci.predicted$Evidence, sep = "_")
boot.main_exp1$ci.predicted <- boot.main_exp1$ci.predicted %>%
  mutate(
    Condition = factor(
      Condition,
      levels = c("Subject_None", "Subject_Evidence",
                 "Partner_Evidence", "Partner_None")
    )
  )

xxdata.agg <- xxdata %>%
  group_by(Condition) %>%
  summarise(
    mean.condition = mean(correct_choice_nr, na.rm = TRUE),
    sd.condition   = sd(correct_choice_nr, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    upper.limit = pmin(mean.condition + sd.condition, 1),
    lower.limit = pmax(mean.condition - sd.condition, 0))


#aggregate on chimp level
xxdata.agg.Chimp <- xxdata %>%
  group_by(Chimp, Condition) %>%
  summarise(mean.resp = mean(correct_choice_nr, na.rm = TRUE))

# move points for plot
xxdata.agg.Chimp <- xxdata.agg.Chimp %>%
  group_by(Condition, mean.resp) %>%
  mutate(trial.per.cell = row_number()) %>%
  ungroup() %>%
  mutate(Condition2 = as.numeric(as.factor(Condition)) -
           0.08 - (trial.per.cell * 0.04))

levels(xxdata.agg.Chimp$Condition)
levels(as.factor(boot.main_exp1$ci.predicted$Condition))

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
    limits = c("Subject No Evidence", "Subject Evidence", "Partner Evidence", "Partner No Evidence"),
    name = "type of evidence",
    labels = c("Subject No Evidence", "Subject Evidence", "Partner Evidence", "Partner No Evidence")
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
