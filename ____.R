
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

# Which Condition.News should stay visible?
keep_cond <- c("Partner Evidence", "Subject Evidence")

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

# move points for plot (KEEP ALL 4 Condition.News so spacing stays identical)
xxdata.agg.Chimp <- xxdata.agg.Chimp %>%
  group_by(Condition, mean.resp) %>%
  mutate(trial.per.cell = row_number()) %>%
  ungroup() %>%
  mutate(
    Condition.New_num = as.numeric(as.factor(Condition.New)),
    Condition2 = Condition.New_num - 0.08 - (trial.per.cell * 0.04),
    show_alpha = ifelse(Condition.New %in% keep_cond, 0.8, 0)   # outer -> invisible
  )

# For lines: ONLY draw inner Condition.News (so no grey lines to outer Condition.News)
xxdata.agg.Chimp_line <- xxdata.agg.Chimp %>%
  filter(Condition.New %in% keep_cond)

# Boot: create numeric x and alpha for visibility
boot.main_exp1$ci.predicted <- boot.main_exp1$ci.predicted %>%
  mutate(
    # map to x = 1..4
    x = as.numeric(Condition.New),
    # visible only for middle two Condition.News
    show_alpha = ifelse(Condition.New %in% c("Partner", "Subject"), 1, 0)
  )

# ggplot
plot_Condition.New <-
  ggplot() +
  geom_line(
    data = xxdata.agg.Chimp_line,
    aes(x = Condition2, y = mean.resp, group = Chimp),
    color = "gray", lty = 1, alpha = .7
  ) +
  geom_point(
    data = xxdata.agg.Chimp,
    aes(x = Condition2, y = mean.resp, fill = Condition.New, alpha = show_alpha),
    size = 2.5, shape = 21, color = "black"
  ) +
  geom_errorbar(
    data = boot.main_exp2$ci.predicted,
    aes(
      x = x,
      y = fitted,
      ymin = lower.cl, ymax = upper.cl,
      color = Condition.New,
      alpha = show_alpha
    ),
    width = 0.1, size = 1
  ) +
  geom_point(
    data = boot.main_exp2$ci.predicted,
    aes(x = x, y = fitted, color = Condition.New, alpha = show_alpha),
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
    aes(x = Condition.New_num, y = mean.resp, fill = Condition.New, alpha = show_alpha),
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

plot_Condition.New
