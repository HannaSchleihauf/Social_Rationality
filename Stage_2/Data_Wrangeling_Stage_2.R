# Make a clean data frame
library(readxl)
library(tidyverse)

xdata <- read_xlsx("./Stage_2/data/Stage_2_Social_Rationality_April_28_for_R.xlsx")

# Convert from wide to long format, including all 'Trial' and 'Filler' columns
data_long <- xdata %>%
  pivot_longer(
    cols = starts_with("Trial") | starts_with("Filler"),  # Include columns starting with "Trial" or "Filler"
    names_to = "Trial",                                   # Create a new column for trial names
    values_to = "Food_Choice_Location"                     # Create a new column for the location values
  )

data_long <- data_long %>%
  mutate(
    Trial_New = sub("([^_]+_[^_]+)_(.*)", "\\1", Trial),  # First part (before second underscore)
    Location = sub("([^_]+_[^_]+)_(.*)", "\\2", Trial)    # Second part (after second underscore)
  )

data_long <- data_long %>%
  select(-Trial) %>% # Drop the original Trial column
  pivot_wider(
    names_from = Who,
    values_from = c(Food_Choice_Location)
  )

data_long <- data_long %>%
  mutate(Subject_1 = ifelse(Subject_1 == FALSE, NA, Subject_1)) %>%
  mutate(Partner_2 = ifelse(Partner_2 == FALSE, NA, Partner_2)) %>%
  mutate(Subject_3 = ifelse(Subject_3 == FALSE, NA, Subject_3)) %>%
  mutate(Subject_1 = ifelse(Subject_1 == TRUE, Location, Subject_1)) %>%
  mutate(Partner_2 = ifelse(Partner_2 == TRUE, Location, Partner_2)) %>%
  mutate(Subject_3 = ifelse(Subject_3 == TRUE, Location, Subject_3)) %>%
  mutate(Food = ifelse(Food == "Food", Location, Food))

data_long <- data_long %>%
  mutate(
    Strong_Evidence_Subject = if_else(
      str_starts(Trial_New, "Trial"),
      case_when(
        Condition %in% c("Subject +, Partner +", "Subject +, Partner -") ~ Food,
        Condition %in% c("Subject -, Partner +", "Subject -, Partner -") ~ "None",
        TRUE ~ NA_character_),
      NA_character_))


data_long <- data_long %>%
      mutate(
    Strong_Evidence_Partner = case_when(
      # Filler condition first → takes precedence
      str_starts(Trial_New, "Filler") ~ Food,
      # Trial condition
      str_starts(Trial_New, "Trial") & Condition %in% c("Subject +, Partner +") ~ case_when(
        Food == "Back" ~ "Front",
        Food == "Front" ~ "Back",
        TRUE ~ NA_character_),
      str_starts(Trial_New, "Trial") & Condition %in% c("Subject -, Partner +") ~ case_when(
        Food == "Back" ~ "Back",
        Food == "Front" ~ "Front",
        TRUE ~ NA_character_),
      str_starts(Trial_New, "Trial") & Condition %in% c("Subject -, Partner -", "Subject +, Partner -") ~ "None",
      TRUE ~ NA_character_))

data_long <- data_long %>%
  select(-Location) %>% # Drop the original Trial column
  group_by(Chimp, Partner, Condition, Session, Trial_New)

data_long <- data_long %>%
  summarise(across(everything(), ~ coalesce(.[1], .[2]), .names = "{.col}")) %>%
  ungroup()

# Remove Filler Trials
data_long_filtered <- data_long %>%
  filter(str_starts(Trial_New, "Trial")) %>%
  mutate(subject_changed = if_else(
     Subject_1 != Subject_3,
    TRUE, FALSE
  )) %>%
  mutate(Trial = str_replace(Trial_New, "Trial_", "")) %>%
  mutate(Condition.New = case_when(
    Condition == "Subject +, Partner +" ~ "SubjectPartner",
    Condition == "Subject +, Partner -" ~ "Subject",
    Condition == "Subject -, Partner +" ~ "Partner",
    Condition == "Subject -, Partner -" ~ "None",
    TRUE ~ NA_character_  # fallback for unmatched cases
  )) %>%
  mutate(Subject_Evidence = case_when(
    Condition.New %in% c("SubjectPartner", "Subject") ~ "Strong",
    Condition.New %in% c("Partner", "None") ~ "None"),
        Partner_Evidence = case_when(
    Condition.New %in% c("SubjectPartner", "Partner") ~ "Strong",
    Condition.New %in% c("Subject", "None") ~ "None"))


levels(as.factor(data_long_filtered$Condition))

data_long_filtered %>%
  group_by(Condition) %>%
  summarise(mean_change = mean(subject_changed, na.rm = TRUE)) %>%
  ggplot(aes(x = Condition, y = mean_change)) +
  geom_col(fill = "steelblue") +
  ylim(0, 1) +
  labs(title = "Mean Subject Change by Condition",
       y = "Proportion of Trials with Belief Change",
       x = "Condition") +
  theme_minimal()

data_long_filtered$Trial <- as.numeric(data_long_filtered$Trial)

write.csv(data_long, "./Stage_2/data/Stage_2_data_for_analysis.csv", row.names = FALSE)

