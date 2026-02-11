# Make a clean data frame

library(readxl)
library(tidyverse)

xdata <- read_xlsx("./Stage_1/data/Social_Rationality_Stage_1_for_R.xlsx")

# Convert from wide to long format, including all 'Trial' and 'Filler' columns
data_long <- xdata %>%
  pivot_longer(
    cols = starts_with("Trial") | starts_with("Filler"),  # Include columns starting with "Trial" or "Filler"
    names_to = "Trial",                                   # Create a new column for trial names
    values_to = "Food_Location"                            # Create a new column for the food location values
  )

data_long <- data_long %>%
  mutate(
    Trial_New = sub("([^_]+_[^_]+)_(.*)", "\\1", Trial),  # First part (before second underscore)
    Location = sub("([^_]+_[^_]+)_(.*)", "\\2", Trial)    # Second part (after second underscore)
  ) %>%
  select(-Trial) %>% # Drop the original Trial column
pivot_wider(
  names_from = Who,
  values_from = c(Food_Location)
) %>%
  mutate(Subject = ifelse(Subject == FALSE, NA, Subject)) %>%
  mutate(Partner = ifelse(Partner == FALSE, NA, Partner)) %>%
  mutate(Subject = ifelse(Subject == TRUE, Location, Subject)) %>%
  mutate(Partner = ifelse(Partner == TRUE, Location, Partner)) %>%
  mutate(Food = ifelse(Food == "Food", Location, Food)) %>%
  select(-Location) %>%
  group_by(id, Chimp, Partner_Name, Condition, Session, Trial_New) %>%
  summarise(across(everything(), ~ coalesce(.[1], .[2]), .names = "{.col}")) %>%
  ungroup() %>%
  mutate(correct_choice = if_else(Food == Subject, "yes", Subject)) %>%
  mutate(correct_choice = if_else((correct_choice == "Front" | correct_choice == "Back"), "no", correct_choice))

data_long$correct_choice_nr <-
  as.numeric(as.factor(data_long$correct_choice)) - 1

#In which trials chose the partner something wrong:
data_long[which(data_long$Partner != data_long$Food), ]

# Exclude fillers
xxdata <-
  data_long  %>%
  filter(!str_starts(Trial_New, "Filler"))


# Remove the "Trial_" part and keep only the number
xxdata$Trial_nr <- as.numeric(gsub("Trial_", "", xxdata$Trial_New))

# change level order:
xxdata <- xxdata %>%
  mutate(
    Condition = factor(
      Condition,
      levels = c("Subject No Evidence", "Subject Evidence",
                 "Partner Evidence", "Partner No Evidence")
    ),
    Receiver_Evidence = case_when(
      Condition %in% c("Subject Evidence", "Subject No Evidence") ~ "Subject",
      Condition %in% c("Partner Evidence", "Partner No Evidence") ~ "Partner",
      TRUE ~ NA_character_
    ),
    Evidence = case_when(
      Condition %in% c("Subject Evidence", "Partner Evidence") ~ "Evidence",
      Condition %in% c("Subject No Evidence", "Partner No Evidence") ~ "None",
      TRUE ~ NA_character_
    ),
    Receiver_Evidence = factor(Receiver_Evidence, levels = c("Subject", "Partner")),
    Evidence = factor(Evidence, levels = c("Evidence", "None"))
  )


save.image("Stage_1.RData")
write.csv(data_long, "./Stage_1/data/Stage_1_data_for_analysis.csv", row.names = FALSE)


