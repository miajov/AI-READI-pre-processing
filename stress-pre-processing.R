library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(lme4)


# List participant folders from directory 
# *Hour* sample timeframe

##change path 
base_dir <- "~/Downloads/OneDrive_stress/"
participant_folders <- list.dirs(base_dir, full.names = FALSE, recursive = FALSE)
participant_ids <- participant_folders

# Process each participant and save individual hourly stress
for (participant_id in participant_ids) {
  message("Processing participant: ", participant_id)
  
  participant_folder <- file.path(base_dir, participant_id)
  json_file <- file.path(participant_folder, paste0(participant_id, "_stress.json"))
  
  if (!file.exists(json_file)) {
    message("  → JSON file not found - skipping.")
    next
  }
  
  json_data <- fromJSON(json_file, flatten = TRUE)
  
  if (is.null(json_data$body$stress)) {
    message("  → No stress data - skipping.")
    next
  }
  
  stress_data <- json_data$body$stress %>%
    mutate(
      user_id = json_data$header$user_id,
      uuid = json_data$header$uuid,
      creation_date = json_data$header$creation_date_time,
      stress_value = stress.value,
      stress_time = ymd_hms(effective_time_frame.date_time)
    ) %>%
    select(user_id, uuid, creation_date, stress_time, stress_value) %>%
    filter(!is.na(stress_value))
  
  hourly_stress <- stress_data %>%
    mutate(hour = floor_date(stress_time, unit = "hour")) %>%
    group_by(user_id, hour) %>%
    summarise(mean_stress = mean(stress_value, na.rm = TRUE), .groups = "drop") %>%
    arrange(hour) %>%
    mutate(hour_number = row_number())
  
  saveRDS(hourly_stress, file = file.path(participant_folder, paste0(participant_id, "_hourly_stress.rds")))
  message("  → Processed and saved hourly stress for participant ", participant_id)
}

# Combine all participants
all_hourly_stress_list <- list()

for (participant_id in participant_ids) {
  participant_folder <- file.path(base_dir, participant_id)
  rds_file <- file.path(participant_folder, paste0(participant_id, "_hourly_stress.rds"))
  
  if (file.exists(rds_file)) {
    df <- readRDS(rds_file)
    df$participant_id <- participant_id
    all_hourly_stress_list[[participant_id]] <- df
  }
}

combined_hourly_stress <- bind_rows(all_hourly_stress_list)

## Open tasks: to inspect missing data
## Open tasks: to plot descriptives 

# Remove zero-stress or invalid days
zero_stress_days <- combined_hourly_stress %>%
  mutate(day = as.Date(hour)) %>%
  group_by(participant_id, day) %>%
  summarise(avg_stress = mean(mean_stress, na.rm = TRUE), .groups = "drop") %>%
  filter(is.na(avg_stress) | avg_stress == 0)

cleaned_stress_data <- combined_hourly_stress %>%
  mutate(day = as.Date(hour)) %>%
  anti_join(zero_stress_days, by = c("participant_id", "day")) %>%
  group_by(participant_id) %>%
  arrange(participant_id, day, hour) %>%
  mutate(
    day_number = dense_rank(day),
    hour_number = row_number()
  ) %>%
  ungroup()

cleaned_stress_data$id <- gsub("AIREADI-", "", cleaned_stress_data$user_id)