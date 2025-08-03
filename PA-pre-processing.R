library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)


# Define base directory
base_dir <- "~/Downloads/garmin_vivosmart5/"

# Process each participant and save individual hourly steps
for (participant_id in matched_ids) {
  message("Processing participant: ", participant_id)
  
  participant_folder <- file.path(base_dir, participant_id)
  json_file <- file.path(participant_folder, paste0(participant_id, "_activity.json"))
  
  if (!file.exists(json_file)) {
    message("  → JSON file not found - skipping.")
    next
  }
  
  json_data <- fromJSON(json_file, flatten = TRUE)
  
  if (is.null(json_data$body$activity)) {
    message("  → No activity data - skipping.")
    next
  }
  
  activity_data <- json_data$body$activity %>%
    mutate(
      user_id = json_data$header$user_id,
      uuid = json_data$header$uuid,
      creation_date = json_data$header$creation_date_time,
      start_date = as.Date(effective_time_frame.time_interval.start_date_time),
      end_date = as.Date(effective_time_frame.time_interval.end_date_time)
    ) %>%
    rename(
      activity = activity_name,
      start_time = effective_time_frame.time_interval.start_date_time,
      end_time = effective_time_frame.time_interval.end_date_time,
      steps = base_movement_quantity.value
    ) %>%
    select(user_id, uuid, creation_date, activity, start_time, end_time, steps, start_date, end_date) %>%
    mutate(
      start_time = ymd_hms(start_time),
      end_time = ymd_hms(end_time),
      steps = as.numeric(steps)
    )
  
  # Filter for relevant activities and valid steps
  activity_filtered <- activity_data %>%
    filter(activity %in% c("walking", "running", "generic")) %>%
    filter(!is.na(steps))
  
  # Aggregate steps per hour
  hourly_steps <- activity_filtered %>%
    mutate(hour = floor_date(start_time, unit = "hour")) %>%  # round down to hour
    group_by(user_id, activity, hour) %>%
    summarise(hourly_steps = sum(steps, na.rm = TRUE), .groups = "drop")
  
  # Pivot to wide format
  hourly_steps_wide <- hourly_steps %>%
    pivot_wider(
      names_from = activity,
      values_from = hourly_steps,
      values_fill = list(hourly_steps = 0)
    )
  
  # Add missing activity columns if needed
  if (!"walking" %in% names(hourly_steps_wide)) hourly_steps_wide$walking <- 0
  if (!"running" %in% names(hourly_steps_wide)) hourly_steps_wide$running <- 0
  if (!"generic" %in% names(hourly_steps_wide)) hourly_steps_wide$generic <- 0
  
  # Ensure numeric columns and create total_steps
  hourly_steps_wide <- hourly_steps_wide %>%
    mutate(
      walking = as.numeric(walking),
      running = as.numeric(running),
      generic = as.numeric(generic),
      total_steps = walking + running  # sum walking + running only
    ) %>%
    arrange(hour) %>%
    mutate(hour_number = row_number())  # index hours per participant
  
  # Save the processed data
  saveRDS(hourly_steps_wide, file = file.path(participant_folder, paste0(participant_id, "_hourly_steps_wide.rds")))
  
  message("  → Processed and saved hourly steps for participant ", participant_id)
}

# --------------------------
# Combine all participants hourly steps into one big dataframe
# --------------------------
all_hourly_steps_list <- list()

for (participant_id in matched_ids) {
  participant_folder <- file.path(base_dir, participant_id)
  rds_file <- file.path(participant_folder, paste0(participant_id, "_hourly_steps_wide.rds"))
  
  if (file.exists(rds_file)) {
    df <- readRDS(rds_file)
    df$participant_id <- participant_id  # Add participant ID column
    all_hourly_steps_list[[participant_id]] <- df
  }
}

combined_hourly_steps <- bind_rows(all_hourly_steps_list)

# Calculate zero-step days per participant-day
zero_step_days <- combined_hourly_steps %>%
  mutate(day = as.Date(hour)) %>%
  group_by(participant_id, day) %>%
  summarise(daily_total_steps = sum(total_steps, na.rm = TRUE), .groups = "drop") %>%
  filter(daily_total_steps == 0)

num_participants_with_missing <- zero_step_days %>%
  distinct(participant_id) %>%
  nrow()

num_zero_step_days <- nrow(zero_step_days)

# Total number of hourly observations before filtering
total_hourly_obs <- nrow(combined_hourly_steps)

# Count how many hourly observations belong to zero-step days
zero_step_hours <- combined_hourly_steps %>%
  mutate(day = as.Date(hour)) %>%
  semi_join(zero_step_days, by = c("participant_id", "day")) %>%
  nrow()

percent_zero_step_hours <- (zero_step_hours / total_hourly_obs) * 100

message("Number of participants with at least one zero-step day: ", num_participants_with_missing)
message("Total zero-step day observations (participant-day): ", num_zero_step_days)
message("Total hourly observations: ", total_hourly_obs)
message("Number of hourly observations in zero-step days: ", zero_step_hours)
message(sprintf("Percentage of zero-step hourly observations: %.2f%%", percent_zero_step_hours))


###how many people have missing observations? to plot 

cleaned_data <- combined_hourly_steps %>%
  mutate(day = as.Date(hour)) %>%                        # extract day
  group_by(participant_id, day) %>%
  mutate(daily_total_steps = sum(total_steps, na.rm = TRUE)) %>%  # total steps per day
  ungroup() %>%
  filter(daily_total_steps > 0) %>%                      # remove days where daily total steps == 0
  group_by(participant_id) %>%
  arrange(participant_id, day, hour) %>%
  mutate(
    day_number = dense_rank(day),                         # day number per participant
    hour_number = row_number()                            # optional, hour index per participant
  ) %>%
  ungroup() %>%
  select(-daily_total_steps)

cleaned_data$id = cleaned_data$user_id

cleaned_data$id <- gsub("AIREADI-", "", cleaned_data$id)