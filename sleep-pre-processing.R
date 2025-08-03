library(dplyr)
library(lubridate)
library(jsonlite)
library(tidyr)
library(stringr)
library(purrr)  


# Base directory (change path)
base_dir <- "~/Downloads/garmin_vivosmart5 3/"
participant_folders <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)

all_participants_data <- list()

for (folder in participant_folders) {
  sleep_files <- list.files(folder, pattern = "_sleep.json$", full.names = TRUE)
  
  for (json_file in sleep_files) {
    participant_id <- basename(json_file) %>% str_remove("_sleep.json")
    json_data <- fromJSON(json_file, flatten = TRUE)
    
    # Extract and prepare sleep data
    sleep_data <- json_data$body$sleep %>%
      bind_rows() %>%
      mutate(
        user_id = json_data$header$user_id,
        start_time = ymd_hms(sleep_stage_time_frame.time_interval.start_date_time),
        end_time = ymd_hms(sleep_stage_time_frame.time_interval.end_date_time),
        date = as.Date(start_time),
        sleep_stage = sleep_stage_state
      ) %>%
      arrange(user_id, date, start_time)
    
    # Remove duplicate or overlapping intervals
    sleep_data <- sleep_data %>%
      group_by(user_id, date) %>%
      arrange(start_time, .by_group = TRUE) %>%
      mutate(
        next_start = lead(start_time),
        curr_end = end_time,
        overlaps_with_next = !is.na(next_start) & curr_end > next_start
      ) %>%
      filter(!overlaps_with_next | is.na(overlaps_with_next)) %>%
      ungroup() %>%
      select(-next_start, -curr_end, -overlaps_with_next)
    
    # Recalculate sleep durations
    sleep_data <- sleep_data %>%
      mutate(
        sleep_duration = as.numeric(difftime(end_time, start_time, units = "mins"))
      )
    
    # Summarize per night
    daily_sleep <- sleep_data %>%
      group_by(user_id, date) %>%
      summarise(
        bed_time = min(start_time),
        wake_time = max(end_time),
        total_sleep_time_mins = sum(sleep_duration, na.rm = TRUE),
        total_sleep_time_hours = total_sleep_time_mins / 60,
        awake_duration = sum(sleep_duration[sleep_stage == "awake"], na.rm = TRUE) / 60,
        deep_duration = sum(sleep_duration[sleep_stage == "deep"], na.rm = TRUE) / 60,
        rem_duration = sum(sleep_duration[sleep_stage == "rem"], na.rm = TRUE) / 60,
        light_duration = sum(sleep_duration[sleep_stage == "light"], na.rm = TRUE) / 60,
        stage_sleep_time = sum(sleep_duration[sleep_stage %in% c("light", "deep", "rem")], na.rm = TRUE) / 60,
        .groups = "drop"
      )
    
    # Summarize per participant
    avg_sleep_summary <- daily_sleep %>%
      group_by(user_id) %>%
      summarise(
        avg_total_sleep_hours = mean(total_sleep_time_hours, na.rm = TRUE),
        avg_stage_sleep_hours = mean(stage_sleep_time, na.rm = TRUE),
        sd_total_sleep_hours = sd(total_sleep_time_hours, na.rm = TRUE),
        sd_stage_sleep_hours = sd(stage_sleep_time, na.rm = TRUE),
        avg_bed_time_sd = sd(as.numeric(difftime(bed_time, as.Date(bed_time), units = "mins")), na.rm = TRUE),
        avg_wake_time_sd = sd(as.numeric(difftime(wake_time, as.Date(wake_time), units = "mins")), na.rm = TRUE),
        avg_awake_hours = mean(awake_duration, na.rm = TRUE),
        avg_deep_hours = mean(deep_duration, na.rm = TRUE),
        avg_rem_hours = mean(rem_duration, na.rm = TRUE),
        avg_light_hours = mean(light_duration, na.rm = TRUE),
        total_days_with_data = n(),
        .groups = "drop"
      )
    
    # Combine with daily data
    daily_sleep <- left_join(daily_sleep, avg_sleep_summary, by = "user_id")
    
    all_participants_data[[participant_id]] <- daily_sleep
    cat("Processed:", participant_id, "\n")
  }
}

# Combine all participants
final_sleep_data <- bind_rows(all_participants_data)

# Summary of corrected durations
summary(final_sleep_data$total_sleep_time_hours)

# Report overlapping count before removal
cat("Overlap removal complete.\nTotal participants:", length(unique(final_sleep_data$user_id)), "\n")

## Check for missing data? per ppt? 