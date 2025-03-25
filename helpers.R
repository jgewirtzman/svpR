# helpers.R - Shared utility functions
library(dplyr)

# Ensure all cost fields are numeric
ensure_numeric_costs <- function(guests) {
  # List all cost columns
  cost_cols <- c(
    "total_cost", "total_guest_charge", "total_host_charge",
    "friday_cost", "saturday_cost", "sunday_cost",
    "friday_guest_charge", "saturday_guest_charge", "sunday_guest_charge",
    "friday_host_charge", "saturday_host_charge", "sunday_host_charge"
  )
  
  # Only convert columns that exist
  existing_cols <- intersect(cost_cols, names(guests))
  
  if (length(existing_cols) > 0) {
    guests <- guests %>%
      mutate(across(all_of(existing_cols), ~as.numeric(replace(., is.na(.), 0))))
  }
  
  return(guests)
}

# Standard function to summarize data by age category
summarize_by_age <- function(data, value_cols) {
  # Check if age_category exists
  if (!"age_category" %in% names(data)) {
    return(summarize_totals(data, value_cols))
  }
  
  # Only include existing columns
  existing_cols <- intersect(value_cols, names(data))
  
  # Summarize by age category
  data %>%
    group_by(age_category) %>%
    summarize(across(all_of(existing_cols), ~sum(., na.rm = TRUE)), .groups = 'drop')
}

# Summarize totals without age category
summarize_totals <- function(data, value_cols) {
  # Only include existing columns
  existing_cols <- intersect(value_cols, names(data))
  
  # Create a simple summary
  summary <- data %>%
    summarize(across(all_of(existing_cols), ~sum(., na.rm = TRUE)))
  
  # Add a category column for consistency
  summary$category <- "Total"
  
  return(summary)
}

# Get meal counts for each day/meal
calculate_meal_counts <- function(guests) {
  guests %>%
    summarize(
      friday_dinner = sum(is_staying_friday | 
                            (fridayshabbat_rsvp == "Joyfully Accept" & !is_staying_friday), 
                          na.rm = TRUE),
      saturday_breakfast = sum(is_staying_friday, na.rm = TRUE),
      saturday_lunch = sum(is_staying_saturday | 
                             saturday_offsite_rsvp %in% c("Yes, I will join for lunch only", 
                                                          "Yes, I will join for lunch and dinner"), 
                           na.rm = TRUE),
      saturday_dinner = sum(is_staying_saturday | 
                              saturday_offsite_rsvp %in% c("Yes, I will join for dinner only", 
                                                           "Yes, I will join for lunch and dinner"), 
                            na.rm = TRUE),
      sunday_breakfast = sum(is_staying_saturday, na.rm = TRUE),
      sunday_lunch = sum(wedding_rsvp == "Joyfully Accept", na.rm = TRUE),
      sunday_dinner = sum(is_staying_sunday, na.rm = TRUE),
      monday_breakfast = sum(is_staying_sunday, na.rm = TRUE)
    )
}