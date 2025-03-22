# Wedding RSVP Tracker and Cost Calculator
# This script processes the guest list CSV and generates reports for:
# 1. Party names and emails
# 2. Accommodation counts and costs
# 3. Meal counts for guests (both on-site and off-site)

# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# Function to read in guest list data
read_guest_list <- function(file_path) {
  # Read the CSV file
  guests <- read_csv(file_path, 
                     col_types = cols(.default = col_character()),
                     na = c("", "NA", "N/A"))
  
  # Clean column names (remove spaces, etc.)
  names(guests) <- names(guests) %>%
    str_to_lower() %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("[^a-z0-9_]", "")
  
  # Print column names for debugging
  cat("Available columns in the dataset:\n")
  cat(paste(names(guests), collapse = ", "), "\n")
  
  return(guests)
}

# Function to generate party-level summary
generate_party_summary <- function(guests) {
  # Find the email column - handle cases where email might have different names
  email_cols <- grep("email", names(guests), ignore.case = TRUE, value = TRUE)
  
  # Check if we found any email columns
  if (length(email_cols) == 0) {
    warning("No email column found in the data")
    guests$email_column <- NA  # Create a dummy column
    email_col_name <- "email_column"
  } else {
    # Use the first email column found
    email_col_name <- email_cols[1]
    # Rename it to a standard name for use in the function
    guests$email_column <- guests[[email_col_name]]
  }
  
  # Create descriptive party names based on guest names
  guests <- guests %>%
    group_by(party) %>%
    mutate(
      party_name = case_when(
        n() == 1 ~ paste0(first_name, " ", last_name),
        n() == 2 ~ paste0(first(first_name), " ", first(last_name), " & ", last(first_name), " ", last(last_name)),
        TRUE ~ paste0(first(last_name), " Family (", n(), " guests)")
      )
    ) %>%
    ungroup()
  
  # Group by party and get email
  party_summary <- guests %>%
    group_by(party) %>%
    summarize(
      party_name = first(party_name),
      party_email = first(na.omit(email_column)),
      total_guests = n(),
      guest_names = paste(paste(first_name, last_name), collapse = ", "),
      wedding_attending = sum(wedding_rsvp == "Joyfully Accept", na.rm = TRUE),
      wedding_declining = sum(wedding_rsvp == "Regretfully Decline", na.rm = TRUE),
      wedding_no_response = sum(is.na(wedding_rsvp)),
      friday_attending = sum(fridayshabbat_rsvp == "Joyfully Accept", na.rm = TRUE),
      saturday_attending = sum(saturday_onsite_rsvp == "Yes, I will join on Saturday" | 
                                 saturday_offsite_rsvp == "Yes, I will join for lunch only" |
                                 saturday_offsite_rsvp == "Yes, I will join for dinner only" |
                                 saturday_offsite_rsvp == "Yes, I will join for lunch and dinner", 
                               na.rm = TRUE)
    )
  
  return(party_summary)
}

# Function to calculate accommodation costs
calculate_accommodation_costs <- function(guests) {
  # Define cost constants
  SATURDAY_HOUSING_COST <- 136
  SATURDAY_MEALS_COST <- 36
  SATURDAY_CAMPING_GROUNDS <- 54
  FRIDAY_HOUSING_COST <- 72
  FRIDAY_MEALS_COST <- 36
  FRIDAY_CAMPING_GROUNDS <- 36
  SUNDAY_MEALS_COST <- 18
  
  # First check which stay columns exist in the dataset
  possible_friday_cols <- c("friday_ifc_stay_1", "ifc_stay_1", "friday_ifc_stay")
  possible_saturday_cols <- c("saturday_ifc_stay_1", "saturday_ifc_stay", "ifc_stay_1")
  possible_sunday_cols <- c("sunday_ifc_stay_1", "sunday_ifc_stay")
  possible_camping_cols <- c("lodgingcamping_weekend", "lodgingcamping_sat_only")
  
  # Process IFC stay data
  accommodation_summary <- guests %>%
    mutate(
      # Check for "Yes" in ANY of the Friday stay columns that exist
      is_staying_friday = rowSums(across(any_of(possible_friday_cols), 
                                         ~ . == "Yes", 
                                         .names = "col_{.col}"), 
                                  na.rm = TRUE) > 0,
      
      # Check for "Yes" in ANY of the Saturday stay columns that exist
      is_staying_saturday = rowSums(across(any_of(possible_saturday_cols), 
                                           ~ . == "Yes", 
                                           .names = "col_{.col}"),
                                    na.rm = TRUE) > 0,
      
      # Check for "Yes" in ANY of the Sunday stay columns that exist
      is_staying_sunday = rowSums(across(any_of(possible_sunday_cols), 
                                         ~ . == "Yes", 
                                         .names = "col_{.col}"),
                                  na.rm = TRUE) > 0,
      
      # Check for "Camping" in ANY of the camping columns that exist
      is_camping = rowSums(across(any_of(possible_camping_cols),
                                  ~ . == "Camping",
                                  .names = "col_{.col}"),
                           na.rm = TRUE) > 0,
      
      # Calculate costs based on accommodation type and nights
      friday_lodging_cost = case_when(
        is_staying_friday & !is_camping ~ FRIDAY_HOUSING_COST,
        is_staying_friday & is_camping ~ FRIDAY_CAMPING_GROUNDS,
        TRUE ~ 0
      ),
      
      friday_meals_cost = case_when(
        is_staying_friday ~ FRIDAY_MEALS_COST,
        TRUE ~ 0
      ),
      
      saturday_lodging_cost = case_when(
        is_staying_saturday & !is_camping ~ SATURDAY_HOUSING_COST,
        is_staying_saturday & is_camping ~ SATURDAY_CAMPING_GROUNDS,
        TRUE ~ 0
      ),
      
      saturday_meals_cost = case_when(
        is_staying_saturday ~ SATURDAY_MEALS_COST,
        TRUE ~ 0
      ),
      
      sunday_cost = case_when(
        is_staying_sunday ~ SUNDAY_MEALS_COST,
        TRUE ~ 0
      ),
      
      # Calculate daily costs
      friday_cost = friday_lodging_cost + friday_meals_cost,
      saturday_cost = saturday_lodging_cost + saturday_meals_cost,
      
      # Calculate category totals
      total_lodging_cost = friday_lodging_cost + saturday_lodging_cost,
      total_meals_cost = friday_meals_cost + saturday_meals_cost + sunday_cost,
      
      # Calculate total cost per guest
      total_cost = friday_cost + saturday_cost + sunday_cost
    )
  
  # Create summary of accommodation counts and costs
  accommodation_counts <- accommodation_summary %>%
    summarize(
      total_guests = n(),
      friday_count = sum(is_staying_friday, na.rm = TRUE),
      saturday_count = sum(is_staying_saturday, na.rm = TRUE),
      sunday_count = sum(is_staying_sunday, na.rm = TRUE),
      camping_count = sum(is_camping, na.rm = TRUE),
      standard_lodging_count = sum((is_staying_friday | is_staying_saturday) & !is_camping, na.rm = TRUE),
      total_friday_cost = sum(friday_cost, na.rm = TRUE),
      total_saturday_cost = sum(saturday_cost, na.rm = TRUE),
      total_sunday_cost = sum(sunday_cost, na.rm = TRUE),
      grand_total_cost = sum(total_cost, na.rm = TRUE)
    )
  
  return(list(
    guest_costs = accommodation_summary,
    summary = accommodation_counts
  ))
}

# Improved function to count meals for all guests
count_meals <- function(guests) {
  # Get list of stay columns to check
  possible_friday_cols <- c("friday_ifc_stay_1", "ifc_stay_1", "friday_ifc_stay")
  possible_saturday_cols <- c("saturday_ifc_stay_1", "saturday_ifc_stay", "ifc_stay_1")
  possible_sunday_cols <- c("sunday_ifc_stay_1", "sunday_ifc_stay")
  
  # Count guests attending each meal
  meal_counts <- guests %>%
    mutate(
      # General attendance
      attending_wedding = wedding_rsvp == "Joyfully Accept",
      attending_friday = fridayshabbat_rsvp == "Joyfully Accept",
      
      # Saturday meal attendance (for off-site guests) - look for specific responses
      attending_saturday_lunch_only = saturday_offsite_rsvp == "Yes, I will join for lunch only",
      attending_saturday_dinner_only = saturday_offsite_rsvp == "Yes, I will join for dinner only",
      attending_saturday_both_meals = saturday_offsite_rsvp == "Yes, I will join for lunch and dinner",
      
      # Staying overnight checks
      staying_at_ifc_friday = rowSums(across(any_of(possible_friday_cols), ~ . == "Yes"), na.rm = TRUE) > 0,
      staying_at_ifc_saturday = rowSums(across(any_of(possible_saturday_cols), ~ . == "Yes"), na.rm = TRUE) > 0,
      staying_at_ifc_sunday = rowSums(across(any_of(possible_sunday_cols), ~ . == "Yes"), na.rm = TRUE) > 0
    ) %>%
    summarize(
      # Meals for on-site guests
      friday_dinner_onsite = sum(staying_at_ifc_friday, na.rm = TRUE),
      saturday_breakfast_onsite = sum(staying_at_ifc_friday, na.rm = TRUE),
      saturday_lunch_onsite = sum(staying_at_ifc_saturday, na.rm = TRUE),
      saturday_dinner_onsite = sum(staying_at_ifc_saturday, na.rm = TRUE),
      sunday_breakfast_onsite = sum(staying_at_ifc_saturday, na.rm = TRUE),
      sunday_dinner_onsite = sum(staying_at_ifc_sunday, na.rm = TRUE),
      monday_breakfast_onsite = sum(staying_at_ifc_sunday, na.rm = TRUE),
      
      # Meals for off-site guests
      friday_dinner_offsite = sum(attending_friday & !staying_at_ifc_friday, na.rm = TRUE),
      saturday_lunch_offsite = sum(attending_saturday_lunch_only | attending_saturday_both_meals, na.rm = TRUE),
      saturday_dinner_offsite = sum(attending_saturday_dinner_only | attending_saturday_both_meals, na.rm = TRUE),
      
      # Wedding reception meal (all attending)
      sunday_lunch_total = sum(attending_wedding, na.rm = TRUE),
      
      # Total counts (for planning)
      total_friday_dinner = sum(friday_dinner_onsite + friday_dinner_offsite, na.rm = TRUE),
      total_saturday_breakfast = sum(saturday_breakfast_onsite, na.rm = TRUE),
      total_saturday_lunch = sum(saturday_lunch_onsite + saturday_lunch_offsite, na.rm = TRUE),
      total_saturday_dinner = sum(saturday_dinner_onsite + saturday_dinner_offsite, na.rm = TRUE),
      total_sunday_breakfast = sum(sunday_breakfast_onsite, na.rm = TRUE),
      total_sunday_lunch = sum(sunday_lunch_total, na.rm = TRUE),
      total_sunday_dinner = sum(sunday_dinner_onsite, na.rm = TRUE),
      total_monday_breakfast = sum(monday_breakfast_onsite, na.rm = TRUE)
    )
  
  return(meal_counts)
}

# Main function to generate all reports
generate_wedding_reports <- function(file_path) {
  # Read guest list
  guests <- read_guest_list(file_path)
  
  # Generate party summary
  party_summary <- generate_party_summary(guests)
  
  # Calculate accommodation costs
  accommodation_results <- calculate_accommodation_costs(guests)
  
  # Count meals for all guests with improved function
  meal_counts <- count_meals(guests)
  
  # Print summary reports
  cat("\n=== WEDDING RSVP SUMMARY ===\n")
  print(party_summary)
  
  cat("\n=== ACCOMMODATION SUMMARY ===\n")
  print(accommodation_results$summary)
  
  cat("\n=== MEAL COUNTS (Detailed) ===\n")
  print(meal_counts)
  
  # Return all results as a list
  return(list(
    guests = guests,
    party_summary = party_summary,
    guest_costs = accommodation_results$guest_costs,
    accommodation_summary = accommodation_results$summary,
    meal_counts = meal_counts
  ))
}

# Additional function to export results to CSV files
export_reports <- function(results, output_dir = ".") {
  # SIMPLIFIED: Force directory creation
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  cat("Ensuring report directory exists:", output_dir, "\n")
  
  # Export party summary
  write_csv(results$party_summary, file.path(output_dir, "party_summary.csv"))
  
  # Export guest costs
  write_csv(results$guest_costs, file.path(output_dir, "guest_accommodation_costs.csv"))
  
  # Export accommodation summary as a simple CSV
  accommodation_df <- as.data.frame(t(as.matrix(results$accommodation_summary)))
  accommodation_df$metric <- rownames(accommodation_df)
  accommodation_df <- accommodation_df[, c(ncol(accommodation_df), 1:(ncol(accommodation_df)-1))]
  names(accommodation_df)[1] <- "metric"
  write_csv(accommodation_df, file.path(output_dir, "accommodation_summary.csv"))
  
  # Export meal counts as a simple CSV
  meal_df <- as.data.frame(t(as.matrix(results$meal_counts)))
  meal_df$meal <- rownames(meal_df)
  meal_df <- meal_df[, c(ncol(meal_df), 1:(ncol(meal_df)-1))]
  names(meal_df)[1] <- "meal"
  write_csv(meal_df, file.path(output_dir, "meal_counts.csv"))
  
  cat("Reports exported to:", output_dir, "\n")
}