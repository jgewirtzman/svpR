# Fixed Age Category Integration
# This script directly integrates the age data into your RSVP system

# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# Define the correct file paths
guest_list_path <- "guestlist.csv"
age_data_path <- "Wedding Budget Invite List.csv"  # Note the double space in the filename
output_dir <- "wedding_reports_with_age"

# Process the guest list data
process_guest_list <- function() {
  # Read the CSV file
  guests <- read_csv(guest_list_path, 
                     col_types = cols(.default = col_character()),
                     na = c("", "NA", "N/A"))
  
  # Clean column names
  names(guests) <- names(guests) %>%
    str_to_lower() %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("[^a-z0-9_]", "")
  
  cat("Processing guest list with", nrow(guests), "guests\n")
  
  # Find stay columns
  possible_friday_cols <- c("friday_ifc_stay_1", "ifc_stay_1", "friday_ifc_stay")
  possible_saturday_cols <- c("saturday_ifc_stay_1", "saturday_ifc_stay", "ifc_stay_1")
  possible_sunday_cols <- c("sunday_ifc_stay_1", "sunday_ifc_stay")
  possible_camping_cols <- c("lodgingcamping_weekend", "lodgingcamping_sat_only")
  
  # Add stay information
  guests <- guests %>%
    mutate(
      # Check for "Yes" in Friday stay columns
      is_staying_friday = rowSums(across(any_of(possible_friday_cols), 
                                         ~ . == "Yes", 
                                         .names = "col_{.col}"), 
                                  na.rm = TRUE) > 0,
      
      # Check for "Yes" in Saturday stay columns
      is_staying_saturday = rowSums(across(any_of(possible_saturday_cols), 
                                           ~ . == "Yes", 
                                           .names = "col_{.col}"),
                                    na.rm = TRUE) > 0,
      
      # Check for "Yes" in Sunday stay columns
      is_staying_sunday = rowSums(across(any_of(possible_sunday_cols), 
                                         ~ . == "Yes", 
                                         .names = "col_{.col}"),
                                  na.rm = TRUE) > 0,
      
      # Check for "Camping" in camping columns
      is_camping = rowSums(across(any_of(possible_camping_cols),
                                  ~ . == "Camping",
                                  .names = "col_{.col}"),
                           na.rm = TRUE) > 0
    )
  
  return(guests)
}

# Process the age data
process_age_data <- function() {
  # Read the age data
  age_data <- read_csv(age_data_path,
                       col_types = cols(.default = col_character()),
                       na = c("", "NA", "N/A"))
  
  # Clean column names
  names(age_data) <- names(age_data) %>%
    str_to_lower() %>%
    str_replace_all(" ", "_")
  
  # Extract age information
  age_info <- age_data %>%
    select(first_name, last_name, age) %>%
    filter(!is.na(first_name) & !is.na(last_name)) %>%
    mutate(
      age_category = case_when(
        age == "Adult" ~ "Adults 21+ Room",
        age == "Child (13+)" ~ "Guests 12-21 Room",
        age == "Child (5-12)" ~ "Children 5-12 Room",
        age == "Child (0-4)" ~ "Children <5 Room",
        TRUE ~ "Unknown"
      )
    )
  
  # Print summary
  age_summary <- age_info %>%
    group_by(age_category) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  
  cat("Extracted age categories:\n")
  print(age_summary)
  
  return(age_info)
}

# Run the integration
run_age_integration <- function() {
  cat("Starting age category integration...\n")
  
  # Create output directories
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0(output_dir, "/age_reports"), showWarnings = FALSE, recursive = TRUE)
  
  # Step 1: Process the guest list
  cat("\n*** Step 1: Processing guest list... ***\n")
  guests <- process_guest_list()
  
  # Step 2: Process the age data
  cat("\n*** Step 2: Processing age data... ***\n")
  age_info <- process_age_data()
  
  # Step 3: Merge the data and calculate costs
  cat("\n*** Step 3: Merging data and calculating costs... ***\n")
  
  # Define cost constants
  SATURDAY_HOUSING_COST <- 136
  SATURDAY_MEALS_COST <- 36
  SATURDAY_CAMPING_GROUNDS <- 54
  FRIDAY_HOUSING_COST <- 72
  FRIDAY_MEALS_COST <- 36
  FRIDAY_CAMPING_GROUNDS <- 36
  SUNDAY_MEALS_COST <- 18
  
  # Merge guest data with age data
  guests_with_age <- guests %>%
    left_join(age_info %>% select(first_name, last_name, age_category),
              by = c("first_name", "last_name"))
  
  # Fill in missing age categories and update camping guests
  guests_with_age <- guests_with_age %>%
    mutate(
      age_category = ifelse(is.na(age_category), "Adults 21+ Room", age_category),
      age_category = ifelse(
        age_category == "Adults 21+ Room" & is_camping,
        "Adults 21+ Camping",
        age_category
      ),
      
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
  
  # Step 4: Generate the summary reports
  cat("\n*** Step 4: Generating summary reports... ***\n")
  
  # Age category summary
  age_category_summary <- guests_with_age %>%
    group_by(age_category) %>%
    summarize(
      total_guests = n(),
      friday_count = sum(is_staying_friday, na.rm = TRUE),
      saturday_count = sum(is_staying_saturday, na.rm = TRUE),
      sunday_count = sum(is_staying_sunday, na.rm = TRUE),
      total_cost = sum(total_cost, na.rm = TRUE)
    ) %>%
    arrange(desc(total_guests))
  
  # Accommodation summary
  accommodation_summary <- guests_with_age %>%
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
  
  # Print summaries
  cat("\n=== AGE CATEGORY SUMMARY ===\n")
  print(age_category_summary)
  
  cat("\n=== ACCOMMODATION SUMMARY ===\n")
  print(accommodation_summary)
  
  # Step 5: Generate detailed reports
  cat("\n*** Step 5: Generating detailed reports... ***\n")
  
  # Age category details
  age_detail_report <- guests_with_age %>%
    select(
      first_name, 
      last_name, 
      party, 
      age_category, 
      is_staying_friday, 
      is_staying_saturday, 
      is_staying_sunday,
      is_camping,
      total_cost
    ) %>%
    arrange(age_category, last_name, first_name)
  
  # Breakdown by night
  age_by_night <- guests_with_age %>%
    group_by(age_category) %>%
    summarize(
      friday_count = sum(is_staying_friday, na.rm = TRUE),
      saturday_count = sum(is_staying_saturday, na.rm = TRUE),
      sunday_count = sum(is_staying_sunday, na.rm = TRUE),
      total_guests = n()
    ) %>%
    arrange(age_category)
  
  # Save reports
  write_csv(age_category_summary, paste0(output_dir, "/age_category_summary.csv"))
  write_csv(age_detail_report, paste0(output_dir, "/age_detail_report.csv"))
  write_csv(age_by_night, paste0(output_dir, "/age_by_night.csv"))
  write_csv(guests_with_age, paste0(output_dir, "/guests_with_age.csv"))
  
  # Create age summary text file
  age_summary_text <- paste0(
    "AGE CATEGORY SUMMARY REPORT\n",
    "Wedding: Cyrena & Jon - June 20-23, 2025\n",
    "Generated: ", format(Sys.Date(), "%B %d, %Y"), "\n\n",
    
    "== Age Category Summary ==\n\n",
    paste(capture.output(print(age_category_summary)), collapse = "\n"), "\n\n",
    
    "== Age Categories by Night ==\n\n",
    paste(capture.output(print(age_by_night)), collapse = "\n"), "\n\n",
    
    "== Important Notes ==\n\n",
    "1. Age Categories:\n",
    "   - Adults 21+ Room: Adults staying in standard rooms\n",
    "   - Adults 21+ Camping: Adults staying in camping accommodations\n",
    "   - Guests 12-21 Room: Teenagers and young adults in standard rooms\n",
    "   - Children 5-12 Room: School-age children in standard rooms\n",
    "   - Children <5 Room: Young children and infants in standard rooms\n\n",
    
    "2. Age data has been extracted from the Wedding Budget Invite List.\n",
    "   Guests without matching records were categorized as 'Adults 21+ Room' by default.\n\n",
    
    "3. These categories are used for room allocation and accommodation planning at the venue.\n"
  )
  
  writeLines(age_summary_text, paste0(output_dir, "/age_summary.txt"))
  
  # Step 6: Generate IFC report
  cat("\n*** Step 6: Generating IFC report... ***\n")
  
  # Get overnight guests only
  ifc_guests <- guests_with_age %>%
    filter(is_staying_friday | is_staying_saturday | is_staying_sunday) %>%
    arrange(last_name, first_name)
  
  # Create summary by night and age category
  age_categories <- unique(ifc_guests$age_category)
  nights <- c("Friday, June 20", "Saturday, June 21", "Sunday, June 22")
  
  night_age_summary <- expand.grid(
    Night = nights,
    Age_Category = age_categories,
    stringsAsFactors = FALSE
  )
  
  # Add counts
  night_age_summary$Guests <- sapply(1:nrow(night_age_summary), function(i) {
    night <- night_age_summary$Night[i]
    category <- night_age_summary$Age_Category[i]
    
    if (night == "Friday, June 20") {
      return(sum(ifc_guests$is_staying_friday & ifc_guests$age_category == category))
    } else if (night == "Saturday, June 21") {
      return(sum(ifc_guests$is_staying_saturday & ifc_guests$age_category == category))
    } else {
      return(sum(ifc_guests$is_staying_sunday & ifc_guests$age_category == category))
    }
  })
  
  # Remove rows with zero guests
  night_age_summary <- night_age_summary %>%
    filter(Guests > 0)
  
  # Create detailed guest list for IFC
  ifc_report <- ifc_guests %>%
    select(
      Last_Name = last_name,
      First_Name = first_name,
      Age_Category = age_category,
      Friday_Night = is_staying_friday,
      Saturday_Night = is_staying_saturday,
      Sunday_Night = is_staying_sunday,
      Accommodation_Type = is_camping
    ) %>%
    mutate(
      Friday_Night = ifelse(Friday_Night, "Yes", "No"),
      Saturday_Night = ifelse(Saturday_Night, "Yes", "No"),
      Sunday_Night = ifelse(Sunday_Night, "Yes", "No"),
      Accommodation_Type = ifelse(Accommodation_Type, "Camping", "Standard Lodging")
    )
  
  # Save IFC reports
  write_csv(night_age_summary, paste0(output_dir, "/ifc_night_age_summary.csv"))
  write_csv(ifc_report, paste0(output_dir, "/ifc_guest_details_with_age.csv"))
  
  cat("\n*** Age category integration complete! ***\n")
  cat("\nReports have been saved to:", output_dir, "\n")
  
  # Return the results
  return(list(
    guests_with_age = guests_with_age,
    age_category_summary = age_category_summary,
    age_by_night = age_by_night,
    accommodation_summary = accommodation_summary,
    ifc_report = ifc_report,
    night_age_summary = night_age_summary
  ))
}

# Run the integration
results <- run_age_integration()