# Wedding Age Category Integration Workflow
# This script runs the entire process of integrating age categories with the RSVP system

# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# Source required scripts
source("wedding-rsvp-tracker.R")
source("cost-summary-generator.R")
source("generate-ifc-report.R")

# Process age categories and generate reports
run_age_category_integration <- function(guest_list_path = "guestlist.csv", 
                                         age_data_path = "Wedding Budget  Invite List 2.csv",
                                         output_dir = "wedding_reports_with_age") {
  # Create output directories
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0(output_dir, "/age_reports"), showWarnings = FALSE, recursive = TRUE)
  
  cat("Starting age category integration workflow...\n")
  
  # Step 1: Extract age data from the Budget Invite List
  cat("\n*** Step 1: Extracting age data from Budget Invite List... ***\n")
  
  # Read the budget invite list file
  age_data <- read_csv(age_data_path,
                       col_types = cols(.default = col_character()),
                       na = c("", "NA", "N/A"))
  
  # Clean column names
  names(age_data) <- names(age_data) %>%
    str_to_lower() %>%
    str_replace_all(" ", "_")
  
  # Extract age information and map to our categories
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
  
  # Print summary of extracted age categories
  age_summary <- age_info %>%
    group_by(age_category) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  
  cat("Extracted age categories from Budget Invite List:\n")
  print(age_summary)
  
  # Save the extracted age data
  write_csv(age_info, paste0(output_dir, "/age_reports/extracted_age_categories.csv"))
  
  # Step 2: Run the main RSVP tracking process and modify to include age categories
  cat("\n*** Step 2: Running main RSVP tracking with age categories... ***\n")
  
  # Read the guest list
  guests <- read_guest_list(guest_list_path)
  
  # Generate party summary
  party_summary <- generate_party_summary(guests)
  
  # Calculate accommodation costs
  accommodation_results <- calculate_accommodation_costs(guests)
  
  # Now add age categories to the guest costs data
  guest_costs_with_age <- accommodation_results$guest_costs %>%
    left_join(age_info %>% select(first_name, last_name, age_category),
              by = c("first_name", "last_name"))
  
  # Handle missing categories and camping
  guest_costs_with_age <- guest_costs_with_age %>%
    mutate(
      age_category = ifelse(is.na(age_category), "Adults 21+ Room", age_category),
      age_category = ifelse(
        age_category == "Adults 21+ Room" & is_camping,
        "Adults 21+ Camping",
        age_category
      )
    )
  
  # Calculate age category summary
  age_category_summary <- guest_costs_with_age %>%
    group_by(age_category) %>%
    summarize(
      total_guests = n(),
      friday_count = sum(is_staying_friday, na.rm = TRUE),
      saturday_count = sum(is_staying_saturday, na.rm = TRUE),
      sunday_count = sum(is_staying_sunday, na.rm = TRUE),
      total_cost = sum(total_cost, na.rm = TRUE)
    ) %>%
    arrange(desc(total_guests))
  
  # Count meals
  meal_counts <- count_meals(guests)
  
  # Run data validation
  data_issues <- validate_guest_data(guests)
  
  # Create complete results object with age categories
  results <- list(
    guests = guests,
    party_summary = party_summary,
    guest_costs = guest_costs_with_age,
    accommodation_summary = accommodation_results$summary,
    age_category_summary = age_category_summary,
    meal_counts = meal_counts,
    data_issues = data_issues
  )
  
  # Step 3: Generate age-specific reports
  cat("\n*** Step 3: Generating age category reports... ***\n")
  
  # Create a detailed age category report
  age_detail_report <- guest_costs_with_age %>%
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
  
  # Create a breakdown by night and age category
  age_by_night <- guest_costs_with_age %>%
    group_by(age_category, night = "Friday") %>%
    summarize(count = sum(is_staying_friday, na.rm = TRUE), .groups = 'drop') %>%
    bind_rows(
      guest_costs_with_age %>%
        group_by(age_category, night = "Saturday") %>%
        summarize(count = sum(is_staying_saturday, na.rm = TRUE), .groups = 'drop')
    ) %>%
    bind_rows(
      guest_costs_with_age %>%
        group_by(age_category, night = "Sunday") %>%
        summarize(count = sum(is_staying_sunday, na.rm = TRUE), .groups = 'drop')
    ) %>%
    arrange(age_category, night)
  
  # Create age category summary by party
  age_by_party <- guest_costs_with_age %>%
    group_by(party, age_category) %>%
    summarize(count = n(), .groups = 'drop') %>%
    pivot_wider(names_from = age_category, values_from = count, values_fill = 0) %>%
    left_join(
      party_summary %>% select(party, party_name),
      by = "party"
    ) %>%
    select(party, party_name, everything()) %>%
    arrange(party)
  
  # Save the reports
  write_csv(age_category_summary, paste0(output_dir, "/age_reports/age_category_summary.csv"))
  write_csv(age_detail_report, paste0(output_dir, "/age_reports/age_detail_report.csv"))
  write_csv(age_by_night, paste0(output_dir, "/age_reports/age_by_night.csv"))
  write_csv(age_by_party, paste0(output_dir, "/age_reports/age_by_party.csv"))
  
  # Create a text summary
  age_summary_text <- paste0(
    "AGE CATEGORY SUMMARY REPORT\n",
    "Wedding: Cyrena & Jon - June 20-23, 2025\n",
    "Generated: ", format(Sys.Date(), "%B %d, %Y"), "\n\n",
    
    "== Age Category Summary ==\n\n",
    paste(capture.output(print(age_category_summary)), collapse = "\n"), "\n\n",
    
    "== Age Categories by Night ==\n\n",
    paste(capture.output(print(age_by_night %>% 
                                 pivot_wider(names_from = night, values_from = count))), 
          collapse = "\n"), "\n\n",
    
    "== Important Notes ==\n\n",
    "1. Age Categories:\n",
    "   - Adults 21+ Room: Adults staying in standard rooms\n",
    "   - Adults 21+ Camping: Adults staying in camping accommodations\n",
    "   - Guests 12-21 Room: Teenagers and young adults in standard rooms\n",
    "   - Children 5-12 Room: School-age children in standard rooms\n",
    "   - Children <5 Room: Young children and infants in standard rooms\n\n",
    
    "2. Age data has been merged from the Budget Invite List. Any guests without\n",
    "   matching age information have been categorized as 'Adults 21+ Room' by default.\n\n",
    
    "3. These categories are used for room allocation and accommodation planning at the venue.\n"
  )
  
  writeLines(age_summary_text, paste0(output_dir, "/age_reports/age_summary.txt"))
  
  # Step 4: Generate IFC report with age categories
  cat("\n*** Step 4: Generating Isabella Freedman Center report with age categories... ***\n")
  
  # Create a modified IFC report that includes age categories
  ifc_guests <- guest_costs_with_age %>%
    filter(is_staying_friday | is_staying_saturday | is_staying_sunday) %>%
    arrange(last_name, first_name)
  
  # Create a summary by night and age category
  night_age_summary <- data.frame(
    Night = rep(c("Friday, June 20", "Saturday, June 21", "Sunday, June 22"), 
                each = length(unique(ifc_guests$age_category))),
    Age_Category = rep(unique(ifc_guests$age_category), times = 3)
  )
  
  # Add counts for each night and age category
  night_age_summary <- night_age_summary %>%
    mutate(
      Guests = case_when(
        Night == "Friday, June 20" ~ sapply(Age_Category, function(ac) 
          sum(ifc_guests$is_staying_friday & ifc_guests$age_category == ac)),
        Night == "Saturday, June 21" ~ sapply(Age_Category, function(ac) 
          sum(ifc_guests$is_staying_saturday & ifc_guests$age_category == ac)),
        Night == "Sunday, June 22" ~ sapply(Age_Category, function(ac) 
          sum(ifc_guests$is_staying_sunday & ifc_guests$age_category == ac))
      )
    ) %>%
    filter(Guests > 0) # Remove rows with zero guests
  
  # Create IFC guest report with age categories
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
  
  # Save the IFC reports with age categories
  write_csv(night_age_summary, paste0(output_dir, "/ifc_night_age_summary.csv"))
  write_csv(ifc_report, paste0(output_dir, "/ifc_guest_details_with_age.csv"))
  
  # Step 5: Export all standard reports with the updated guest data
  cat("\n*** Step 5: Exporting standard reports with age categories... ***\n")
  
  # Export the main reports using the standard function
  export_reports(results, output_dir)
  
  # Generate meal planning report with age categories
  if (exists("generate_meal_planning_report")) {
    cat("\n*** Step 6: Generating meal planning report with age categories... ***\n")
    generate_meal_planning_report(results, paste0(output_dir, "/meal_planning_report.csv"))
  }
  
  # Generate IFC report (using standard function)
  if (exists("generate_ifc_report")) {
    cat("\n*** Step 7: Generating full IFC report... ***\n")
    generate_ifc_report(results, paste0(output_dir, "/ifc_report.csv"))
  }
  
  cat("\n*** Age category integration workflow complete! ***\n")
  cat("\nReports can be found in:", output_dir, "\n")
  cat("Age-specific reports can be found in:", paste0(output_dir, "/age_reports"), "\n")
  
  return(results)
}

# Example usage:
# results_with_age <- run_age_category_integration("guestlist.csv", "Wedding Budget  Invite List 2.csv", "wedding_reports_with_age")
