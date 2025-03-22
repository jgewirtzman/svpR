# Isabella Freedman Center Report Generator
# This script creates a formatted report for the Isabella Freedman Center

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(knitr)
library(rmarkdown)

# Function to generate a report for the Isabella Freedman Center
generate_ifc_report <- function(results, output_file = "ifc_report.csv") {
  # SIMPLIFIED: Force directory creation
  output_dir <- dirname(output_file)
  if (output_dir != "." && output_dir != "") {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    cat("Ensuring output directory exists:", output_dir, "\n")
  }
  
  # Extract all guests staying at IFC - use is_staying columns from guest_costs
  # which already properly handles multiple column variations
  ifc_guests <- results$guest_costs %>%
    filter(is_staying_friday | is_staying_saturday | is_staying_sunday) %>%
    arrange(last_name, first_name)
  
  # Create a summary by night
  night_summary <- data.frame(
    Night = c("Friday, June 20", "Saturday, June 21", "Sunday, June 22"),
    Guests = c(
      sum(ifc_guests$is_staying_friday),
      sum(ifc_guests$is_staying_saturday),
      sum(ifc_guests$is_staying_sunday)
    ),
    Standard_Lodging = c(
      sum(ifc_guests$is_staying_friday & !ifc_guests$is_camping),
      sum(ifc_guests$is_staying_saturday & !ifc_guests$is_camping),
      sum(ifc_guests$is_staying_sunday & !ifc_guests$is_camping)
    ),
    Camping = c(
      sum(ifc_guests$is_staying_friday & ifc_guests$is_camping),
      sum(ifc_guests$is_staying_saturday & ifc_guests$is_camping),
      sum(ifc_guests$is_staying_sunday & ifc_guests$is_camping)
    )
  )
  
  # Create a report for IFC with detailed stay information
  ifc_report <- data.frame(
    Last_Name = ifc_guests$last_name,
    First_Name = ifc_guests$first_name,
    Friday_Night = ifelse(ifc_guests$is_staying_friday, "Yes", "No"),
    Saturday_Night = ifelse(ifc_guests$is_staying_saturday, "Yes", "No"),
    Sunday_Night = ifelse(ifc_guests$is_staying_sunday, "Yes", "No"),
    Accommodation_Type = ifelse(ifc_guests$is_camping, "Camping", "Standard Lodging")
  )
  
  # Create meal counts by night (using the improved meal tracking)
  meal_counts <- results$meal_counts
  
  # Format meal summary for the report
  meal_summary <- data.frame(
    Meal = c(
      "Friday Dinner", 
      "Saturday Breakfast", 
      "Saturday Lunch", 
      "Saturday Dinner", 
      "Sunday Breakfast",
      "Sunday Lunch (Wedding)",
      "Sunday Dinner",
      "Monday Breakfast"
    ),
    On_Site_Guests = c(
      meal_counts$friday_dinner_onsite,
      meal_counts$saturday_breakfast_onsite,
      meal_counts$saturday_lunch_onsite,
      meal_counts$saturday_dinner_onsite,
      meal_counts$sunday_breakfast_onsite,
      0, # Sunday lunch is for all wedding guests
      meal_counts$sunday_dinner_onsite,
      meal_counts$monday_breakfast_onsite
    ),
    Off_Site_Guests = c(
      meal_counts$friday_dinner_offsite,
      0, # Off-site guests don't get Saturday breakfast
      meal_counts$saturday_lunch_offsite,
      meal_counts$saturday_dinner_offsite,
      0, # Off-site guests don't get Sunday breakfast
      0, # Handled separately for wedding
      0, # Off-site guests don't get Sunday dinner
      0  # Off-site guests don't get Monday breakfast
    ),
    Total = c(
      meal_counts$total_friday_dinner,
      meal_counts$total_saturday_breakfast,
      meal_counts$total_saturday_lunch,
      meal_counts$total_saturday_dinner,
      meal_counts$total_sunday_breakfast,
      meal_counts$total_sunday_lunch,
      meal_counts$total_sunday_dinner,
      meal_counts$total_monday_breakfast
    )
  )
  
  # Create guest counts by accommodation type
  accommodation_summary <- ifc_guests %>%
    group_by(Accommodation = ifelse(is_camping, "Camping", "Standard Lodging")) %>%
    summarize(Count = n())
  
  # Add meal preferences and dietary restrictions
  if (!is.null(results$guests$meal_preferences) || !is.null(results$guests$dietary_restrictions)) {
    guest_details <- results$guests %>%
      select(first_name, last_name, meal_preferences, dietary_restrictions) %>%
      mutate(
        meal_preferences = ifelse(is.na(meal_preferences), "", as.character(meal_preferences)),
        dietary_restrictions = ifelse(is.na(dietary_restrictions), "", as.character(dietary_restrictions))
      )
    
    ifc_report <- ifc_report %>%
      left_join(guest_details, by = c("First_Name" = "first_name", "Last_Name" = "last_name")) %>%
      rename(Meal_Preference = meal_preferences, Dietary_Restrictions = dietary_restrictions)
  }
  
  # Write out CSV files instead of generating a PDF
  csv_base_name <- tools::file_path_sans_ext(output_file)
  
  # Write night summary
  write_csv(night_summary, paste0(csv_base_name, "_night_summary.csv"))
  
  # Write accommodation summary
  write_csv(accommodation_summary, paste0(csv_base_name, "_accommodation_summary.csv"))
  
  # Write meal summary
  write_csv(meal_summary, paste0(csv_base_name, "_meal_summary.csv"))
  
  # Write detailed guest list
  write_csv(ifc_report, paste0(csv_base_name, "_guest_details.csv"))
  
  # Create a compilation of all data in one CSV for easy import
  complete_report <- list(
    night_summary = night_summary,
    accommodation_summary = accommodation_summary,
    meal_summary = meal_summary,
    guest_details = ifc_report
  )
  
  # Create a single text summary file for easy reference
  summary_text <- paste0(
    "Isabella Freedman Center - Guest Stay Report\n",
    "For Wedding: Cyrena & Jon - June 20-23, 2025\n",
    "Generated: ", format(Sys.Date(), "%B %d, %Y"), "\n\n",
    
    "== Summary of Guest Stays ==\n\n",
    paste(capture.output(print(night_summary)), collapse = "\n"), "\n\n",
    
    "== Accommodation Types ==\n\n",
    paste(capture.output(print(accommodation_summary)), collapse = "\n"), "\n\n",
    
    "== Meal Planning Summary ==\n\n",
    paste(capture.output(print(meal_summary)), collapse = "\n"), "\n\n",
    
    "== Important Notes ==\n\n",
    "1. Meal Inclusion by Stay:\n",
    "   - Friday night guests: Friday dinner, Saturday breakfast\n",
    "   - Saturday night guests: Saturday lunch, dinner, Sunday breakfast\n",
    "   - Sunday night guests: Special catering for Sunday dinner, Monday breakfast\n",
    "   - All wedding guests: Sunday lunch (wedding meal)\n\n",
    
    "2. All guests listed are part of the wedding party for Cyrena & Jon.\n",
    "3. Please direct any questions to wedding@example.com or call (123) 456-7890.\n",
    "4. Special accommodation requests have been noted in the detailed guest list.\n"
  )
  
  # Write the text summary
  writeLines(summary_text, paste0(csv_base_name, "_summary.txt"))
  
  cat("IFC report files generated in directory:", dirname(output_file), "\n")
  
  # Return the data used in the report
  return(complete_report)
}

# Generate a simplified one-page summary for the venue
generate_ifc_summary <- function(results, output_file = "ifc_summary.csv") {
  # Create directory if it doesn't exist
  output_dir <- dirname(output_file)
  if (output_dir != "." && output_dir != "") {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  # Extract just the key information for a simple summary
  ifc_guests <- results$guest_costs %>%
    filter(is_staying_friday | is_staying_saturday | is_staying_sunday)
  
  # Summary counts
  summary_counts <- data.frame(
    Category = c(
      "Total Overnight Guests",
      "Friday Night Total",
      "Saturday Night Total", 
      "Sunday Night Total",
      "Camping Guests",
      "Lodging Guests",
      "Special Meal Requirements"
    ),
    Count = c(
      nrow(ifc_guests),
      sum(ifc_guests$is_staying_friday),
      sum(ifc_guests$is_staying_saturday),
      sum(ifc_guests$is_staying_sunday),
      sum(ifc_guests$is_camping),
      sum(!ifc_guests$is_camping & (ifc_guests$is_staying_friday | ifc_guests$is_staying_saturday | ifc_guests$is_staying_sunday)),
      sum(!is.na(results$guests$dietary_restrictions) & results$guests$dietary_restrictions != "")
    )
  )
  
  # Meal counts
  meal_counts <- results$meal_counts
  
  # Create a simplified meal summary
  meal_summary <- data.frame(
    Meal = c(
      "Friday Dinner",
      "Saturday Breakfast",
      "Saturday Lunch",
      "Saturday Dinner",
      "Sunday Breakfast",
      "Sunday Lunch (Wedding)",
      "Sunday Dinner",
      "Monday Breakfast"
    ),
    Total_Count = c(
      meal_counts$total_friday_dinner,
      meal_counts$total_saturday_breakfast,
      meal_counts$total_saturday_lunch,
      meal_counts$total_saturday_dinner,
      meal_counts$total_sunday_breakfast,
      meal_counts$total_sunday_lunch,
      meal_counts$total_sunday_dinner,
      meal_counts$total_monday_breakfast
    )
  )
  
  # Write these summary files
  write_csv(summary_counts, output_file)
  write_csv(meal_summary, paste0(tools::file_path_sans_ext(output_file), "_meals.csv"))
  
  # Create a single-page text summary
  summary_text <- paste0(
    "ISABELLA FREEDMAN CENTER - ONE PAGE SUMMARY\n",
    "Wedding: Cyrena & Jon - June 20-23, 2025\n",
    "Generated: ", format(Sys.Date(), "%B %d, %Y"), "\n\n",
    
    "GUEST COUNTS:\n",
    "- Total Overnight Guests: ", nrow(ifc_guests), "\n",
    "- Friday Night: ", sum(ifc_guests$is_staying_friday), "\n",
    "- Saturday Night: ", sum(ifc_guests$is_staying_saturday), "\n",
    "- Sunday Night: ", sum(ifc_guests$is_staying_sunday), "\n",
    "- Camping: ", sum(ifc_guests$is_camping), "\n",
    "- Standard Lodging: ", sum(!ifc_guests$is_camping & (ifc_guests$is_staying_friday | ifc_guests$is_staying_saturday | ifc_guests$is_staying_sunday)), "\n\n",
    
    "MEAL COUNTS:\n",
    "- Friday Dinner: ", meal_counts$total_friday_dinner, "\n",
    "- Saturday Breakfast: ", meal_counts$total_saturday_breakfast, "\n",
    "- Saturday Lunch: ", meal_counts$total_saturday_lunch, "\n",
    "- Saturday Dinner: ", meal_counts$total_saturday_dinner, "\n",
    "- Sunday Breakfast: ", meal_counts$total_sunday_breakfast, "\n",
    "- Sunday Lunch (Wedding): ", meal_counts$total_sunday_lunch, "\n",
    "- Sunday Dinner: ", meal_counts$total_sunday_dinner, "\n",
    "- Monday Breakfast: ", meal_counts$total_monday_breakfast, "\n\n",
    
    "NOTES:\n",
    "- Detailed guest lists are provided in separate files\n",
    "- Special meal requirements: ", sum(!is.na(results$guests$dietary_restrictions) & results$guests$dietary_restrictions != ""), " guests\n",
    "- Primary contact: wedding@example.com / (123) 456-7890\n"
  )
  
  # Write the text summary
  writeLines(summary_text, paste0(tools::file_path_sans_ext(output_file), "_onepage.txt"))
  
  cat("IFC summary generated:", output_file, "\n")
  
  # Return the summary data
  return(list(
    summary_counts = summary_counts,
    meal_summary = meal_summary
  ))
}

# Example usage (uncomment to use):
# source("wedding-rsvp-tracker.R")
# results <- generate_wedding_reports("guestlist.csv")
# ifc_report <- generate_ifc_report(results, "ifc_guest_report.csv")
# ifc_summary <- generate_ifc_summary(results, "ifc_summary.csv")