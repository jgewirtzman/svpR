# Function to generate a detailed meal planning report
generate_meal_planning_report <- function(results, output_file = "meal_planning_report.csv") {
  # Force directory creation
  output_dir <- dirname(output_file)
  if (output_dir != "." && output_dir != "") {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    cat("Ensuring output directory exists:", output_dir, "\n")
  }
  
  # Get meal counts from results
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
  
  # Create dietary restrictions summary
  dietary_restrictions <- results$guests %>%
    filter(!is.na(dietary_restrictions) & dietary_restrictions != "") %>%
    group_by(restriction = dietary_restrictions) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  
  # Create meal preferences summary
  meal_preferences <- results$guests %>%
    filter(!is.na(meal_preferences)) %>%
    group_by(preference = meal_preferences) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  
  # Create a list of guests with special requirements
  special_diet_guests <- results$guests %>%
    filter(!is.na(dietary_restrictions) & dietary_restrictions != "") %>%
    select(
      first_name,
      last_name,
      meal_preferences,
      dietary_restrictions
    ) %>%
    rename(
      "First Name" = first_name,
      "Last Name" = last_name,
      "Meal Preference" = meal_preferences,
      "Dietary Restrictions" = dietary_restrictions
    )
  
  # Write out CSV files
  csv_base_name <- tools::file_path_sans_ext(output_file)
  
  # Write meal summary
  write_csv(meal_summary, paste0(csv_base_name, "_meal_summary.csv"))
  
  # Write meal preferences
  write_csv(meal_preferences, paste0(csv_base_name, "_meal_preferences.csv"))
  
  # Write dietary restrictions
  write_csv(dietary_restrictions, paste0(csv_base_name, "_dietary_restrictions.csv"))
  
  # Write special diet guests
  write_csv(special_diet_guests, paste0(csv_base_name, "_special_diet_guests.csv"))
  
  # Create a text summary file for easy reference
  summary_text <- paste0(
    "Wedding Meal Planning Report\n",
    "For Wedding: Cyrena & Jon - June 20-23, 2025\n",
    "Generated: ", format(Sys.Date(), "%B %d, %Y"), "\n\n",
    
    "== Meal Planning Summary ==\n\n",
    paste(capture.output(print(meal_summary)), collapse = "\n"), "\n\n",
    
    "== Meal Preferences ==\n\n",
    paste(capture.output(print(meal_preferences)), collapse = "\n"), "\n\n",
    
    "== Dietary Restrictions ==\n\n",
    paste(capture.output(print(dietary_restrictions)), collapse = "\n"), "\n\n",
    
    "== Important Notes ==\n\n",
    "1. Meal Inclusion by Stay:\n",
    "   - Friday night guests: Friday dinner, Saturday breakfast\n",
    "   - Saturday night guests: Saturday lunch, dinner, Sunday breakfast\n",
    "   - Sunday night guests: Special catering for Sunday dinner, Monday breakfast\n",
    "   - All wedding guests: Sunday lunch (wedding meal)\n\n",
    
    "2. Saturday Off-Site Guest Meals:\n",
    "   - Guests can choose to join for lunch only, dinner only, or both meals\n",
    "   - The counts above reflect these preferences from RSVP responses\n\n",
    
    "3. Dietary Information:\n",
    "   - Non-meat options should be available at all meals\n",
    "   - Please review the detailed dietary restrictions list and ensure appropriate options are available\n",
    "   - See special_diet_guests.csv for individual dietary needs\n\n",
    
    "4. Catering Planning:\n",
    "   - Sunday lunch (wedding reception) is the largest meal requiring service for all guests\n",
    "   - Sunday dinner and Monday breakfast are special catering for overnight guests only\n"
  )
  
  # Write the text summary
  writeLines(summary_text, paste0(csv_base_name, "_summary.txt"))
  
  # Create one-page quick reference for catering staff
  catering_reference <- paste0(
    "MEAL PLANNING QUICK REFERENCE\n",
    "Wedding: Cyrena & Jon - June 20-23, 2025\n\n",
    
    "TOTAL MEAL COUNTS:\n",
    "- Friday Dinner: ", meal_counts$total_friday_dinner, "\n",
    "- Saturday Breakfast: ", meal_counts$total_saturday_breakfast, "\n",
    "- Saturday Lunch: ", meal_counts$total_saturday_lunch, "\n",
    "- Saturday Dinner: ", meal_counts$total_saturday_dinner, "\n",
    "- Sunday Breakfast: ", meal_counts$total_sunday_breakfast, "\n",
    "- Sunday Lunch (Wedding): ", meal_counts$total_sunday_lunch, "\n",
    "- Sunday Dinner: ", meal_counts$total_sunday_dinner, "\n",
    "- Monday Breakfast: ", meal_counts$total_monday_breakfast, "\n\n",
    
    "SPECIAL DIETS SUMMARY:\n"
  )
  
  # Add special diet summary
  if (nrow(dietary_restrictions) > 0) {
    for (i in 1:nrow(dietary_restrictions)) {
      catering_reference <- paste0(
        catering_reference,
        "- ", dietary_restrictions$restriction[i], ": ", dietary_restrictions$count[i], " guests\n"
      )
    }
  } else {
    catering_reference <- paste0(
      catering_reference, 
      "- No special dietary requirements recorded\n"
    )
  }
  
  # Add meal preferences
  catering_reference <- paste0(
    catering_reference,
    "\nMEAL PREFERENCES:\n"
  )
  
  if (nrow(meal_preferences) > 0) {
    for (i in 1:nrow(meal_preferences)) {
      catering_reference <- paste0(
        catering_reference,
        "- ", meal_preferences$preference[i], ": ", meal_preferences$count[i], " guests\n"
      )
    }
  } else {
    catering_reference <- paste0(
      catering_reference, 
      "- No meal preferences recorded\n"
    )
  }
  
  # Write quick reference
  writeLines(catering_reference, paste0(csv_base_name, "_quickref.txt"))
  
  cat("Meal planning report files generated in directory:", dirname(output_file), "\n")
  
  # Return the meal summaries
  return(list(
    meal_summary = meal_summary,
    meal_preferences = meal_preferences,
    dietary_restrictions = dietary_restrictions,
    special_diet_guests = special_diet_guests
  ))
}