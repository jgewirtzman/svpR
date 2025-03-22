# Isabella Freedman Center Report Generator
# This script creates formatted reports for the Isabella Freedman Center with enhanced
# meal tracking, age categories, and financial breakdowns

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(knitr)
library(rmarkdown)

# Function to generate a comprehensive report for the Isabella Freedman Center
generate_ifc_report <- function(results, output_file = "ifc_report.csv") {
  # Force directory creation
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
    ),
    Total_IFC_Charge = c(
      sum(ifc_guests$friday_cost[ifc_guests$is_staying_friday]),
      sum(ifc_guests$saturday_cost[ifc_guests$is_staying_saturday]),
      sum(ifc_guests$sunday_cost[ifc_guests$is_staying_sunday])
    ),
    Guest_Charge = c(
      sum(ifc_guests$friday_guest_charge[ifc_guests$is_staying_friday]),
      sum(ifc_guests$saturday_guest_charge[ifc_guests$is_staying_saturday]),
      sum(ifc_guests$sunday_guest_charge[ifc_guests$is_staying_sunday])
    ),
    Host_Charge = c(
      sum(ifc_guests$friday_host_charge[ifc_guests$is_staying_friday]),
      sum(ifc_guests$saturday_host_charge[ifc_guests$is_staying_saturday]),
      sum(ifc_guests$sunday_host_charge[ifc_guests$is_staying_sunday])
    )
  )
  
  # Add age category breakdown by night
  age_by_night <- data.frame()
  for (night in c("Friday", "Saturday", "Sunday")) {
    is_staying_col <- paste0("is_staying_", tolower(night))
    night_data <- ifc_guests %>%
      filter(!!sym(is_staying_col)) %>%
      group_by(age_category) %>%
      summarize(count = n()) %>%
      mutate(Night = paste0(night, ", June ", ifelse(night == "Friday", "20", ifelse(night == "Saturday", "21", "22"))))
    
    age_by_night <- bind_rows(age_by_night, night_data)
  }
  
  # Create a report for IFC with detailed stay information
  ifc_report <- data.frame(
    Last_Name = ifc_guests$last_name,
    First_Name = ifc_guests$first_name,
    Age_Category = ifc_guests$age_category,
    Friday_Night = ifelse(ifc_guests$is_staying_friday, "Yes", "No"),
    Saturday_Night = ifelse(ifc_guests$is_staying_saturday, "Yes", "No"),
    Sunday_Night = ifelse(ifc_guests$is_staying_sunday, "Yes", "No"),
    Accommodation_Type = ifelse(ifc_guests$is_camping, "Camping", "Standard Lodging"),
    Friday_Cost = ifc_guests$friday_cost,
    Saturday_Cost = ifc_guests$saturday_cost,
    Sunday_Cost = ifc_guests$sunday_cost,
    Total_IFC_Cost = ifc_guests$total_cost,
    Guest_Charge = ifc_guests$total_guest_charge,
    Host_Charge = ifc_guests$total_host_charge
  )
  
  # Create meal counts by night (using the improved meal tracking)
  meal_counts <- results$meal_counts
  
  # Format meal summary for the report with age categories
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
    ),
    Vegetarian = c(
      meal_counts$friday_dinner_vegetarian,
      meal_counts$saturday_breakfast_vegetarian,
      meal_counts$saturday_lunch_vegetarian,
      meal_counts$saturday_dinner_vegetarian,
      meal_counts$sunday_breakfast_vegetarian,
      meal_counts$sunday_lunch_vegetarian,
      meal_counts$sunday_dinner_vegetarian,
      meal_counts$monday_breakfast_vegetarian
    ),
    Special_Diet = c(
      meal_counts$friday_dinner_special_diet,
      meal_counts$saturday_breakfast_special_diet,
      meal_counts$saturday_lunch_special_diet,
      meal_counts$saturday_dinner_special_diet,
      meal_counts$sunday_breakfast_special_diet,
      meal_counts$sunday_lunch_special_diet,
      meal_counts$sunday_dinner_special_diet,
      meal_counts$monday_breakfast_special_diet
    )
  )
  
  # Create guest counts by accommodation type and age category
  accommodation_summary <- ifc_guests %>%
    group_by(Accommodation = ifelse(is_camping, "Camping", "Standard Lodging"), 
             Age_Category = age_category) %>%
    summarize(
      Count = n(),
      Friday_Count = sum(is_staying_friday),
      Saturday_Count = sum(is_staying_saturday),
      Sunday_Count = sum(is_staying_sunday),
      Total_IFC_Cost = sum(total_cost),
      Guest_Charge = sum(total_guest_charge),
      Host_Charge = sum(total_host_charge)
    )
  
  # Create comprehensive meal plan by age category
  if (!is.null(results$meal_counts_by_age)) {
    meal_plan_by_age <- results$meal_counts_by_age$all_meals
  } else {
    meal_plan_by_age <- data.frame(
      meal = character(),
      age_category = character(), 
      count = integer(),
      veg_count = integer(),
      meat_fish_count = integer(),
      special_diet_count = integer()
    )
  }
  
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
  
  # Create detailed schedule for each meal
  meal_schedule <- results$ifc_schedule
  
  # Write out CSV files
  csv_base_name <- tools::file_path_sans_ext(output_file)
  
  # Write night summary
  write_csv(night_summary, paste0(csv_base_name, "_night_summary.csv"))
  
  # Write age by night
  write_csv(age_by_night, paste0(csv_base_name, "_age_by_night.csv"))
  
  # Write accommodation summary
  write_csv(accommodation_summary, paste0(csv_base_name, "_accommodation_summary.csv"))
  
  # Write meal summary
  write_csv(meal_summary, paste0(csv_base_name, "_meal_summary.csv"))
  
  # Write meal plan by age
  write_csv(meal_plan_by_age, paste0(csv_base_name, "_meal_plan_by_age.csv"))
  
  # Write detailed guest list
  write_csv(ifc_report, paste0(csv_base_name, "_guest_details.csv"))
  
  # Write meal schedule
  if (!is.null(meal_schedule)) {
    write_csv(meal_schedule, paste0(csv_base_name, "_meal_schedule.csv"))
  }
  
  # Create a single text summary file for easy reference
  summary_text <- paste0(
    "Isabella Freedman Center - Guest Stay Report\n",
    "For Wedding: Cyrena & Jon - June 20-23, 2025\n",
    "Generated: ", format(Sys.Date(), "%B %d, %Y"), "\n\n",
    
    "== Summary of Guest Stays ==\n\n",
    paste(capture.output(print(night_summary)), collapse = "\n"), "\n\n",
    
    "== Age Categories by Night ==\n\n",
    paste(capture.output(print(age_by_night %>% 
                                 pivot_wider(names_from = Night, values_from = count, values_fill = 0))), 
          collapse = "\n"), "\n\n",
    
    "== Accommodation Types ==\n\n",
    paste(capture.output(print(accommodation_summary)), collapse = "\n"), "\n\n",
    
    "== Meal Planning Summary ==\n\n",
    paste(capture.output(print(meal_summary)), collapse = "\n"), "\n\n",
    
    "== Financial Summary ==\n\n",
    "Total IFC Charge: $", sum(ifc_guests$total_cost), "\n",
    "Total Guest Charge: $", sum(ifc_guests$total_guest_charge), "\n",
    "Total Host Charge: $", sum(ifc_guests$total_host_charge), "\n\n",
    
    "== Important Notes ==\n\n",
    "1. Meal Inclusion by Stay:\n",
    "   - Friday night guests: Friday dinner, Saturday breakfast\n",
    "   - Saturday night guests: Saturday lunch, dinner, Sunday breakfast\n",
    "   - Sunday night guests: Special catering for Sunday dinner, Monday breakfast\n",
    "   - All wedding guests: Sunday lunch (wedding meal)\n\n",
    
    "2. All guests listed are part of the wedding party for Cyrena & Jon.\n",
    "3. Please direct any questions to wedding@example.com or call (123) 456-7890.\n",
    "4. Special accommodation requests have been noted in the detailed guest list.\n",
    "5. Age categories are used for room assignments and meal planning.\n"
  )
  
  # Write the text summary
  writeLines(summary_text, paste0(csv_base_name, "_summary.txt"))
  
  cat("IFC report files generated in directory:", dirname(output_file), "\n")
  
  # Return the data used in the report
  return(list(
    night_summary = night_summary,
    age_by_night = age_by_night,
    accommodation_summary = accommodation_summary,
    meal_summary = meal_summary,
    meal_plan_by_age = meal_plan_by_age,
    guest_details = ifc_report,
    meal_schedule = meal_schedule
  ))
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
      "Special Meal Requirements",
      "Adults 21+",
      "Guests 12-21",
      "Children 5-12",
      "Children <5"
    ),
    Count = c(
      nrow(ifc_guests),
      sum(ifc_guests$is_staying_friday),
      sum(ifc_guests$is_staying_saturday),
      sum(ifc_guests$is_staying_sunday),
      sum(ifc_guests$is_camping),
      sum(!ifc_guests$is_camping & (ifc_guests$is_staying_friday | ifc_guests$is_staying_saturday | ifc_guests$is_staying_sunday)),
      sum(!is.na(results$guests$dietary_restrictions) & results$guests$dietary_restrictions != ""),
      sum(grepl("Adults 21\\+", ifc_guests$age_category)),
      sum(grepl("Guests 12-21", ifc_guests$age_category)),
      sum(grepl("Children 5-12", ifc_guests$age_category)),
      sum(grepl("Children <5", ifc_guests$age_category))
    )
  )
  
  # Financial summary
  financial_summary <- data.frame(
    Category = c(
      "Total IFC Charge",
      "Guest Charge",
      "Host Charge",
      "Friday IFC Total",
      "Saturday IFC Total",
      "Sunday IFC Total"
    ),
    Amount = c(
      paste0("$", format(sum(ifc_guests$total_cost), big.mark = ",")),
      paste0("$", format(sum(ifc_guests$total_guest_charge), big.mark = ",")),
      paste0("$", format(sum(ifc_guests$total_host_charge), big.mark = ",")),
      paste0("$", format(sum(ifc_guests$friday_cost), big.mark = ",")),
      paste0("$", format(sum(ifc_guests$saturday_cost), big.mark = ",")),
      paste0("$", format(sum(ifc_guests$sunday_cost), big.mark = ","))
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
    ),
    Vegetarian = c(
      meal_counts$friday_dinner_vegetarian,
      meal_counts$saturday_breakfast_vegetarian,
      meal_counts$saturday_lunch_vegetarian,
      meal_counts$saturday_dinner_vegetarian,
      meal_counts$sunday_breakfast_vegetarian,
      meal_counts$sunday_lunch_vegetarian,
      meal_counts$sunday_dinner_vegetarian,
      meal_counts$monday_breakfast_vegetarian
    ),
    Special_Diet = c(
      meal_counts$friday_dinner_special_diet,
      meal_counts$saturday_breakfast_special_diet,
      meal_counts$saturday_lunch_special_diet,
      meal_counts$saturday_dinner_special_diet,
      meal_counts$sunday_breakfast_special_diet,
      meal_counts$sunday_lunch_special_diet,
      meal_counts$sunday_dinner_special_diet,
      meal_counts$monday_breakfast_special_diet
    )
  )
  
  # Write these summary files
  write_csv(summary_counts, output_file)
  write_csv(financial_summary, paste0(tools::file_path_sans_ext(output_file), "_financial.csv"))
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
    
    "AGE BREAKDOWN:\n",
    "- Adults 21+: ", sum(grepl("Adults 21\\+", ifc_guests$age_category)), "\n",
    "- Guests 12-21: ", sum(grepl("Guests 12-21", ifc_guests$age_category)), "\n",
    "- Children 5-12: ", sum(grepl("Children 5-12", ifc_guests$age_category)), "\n",
    "- Children <5: ", sum(grepl("Children <5", ifc_guests$age_category)), "\n\n",
    
    "FINANCIAL SUMMARY:\n",
    "- Total IFC Charge: $", format(sum(ifc_guests$total_cost), big.mark = ","), "\n",
    "- Guest Charge: $", format(sum(ifc_guests$total_guest_charge), big.mark = ","), "\n",
    "- Host Charge: $", format(sum(ifc_guests$total_host_charge), big.mark = ","), "\n\n",
    
    "MEAL COUNTS:\n",
    "- Friday Dinner: ", meal_counts$total_friday_dinner, " (", meal_counts$friday_dinner_vegetarian, " veg)\n",
    "- Saturday Breakfast: ", meal_counts$total_saturday_breakfast, " (", meal_counts$saturday_breakfast_vegetarian, " veg)\n",
    "- Saturday Lunch: ", meal_counts$total_saturday_lunch, " (", meal_counts$saturday_lunch_vegetarian, " veg)\n",
    "- Saturday Dinner: ", meal_counts$total_saturday_dinner, " (", meal_counts$saturday_dinner_vegetarian, " veg)\n",
    "- Sunday Breakfast: ", meal_counts$total_sunday_breakfast, " (", meal_counts$sunday_breakfast_vegetarian, " veg)\n",
    "- Sunday Lunch (Wedding): ", meal_counts$total_sunday_lunch, " (", meal_counts$sunday_lunch_vegetarian, " veg)\n",
    "- Sunday Dinner: ", meal_counts$total_sunday_dinner, " (", meal_counts$sunday_dinner_vegetarian, " veg)\n",
    "- Monday Breakfast: ", meal_counts$total_monday_breakfast, " (", meal_counts$monday_breakfast_vegetarian, " veg)\n\n",
    
    "NOTES:\n",
    "- Detailed guest lists are provided in separate files\n",
    "- Special meal requirements: ", sum(!is.na(results$guests$dietary_restrictions) & results$guests$dietary_restrictions != ""), " guests\n",
    "- Primary contact: wedding@example.com / (123) 456-7890\n",
    "- See 'guest_details.csv' for complete list of dietary restrictions and meal preferences\n"
  )
  
  # Write the text summary
  writeLines(summary_text, paste0(tools::file_path_sans_ext(output_file), "_onepage.txt"))
  
  cat("IFC summary generated:", output_file, "\n")
  
  # Return the summary data
  return(list(
    summary_counts = summary_counts,
    financial_summary = financial_summary,
    meal_summary = meal_summary
  ))
}

# Generate a roster of all guests by meal for the IFC staff
generate_ifc_meal_roster <- function(results, output_file = "ifc_meal_roster.csv") {
  # Create directory if it doesn't exist
  output_dir <- dirname(output_file)
  if (output_dir != "." && output_dir != "") {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  # Get all guests who may attend any meal
  guests <- results$guest_costs %>%
    filter(is_staying_friday | is_staying_saturday | is_staying_sunday | wedding_rsvp == "Joyfully Accept" | 
             fridayshabbat_rsvp == "Joyfully Accept" | !is.na(saturday_onsite_rsvp) | !is.na(saturday_offsite_rsvp))
  
  # Create meal attendance flags
  meal_roster <- guests %>%
    mutate(
      # Meal attendance based on stays and RSVPs
      friday_dinner = is_staying_friday | (fridayshabbat_rsvp == "Joyfully Accept" & !is_staying_friday),
      saturday_breakfast = is_staying_friday,
      saturday_lunch = is_staying_saturday | saturday_onsite_rsvp == "Yes, I will join on Saturday" | 
        saturday_offsite_rsvp == "Yes, I will join for lunch only" | saturday_offsite_rsvp == "Yes, I will join for lunch and dinner",
      saturday_dinner = is_staying_saturday | saturday_onsite_rsvp == "Yes, I will join on Saturday" | 
        saturday_offsite_rsvp == "Yes, I will join for dinner only" | saturday_offsite_rsvp == "Yes, I will join for lunch and dinner",
      sunday_breakfast = is_staying_saturday,
      sunday_lunch = wedding_rsvp == "Joyfully Accept",
      sunday_dinner = is_staying_sunday,
      monday_breakfast = is_staying_sunday
    ) %>%
    select(
      last_name, 
      first_name, 
      age_category,
      meal_preferences,
      dietary_restrictions,
      friday_dinner, 
      saturday_breakfast, 
      saturday_lunch, 
      saturday_dinner, 
      sunday_breakfast, 
      sunday_lunch, 
      sunday_dinner, 
      monday_breakfast
    )
  
  # Create individual meal rosters
  meal_list <- c(
    "friday_dinner", 
    "saturday_breakfast", 
    "saturday_lunch", 
    "saturday_dinner", 
    "sunday_breakfast", 
    "sunday_lunch", 
    "sunday_dinner", 
    "monday_breakfast"
  )
  
  meal_names <- c(
    "Friday Dinner", 
    "Saturday Breakfast", 
    "Saturday Lunch", 
    "Saturday Dinner", 
    "Sunday Breakfast", 
    "Sunday Lunch (Wedding)", 
    "Sunday Dinner", 
    "Monday Breakfast"
  )
  
  for (i in 1:length(meal_list)) {
    meal_col <- meal_list[i]
    meal_name <- meal_names[i]
    
    # Get guests attending this meal
    attendees <- meal_roster %>%
      filter(!!sym(meal_col)) %>%
      select(last_name, first_name, age_category, meal_preferences, dietary_restrictions) %>%
      arrange(last_name, first_name)
    
    # Write to CSV
    write_csv(attendees, paste0(tools::file_path_sans_ext(output_file), "_", meal_col, ".csv"))
    
    # Create summary by age category and meal preference
    summary <- attendees %>%
      group_by(age_category, meal_preferences) %>%
      summarize(count = n()) %>%
      arrange(age_category, meal_preferences)
    
    write_csv(summary, paste0(tools::file_path_sans_ext(output_file), "_", meal_col, "_summary.csv"))
  }
  
  # Write the complete roster
  write_csv(meal_roster, output_file)
  
  # Create a roster summary text file
  summary_text <- paste0(
    "IFC MEAL ROSTER SUMMARY\n",
    "Wedding: Cyrena & Jon - June 20-23, 2025\n",
    "Generated: ", format(Sys.Date(), "%B %d, %Y"), "\n\n"
  )
  
  # Add meal counts
  for (i in 1:length(meal_list)) {
    meal_col <- meal_list[i]
    meal_name <- meal_names[i]
    
    # Get count and breakdown
    total_count <- sum(meal_roster[[meal_col]])
    veg_count <- sum(meal_roster[[meal_col]] & (meal_roster$meal_preferences == "No meat" | 
                                                  meal_roster$meal_preferences == "Opt-in for fish only"))
    
    special_diet_count <- sum(meal_roster[[meal_col]] & !is.na(meal_roster$dietary_restrictions) & 
                                meal_roster$dietary_restrictions != "")
    
    summary_text <- paste0(
      summary_text,
      meal_name, ":\n",
      "- Total: ", total_count, "\n",
      "- Vegetarian/Fish Only: ", veg_count, "\n",
      "- Special Dietary Needs: ", special_diet_count, "\n\n"
    )
  }
  
  # Add notes
  summary_text <- paste0(
    summary_text,
    "NOTES:\n",
    "- Each meal has its own CSV file with complete attendee list\n",
    "- Individual meal summary files show breakdowns by age category\n",
    "- Special dietary needs are noted for each guest\n",
    "- The main roster file contains all guests and their meal attendance\n"
  )
  
  # Write the summary
  writeLines(summary_text, paste0(tools::file_path_sans_ext(output_file), "_summary.txt"))
  
  cat("IFC meal roster generated:", output_file, "\n")
  
  return(list(
    meal_roster = meal_roster
  ))
}

# Example usage (uncomment to use):
# source("wedding-rsvp-tracker.R")
# results <- generate_wedding_reports("guestlist.csv")
# ifc_report <- generate_ifc_report(results, "ifc_guest_report.csv")
# ifc_summary <- generate_ifc_summary(results, "ifc_summary.csv")
# ifc_meal_roster <- generate_ifc_meal_roster(results, "ifc_meal_roster.csv")