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
  
  # Check if we have guest costs data to work with
  if (is.null(results$guest_costs) && is.null(results$guests)) {
    stop("No guest data available in results. Cannot generate IFC report.")
  }
  
  # Determine the best source of guest data
  if (!is.null(results$guest_costs)) {
    guests_for_ifc <- results$guest_costs
  } else {
    guests_for_ifc <- results$guests
  }
  
  # Extract all guests staying at IFC
  ifc_guests <- guests_for_ifc %>%
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
  
  # Add financial columns if available
  if ("total_ifc_cost" %in% names(ifc_guests) && 
      "total_guest_charge" %in% names(ifc_guests) && 
      "total_host_charge" %in% names(ifc_guests)) {
    
    night_summary$Total_IFC_Charge <- c(
      sum(ifc_guests$friday_ifc_cost[ifc_guests$is_staying_friday], na.rm = TRUE),
      sum(ifc_guests$saturday_ifc_cost[ifc_guests$is_staying_saturday], na.rm = TRUE),
      sum(ifc_guests$sunday_ifc_meals[ifc_guests$is_staying_sunday], na.rm = TRUE)
    )
    
    night_summary$Guest_Charge <- c(
      sum(ifc_guests$friday_guest_charge[ifc_guests$is_staying_friday], na.rm = TRUE),
      sum(ifc_guests$saturday_guest_charge[ifc_guests$is_staying_saturday], na.rm = TRUE),
      sum(ifc_guests$sunday_guest_charge[ifc_guests$is_staying_sunday], na.rm = TRUE)
    )
    
    night_summary$Host_Charge <- c(
      sum(ifc_guests$friday_host_charge[ifc_guests$is_staying_friday], na.rm = TRUE),
      sum(ifc_guests$saturday_host_charge[ifc_guests$is_staying_saturday], na.rm = TRUE),
      sum(ifc_guests$sunday_host_charge[ifc_guests$is_staying_sunday], na.rm = TRUE)
    )
  }
  
  # Add age category breakdown by night if available
  age_by_night <- NULL
  if ("age_category" %in% names(ifc_guests)) {
    age_by_night <- data.frame()
    for (night in c("Friday", "Saturday", "Sunday")) {
      night_column <- paste0("is_staying_", tolower(night))
      if (night_column %in% names(ifc_guests)) {
        night_data <- ifc_guests %>%
          filter(!!sym(night_column)) %>%
          group_by(age_category) %>%
          summarize(count = n(), .groups = 'drop') %>%
          mutate(Night = paste0(night, ", June ", 
                                ifelse(night == "Friday", "20", 
                                       ifelse(night == "Saturday", "21", "22"))))
        
        age_by_night <- bind_rows(age_by_night, night_data)
      }
    }
  }
  
  # Create a report for IFC with detailed stay information
  ifc_report <- data.frame(
    Last_Name = ifc_guests$last_name,
    First_Name = ifc_guests$first_name
  )
  
  # Add age category if available
  if ("age_category" %in% names(ifc_guests)) {
    ifc_report$Age_Category <- ifc_guests$age_category
  }
  
  # Add stay information
  ifc_report$Friday_Night <- ifelse(ifc_guests$is_staying_friday, "Yes", "No")
  ifc_report$Saturday_Night <- ifelse(ifc_guests$is_staying_saturday, "Yes", "No")
  ifc_report$Sunday_Night <- ifelse(ifc_guests$is_staying_sunday, "Yes", "No")
  
  # Add accommodation type
  ifc_report$Accommodation_Type <- ifelse(ifc_guests$is_camping, "Camping", "Standard Lodging")
  
  # Add financial information if available
  if (all(c("friday_ifc_cost", "saturday_ifc_cost", "sunday_ifc_meals") %in% names(ifc_guests))) {
    ifc_report$Friday_Cost <- ifc_guests$friday_ifc_cost
    ifc_report$Saturday_Cost <- ifc_guests$saturday_ifc_cost
    ifc_report$Sunday_Cost <- ifc_guests$sunday_ifc_meals
    ifc_report$Total_IFC_Cost <- ifc_guests$total_ifc_cost
  }
  
  if (all(c("friday_guest_charge", "saturday_guest_charge", "sunday_guest_charge") %in% names(ifc_guests))) {
    ifc_report$Friday_Guest_Charge <- ifc_guests$friday_guest_charge
    ifc_report$Saturday_Guest_Charge <- ifc_guests$saturday_guest_charge
    ifc_report$Sunday_Guest_Charge <- ifc_guests$sunday_guest_charge
    ifc_report$Total_Guest_Charge <- ifc_guests$total_guest_charge
  }
  
  if (all(c("friday_host_charge", "saturday_host_charge", "sunday_host_charge") %in% names(ifc_guests))) {
    ifc_report$Friday_Host_Charge <- ifc_guests$friday_host_charge
    ifc_report$Saturday_Host_Charge <- ifc_guests$saturday_host_charge
    ifc_report$Sunday_Host_Charge <- ifc_guests$sunday_host_charge
    ifc_report$Total_Host_Charge <- ifc_guests$total_host_charge
  }
  
  # Create meal counts
  # Try to use meal_counts from results
  meal_counts <- NULL
  if (!is.null(results$meal_counts)) {
    meal_counts <- results$meal_counts
  } else {
    # Create basic meal counts
    meal_counts <- list(
      friday_dinner_onsite = sum(ifc_guests$is_staying_friday, na.rm = TRUE),
      saturday_breakfast_onsite = sum(ifc_guests$is_staying_friday, na.rm = TRUE),
      saturday_lunch_onsite = sum(ifc_guests$is_staying_saturday, na.rm = TRUE),
      saturday_dinner_onsite = sum(ifc_guests$is_staying_saturday, na.rm = TRUE),
      sunday_breakfast_onsite = sum(ifc_guests$is_staying_saturday, na.rm = TRUE),
      sunday_dinner_onsite = sum(ifc_guests$is_staying_sunday, na.rm = TRUE),
      monday_breakfast_onsite = sum(ifc_guests$is_staying_sunday, na.rm = TRUE),
      
      # Assume no off-site meals if we don't have data
      friday_dinner_offsite = 0,
      saturday_lunch_offsite = 0,
      saturday_dinner_offsite = 0,
      
      # Assume all attending guests for wedding
      total_sunday_lunch = sum(ifelse("is_attending_wedding" %in% names(ifc_guests), 
                                      ifc_guests$is_attending_wedding, nrow(ifc_guests)), na.rm = TRUE),
      
      # Total counts
      total_friday_dinner = sum(ifc_guests$is_staying_friday, na.rm = TRUE),
      total_saturday_breakfast = sum(ifc_guests$is_staying_friday, na.rm = TRUE),
      total_saturday_lunch = sum(ifc_guests$is_staying_saturday, na.rm = TRUE),
      total_saturday_dinner = sum(ifc_guests$is_staying_saturday, na.rm = TRUE),
      total_sunday_breakfast = sum(ifc_guests$is_staying_saturday, na.rm = TRUE),
      total_sunday_lunch = sum(ifelse("is_attending_wedding" %in% names(ifc_guests), 
                                      ifc_guests$is_attending_wedding, nrow(ifc_guests)), na.rm = TRUE),
      total_sunday_dinner = sum(ifc_guests$is_staying_sunday, na.rm = TRUE),
      total_monday_breakfast = sum(ifc_guests$is_staying_sunday, na.rm = TRUE)
    )
  }
  
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
  
  # Add vegetarian and special diet counts if available
  if ("is_vegetarian" %in% names(ifc_guests) || "vegetarian_count" %in% names(meal_counts)) {
    veg_count <- if ("vegetarian_count" %in% names(meal_counts)) {
      meal_counts$vegetarian_count
    } else {
      sum(ifc_guests$is_vegetarian, na.rm = TRUE)
    }
    
    meal_summary$Vegetarian <- rep(veg_count, nrow(meal_summary))
  }
  
  if ("has_special_diet" %in% names(ifc_guests) || "special_diet_count" %in% names(meal_counts)) {
    special_diet_count <- if ("special_diet_count" %in% names(meal_counts)) {
      meal_counts$special_diet_count
    } else {
      sum(ifc_guests$has_special_diet, na.rm = TRUE)
    }
    
    meal_summary$Special_Diet <- rep(special_diet_count, nrow(meal_summary))
  }
  
  # Create guest counts by accommodation type and age category
  accommodation_summary <- NULL
  if ("age_category" %in% names(ifc_guests)) {
    accommodation_summary <- ifc_guests %>%
      group_by(Accommodation = ifelse(is_camping, "Camping", "Standard Lodging"), 
               Age_Category = age_category) %>%
      summarize(
        Count = n(),
        Friday_Count = sum(is_staying_friday, na.rm = TRUE),
        Saturday_Count = sum(is_staying_saturday, na.rm = TRUE),
        Sunday_Count = sum(is_staying_sunday, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Add financial data if available
    if (all(c("total_ifc_cost", "total_guest_charge", "total_host_charge") %in% names(ifc_guests))) {
      accommodation_summary <- accommodation_summary %>%
        left_join(
          ifc_guests %>%
            group_by(Accommodation = ifelse(is_camping, "Camping", "Standard Lodging"), 
                     Age_Category = age_category) %>%
            summarize(
              Total_IFC_Cost = sum(total_ifc_cost, na.rm = TRUE),
              Guest_Charge = sum(total_guest_charge, na.rm = TRUE),
              Host_Charge = sum(total_host_charge, na.rm = TRUE),
              .groups = 'drop'
            ),
          by = c("Accommodation", "Age_Category")
        )
    }
  }
  
  # Create comprehensive meal plan by age category if available
  meal_plan_by_age <- NULL
  if ("age_category" %in% names(ifc_guests) && !is.null(results$meal_counts_by_age)) {
    meal_plan_by_age <- results$meal_counts_by_age$by_age_long
  }
  
  # Add meal preferences and dietary restrictions if available
  if (any(c("meal_preferences", "dietary_restrictions") %in% names(ifc_guests))) {
    relevant_cols <- c("first_name", "last_name")
    if ("meal_preferences" %in% names(ifc_guests)) relevant_cols <- c(relevant_cols, "meal_preferences")
    if ("dietary_restrictions" %in% names(ifc_guests)) relevant_cols <- c(relevant_cols, "dietary_restrictions")
    
    guest_details <- ifc_guests %>%
      select(all_of(relevant_cols)) %>%
      mutate(
        meal_preferences = ifelse(is.na(meal_preferences), "", as.character(meal_preferences)),
        dietary_restrictions = ifelse(is.na(dietary_restrictions), "", as.character(dietary_restrictions))
      )
    
    ifc_report <- ifc_report %>%
      left_join(guest_details, by = c("First_Name" = "first_name", "Last_Name" = "last_name"))
    
    if ("meal_preferences" %in% names(guest_details)) {
      names(ifc_report)[names(ifc_report) == "meal_preferences"] <- "Meal_Preference"
    }
    
    if ("dietary_restrictions" %in% names(guest_details)) {
      names(ifc_report)[names(ifc_report) == "dietary_restrictions"] <- "Dietary_Restrictions"
    }
  }
  
  # Create detailed schedule for each meal
  meal_schedule <- NULL
  if (!is.null(results$ifc_roster)) {
    meal_schedule <- results$ifc_roster
  } else if ("is_attending_wedding" %in% names(ifc_guests) && 
             "is_attending_friday" %in% names(ifc_guests) && 
             "is_attending_saturday_lunch" %in% names(ifc_guests) &&
             "is_attending_saturday_dinner" %in% names(ifc_guests)) {
    # Create from scratch
    meal_schedule <- ifc_guests %>%
      mutate(
        friday_dinner = is_staying_friday | is_attending_friday,
        saturday_breakfast = is_staying_friday,
        saturday_lunch = is_staying_saturday | is_attending_saturday_lunch,
        saturday_dinner = is_staying_saturday | is_attending_saturday_dinner,
        sunday_breakfast = is_staying_saturday,
        sunday_lunch = is_attending_wedding,
        sunday_dinner = is_staying_sunday,
        monday_breakfast = is_staying_sunday
      ) %>%
      select(
        first_name, last_name, 
        friday_dinner, saturday_breakfast, saturday_lunch, saturday_dinner,
        sunday_breakfast, sunday_lunch, sunday_dinner, monday_breakfast
      )
  }
  
  # Write out CSV files
  csv_base_name <- tools::file_path_sans_ext(output_file)
  
  # Write night summary
  write_csv(night_summary, paste0(csv_base_name, "_night_summary.csv"))
  
  # Write age by night if available
  if (!is.null(age_by_night) && nrow(age_by_night) > 0) {
    write_csv(age_by_night, paste0(csv_base_name, "_age_by_night.csv"))
  }
  
  # Write accommodation summary if available
  if (!is.null(accommodation_summary) && nrow(accommodation_summary) > 0) {
    write_csv(accommodation_summary, paste0(csv_base_name, "_accommodation_summary.csv"))
  }
  
  # Write meal summary
  write_csv(meal_summary, paste0(csv_base_name, "_meal_summary.csv"))
  
  # Write meal plan by age if available
  if (!is.null(meal_plan_by_age) && nrow(meal_plan_by_age) > 0) {
    write_csv(meal_plan_by_age, paste0(csv_base_name, "_meal_plan_by_age.csv"))
  }
  
  # Write detailed guest list
  write_csv(ifc_report, paste0(csv_base_name, "_guest_details.csv"))
  
  # Write meal schedule if available
  if (!is.null(meal_schedule) && nrow(meal_schedule) > 0) {
    write_csv(meal_schedule, paste0(csv_base_name, "_meal_schedule.csv"))
  }
  
  # Create a single text summary file for easy reference
  summary_text <- paste0(
    "Isabella Freedman Center - Guest Stay Report\n",
    "For Wedding: Cyrena & Jon - June 20-23, 2025\n",
    "Generated: ", format(Sys.Date(), "%B %d, %Y"), "\n\n",
    
    "== Summary of Guest Stays ==\n\n",
    paste(capture.output(print(night_summary)), collapse = "\n"), "\n\n"
  )
  
  # Add age categories section if available
  if (!is.null(age_by_night) && nrow(age_by_night) > 0) {
    summary_text <- paste0(summary_text,
                           "== Age Categories by Night ==\n\n",
                           paste(capture.output(print(age_by_night %>% 
                                                        pivot_wider(names_from = Night, values_from = count, values_fill = 0))), 
                                 collapse = "\n"), "\n\n")
  }
  
  # Add accommodation section if available
  if (!is.null(accommodation_summary) && nrow(accommodation_summary) > 0) {
    summary_text <- paste0(summary_text,
                           "== Accommodation Types ==\n\n",
                           paste(capture.output(print(accommodation_summary)), collapse = "\n"), "\n\n")
  }
  
  # Add meal planning section
  summary_text <- paste0(summary_text,
                         "== Meal Planning Summary ==\n\n",
                         paste(capture.output(print(meal_summary)), collapse = "\n"), "\n\n")
  
  # Add financial summary if available
  if ("Total_IFC_Cost" %in% names(ifc_report)) {
    summary_text <- paste0(summary_text,
                           "== Financial Summary ==\n\n",
                           "Total IFC Charge: $", sum(ifc_report$Total_IFC_Cost, na.rm = TRUE), "\n")
    
    if ("Total_Guest_Charge" %in% names(ifc_report)) {
      summary_text <- paste0(summary_text,
                             "Total Guest Charge: $", sum(ifc_report$Total_Guest_Charge, na.rm = TRUE), "\n")
    }
    
    if ("Total_Host_Charge" %in% names(ifc_report)) {
      summary_text <- paste0(summary_text,
                             "Total Host Charge: $", sum(ifc_report$Total_Host_Charge, na.rm = TRUE), "\n")
    }
    
    summary_text <- paste0(summary_text, "\n")
  }
  
  # Add important notes
  summary_text <- paste0(summary_text,
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
  
  # Check if we have guest data to work with
  if (is.null(results$guest_costs) && is.null(results$guests)) {
    stop("No guest data available in results. Cannot generate IFC summary.")
  }
  
  # Determine the best source of guest data
  if (!is.null(results$guest_costs)) {
    ifc_guests <- results$guest_costs %>%
      filter(is_staying_friday | is_staying_saturday | is_staying_sunday)
  } else {
    ifc_guests <- results$guests %>%
      filter(is_staying_friday | is_staying_saturday | is_staying_sunday)
  }
  
  # Summary counts
  summary_counts <- data.frame(
    Category = c(
      "Total Overnight Guests",
      "Friday Night Total",
      "Saturday Night Total", 
      "Sunday Night Total",
      "Camping Guests",
      "Lodging Guests"
    ),
    Count = c(
      nrow(ifc_guests),
      sum(ifc_guests$is_staying_friday, na.rm = TRUE),
      sum(ifc_guests$is_staying_saturday, na.rm = TRUE),
      sum(ifc_guests$is_staying_sunday, na.rm = TRUE),
      sum(ifc_guests$is_camping, na.rm = TRUE),
      sum(!ifc_guests$is_camping & (ifc_guests$is_staying_friday | ifc_guests$is_staying_saturday | ifc_guests$is_staying_sunday), na.rm = TRUE)
    )
  )
  
  # Add special diet count if available
  if ("dietary_restrictions" %in% names(results$guests)) {
    special_diet_row <- data.frame(
      Category = "Special Meal Requirements",
      Count = sum(!is.na(results$guests$dietary_restrictions) & results$guests$dietary_restrictions != "", na.rm = TRUE)
    )
    summary_counts <- rbind(summary_counts, special_diet_row)
  }
  
  # Add age categories if available
  if ("age_category" %in% names(ifc_guests)) {
    age_counts <- ifc_guests %>%
      group_by(category = age_category) %>%
      summarize(count = n()) %>%
      mutate(Category = category, Count = count) %>%
      select(Category, Count)
    
    summary_counts <- rbind(summary_counts, age_counts)
  }
  
  # Financial summary
  financial_summary <- NULL
  if (all(c("total_ifc_cost", "total_guest_charge", "total_host_charge", 
            "friday_ifc_cost", "saturday_ifc_cost", "sunday_ifc_meals") %in% names(ifc_guests))) {
    
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
        paste0("$", format(sum(ifc_guests$total_ifc_cost, na.rm = TRUE), big.mark = ",")),
        paste0("$", format(sum(ifc_guests$total_guest_charge, na.rm = TRUE), big.mark = ",")),
        paste0("$", format(sum(ifc_guests$total_host_charge, na.rm = TRUE), big.mark = ",")),
        paste0("$", format(sum(ifc_guests$friday_ifc_cost, na.rm = TRUE), big.mark = ",")),
        paste0("$", format(sum(ifc_guests$saturday_ifc_cost, na.rm = TRUE), big.mark = ",")),
        paste0("$", format(sum(ifc_guests$sunday_ifc_meals, na.rm = TRUE), big.mark = ","))
      )
    )
  }
  
  # Meal counts - try using meal_counts from results
  meal_counts <- NULL
  if (!is.null(results$meal_counts)) {
    meal_counts <- results$meal_counts
  } else {
    # Create basic meal counts
    meal_counts <- list(
      total_friday_dinner = sum(ifc_guests$is_staying_friday, na.rm = TRUE),
      total_saturday_breakfast = sum(ifc_guests$is_staying_friday, na.rm = TRUE),
      total_saturday_lunch = sum(ifc_guests$is_staying_saturday, na.rm = TRUE),
      total_saturday_dinner = sum(ifc_guests$is_staying_saturday, na.rm = TRUE),
      total_sunday_breakfast = sum(ifc_guests$is_staying_saturday, na.rm = TRUE),
      total_sunday_lunch = sum(ifelse("is_attending_wedding" %in% names(ifc_guests), 
                                      ifc_guests$is_attending_wedding, nrow(ifc_guests)), na.rm = TRUE),
      total_sunday_dinner = sum(ifc_guests$is_staying_sunday, na.rm = TRUE),
      total_monday_breakfast = sum(ifc_guests$is_staying_sunday, na.rm = TRUE)
    )
  }
  
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
  
  # Add vegetarian and special diet counts if available
  if ("vegetarian_count" %in% names(meal_counts) || "is_vegetarian" %in% names(ifc_guests)) {
    veg_count <- if ("vegetarian_count" %in% names(meal_counts)) {
      rep(meal_counts$vegetarian_count, nrow(meal_summary))
    } else if ("is_vegetarian" %in% names(ifc_guests)) {
      rep(sum(ifc_guests$is_vegetarian, na.rm = TRUE), nrow(meal_summary))
    } else {
      rep(0, nrow(meal_summary))
    }
    
    meal_summary$Vegetarian <- veg_count
  }
  
  if ("special_diet_count" %in% names(meal_counts) || "has_special_diet" %in% names(ifc_guests)) {
    special_diet_count <- if ("special_diet_count" %in% names(meal_counts)) {
      rep(meal_counts$special_diet_count, nrow(meal_summary))
    } else if ("has_special_diet" %in% names(ifc_guests)) {
      rep(sum(ifc_guests$has_special_diet, na.rm = TRUE), nrow(meal_summary))
    } else {
      rep(0, nrow(meal_summary))
    }
    
    meal_summary$Special_Diet <- special_diet_count
  }
  
  # Write these summary files
  write_csv(summary_counts, output_file)
  if (!is.null(financial_summary)) {
    write_csv(financial_summary, paste0(tools::file_path_sans_ext(output_file), "_financial.csv"))
  }
  write_csv(meal_summary, paste0(tools::file_path_sans_ext(output_file), "_meals.csv"))
  
  # Create a single-page text summary
  summary_text <- paste0(
    "ISABELLA FREEDMAN CENTER - ONE PAGE SUMMARY\n",
    "Wedding: Cyrena & Jon - June 20-23, 2025\n",
    "Generated: ", format(Sys.Date(), "%B %d, %Y"), "\n\n",
    
    "GUEST COUNTS:\n",
    "- Total Overnight Guests: ", nrow(ifc_guests), "\n",
    "- Friday Night: ", sum(ifc_guests$is_staying_friday, na.rm = TRUE), "\n",
    "- Saturday Night: ", sum(ifc_guests$is_staying_saturday, na.rm = TRUE), "\n",
    "- Sunday Night: ", sum(ifc_guests$is_staying_sunday, na.rm = TRUE), "\n",
    "- Camping: ", sum(ifc_guests$is_camping, na.rm = TRUE), "\n",
    "- Standard Lodging: ", sum(!ifc_guests$is_camping & (ifc_guests$is_staying_friday | ifc_guests$is_staying_saturday | ifc_guests$is_staying_sunday), na.rm = TRUE), "\n\n"
  )
  
  # Add age breakdown if available
  if ("age_category" %in% names(ifc_guests)) {
    age_summary <- ifc_guests %>%
      group_by(age_category) %>%
      summarize(count = n())
    
    summary_text <- paste0(summary_text, "AGE BREAKDOWN:\n")
    for (i in 1:nrow(age_summary)) {
      summary_text <- paste0(summary_text, 
                             "- ", age_summary$age_category[i], ": ", 
                             age_summary$count[i], "\n")
    }
    summary_text <- paste0(summary_text, "\n")
  }
  
  # Add financial summary if available
  if (all(c("total_ifc_cost", "total_guest_charge", "total_host_charge") %in% names(ifc_guests))) {
    summary_text <- paste0(summary_text,
                           "FINANCIAL SUMMARY:\n",
                           "- Total IFC Charge: $", format(sum(ifc_guests$total_ifc_cost, na.rm = TRUE), big.mark = ","), "\n",
                           "- Guest Charge: $", format(sum(ifc_guests$total_guest_charge, na.rm = TRUE), big.mark = ","), "\n",
                           "- Host Charge: $", format(sum(ifc_guests$total_host_charge, na.rm = TRUE), big.mark = ","), "\n\n")
  }
  
  # Add meal counts
  summary_text <- paste0(summary_text, "MEAL COUNTS:\n")
  for (i in 1:nrow(meal_summary)) {
    summary_text <- paste0(summary_text, 
                           "- ", meal_summary$Meal[i], ": ", meal_summary$Total_Count[i])
    
    if ("Vegetarian" %in% names(meal_summary)) {
      summary_text <- paste0(summary_text, " (", meal_summary$Vegetarian[i], " veg)")
    }
    
    summary_text <- paste0(summary_text, "\n")
  }
  
  # Add notes
  summary_text <- paste0(summary_text, "\n",
                         "NOTES:\n",
                         "- Detailed guest lists are provided in separate files\n")
  
  if ("dietary_restrictions" %in% names(results$guests)) {
    special_diet_count <- sum(!is.na(results$guests$dietary_restrictions) & 
                                results$guests$dietary_restrictions != "", na.rm = TRUE)
    summary_text <- paste0(summary_text, 
                           "- Special meal requirements: ", special_diet_count, " guests\n")
  }
  
  summary_text <- paste0(summary_text,
                         "- Primary contact: wedding@example.com / (123) 456-7890\n",
                         "- See 'guest_details.csv' for complete list of dietary restrictions and meal preferences\n")
  
  # Write the summary
  writeLines(summary_text, paste0(tools::file_path_sans_ext(output_file), "_onepage.txt"))
  
  cat("IFC summary generated:", output_file, "\n")
  
  # Return the summary data
  return(list(
    summary_counts = summary_counts,
    financial_summary = if (exists("financial_summary")) financial_summary else NULL,
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
  
  # Check if we have guest data to work with
  if (is.null(results$guest_costs) && is.null(results$guests)) {
    stop("No guest data available in results. Cannot generate meal roster.")
  }
  
  # Determine the best source of guest data
  if (!is.null(results$guest_costs)) {
    guests <- results$guest_costs
  } else {
    guests <- results$guests
  }
  
  # Get all guests who may attend any meal
  meal_guests <- guests %>%
    filter(is_staying_friday | is_staying_saturday | is_staying_sunday | 
             ("is_attending_wedding" %in% names(guests) & is_attending_wedding) | 
             ("is_attending_friday" %in% names(guests) & is_attending_friday) | 
             ("is_attending_saturday_lunch" %in% names(guests) & is_attending_saturday_lunch) | 
             ("is_attending_saturday_dinner" %in% names(guests) & is_attending_saturday_dinner))
  
  # Create meal attendance flags - handle case where attendance variables might not exist
  has_attendance_flags <- all(c("is_attending_wedding", "is_attending_friday", 
                                "is_attending_saturday_lunch", "is_attending_saturday_dinner") %in% names(meal_guests))
  
  meal_roster <- meal_guests %>%
    mutate(
      # Meal attendance based on stays and RSVPs
      friday_dinner = is_staying_friday | 
        (has_attendance_flags & is_attending_friday & !is_staying_friday),
      
      saturday_breakfast = is_staying_friday,
      
      saturday_lunch = is_staying_saturday | 
        (has_attendance_flags & is_attending_saturday_lunch & !is_staying_saturday),
      
      saturday_dinner = is_staying_saturday | 
        (has_attendance_flags & is_attending_saturday_dinner & !is_staying_saturday),
      
      sunday_breakfast = is_staying_saturday,
      
      sunday_lunch = if (has_attendance_flags) is_attending_wedding else TRUE,
      
      sunday_dinner = is_staying_sunday,
      
      monday_breakfast = is_staying_sunday
    )
  
  # Add dietary information if available
  meal_roster <- meal_roster %>%
    mutate(
      is_vegetarian = if ("is_vegetarian" %in% names(meal_roster)) 
        is_vegetarian 
      else if ("meal_preferences" %in% names(meal_roster)) 
        meal_preferences == "No meat" | meal_preferences == "Opt-in for fish only" 
      else FALSE,
      
      has_special_diet = if ("has_special_diet" %in% names(meal_roster)) 
        has_special_diet 
      else if ("dietary_restrictions" %in% names(meal_roster)) 
        !is.na(dietary_restrictions) & dietary_restrictions != "" 
      else FALSE
    )
  
  # Select relevant columns
  columns_to_select <- c(
    "last_name", 
    "first_name"
  )
  
  # Add age_category if it exists
  if ("age_category" %in% names(meal_roster)) {
    columns_to_select <- c(columns_to_select, "age_category")
  }
  
  # Add meal preferences and restrictions if they exist
  if ("meal_preferences" %in% names(meal_roster)) {
    columns_to_select <- c(columns_to_select, "meal_preferences")
  }
  
  if ("dietary_restrictions" %in% names(meal_roster)) {
    columns_to_select <- c(columns_to_select, "dietary_restrictions")
  }
  
  # Add meal flags
  meal_flags <- c(
    "friday_dinner", 
    "saturday_breakfast", 
    "saturday_lunch", 
    "saturday_dinner", 
    "sunday_breakfast", 
    "sunday_lunch", 
    "sunday_dinner", 
    "monday_breakfast"
  )
  
  columns_to_select <- c(columns_to_select, meal_flags)
  
  # Create roster
  final_roster <- meal_roster %>%
    select(all_of(columns_to_select))
  
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
  
  # Iterate through each meal and create roster
  for (i in 1:length(meal_list)) {
    meal_col <- meal_list[i]
    meal_name <- meal_names[i]
    
    # Get guests attending this meal
    attendees <- final_roster %>%
      filter(!!sym(meal_col))
    
    if (nrow(attendees) > 0) {
      # Base columns to select
      select_cols <- c("last_name", "first_name")
      
      # Add age category if it exists
      if ("age_category" %in% names(attendees)) {
        select_cols <- c(select_cols, "age_category")
      }
      
      # Add meal preferences and restrictions if they exist
      if ("meal_preferences" %in% names(attendees)) {
        select_cols <- c(select_cols, "meal_preferences")
      }
      
      if ("dietary_restrictions" %in% names(attendees)) {
        select_cols <- c(select_cols, "dietary_restrictions")
      }
      
      # Create roster for this meal
      meal_attendees <- attendees %>%
        select(all_of(select_cols)) %>%
        arrange(last_name, first_name)
      
      # Write to CSV
      write_csv(meal_attendees, 
                paste0(tools::file_path_sans_ext(output_file), "_", meal_col, ".csv"))
      
      # Create summary by age category and meal preference
      summary_cols <- c()
      if ("age_category" %in% names(attendees)) {
        summary_cols <- c(summary_cols, "age_category")
      }
      
      if ("meal_preferences" %in% names(attendees)) {
        summary_cols <- c(summary_cols, "meal_preferences")
      }
      
      if (length(summary_cols) > 0) {
        summary <- attendees %>%
          group_by_at(vars(all_of(summary_cols))) %>%
          summarize(count = n(), .groups = "drop") %>%
          arrange_at(vars(all_of(summary_cols)))
        
        write_csv(summary, 
                  paste0(tools::file_path_sans_ext(output_file), "_", meal_col, "_summary.csv"))
      }
    }
  }
  
  # Write the complete roster
  write_csv(final_roster, output_file)
  
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
    total_count <- sum(final_roster[[meal_col]], na.rm = TRUE)
    
    veg_count <- if ("is_vegetarian" %in% names(final_roster)) {
      sum(final_roster[[meal_col]] & final_roster$is_vegetarian, na.rm = TRUE)
    } else {
      0
    }
    
    special_diet_count <- if ("has_special_diet" %in% names(final_roster)) {
      sum(final_roster[[meal_col]] & final_roster$has_special_diet, na.rm = TRUE)
    } else {
      0
    }
    
    summary_text <- paste0(
      summary_text,
      meal_name, ":\n",
      "- Total: ", total_count, "\n"
    )
    
    if (veg_count > 0) {
      summary_text <- paste0(summary_text, "- Vegetarian/Fish Only: ", veg_count, "\n")
    }
    
    if (special_diet_count > 0) {
      summary_text <- paste0(summary_text, "- Special Dietary Needs: ", special_diet_count, "\n")
    }
    
    summary_text <- paste0(summary_text, "\n")
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
    meal_roster = final_roster
  ))
}

# Example usage (uncomment to use):
# source("wedding-rsvp-tracker.R")
# results <- generate_wedding_reports("guestlist.csv")
# ifc_report <- generate_ifc_report(results, "ifc_guest_report.csv")
# ifc_summary <- generate_ifc_summary(results, "ifc_summary.csv")
# ifc_meal_roster <- generate_ifc_meal_roster(results, "ifc_meal_roster.csv")