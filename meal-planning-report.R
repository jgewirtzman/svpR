# Function to generate a detailed meal planning report with enhanced age category support
generate_meal_planning_report <- function(results, output_file = "meal_planning_report.pdf") {
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
  
  # Add vegetarian and special diet counts if available
  if ("friday_dinner_vegetarian" %in% names(meal_counts)) {
    meal_summary$Vegetarian <- c(
      meal_counts$friday_dinner_vegetarian,
      meal_counts$saturday_breakfast_vegetarian,
      meal_counts$saturday_lunch_vegetarian,
      meal_counts$saturday_dinner_vegetarian,
      meal_counts$sunday_breakfast_vegetarian,
      meal_counts$sunday_lunch_vegetarian,
      meal_counts$sunday_dinner_vegetarian,
      meal_counts$monday_breakfast_vegetarian
    )
  }
  
  if ("friday_dinner_special_diet" %in% names(meal_counts)) {
    meal_summary$Special_Diet <- c(
      meal_counts$friday_dinner_special_diet,
      meal_counts$saturday_breakfast_special_diet,
      meal_counts$saturday_lunch_special_diet,
      meal_counts$saturday_dinner_special_diet,
      meal_counts$sunday_breakfast_special_diet,
      meal_counts$sunday_lunch_special_diet,
      meal_counts$sunday_dinner_special_diet,
      meal_counts$monday_breakfast_special_diet
    )
  }
  
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
      age_category,
      meal_preferences,
      dietary_restrictions
    ) %>%
    rename(
      "First Name" = first_name,
      "Last Name" = last_name,
      "Age Category" = age_category,
      "Meal Preference" = meal_preferences,
      "Dietary Restrictions" = dietary_restrictions
    )
  
  # Extract meal rosters by age category if available
  meal_by_age_data <- NULL
  if (!is.null(results$meal_counts_by_age) && !is.null(results$meal_counts_by_age$all_meals)) {
    meal_by_age_data <- results$meal_counts_by_age$all_meals %>%
      rename(
        "Meal" = meal,
        "Age Category" = age_category,
        "Count" = count,
        "Vegetarian" = veg_count,
        "Meat/Fish" = meat_fish_count,
        "Special Diet" = special_diet_count
      )
  } else if (!is.null(results$meal_rosters)) {
    # Alternative extraction from meal_rosters
    rosters <- results$meal_rosters
    meal_by_age_data <- data.frame(
      Meal = character(),
      "Age Category" = character(),
      Count = integer(),
      Vegetarian = integer(),
      "Meat/Fish" = integer(),
      "Special Diet" = integer(),
      stringsAsFactors = FALSE
    )
    
    for (meal_code in names(rosters)) {
      if (!is.null(rosters[[meal_code]]$summary)) {
        meal_data <- rosters[[meal_code]]$summary %>%
          mutate(
            Meal = rosters[[meal_code]]$meal_name,
            "Age Category" = age_category
          ) %>%
          rename(
            Count = total_guests,
            Vegetarian = vegetarian_count,
            "Meat/Fish" = meat_fish_count,
            "Special Diet" = special_diet_count
          ) %>%
          select(Meal, "Age Category", Count, Vegetarian, "Meat/Fish", "Special Diet")
        
        meal_by_age_data <- rbind(meal_by_age_data, meal_data)
      }
    }
  }
  
  # Ensure we have meal attendee lists for each meal
  meal_attendees <- list()
  meal_codes <- c("friday_dinner", "saturday_breakfast", "saturday_lunch", 
                  "saturday_dinner", "sunday_breakfast", "sunday_lunch", 
                  "sunday_dinner", "monday_breakfast")
  
  meal_names <- c("Friday Dinner", "Saturday Breakfast", "Saturday Lunch", 
                  "Saturday Dinner", "Sunday Breakfast", "Sunday Lunch (Wedding)", 
                  "Sunday Dinner", "Monday Breakfast")
  
  # Function to identify guests attending a specific meal
  get_meal_attendees <- function(guests, meal_code) {
    # Determine which guests attend this meal based on their RSVP status
    attendees <- switch(meal_code,
                        "friday_dinner" = guests %>% filter(is_staying_friday | 
                                                              (fridayshabbat_rsvp == "Joyfully Accept" & !is_staying_friday)),
                        "saturday_breakfast" = guests %>% filter(is_staying_friday),
                        "saturday_lunch" = guests %>% filter(is_staying_saturday | 
                                                               saturday_offsite_rsvp == "Yes, I will join for lunch only" | 
                                                               saturday_offsite_rsvp == "Yes, I will join for lunch and dinner"),
                        "saturday_dinner" = guests %>% filter(is_staying_saturday | 
                                                                saturday_offsite_rsvp == "Yes, I will join for dinner only" | 
                                                                saturday_offsite_rsvp == "Yes, I will join for lunch and dinner"),
                        "sunday_breakfast" = guests %>% filter(is_staying_saturday),
                        "sunday_lunch" = guests %>% filter(wedding_rsvp == "Joyfully Accept"),
                        "sunday_dinner" = guests %>% filter(is_staying_sunday),
                        "monday_breakfast" = guests %>% filter(is_staying_sunday),
                        guests %>% filter(FALSE) # Empty default
    )
    
    return(attendees)
  }
  
  # Create meal attendee lists
  for (i in 1:length(meal_codes)) {
    meal_code <- meal_codes[i]
    meal_name <- meal_names[i]
    
    # Check if meal roster is already available
    if (!is.null(results$meal_rosters) && meal_code %in% names(results$meal_rosters)) {
      meal_attendees[[meal_code]] <- results$meal_rosters[[meal_code]]$attendees
    } else {
      # Generate attendee list from guest data
      meal_attendees[[meal_code]] <- get_meal_attendees(results$guests, meal_code)
    }
  }
  
  # Create CSV files for each meal with detailed attendee lists
  csv_base_name <- tools::file_path_sans_ext(output_file)
  dir.create(paste0(csv_base_name, "_meal_details"), showWarnings = FALSE, recursive = TRUE)
  
  for (i in 1:length(meal_codes)) {
    meal_code <- meal_codes[i]
    meal_name <- meal_names[i]
    
    attendees <- meal_attendees[[meal_code]]
    
    if (nrow(attendees) > 0) {
      # Create a detailed attendee list with dietary info
      attendee_list <- attendees %>%
        select(first_name, last_name, age_category, meal_preferences, dietary_restrictions) %>%
        rename(
          "First Name" = first_name,
          "Last Name" = last_name,
          "Age Category" = age_category,
          "Meal Preference" = meal_preferences,
          "Dietary Restrictions" = dietary_restrictions
        )
      
      # Save the attendee list
      write_csv(attendee_list, 
                paste0(csv_base_name, "_meal_details/", 
                       gsub(" ", "_", tolower(meal_code)), "_attendees.csv"))
      
      # Create a summary by age category
      age_summary <- attendees %>%
        group_by(age_category) %>%
        summarize(
          Total = n(),
          Vegetarian = sum(is_vegetarian, na.rm = TRUE),
          `Meat/Fish` = sum(!is_vegetarian, na.rm = TRUE),
          `Special Diet` = sum(has_special_diet, na.rm = TRUE)
        )
      
      # Save the age summary
      write_csv(age_summary, 
                paste0(csv_base_name, "_meal_details/", 
                       gsub(" ", "_", tolower(meal_code)), "_by_age.csv"))
    }
  }
  
  # Write out the main CSV files
  write_csv(meal_summary, paste0(csv_base_name, "_meal_summary.csv"))
  write_csv(meal_preferences, paste0(csv_base_name, "_meal_preferences.csv"))
  write_csv(dietary_restrictions, paste0(csv_base_name, "_dietary_restrictions.csv"))
  write_csv(special_diet_guests, paste0(csv_base_name, "_special_diet_guests.csv"))
  
  if (!is.null(meal_by_age_data) && nrow(meal_by_age_data) > 0) {
    write_csv(meal_by_age_data, paste0(csv_base_name, "_meal_by_age.csv"))
  }
  
  # Create a markdown report
  rmd_content <- '
---
title: "Wedding Meal Planning Report"
subtitle: "For Wedding: Cyrena & Jon - June 20-23, 2025"
date: "`r format(Sys.Date(), "%B %d, %Y")`"
output: pdf_document
---

## Meal Planning Summary

```{r, echo=FALSE}
knitr::kable(meal_summary, 
             caption = "Meal Counts by Day",
             align = "lccc")
```

## Meal Preferences

```{r, echo=FALSE}
knitr::kable(meal_preferences, 
             caption = "Guest Meal Preferences",
             align = "lc")
```

## Dietary Restrictions

```{r, echo=FALSE}
knitr::kable(dietary_restrictions, 
             caption = "Guest Dietary Restrictions",
             align = "lc")
```
'
  
  # Add meal by age section if available
  if (!is.null(meal_by_age_data) && nrow(meal_by_age_data) > 0) {
    rmd_content <- paste0(rmd_content, '

## Meals by Age Category

```{r, echo=FALSE}
# Display meals by age category
knitr::kable(meal_by_age_data, 
             caption = "Meal Breakdown by Age Category",
             align = "llcccc")
```
')
  }
  
  # Add special diet guests section
  rmd_content <- paste0(rmd_content, '

## Guests with Special Dietary Requirements

```{r, echo=FALSE}
knitr::kable(special_diet_guests, 
             caption = "Guests with Special Dietary Requirements",
             align = "lllll")
```

## Important Notes

1. **Meal Inclusion by Stay:**
   - Friday night guests: Friday dinner, Saturday breakfast
   - Saturday night guests: Saturday lunch, dinner, Sunday breakfast
   - Sunday night guests: Special catering for Sunday dinner, Monday breakfast
   - All wedding guests: Sunday lunch (wedding meal)

2. **Saturday Off-Site Guest Meals:**
   - Guests can choose to join for lunch only, dinner only, or both meals
   - The counts above reflect these preferences from RSVP responses

3. **Dietary Information:**
   - Non-meat options should be available at all meals
   - Please review the detailed dietary restrictions list and ensure appropriate options are available
   - Special individual dietary needs are listed in the "Guest Dietary Restrictions" table

4. **Catering Planning:**
   - Sunday lunch (wedding reception) is the largest meal requiring service for all guests
   - Sunday dinner and Monday breakfast are special catering for overnight guests only
   - Detailed per-meal attendee lists with dietary information are provided in separate CSV files
')
  
  # Write the RMD file
  rmd_file <- tempfile(fileext = ".Rmd")
  writeLines(rmd_content, rmd_file)
  
  # Render the PDF with error handling
  tryCatch({
    rmarkdown::render(
      input = rmd_file,
      output_file = output_file,
      params = list(
        meal_summary = meal_summary,
        meal_preferences = meal_preferences,
        dietary_restrictions = dietary_restrictions,
        special_diet_guests = special_diet_guests,
        meal_by_age_data = meal_by_age_data
      ),
      quiet = TRUE
    )
    cat("Meal planning report generated:", output_file, "\n")
  }, error = function(e) {
    cat("Error generating meal planning report PDF:", conditionMessage(e), "\n")
    cat("CSV reports have been generated instead.\n")
  })
  
  # Clean up
  unlink(rmd_file)
  
  # Create a text summary for quick reference
  summary_text <- paste0(
    "MEAL PLANNING QUICK REFERENCE\n",
    "Wedding: Cyrena & Jon - June 20-23, 2025\n\n",
    
    "TOTAL MEAL COUNTS:\n"
  )
  
  for (i in 1:nrow(meal_summary)) {
    summary_text <- paste0(summary_text,
                           "- ", meal_summary$Meal[i], ": ", meal_summary$Total[i])
    
    if ("Vegetarian" %in% names(meal_summary)) {
      summary_text <- paste0(summary_text,
                             " (", meal_summary$Vegetarian[i], " vegetarian)")
    }
    
    if ("Special_Diet" %in% names(meal_summary)) {
      summary_text <- paste0(summary_text,
                             " (", meal_summary$Special_Diet[i], " special diet)")
    }
    
    summary_text <- paste0(summary_text, "\n")
  }
  
  # Add dietary restrictions summary
  summary_text <- paste0(summary_text, 
                         "\nDIETARY RESTRICTIONS:\n")
  
  if (nrow(dietary_restrictions) > 0) {
    for (i in 1:nrow(dietary_restrictions)) {
      summary_text <- paste0(summary_text,
                             "- ", dietary_restrictions$restriction[i], ": ", 
                             dietary_restrictions$count[i], " guests\n")
    }
  } else {
    summary_text <- paste0(summary_text, "- No special dietary requirements recorded\n")
  }
  
  # Add meal preferences summary
  summary_text <- paste0(summary_text, 
                         "\nMEAL PREFERENCES:\n")
  
  if (nrow(meal_preferences) > 0) {
    for (i in 1:nrow(meal_preferences)) {
      summary_text <- paste0(summary_text,
                             "- ", meal_preferences$preference[i], ": ", 
                             meal_preferences$count[i], " guests\n")
    }
  } else {
    summary_text <- paste0(summary_text, "- No meal preferences recorded\n")
  }
  
  # Add notes about detailed files
  summary_text <- paste0(summary_text, 
                         "\nDETAILED REPORTS:\n",
                         "- Detailed meal attendee lists by age category are available in the '",
                         basename(csv_base_name), "_meal_details' directory\n",
                         "- Each meal has its own CSV file with complete dietary information\n")
  
  # Write the text summary
  writeLines(summary_text, paste0(csv_base_name, "_summary.txt"))
  
  # Return the meal data for potential further processing
  return(list(
    meal_summary = meal_summary,
    meal_preferences = meal_preferences,
    dietary_restrictions = dietary_restrictions,
    special_diet_guests = special_diet_guests,
    meal_by_age_data = meal_by_age_data,
    meal_attendees = meal_attendees
  ))
}