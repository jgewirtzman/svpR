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
  
  # Add age categories if available
  if ("age_category" %in% names(results$guests)) {
    # Age-based meal summary - only if age_category exists
    meal_summary_by_age <- data.frame()
    
    # If meal_counts_by_age exists in results
    if (!is.null(results$meal_counts_by_age) && !is.null(results$meal_counts_by_age$all_meals)) {
      meal_summary_by_age <- results$meal_counts_by_age$all_meals %>%
        rename(
          "Meal" = meal,
          "Age Category" = age_category,
          "Count" = count,
          "Vegetarian/Fish" = veg_count,
          "Meat/Fish" = meat_fish_count,
          "Special Diet" = special_diet_count
        )
    } else {
      # Create a basic age category summary if meal_counts_by_age doesn't exist
      age_categories <- unique(results$guests$age_category)
      
      for (age_cat in age_categories) {
        # Filter guests by age category
        age_guests <- results$guests %>% filter(age_category == age_cat)
        
        # Add a row for each meal and age category
        for (meal_name in meal_summary$Meal) {
          meal_summary_by_age <- rbind(meal_summary_by_age, data.frame(
            "Meal" = meal_name,
            "Age Category" = age_cat,
            "Count" = nrow(age_guests),
            "Vegetarian/Fish" = sum(age_guests$meal_preferences == "No meat" | 
                                      age_guests$meal_preferences == "Opt-in for fish only", na.rm = TRUE),
            "Meat/Fish" = sum(age_guests$meal_preferences == "Opt-in for chicken and fish" | 
                                age_guests$meal_preferences == "Opt-in for chicken only", na.rm = TRUE),
            "Special Diet" = sum(!is.na(age_guests$dietary_restrictions) & 
                                   age_guests$dietary_restrictions != "", na.rm = TRUE)
          ))
        }
      }
    }
    
    # Add age category to special diet guests
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
  }
  
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
  
  # Write age-based meal summary if it exists
  if (exists("meal_summary_by_age") && nrow(meal_summary_by_age) > 0) {
    write_csv(meal_summary_by_age, paste0(csv_base_name, "_meal_summary_by_age.csv"))
  }
  
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
  
  # Add age category breakdown if available
  if (exists("meal_summary_by_age") && nrow(meal_summary_by_age) > 0) {
    catering_reference <- paste0(
      catering_reference,
      "\nAGE CATEGORIES:\n"
    )
    
    # Get unique age categories and counts
    age_counts <- results$guests %>%
      group_by(age_cat = age_category) %>%
      summarize(count = n()) %>%
      arrange(desc(count))
    
    for (i in 1:nrow(age_counts)) {
      catering_reference <- paste0(
        catering_reference,
        "- ", age_counts$age_cat[i], ": ", age_counts$count[i], " guests\n"
      )
    }
  }
  
  # Write quick reference
  writeLines(catering_reference, paste0(csv_base_name, "_quickref.txt"))
  
  cat("Meal planning report files generated in directory:", dirname(output_file), "\n")
  
  # If RMarkdown is available, try to generate a PDF
  if (requireNamespace("rmarkdown", quietly = TRUE)) {
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
knitr::kable(params$meal_summary, 
             caption = "Meal Counts by Day",
             align = "lccc")
```

## Meal Preferences

```{r, echo=FALSE}
knitr::kable(params$meal_preferences, 
             caption = "Guest Meal Preferences",
             align = "lc")
```

## Dietary Restrictions

```{r, echo=FALSE}
knitr::kable(params$dietary_restrictions, 
             caption = "Guest Dietary Restrictions",
             align = "lc")
```

'
    
    # Add age category section if available
    if (exists("meal_summary_by_age") && nrow(meal_summary_by_age) > 0) {
      rmd_content <- paste0(rmd_content, '
## Meal Counts by Age Category

```{r, echo=FALSE}
knitr::kable(params$meal_summary_by_age, 
             caption = "Meal Counts by Age Category",
             align = "lccccc")
```

')
    }
    
    # Add important notes section
    rmd_content <- paste0(rmd_content, '
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
')
    
    # Write the RMD file
    rmd_file <- tempfile(fileext = ".Rmd")
    writeLines(rmd_content, rmd_file)
    
    # Render the PDF with error handling
    pdf_output_file <- paste0(tools::file_path_sans_ext(output_file), ".pdf")
    
    tryCatch({
      params_list <- list(
        meal_summary = meal_summary,
        meal_preferences = meal_preferences,
        dietary_restrictions = dietary_restrictions
      )
      
      # Add meal_summary_by_age if it exists
      if (exists("meal_summary_by_age") && nrow(meal_summary_by_age) > 0) {
        params_list$meal_summary_by_age <- meal_summary_by_age
      }
      
      rmarkdown::render(
        input = rmd_file,
        output_file = pdf_output_file,
        params = params_list,
        quiet = TRUE
      )
      cat("Meal planning PDF report generated:", pdf_output_file, "\n")
    }, error = function(e) {
      cat("Error generating meal planning PDF report:", conditionMessage(e), "\n")
      cat("CSV reports have been generated instead.\n")
    })
    
    # Clean up
    unlink(rmd_file)
  } else {
    cat("Note: RMarkdown package not available. Only CSV reports were generated.\n")
  }
  
  # Return the meal summaries
  return(list(
    meal_summary = meal_summary,
    meal_preferences = meal_preferences,
    dietary_restrictions = dietary_restrictions,
    special_diet_guests = special_diet_guests,
    meal_summary_by_age = if (exists("meal_summary_by_age")) meal_summary_by_age else NULL
  ))
}