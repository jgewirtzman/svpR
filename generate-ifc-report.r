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
  if ("friday_cost" %in% names(ifc_guests) && 
      "saturday_cost" %in% names(ifc_guests) && 
      "sunday_cost" %in% names(ifc_guests)) {
    
    night_summary$Total_IFC_Charge <- c(
      sum(ifc_guests$friday_cost[ifc_guests$is_staying_friday], na.rm = TRUE),
      sum(ifc_guests$saturday_cost[ifc_guests$is_staying_saturday], na.rm = TRUE),
      sum(ifc_guests$sunday_cost[ifc_guests$is_staying_sunday], na.rm = TRUE)
    )
    
    if ("friday_guest_charge" %in% names(ifc_guests) && 
        "saturday_guest_charge" %in% names(ifc_guests) && 
        "sunday_guest_charge" %in% names(ifc_guests)) {
      
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
  
  # Add meal attendance flags
  ifc_report$Friday_Dinner <- ifelse(ifc_guests$is_staying_friday, "Yes", "No")
  ifc_report$Saturday_Breakfast <- ifelse(ifc_guests$is_staying_friday, "Yes", "No")
  ifc_report$Saturday_Lunch <- ifelse(ifc_guests$is_staying_saturday, "Yes", "No")
  ifc_report$Saturday_Dinner <- ifelse(ifc_guests$is_staying_saturday, "Yes", "No")
  ifc_report$Sunday_Breakfast <- ifelse(ifc_guests$is_staying_saturday, "Yes", "No")
  ifc_report$Sunday_Lunch <- ifelse(ifc_guests$attending_wedding, "Yes", "No")
  ifc_report$Sunday_Dinner <- ifelse(ifc_guests$is_staying_sunday, "Yes", "No")
  ifc_report$Monday_Breakfast <- ifelse(ifc_guests$is_staying_sunday, "Yes", "No")
  
  # Add dietary preferences and restrictions
  if ("meal_preferences" %in% names(ifc_guests)) {
    ifc_report$Meal_Preference <- ifc_guests$meal_preferences
  }
  
  if ("dietary_restrictions" %in% names(ifc_guests)) {
    ifc_report$Dietary_Restrictions <- ifc_guests$dietary_restrictions
  }
  
  # Add vegetarian and special diet flags
  if ("is_vegetarian" %in% names(ifc_guests)) {
    ifc_report$Vegetarian <- ifelse(ifc_guests$is_vegetarian, "Yes", "No")
  }
  
  if ("has_special_diet" %in% names(ifc_guests)) {
    ifc_report$Special_Diet <- ifelse(ifc_guests$has_special_diet, "Yes", "No")
  }
  
  # Add financial information if available
  if ("friday_cost" %in% names(ifc_guests) && 
      "saturday_cost" %in% names(ifc_guests) && 
      "sunday_cost" %in% names(ifc_guests)) {
    
    ifc_report$Friday_Cost <- ifc_guests$friday_cost
    ifc_report$Saturday_Cost <- ifc_guests$saturday_cost
    ifc_report$Sunday_Cost <- ifc_guests$sunday_cost
    ifc_report$Total_IFC_Cost <- ifc_guests$total_cost
  }
  
  if ("friday_guest_charge" %in% names(ifc_guests) && 
      "saturday_guest_charge" %in% names(ifc_guests) && 
      "sunday_guest_charge" %in% names(ifc_guests)) {
    
    ifc_report$Friday_Guest_Charge <- ifc_guests$friday_guest_charge
    ifc_report$Saturday_Guest_Charge <- ifc_guests$saturday_guest_charge
    ifc_report$Sunday_Guest_Charge <- ifc_guests$sunday_guest_charge
    ifc_report$Total_Guest_Charge <- ifc_guests$total_guest_charge
  }
  
  if ("friday_host_charge" %in% names(ifc_guests) && 
      "saturday_host_charge" %in% names(ifc_guests) && 
      "sunday_host_charge" %in% names(ifc_guests)) {
    
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
      total_sunday_lunch = sum(ifc_guests$attending_wedding, na.rm = TRUE),
      
      # Total counts
      total_friday_dinner = sum(ifc_guests$is_staying_friday, na.rm = TRUE),
      total_saturday_breakfast = sum(ifc_guests$is_staying_friday, na.rm = TRUE),
      total_saturday_lunch = sum(ifc_guests$is_staying_saturday, na.rm = TRUE),
      total_saturday_dinner = sum(ifc_guests$is_staying_saturday, na.rm = TRUE),
      total_sunday_breakfast = sum(ifc_guests$is_staying_saturday, na.rm = TRUE),
      total_sunday_lunch = sum(ifc_guests$attending_wedding, na.rm = TRUE),
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
  if ("is_vegetarian" %in% names(ifc_guests)) {
    vegetarian_counts <- c(
      sum(ifc_guests$is_staying_friday & ifc_guests$is_vegetarian, na.rm = TRUE),
      sum(ifc_guests$is_staying_friday & ifc_guests$is_vegetarian, na.rm = TRUE),
      sum(ifc_guests$is_staying_saturday & ifc_guests$is_vegetarian, na.rm = TRUE),
      sum(ifc_guests$is_staying_saturday & ifc_guests$is_vegetarian, na.rm = TRUE),
      sum(ifc_guests$is_staying_saturday & ifc_guests$is_vegetarian, na.rm = TRUE),
      sum(ifc_guests$attending_wedding & ifc_guests$is_vegetarian, na.rm = TRUE),
      sum(ifc_guests$is_staying_sunday & ifc_guests$is_vegetarian, na.rm = TRUE),
      sum(ifc_guests$is_staying_sunday & ifc_guests$is_vegetarian, na.rm = TRUE)
    )
    meal_summary$Vegetarian <- vegetarian_counts
  }
  
  if ("has_special_diet" %in% names(ifc_guests)) {
    special_diet_counts <- c(
      sum(ifc_guests$is_staying_friday & ifc_guests$has_special_diet, na.rm = TRUE),
      sum(ifc_guests$is_staying_friday & ifc_guests$has_special_diet, na.rm = TRUE),
      sum(ifc_guests$is_staying_saturday & ifc_guests$has_special_diet, na.rm = TRUE),
      sum(ifc_guests$is_staying_saturday & ifc_guests$has_special_diet, na.rm = TRUE),
      sum(ifc_guests$is_staying_saturday & ifc_guests$has_special_diet, na.rm = TRUE),
      sum(ifc_guests$attending_wedding & ifc_guests$has_special_diet, na.rm = TRUE),
      sum(ifc_guests$is_staying_sunday & ifc_guests$has_special_diet, na.rm = TRUE),
      sum(ifc_guests$is_staying_sunday & ifc_guests$has_special_diet, na.rm = TRUE)
    )
    meal_summary$Special_Diet <- special_diet_counts
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
    if (all(c("total_cost", "total_guest_charge", "total_host_charge") %in% names(ifc_guests))) {
      accommodation_summary <- accommodation_summary %>%
        left_join(
          ifc_guests %>%
            group_by(Accommodation = ifelse(is_camping, "Camping", "Standard Lodging"), 
                     Age_Category = age_category) %>%
            summarize(
              Total_IFC_Cost = sum(total_cost, na.rm = TRUE),
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
    meal_plan_by_age <- results$meal_counts_by_age$all_meals
  }
  
  # Generate meal roster by age category
  meal_roster_by_age <- data.frame()
  
  if ("age_category" %in% names(ifc_guests)) {
    # List of all meals
    meals <- c("Friday_Dinner", "Saturday_Breakfast", "Saturday_Lunch", "Saturday_Dinner",
               "Sunday_Breakfast", "Sunday_Lunch", "Sunday_Dinner", "Monday_Breakfast")
    
    # For each meal, add attendees by age category
    for (meal in meals) {
      meal_col <- tolower(meal)
      
      # Get attendees for this meal
      attendees <- ifc_report %>%
        filter(!!sym(meal) == "Yes") %>%
        group_by(Age_Category) %>%
        summarize(
          Count = n(),
          .groups = 'drop'
        ) %>%
        mutate(Meal = meal)
      
      # Add to the overall roster
      meal_roster_by_age <- bind_rows(meal_roster_by_age, attendees)
    }
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
  
  # Write meal roster by age if available
  if (nrow(meal_roster_by_age) > 0) {
    write_csv(meal_roster_by_age, paste0(csv_base_name, "_meal_roster_by_age.csv"))
  }
  
  # Write detailed guest list
  write_csv(ifc_report, paste0(csv_base_name, "_guest_details.csv"))
  
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
  
  # Add meal roster by age section if available
  if (nrow(meal_roster_by_age) > 0) {
    summary_text <- paste0(summary_text,
                           "== Meal Roster by Age Category ==\n\n",
                           paste(capture.output(print(meal_roster_by_age %>%
                                                        pivot_wider(names_from = Meal, values_from = Count, values_fill = 0))),
                                 collapse = "\n"), "\n\n")
  }
  
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
  
  # Try to generate a PDF report if rmarkdown is available
  if (requireNamespace("rmarkdown", quietly = TRUE)) {
    # Create a PDF report with all the information
    rmd_content <- '
---
title: "Isabella Freedman Center - Guest Stay Report"
subtitle: "For Wedding: Cyrena & Jon - June 20-23, 2025"
date: "`r format(Sys.Date(), "%B %d, %Y")`"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(knitr)
library(dplyr)
library(tidyr)
```

## Summary of Guest Stays

```{r}
kable(night_summary, caption = "Guest Counts by Night")
```

## Accommodation by Age Category

```{r}
if (!is.null(accommodation_summary) && nrow(accommodation_summary) > 0) {
  kable(accommodation_summary %>% select(Accommodation, Age_Category, Count, Friday_Count, Saturday_Count, Sunday_Count),
       caption = "Accommodation by Age Category")
}
```

## Meal Planning Summary

```{r}
kable(meal_summary, caption = "Meal Counts")
```

## Financial Summary

```{r}
if (exists("financial_summary")) {
  kable(financial_summary, caption = "Financial Summary")
} else {
  financial_summary <- data.frame(
    Category = c("Total IFC Charge", "Guest Charge", "Host Charge"),
    Amount = c(
      paste0("$", format(sum(ifc_report$Total_IFC_Cost, na.rm = TRUE), big.mark = ",")),
      paste0("$", format(sum(ifc_report$Total_Guest_Charge, na.rm = TRUE), big.mark = ",")),
      paste0("$", format(sum(ifc_report$Total_Host_Charge, na.rm = TRUE), big.mark = ","))
    )
  )
  kable(financial_summary, caption = "Financial Summary")
}
```

## Important Notes

1. **Meal Inclusion by Stay:**
   - Friday night guests: Friday dinner, Saturday breakfast
   - Saturday night guests: Saturday lunch, dinner, Sunday breakfast
   - Sunday night guests: Special catering for Sunday dinner, Monday breakfast
   - All wedding guests: Sunday lunch (wedding meal)

2. All guests listed are part of the wedding party for Cyrena & Jon.

3. Please direct any questions to wedding@example.com or call (123) 456-7890.

4. Special accommodation requests have been noted in the detailed guest list.

5. Age categories are used for room assignments and meal planning.

## Guest List by Age Category

```{r}
ifc_report_summary <- ifc_report %>%
  group_by(Age_Category) %>%
  summarize(
    Total = n(),
    Friday = sum(Friday_Night == "Yes"),
    Saturday = sum(Saturday_Night == "Yes"),
    Sunday = sum(Sunday_Night == "Yes"),
    Camping = sum(Accommodation_Type == "Camping"),
    Standard = sum(Accommodation_Type == "Standard Lodging")
  )

kable(ifc_report_summary, caption = "Guest List Summary by Age Category")
```

## Meal Roster by Age Category

```{r}
if (exists("meal_roster_by_age") && nrow(meal_roster_by_age) > 0) {
  kable(meal_roster_by_age %>%
          pivot_wider(names_from = Meal, values_from = Count, values_fill = 0),
        caption = "Meal Roster by Age Category")
}
```
'

# Write the RMD file
rmd_file <- tempfile(fileext = ".Rmd")
writeLines(rmd_content, rmd_file)

# Render the PDF with error handling
tryCatch({
  pdf_output_file <- paste0(csv_base_name, ".pdf")
  
  # Create environment with required data
  render_env <- new.env()
  render_env$night_summary <- night_summary
  render_env$meal_summary <- meal_summary
  render_env$ifc_report <- ifc_report
  
  if (!is.null(accommodation_summary) && nrow(accommodation_summary) > 0) {
    render_env$accommodation_summary <- accommodation_summary
  }
  
  if (!is.null(meal_roster_by_age) && nrow(meal_roster_by_age) > 0) {
    render_env$meal_roster_by_age <- meal_roster_by_age
  }
  
  rmarkdown::render(
    input = rmd_file,
    output_file = pdf_output_file,
    envir = render_env,
    quiet = TRUE
  )
  cat("Generated IFC report PDF:", pdf_output_file, "\n")
}, error = function(e) {
  cat("Error generating IFC report PDF:", conditionMessage(e), "\n")
  cat("CSV reports have been generated instead.\n")
})

# Clean up
unlink(rmd_file)
  }

cat("IFC report files generated in directory:", dirname(output_file), "\n")

# Return the data used in the report
return(list(
  night_summary = night_summary,
  age_by_night = age_by_night,
  accommodation_summary = accommodation_summary,
  meal_summary = meal_summary,
  meal_plan_by_age = meal_plan_by_age,
  meal_roster_by_age = meal_roster_by_age,
  guest_details = ifc_report
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
  if ("has_special_diet" %in% names(ifc_guests)) {
    special_diet_row <- data.frame(
      Category = "Special Meal Requirements",
      Count = sum(ifc_guests$has_special_diet, na.rm = TRUE)
    )
    summary_counts <- rbind(summary_counts, special_diet_row)
  } else if ("dietary_restrictions" %in% names(ifc_guests)) {
    special_diet_row <- data.frame(
      Category = "Special Meal Requirements",
      Count = sum(!is.na(ifc_guests$dietary_restrictions) & ifc_guests$dietary_restrictions != "", na.rm = TRUE)
    )
    summary_counts <- rbind(summary_counts, special_diet_row)
  }
  
  # Add age categories if available
  if ("age_category" %in% names(ifc_guests)) {
    age_counts <- ifc_guests %>%
      group_by(Category = age_category) %>%
      summarize(Count = n()) %>%
      select(Category, Count)
    
    summary_counts <- rbind(summary_counts, age_counts)
  }
  
  # Financial summary
  financial_summary <- NULL
  if (all(c("total_cost", "total_guest_charge", "total_host_charge") %in% names(ifc_guests))) {
    
    financial_summary <- data.frame(
      Category = c(
        "Total IFC Charge",
        "Guest Charge",
        "Host Charge"
      ),
      Amount = c(
        paste0("$", format(sum(ifc_guests$total_cost, na.rm = TRUE), big.mark = ",")),
        paste0("$", format(sum(ifc_guests$total_guest_charge, na.rm = TRUE), big.mark = ",")),
        paste0("$", format(sum(ifc_guests$total_host_charge, na.rm = TRUE), big.mark = ","))
      )
    )
    
    # Add breakdown by day if available
    if (all(c("friday_cost", "saturday_cost", "sunday_cost") %in% names(ifc_guests))) {
      daily_costs <- data.frame(
        Category = c(
          "Friday IFC Total",
          "Saturday IFC Total",
          "Sunday IFC Total"
        ),
        Amount = c(
          paste0("$", format(sum(ifc_guests$friday_cost, na.rm = TRUE), big.mark = ",")),
          paste0("$", format(sum(ifc_guests$saturday_cost, na.rm = TRUE), big.mark = ",")),
          paste0("$", format(sum(ifc_guests$sunday_cost, na.rm = TRUE), big.mark = ","))
        )
      )
      
      financial_summary <- rbind(financial_summary, daily_costs)
    }
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
      total_sunday_lunch = sum(ifelse("attending_wedding" %in% names(ifc_guests), 
                                      ifc_guests$attending_wedding, nrow(ifc_guests)), na.rm = TRUE),
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
  if ("is_vegetarian" %in% names(ifc_guests)) {
    vegetarian_count <- sum(ifc_guests$is_vegetarian, na.rm = TRUE)
    meal_summary$Vegetarian <- rep(vegetarian_count, nrow(meal_summary))
  }
  
  if ("has_special_diet" %in% names(ifc_guests)) {
    special_diet_count <- sum(ifc_guests$has_special_diet, na.rm = TRUE)
    meal_summary$Special_Diet <- rep(special_diet_count, nrow(meal_summary))
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
  if (all(c("total_cost", "total_guest_charge", "total_host_charge") %in% names(ifc_guests))) {
    summary_text <- paste0(summary_text,
                           "FINANCIAL SUMMARY:\n",
                           "- Total IFC Charge: $", format(sum(ifc_guests$total_cost, na.rm = TRUE), big.mark = ","), "\n",
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
    
    if ("Special_Diet" %in% names(meal_summary)) {
      summary_text <- paste0(summary_text, " (", meal_summary$Special_Diet[i], " special diet)")
    }
    
    summary_text <- paste0(summary_text, "\n")
  }
  
  # Add notes
  summary_text <- paste0(summary_text, "\n",
                         "NOTES:\n",
                         "- Detailed guest lists are provided in separate files\n")
  
  if ("has_special_diet" %in% names(ifc_guests)) {
    special_diet_count <- sum(ifc_guests$has_special_diet, na.rm = TRUE)
    summary_text <- paste0(summary_text, 
                           "- Special meal requirements: ", special_diet_count, " guests\n")
  } else if ("dietary_restrictions" %in% names(ifc_guests)) {
    special_diet_count <- sum(!is.na(ifc_guests$dietary_restrictions) & 
                                ifc_guests$dietary_restrictions != "", na.rm = TRUE)
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
generate_meal_roster <- function(results, output_file = "meal_roster.csv") {
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
  
  # Make sure attendance variables exist
  if (!("attending_wedding" %in% names(guests))) {
    guests <- guests %>%
      mutate(
        attending_wedding = wedding_rsvp == "Joyfully Accept",
        attending_friday = fridayshabbat_rsvp == "Joyfully Accept",
        attending_saturday_lunch_only = saturday_offsite_rsvp == "Yes, I will join for lunch only",
        attending_saturday_dinner_only = saturday_offsite_rsvp == "Yes, I will join for dinner only",
        attending_saturday_both_meals = saturday_offsite_rsvp == "Yes, I will join for lunch and dinner"
      )
  }
  
  # Get all guests who may attend any meal
  meal_guests <- guests %>%
    mutate(
      # Add meal attendance flags
      friday_dinner = is_staying_friday | (attending_friday & !is_staying_friday),
      saturday_breakfast = is_staying_friday,
      saturday_lunch = is_staying_saturday | attending_saturday_lunch_only | attending_saturday_both_meals,
      saturday_dinner = is_staying_saturday | attending_saturday_dinner_only | attending_saturday_both_meals,
      sunday_breakfast = is_staying_saturday,
      sunday_lunch = attending_wedding,
      sunday_dinner = is_staying_sunday,
      monday_breakfast = is_staying_sunday
    ) %>%
    filter(friday_dinner | saturday_breakfast | saturday_lunch | saturday_dinner | 
             sunday_breakfast | sunday_lunch | sunday_dinner | monday_breakfast)
  
  # Create meal attendance flags dictionary
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
  
  # Create the base roster with all columns
  columns_to_select <- c(
    "last_name", 
    "first_name",
    "age_category"
  )
  
  # Add dietary information if available
  if ("meal_preferences" %in% names(meal_guests)) {
    columns_to_select <- c(columns_to_select, "meal_preferences")
  }
  
  if ("dietary_restrictions" %in% names(meal_guests)) {
    columns_to_select <- c(columns_to_select, "dietary_restrictions")
  }
  
  if ("is_vegetarian" %in% names(meal_guests)) {
    columns_to_select <- c(columns_to_select, "is_vegetarian")
  }
  
  if ("has_special_diet" %in% names(meal_guests)) {
    columns_to_select <- c(columns_to_select, "has_special_diet")
  }
  
  # Add meal flags
  columns_to_select <- c(columns_to_select, meal_flags)
  
  # Filter columns to only those that exist
  columns_to_select <- columns_to_select[columns_to_select %in% names(meal_guests)]
  
  # Create the complete roster
  meal_roster <- meal_guests %>%
    select(all_of(columns_to_select))
  
  # Write the full roster
  write_csv(meal_roster, output_file)
  
  # Iterate through each meal and create specific roster
  for (i in 1:length(meal_flags)) {
    meal_flag <- meal_flags[i]
    meal_name <- meal_names[i]
    
    # Create a cleanly formatted filename
    meal_file <- paste0(
      tools::file_path_sans_ext(output_file),
      "_",
      gsub(" ", "_", tolower(meal_name)),
      ".csv"
    )
    
    # Get attendees for this meal
    meal_attendees <- meal_roster %>%
      filter(!!sym(meal_flag)) %>%
      arrange(last_name, first_name)
    
    # Write meal-specific roster
    write_csv(meal_attendees, meal_file)
    
    # Create summary by age category if available
    if ("age_category" %in% names(meal_attendees)) {
      age_summary <- meal_attendees %>%
        group_by(age_category) %>%
        summarize(
          count = n(),
          vegetarian_count = if ("is_vegetarian" %in% names(meal_attendees)) 
            sum(is_vegetarian, na.rm = TRUE) else NA,
          special_diet_count = if ("has_special_diet" %in% names(meal_attendees)) 
            sum(has_special_diet, na.rm = TRUE) else NA
        )
      
      # Write age category summary
      age_summary_file <- paste0(
        tools::file_path_sans_ext(output_file),
        "_",
        gsub(" ", "_", tolower(meal_name)),
        "_by_age.csv"
      )
      
      write_csv(age_summary, age_summary_file)
    }
  }
  
  # Create summary text
  summary_text <- paste0(
    "MEAL ROSTER SUMMARY\n",
    "Wedding: Cyrena & Jon - June 20-23, 2025\n",
    "Generated: ", format(Sys.Date(), "%B %d, %Y"), "\n\n",
    
    "This directory contains meal rosters for each meal with detailed information about attendees.\n\n",
    
    "MEAL COUNTS:\n"
  )
  
  for (i in 1:length(meal_flags)) {
    meal_flag <- meal_flags[i]
    meal_name <- meal_names[i]
    
    count <- sum(meal_roster[[meal_flag]], na.rm = TRUE)
    
    veg_count <- if ("is_vegetarian" %in% names(meal_roster)) 
      sum(meal_roster[[meal_flag]] & meal_roster$is_vegetarian, na.rm = TRUE) else NA
    
    special_count <- if ("has_special_diet" %in% names(meal_roster)) 
      sum(meal_roster[[meal_flag]] & meal_roster$has_special_diet, na.rm = TRUE) else NA
    
    summary_text <- paste0(summary_text, 
                           "- ", meal_name, ": ", count, " attendees")
    
    if (!is.na(veg_count)) {
      summary_text <- paste0(summary_text, 
                             " (", veg_count, " vegetarian)")
    }
    
    if (!is.na(special_count)) {
      summary_text <- paste0(summary_text, 
                             " (", special_count, " special diet)")
    }
    
    summary_text <- paste0(summary_text, "\n")
  }
  
  summary_text <- paste0(summary_text, "\n",
                         "FILES IN THIS DIRECTORY:\n",
                         "- ", basename(output_file), ": Complete roster with all meals\n")
  
  for (i in 1:length(meal_flags)) {
    meal_flag <- meal_flags[i]
    meal_name <- meal_names[i]
    
    meal_file <- paste0(
      basename(tools::file_path_sans_ext(output_file)),
      "_",
      gsub(" ", "_", tolower(meal_name)),
      ".csv"
    )
    
    summary_text <- paste0(summary_text,
                           "- ", meal_file, ": Attendees for ", meal_name, "\n")
  }
  
  # Write summary
  summary_file <- paste0(tools::file_path_sans_ext(output_file), "_summary.txt")
  writeLines(summary_text, summary_file)
  
  cat("Meal roster generated:", output_file, "\n")
  
  # Return the roster
  return(list(
    meal_roster = meal_roster
  ))
}