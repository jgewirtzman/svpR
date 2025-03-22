# Isabella Freedman Center Report Generator
# This script creates a formatted report for the Isabella Freedman Center

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(knitr)
library(rmarkdown)

# Function to generate a report for the Isabella Freedman Center
generate_ifc_report <- function(results, output_file = "ifc_report.pdf") {
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
  
  # Create a markdown report - using single quotes to avoid interpolation issues
  rmd_content <- '
---
title: "Isabella Freedman Center - Guest Stay Report"
subtitle: "For Wedding: Cyrena & Jon - June 20-23, 2025"
date: "`r format(Sys.Date(), \'%B %d, %Y\')`"
output: pdf_document
---

## Summary of Guest Stays

```{r, echo=FALSE}
knitr::kable(night_summary, 
             caption = "Number of Guests per Night",
             align = "lccc")
```

```{r, echo=FALSE}
knitr::kable(accommodation_summary, 
             caption = "Accommodation Types",
             align = "lc")
```

## Meal Planning Summary

```{r, echo=FALSE}
knitr::kable(meal_summary, 
             caption = "Meal Counts by Day",
             align = "lccc")
```

## Detailed Guest List

```{r, echo=FALSE}
knitr::kable(ifc_report, 
             caption = "Guest Stay Details",
             align = "llccccll")
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
  '
  
  # Write the RMD file
  rmd_file <- tempfile(fileext = ".Rmd")
  writeLines(rmd_content, rmd_file)
  
  # Render the PDF with error handling
  tryCatch({
    rmarkdown::render(
      input = rmd_file,
      output_file = output_file,
      params = list(
        night_summary = night_summary,
        accommodation_summary = accommodation_summary,
        meal_summary = meal_summary,
        ifc_report = ifc_report
      ),
      quiet = TRUE
    )
    cat("IFC report generated:", output_file, "\n")
  }, error = function(e) {
    cat("Error generating IFC report:", conditionMessage(e), "\n")
  })
  
  # Clean up
  unlink(rmd_file)
  
  # Return the data used in the report
  return(list(
    night_summary = night_summary,
    accommodation_summary = accommodation_summary,
    meal_summary = meal_summary,
    guest_details = ifc_report
  ))
}

# Example usage (uncomment to use):
# source("wedding-rsvp-tracker.R")
# results <- generate_wedding_reports("guestlist.csv")
# ifc_report <- generate_ifc_report(results, "ifc_guest_report.pdf")