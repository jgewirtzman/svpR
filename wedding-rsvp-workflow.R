# Wedding RSVP Workflow
# This script demonstrates how to use all the wedding RSVP tracking components

# Load the required packages
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(knitr)
library(shiny)

# Function to generate a detailed meal planning report
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
  
  # Create dietary restrictions summary
  dietary_restrictions <- results$guests %>%
    filter(!is.na(dietary_restrictions) & dietary_restrictions != "") %>%
    group_by(dietary_restrictions) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  
  # Create meal preferences summary
  meal_preferences <- results$guests %>%
    filter(!is.na(meal_preferences)) %>%
    group_by(meal_preferences) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  
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
        meal_summary = meal_summary,
        meal_preferences = meal_preferences,
        dietary_restrictions = dietary_restrictions
      ),
      quiet = TRUE
    )
    cat("Meal planning report generated:", output_file, "\n")
  }, error = function(e) {
    cat("Error generating meal planning report:", conditionMessage(e), "\n")
  })
  
  # Clean up
  unlink(rmd_file)
}

# Print working directory for debugging
cat("Current working directory:", getwd(), "\n")

# File path to your guest list CSV
# Replace this with the path to your actual file
guest_list_file <- "guestlist.csv"

# SIMPLIFIED DIRECTORY HANDLING - Clean start
# First remove any existing directories to avoid confusion
unlink("wedding_reports", recursive = TRUE)
unlink("wedding_costs", recursive = TRUE)
unlink("wedding_emails", recursive = TRUE)

# Then create fresh directories
dir.create("wedding_reports", recursive = TRUE)
dir.create("wedding_costs", recursive = TRUE)
dir.create("wedding_emails", recursive = TRUE)

cat("Created fresh directories for wedding reports\n")

# Now source the main RSVP tracking scripts
source("wedding-rsvp-tracker.R")
source("cost-summary-generator.R")
source("generate-ifc-report.R")

# Step 1: Generate basic reports
cat("\n*** Step 1: Generating basic RSVP reports... ***\n")
results <- generate_wedding_reports(guest_list_file)

# Step 2: Export reports to CSV files
cat("\n*** Step 2: Exporting reports to CSV... ***\n")
export_reports(results, "wedding_reports")

# Step 3: Generate cost summaries and PDFs
cat("\n*** Step 3: Generating cost summaries and PDFs... ***\n")
cost_results <- run_cost_summary(guest_list_file)

# Step 4: Generate IFC report
cat("\n*** Step 4: Generating Isabella Freedman Center report... ***\n")
ifc_report <- generate_ifc_report(results, "wedding_reports/ifc_guest_report.pdf")

# Step 5: Generate meal planning summary report
cat("\n*** Step 5: Generating detailed meal planning report... ***\n")
generate_meal_planning_report(results, "wedding_reports/meal_planning_report.pdf")

# Step 6: Launch the dashboard (comment out if not needed)
cat("\n*** Step 6: Launching the dashboard... ***\n")
# This will launch the dashboard. Comment out if you don't want to run it immediately
 shiny::runApp("wedding-dashboard.R")

cat("\n*** Wedding RSVP analysis complete! ***\n")
cat("\nReports can be found in:\n")
cat("- Main reports: wedding_reports\n")
cat("- Cost PDFs: wedding_costs\n")
cat("- Email templates: wedding_emails\n")