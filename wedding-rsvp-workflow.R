# Wedding RSVP Workflow - Fixed Version
# This script demonstrates how to use all the wedding RSVP tracking components

# Load the required packages
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(knitr)
library(shiny)

# Check if we have the necessary packages for PDF generation
check_pdf_dependencies <- function() {
  pdf_packages <- c("rmarkdown", "knitr")
  missing_packages <- pdf_packages[!sapply(pdf_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    cat("WARNING: The following packages required for PDF generation are missing:", 
        paste(missing_packages, collapse = ", "), "\n")
    cat("PDF reports will be skipped. To enable PDF generation, install these packages with:\n")
    cat("install.packages(c('", paste(missing_packages, collapse = "', '"), "'))\n")
    return(FALSE)
  }
  
  # Check if we can generate PDFs (need LaTeX/TinyTeX)
  if (!rmarkdown::pandoc_available()) {
    cat("WARNING: Pandoc is not available. PDF generation will be skipped.\n")
    cat("Make sure Pandoc is installed and on your PATH.\n")
    return(FALSE)
  }
  
  # Attempt to detect LaTeX installation
  if (requireNamespace("tinytex", quietly = TRUE)) {
    if (!tinytex::is_tinytex()) {
      cat("WARNING: TinyTeX not detected. PDF generation may fail.\n")
      cat("Consider installing TinyTeX with: tinytex::install_tinytex()\n")
      # We'll still try to generate PDFs even without TinyTeX
    }
  } else {
    cat("WARNING: The tinytex package is not installed. PDF detection not possible.\n")
    cat("Consider installing with: install.packages('tinytex')\n")
  }
  
  return(TRUE)
}

# Function to generate a detailed meal planning report with error handling
generate_meal_planning_report <- function(results, output_file = "meal_planning_report.pdf") {
  # Check if PDF generation is possible
  can_generate_pdfs <- check_pdf_dependencies()
  
  # Force directory creation with absolute path
  output_dir <- dirname(output_file)
  if (output_dir != "." && output_dir != "") {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    cat("Ensuring output directory exists:", normalizePath(output_dir), "\n")
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
  
  # Always write CSV versions of the reports (more reliable than PDF)
  csv_base_name <- tools::file_path_sans_ext(output_file)
  write_csv(meal_summary, paste0(csv_base_name, "_meal_summary.csv"))
  write_csv(dietary_restrictions, paste0(csv_base_name, "_dietary_restrictions.csv"))
  write_csv(meal_preferences, paste0(csv_base_name, "_meal_preferences.csv"))
  
  # Create a text summary for easy reference
  summary_text <- paste0(
    "MEAL PLANNING SUMMARY\n",
    "Wedding: Cyrena & Jon - June 20-23, 2025\n",
    "Generated: ", format(Sys.Date(), "%B %d, %Y"), "\n\n",
    "== Meal Planning Summary ==\n\n"
  )
  
  # Add meal summary to text
  for (i in 1:nrow(meal_summary)) {
    summary_text <- paste0(
      summary_text,
      meal_summary$Meal[i], ":\n",
      "  On-Site Guests: ", meal_summary$On_Site_Guests[i], "\n",
      "  Off-Site Guests: ", meal_summary$Off_Site_Guests[i], "\n",
      "  Total: ", meal_summary$Total[i], "\n\n"
    )
  }
  
  # Add dietary restrictions
  summary_text <- paste0(summary_text, "== Dietary Restrictions ==\n\n")
  if (nrow(dietary_restrictions) > 0) {
    for (i in 1:nrow(dietary_restrictions)) {
      summary_text <- paste0(
        summary_text,
        dietary_restrictions$dietary_restrictions[i], ": ", 
        dietary_restrictions$count[i], " guests\n"
      )
    }
  } else {
    summary_text <- paste0(summary_text, "No dietary restrictions recorded.\n")
  }
  
  # Add meal preferences
  summary_text <- paste0(summary_text, "\n== Meal Preferences ==\n\n")
  if (nrow(meal_preferences) > 0) {
    for (i in 1:nrow(meal_preferences)) {
      summary_text <- paste0(
        summary_text,
        meal_preferences$meal_preferences[i], ": ", 
        meal_preferences$count[i], " guests\n"
      )
    }
  } else {
    summary_text <- paste0(summary_text, "No meal preferences recorded.\n")
  }
  
  # Add notes
  summary_text <- paste0(summary_text, "\n== Important Notes ==\n\n",
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
                         "   - Please review the detailed dietary restrictions list and ensure appropriate options are available\n\n",
                         
                         "4. Catering Planning:\n",
                         "   - Sunday lunch (wedding reception) is the largest meal requiring service for all guests\n",
                         "   - Sunday dinner and Monday breakfast are special catering for overnight guests only\n"
  )
  
  # Write the text summary
  writeLines(summary_text, paste0(csv_base_name, "_summary.txt"))
  cat("Meal planning text summary generated:", paste0(csv_base_name, "_summary.txt"), "\n")
  
  # Only attempt PDF if we can generate it
  if (can_generate_pdfs) {
    # Create a markdown report
    rmd_content <- '---
title: "Wedding Meal Planning Report"
subtitle: "For Wedding: Cyrena & Jon - June 20-23, 2025"
date: "`r format(Sys.Date(), \'%B %d, %Y\')`"
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
    
    # Define environment with parameters
    env <- new.env()
    env$meal_summary <- meal_summary
    env$meal_preferences <- meal_preferences
    env$dietary_restrictions <- dietary_restrictions
    
    # Render the PDF with error handling
    tryCatch({
      rmarkdown::render(
        input = rmd_file,
        output_file = output_file,
        envir = env,
        quiet = TRUE
      )
      cat("Meal planning PDF report generated:", output_file, "\n")
    }, error = function(e) {
      cat("Error generating meal planning PDF report:", conditionMessage(e), "\n")
      cat("Using text and CSV reports instead.\n")
    })
    
    # Clean up
    unlink(rmd_file)
  } else {
    cat("Skipping PDF generation due to missing dependencies.\n")
    cat("Text and CSV reports have been generated instead.\n")
  }
  
  # Return the data for potential further use
  return(list(
    meal_summary = meal_summary,
    meal_preferences = meal_preferences,
    dietary_restrictions = dietary_restrictions
  ))
}

# Function to clean guest data
preprocess_guest_data <- function(guests) {
  # Find rows with missing names
  missing_names <- is.na(guests$first_name) | guests$first_name == ""
  
  if (any(missing_names)) {
    cat("Found", sum(missing_names), "entries with missing first names.\n")
    
    # Create a "cleaning log" of issues
    missing_name_guests <- guests[missing_names, ]
    write_csv(missing_name_guests, "missing_name_guests.csv")
    cat("Exported list of guests with missing names to missing_name_guests.csv\n")
    
    # For simple placeholder fix
    guests$first_name[missing_names] <- ifelse(
      !is.na(guests$last_name[missing_names]) & guests$last_name[missing_names] != "",
      paste0("Guest of ", guests$last_name[missing_names]),
      paste0("Guest #", which(missing_names))
    )
    
    cat("Applied placeholder names for missing entries.\n")
  }
  
  return(guests)
}

# In wedding-rsvp-workflow.R
run_fixed_workflow <- function() {
  # Print working directory for debugging
  cat("Current working directory:", normalizePath(getwd()), "\n")
  
  # Make sure required files exist
  required_files <- c("rates.R", "helpers.R", "wedding-rsvp-tracker.R")
  for (file in required_files) {
    if (!file.exists(file)) {
      stop("Required file not found: ", file)
    }
  }
  
  # Source required functions
  source("rates.R")
  source("helpers.R")
  source("wedding-rsvp-tracker.R")
  source("cost-summary-generator.R")
  
  # File path to your guest list CSV
  guest_list_file <- "guestlist.csv"
  
  # Ensure the file exists
  if (!file.exists(guest_list_file)) {
    stop("Guest list file not found: ", guest_list_file)
  }
  
  # Setup directories
  reports_dir <- file.path(getwd(), "wedding_reports")
  costs_dir <- file.path(getwd(), "wedding_costs")
  emails_dir <- file.path(getwd(), "wedding_emails")
  
  # Create directories
  for (dir in c(reports_dir, costs_dir, emails_dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  cat("Created directories for wedding reports\n")
  
  # Step 1: Read and preprocess guest data
  cat("\n*** Step 1: Reading and preprocessing guest data... ***\n")
  guest_data <- read_csv(guest_list_file,
                         col_types = cols(.default = col_character()),
                         na = c("", "NA", "N/A"))
  
  # Load rates once
  rates_data <- load_rates("charge_rates.csv")
  
  # Step 2: Generate reports with processed data
  cat("\n*** Step 2: Generating RSVP reports... ***\n")
  results <- generate_wedding_reports(guest_list_file)
  
  # Step 3: Export reports using the pre-calculated data
  cat("\n*** Step 3: Exporting reports to CSV... ***\n")
  export_reports(results, reports_dir)
  
  # Step 4: Generate cost summaries and PDFs using pre-calculated data
  cat("\n*** Step 4: Generating cost summaries and PDFs... ***\n")
  generate_cost_pdfs(results, costs_dir)
  
  # Step 5: Generate simplified emails with pre-calculated data
  cat("\n*** Step 5: Generating email templates... ***\n")
  generate_simplified_emails(results, emails_dir)
  
  cat("\n*** Wedding RSVP analysis complete! ***\n")
  cat("\nReports can be found in:\n")
  cat("- Main reports:", reports_dir, "\n")
  cat("- Cost PDFs:", costs_dir, "\n")
  cat("- Email templates:", emails_dir, "\n")
  
  return(results)
}

# Run the fixed workflow
run_fixed_workflow()

runApp("app.R")


#produce summary
# Load libraries
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# ── Load data ─────────────────────────────
roster <- read_csv("wedding_reports/ifc_roster.csv")
roster <- roster %>%
  mutate(
    age_category = case_when(
      str_detect(age_category, "21\\+") ~ "Adult 21+",
      str_detect(age_category, "12-21") ~ "Teen 12–21",
      str_detect(age_category, "5-12") ~ "Child 5–12",
      str_detect(age_category, "<5") ~ "Infant <5",
      TRUE ~ age_category
    )
  )

parties <- read_csv("wedding_reports/party_summary.csv")
rates <- read_csv("charge_rates.csv")
guest_emails <- read_csv("wedding_reports/guest_accommodation_costs.csv") %>%
  mutate(name = paste(first_name, last_name)) %>%
  select(name, email = email5)

# ── Clean up rates table ─────────────────
rates_clean <- rates %>%
  fill(Night) %>%
  select(Night, Category,
         AccommodationRate = `Listed Standard Guest Rate`,
         MealRate = `Listed Standard Guest Meal Rate`)

# Remove unnecessary labels
rates_clean$Night <- str_replace(rates_clean$Night, " \\(no meals option\\)", "")

# Build accommodation rate lookup by night and age category
accommodation_rates <- rates_clean %>%
  filter(str_detect(Category, "Room|Camping")) %>%
  mutate(age_category = case_when(
    str_detect(Category, "21\\+") ~ "Adult 21+",
    str_detect(Category, "12-21") ~ "Teen 12–21",
    str_detect(Category, "5-12") ~ "Child 5–12",
    str_detect(Category, "<5") ~ "Infant <5",
    TRUE ~ "Unknown"
  )) %>%
  select(Night, age_category, AccommodationRate)


# ── Helper functions to get rates ────────
get_accommodation_rate <- function(night) {
  vals <- rates_clean %>%
    filter(Night == night, str_detect(Category, "Room|Camping")) %>%
    pull(AccommodationRate)
  if (length(vals) == 0) return(0)
  max(vals, na.rm = TRUE)
}

get_rate_for_age <- function(night, age_category) {
  rate <- accommodation_rates %>%
    filter(Night == night, age_category == !!age_category) %>%
    pull(AccommodationRate)
  if (length(rate) == 0) return(0)
  return(rate[1])
}


get_meal_rate <- function(night) {
  vals <- rates_clean %>%
    filter(Night == night, !is.na(MealRate)) %>%
    pull(MealRate)
  if (length(vals) == 0) return(0)
  max(vals, na.rm = TRUE)
}

# ── Calculate guest-level costs ───────────
roster_costs <- roster %>%
  mutate(name = paste(first_name, last_name)) %>%
  rowwise() %>%
  mutate(
    `Friday Accommodation Cost` = if (is_staying_friday) get_rate_for_age("Friday", age_category) else 0,
    `Saturday Accommodation Cost` = if (is_staying_saturday) get_rate_for_age("Saturday", age_category) else 0,
    `Sunday Accommodation Cost` = if (is_staying_sunday) get_rate_for_age("Sunday", age_category) else 0,
    
    `Friday Meal Cost` = if (is_staying_friday) get_meal_rate("Friday") else 0,
    `Saturday Meal Cost` = if (is_staying_saturday) get_meal_rate("Saturday") else 0,
    `Sunday Meal Cost` = if (is_staying_sunday) get_meal_rate("Sunday") else 0
  ) %>%
  ungroup()




# ── Join party info & email ───────────────
roster_joined <- roster_costs %>%
  left_join(guest_emails, by = "name") %>%
  left_join(parties %>% select(party, party_name, party_email), by = "party") %>%
  select(
    name, email, party_name, party_email,
    starts_with("Friday"), starts_with("Saturday"), starts_with("Sunday")
  )

# ── Filter to only those who are actually attending anything ──────────────
final_output <- roster_joined %>%
  filter(`Friday Accommodation Cost` > 0 | `Friday Meal Cost` > 0 |
           `Saturday Accommodation Cost` > 0 | `Saturday Meal Cost` > 0 |
           `Sunday Accommodation Cost` > 0 | `Sunday Meal Cost` > 0)

# ── Optional: write to CSV ────────────────
 write_csv(final_output, "individual_guest_costs.csv")

# View in RStudio
#View(final_output)
 
 ##make invoices
 
 
 # Load required libraries
 library(dplyr)
 library(readr)
 library(rmarkdown)
 
 # Load guest cost data
 guests <- read_csv("individual_guest_costs.csv")
 
 # Prepare party-level summaries
 party_groups <- guests %>%
   group_by(party_name, party_email) %>%
   summarise(
     guests = list(name),
     arrival_days = list(
       unique(c(
         if (any(`Friday Accommodation Cost` > 0 | `Friday Meal Cost` > 0)) "Friday",
         if (any(`Saturday Accommodation Cost` > 0 | `Saturday Meal Cost` > 0)) "Saturday"
       ))
     ),
     guests_df = list(cur_data_all()),
     total_due = sum(`Friday Meal Cost` + `Friday Accommodation Cost` +
                       `Saturday Meal Cost` + `Saturday Accommodation Cost` +
                       `Sunday Meal Cost` + `Sunday Accommodation Cost`),
     .groups = "drop"
   )
 
 # Loop and render PDF for each party
 dir.create("invoices", showWarnings = FALSE)
 
 for (i in 1:nrow(party_groups)) {
   this <- party_groups[i, ]
   
   rmarkdown::render(
     input = "invoice_template.Rmd",
     output_file = paste0("invoices/", make.names(this$party_name), "_invoice.pdf"),
     params = list(
       party_name = this$party_name,
       guests = this$guests[[1]],
       arrival_days = this$arrival_days[[1]],
       guests_df = this$guests_df[[1]],
       total_due = this$total_due
     ),
     envir = new.env()
   )
 }
 

