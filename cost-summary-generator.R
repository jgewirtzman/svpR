# Wedding Cost Summary Generator
# This script generates a detailed cost breakdown for each guest/party

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(knitr)
library(rmarkdown)

# Function to generate a cost summary for each party
generate_cost_summary <- function(file_path) {
  # Source the main RSVP tracking functions
  # source("wedding-rsvp-tracker.R")
  
  # Run the main report generation
  results <- generate_wedding_reports(file_path)
  
  # Get party list and guest costs
  party_summary <- results$party_summary
  guest_costs <- results$guest_costs
  
  # Define cost constants for reference
  costs <- list(
    saturday_housing = 136,
    saturday_meals = 36,
    saturday_camping = 54,
    friday_housing = 72,
    friday_meals = 36,
    friday_camping = 36,
    sunday_meals = 18
  )
  
  # Create a cost breakdown by party
  party_costs <- guest_costs %>%
    group_by(party) %>%
    summarize(
      total_guests = n(),
      # Per-night stay counts
      staying_friday = sum(is_staying_friday, na.rm = TRUE),
      staying_saturday = sum(is_staying_saturday, na.rm = TRUE),
      staying_sunday = sum(is_staying_sunday, na.rm = TRUE),
      
      # Camping vs. lodging counts for each night
      friday_camping = sum(is_staying_friday & is_camping, na.rm = TRUE),
      friday_lodging = sum(is_staying_friday & !is_camping, na.rm = TRUE),
      saturday_camping = sum(is_staying_saturday & is_camping, na.rm = TRUE),
      saturday_lodging = sum(is_staying_saturday & !is_camping, na.rm = TRUE),
      
      # Total camping and lodging counts
      camping = sum(is_camping, na.rm = TRUE),
      standard_lodging = sum((is_staying_friday | is_staying_saturday) & !is_camping, na.rm = TRUE),
      
      # Cost breakdowns by category
      total_friday_lodging_cost = sum(friday_lodging_cost, na.rm = TRUE),
      total_friday_meals_cost = sum(friday_meals_cost, na.rm = TRUE),
      total_friday_cost = sum(friday_cost, na.rm = TRUE),
      
      total_saturday_lodging_cost = sum(saturday_lodging_cost, na.rm = TRUE),
      total_saturday_meals_cost = sum(saturday_meals_cost, na.rm = TRUE),
      total_saturday_cost = sum(saturday_cost, na.rm = TRUE),
      
      total_sunday_cost = sum(sunday_cost, na.rm = TRUE),
      
      total_lodging_cost = sum(total_lodging_cost, na.rm = TRUE),
      total_meals_cost = sum(total_meals_cost, na.rm = TRUE),
      grand_total = sum(total_cost, na.rm = TRUE)
    ) %>%
    # Join with party_summary to get email and party name
    left_join(party_summary %>% select(party, party_name, party_email, guest_names), by = "party")
  
  # Generate a detailed breakdown for each party
  party_details <- lapply(unique(party_costs$party), function(p) {
    # Get party info
    party_info <- party_costs %>% filter(party == p)
    
    # Get all guests in this party
    party_guests <- guest_costs %>% 
      filter(party == p) %>%
      arrange(desc(total_cost))
    
    # Create a text summary
    summary_text <- paste0(
      "Party: ", party_info$party_name, "\n",
      "Guests: ", party_info$guest_names, "\n",
      "Email: ", party_info$party_email, "\n",
      "Total Guests: ", party_info$total_guests, "\n",
      "Total Cost: $", party_info$grand_total, "\n\n",
      "Accommodation Summary:\n",
      "- Friday night: ", party_info$staying_friday, " guests\n",
      "- Saturday night: ", party_info$staying_saturday, " guests\n",
      "- Sunday night: ", party_info$staying_sunday, " guests\n",
      "- Camping: ", party_info$camping, " guests\n",
      "- Standard lodging: ", party_info$standard_lodging, " guests\n\n",
      "Cost Breakdown:\n",
      "- Friday: $", party_info$total_friday_cost, "\n",
      "  - Lodging: $", party_info$total_friday_lodging_cost, "\n",
      "  - Meals: $", party_info$total_friday_meals_cost, "\n",
      "- Saturday: $", party_info$total_saturday_cost, "\n", 
      "  - Lodging: $", party_info$total_saturday_lodging_cost, "\n",
      "  - Meals: $", party_info$total_saturday_meals_cost, "\n",
      "- Sunday: $", party_info$total_sunday_cost, "\n\n",
      "Individual Guest Details:\n"
    )
    
    # Add details for each guest
    guest_details <- paste(sapply(1:nrow(party_guests), function(i) {
      guest <- party_guests[i, ]
      
      # Construct the staying string
      staying_days <- c()
      if(guest$is_staying_friday) staying_days <- c(staying_days, "Friday")
      if(guest$is_staying_saturday) staying_days <- c(staying_days, "Saturday")
      if(guest$is_staying_sunday) staying_days <- c(staying_days, "Sunday")
      
      staying_str <- if(length(staying_days) > 0) paste(staying_days, collapse = ", ") else "Not staying overnight"
      
      # Construct accommodation type
      accommodation_type <- if(guest$is_staying_friday || guest$is_staying_saturday) {
        if(guest$is_camping) "Camping" else "Standard Lodging"
      } else {
        "Not staying overnight"
      }
      
      # Construct meal information
      meal_info <- ""
      if(guest$is_staying_friday) {
        meal_info <- paste0(meal_info, "   - Friday: Dinner, Saturday: Breakfast\n")
      }
      if(guest$is_staying_saturday) {
        meal_info <- paste0(meal_info, "   - Saturday: Lunch, Dinner, Sunday: Breakfast\n")
      }
      if(guest$is_staying_sunday) {
        meal_info <- paste0(meal_info, "   - Sunday: Dinner, Monday: Breakfast (special catering)\n")
      }
      if(meal_info == "") {
        meal_info <- "   - No overnight meals included\n"
      }
      
      paste0(
        i, ". ", guest$first_name, " ", guest$last_name, "\n",
        "   - Staying: ", staying_str, "\n",
        "   - Accommodation Type: ", accommodation_type, "\n",
        "   - Included Meals:\n", meal_info,
        "   - Friday Cost: $", guest$friday_cost, " (Lodging: $", guest$friday_lodging_cost, 
        ", Meals: $", guest$friday_meals_cost, ")\n",
        "   - Saturday Cost: $", guest$saturday_cost, " (Lodging: $", guest$saturday_lodging_cost, 
        ", Meals: $", guest$saturday_meals_cost, ")\n",
        "   - Sunday Cost: $", guest$sunday_cost, "\n",
        "   - Total Cost: $", guest$total_cost, " (Lodging: $", guest$total_lodging_cost, 
        ", Meals: $", guest$total_meals_cost, ")\n"
      )
    }), collapse = "\n")
    
    # Combine all text
    full_summary <- paste0(summary_text, guest_details)
    
    return(list(
      party = p,
      party_name = party_info$party_name,
      guest_names = party_info$guest_names,
      email = party_info$party_email,
      summary = full_summary
    ))
  })
  
  return(list(
    party_costs = party_costs,
    party_details = party_details
  ))
}

# Function to generate a PDF invoice/summary for each party
generate_cost_pdfs <- function(results, output_dir = "wedding_costs") {
  # SIMPLIFIED: Force directory creation
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  cat("Ensuring output directory exists:", output_dir, "\n")
  
  # Create a template for the PDF
  template <- "
---
title: \"Wedding Accommodation Summary\"
subtitle: \"For: %PARTY_NAME%\"
date: \"Generated: `r format(Sys.Date(), '%B %d, %Y')`\"
output: pdf_document
---

## Cost Summary

**Party Name:** %PARTY_NAME%  
**Guests:** %GUEST_NAMES%  
**Contact Email:** %EMAIL%

### Accommodation Details

```{r echo=FALSE, results='asis'}
cat(party_details)
```

### Important Notes

1. **Meals included with overnight stays:**
   - Friday stays include: Friday dinner and Saturday breakfast
   - Saturday stays include: Saturday lunch, Saturday dinner, and Sunday breakfast
   - Sunday stays include: Special catering for Sunday dinner and Monday breakfast

2. **Wedding reception meal (Sunday lunch) is included for all wedding guests.**

### Payment Information

Please make your payment by May 15, 2025 using one of the following methods:

1. **Venmo**: @wedding-account
2. **Zelle**: payments@wedding-email.com
3. **Check**: Mail to Wedding Couple, 123 Wedding Lane, Wedding City, WS 12345

### Questions?

If you have any questions about your accommodations or costs, please contact us at:
- Email: help@wedding-email.com
- Phone: (555) 123-4567

Thank you for joining us for our wedding celebration!
"
  
  # Generate a PDF for each party
  for (party_detail in results$party_details) {
    # Get party details
    party_id <- party_detail$party
    party_name <- party_detail$party_name
    party_email <- party_detail$email
    party_details <- party_detail$summary
    guest_names <- party_detail$guest_names
    
    # Create a unique filename
    filename <- file.path(output_dir, paste0(
      "cost_summary_", 
      gsub("[^a-zA-Z0-9]", "_", party_name), 
      ".pdf"
    ))
    
    # Create a temporary Rmd file
    temp_rmd <- tempfile(fileext = ".Rmd")
    temp_content <- template
    temp_content <- gsub("%PARTY_NAME%", party_name, temp_content)
    temp_content <- gsub("%GUEST_NAMES%", guest_names, temp_content)
    temp_content <- gsub("%EMAIL%", party_email, temp_content)
    
    write(temp_content, temp_rmd)
    
    # Render the PDF (this creates a PDF file)
    tryCatch({
      rmarkdown::render(
        input = temp_rmd,
        output_file = filename,
        params = list(party_details = party_details),
        quiet = TRUE
      )
    }, error = function(e) {
      cat("Error generating PDF for party", party_name, ":", conditionMessage(e), "\n")
    })
    
    # Remove the temporary file
    unlink(temp_rmd)
  }
  
  cat("Generated", length(results$party_details), "cost summary PDFs in", output_dir, "\n")
}

# Function to generate a complete email for each party
generate_cost_emails <- function(results, output_dir = "wedding_emails") {
  # SIMPLIFIED: Force directory creation
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  cat("Ensuring email directory exists:", output_dir, "\n")
  
  # Email template
  email_template <- "
To: %EMAIL%
Subject: Your Wedding Accommodation Details and Costs

Dear %PARTY_NAME%,

Thank you for your RSVP to our wedding! We're excited to have you join us for this special occasion.

Here's a summary of your accommodation details and costs:

%DETAILS%

IMPORTANT NOTES:
- Meals included with overnight stays:
  - Friday stays include: Friday dinner and Saturday breakfast
  - Saturday stays include: Saturday lunch, Saturday dinner, and Sunday breakfast
  - Sunday stays include: Special catering for Sunday dinner and Monday breakfast
- Wedding reception meal (Sunday lunch) is included for all wedding guests.

Payment Options:
1. Venmo: @wedding-account
2. Zelle: payments@wedding-email.com
3. Check: Mail to Wedding Couple, 123 Wedding Lane, Wedding City, WS 12345

Please make your payment by May 15, 2025.

If you have any questions, please don't hesitate to reach out to us at help@wedding-email.com or call (555) 123-4567.

Looking forward to celebrating with you!

Best wishes,
The Wedding Couple
"
  
  # Generate an email for each party
  for (party_detail in results$party_details) {
    # Get party details
    party_id <- party_detail$party
    party_name <- party_detail$party_name
    party_email <- party_detail$email
    party_details <- party_detail$summary
    
    # Create email content
    email_content <- email_template
    email_content <- gsub("%PARTY_NAME%", party_name, email_content)
    email_content <- gsub("%EMAIL%", party_email, email_content)
    email_content <- gsub("%DETAILS%", party_details, email_content)
    
    # Create a unique filename
    filename <- file.path(output_dir, paste0(
      "email_", 
      gsub("[^a-zA-Z0-9]", "_", party_name), 
      ".txt"
    ))
    
    # Write email to file
    tryCatch({
      write(email_content, filename)
    }, error = function(e) {
      cat("Error generating email for party", party_name, ":", conditionMessage(e), "\n")
    })
  }
  
  cat("Generated", length(results$party_details), "cost summary emails in", output_dir, "\n")
}

# Main function to run the cost summary generation
run_cost_summary <- function(file_path) {
  # Print current working directory for debugging
  cat("Current working directory for cost summary:", getwd(), "\n")
  
  # SIMPLIFIED: Force directory creation right at the start
  dir.create("wedding_costs", showWarnings = FALSE, recursive = TRUE)
  dir.create("wedding_emails", showWarnings = FALSE, recursive = TRUE)
  
  # Ensure we're using simple directory paths, not nested ones
  costs_dir <- "wedding_costs"
  emails_dir <- "wedding_emails"
  
  # Generate cost summaries
  results <- generate_cost_summary(file_path)
  
  # Generate PDFs - using simple paths
  generate_cost_pdfs(results, costs_dir)
  
  # Generate emails - using simple paths 
  generate_cost_emails(results, emails_dir)
  
  return(results)
}

# Example usage:
# results <- run_cost_summary("path/to/guestlist.csv")