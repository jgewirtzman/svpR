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

# Function to generate CSV-based cost summaries for each party (replaces PDF generation)
generate_cost_pdfs <- function(results, output_dir = "wedding_costs") {
  # Create directory if it doesn't exist
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  cat("Ensuring output directory exists:", output_dir, "\n")
  
  # Generate a CSV for each party
  for (party_detail in results$party_details) {
    # Get party details
    party_id <- party_detail$party
    party_name <- party_detail$party_name
    
    # Create a unique filename
    filename <- file.path(output_dir, paste0(
      "cost_summary_", 
      gsub("[^a-zA-Z0-9]", "_", party_name), 
      ".csv"
    ))
    
    # Create a simple data frame with the summary
    party_info <- results$party_costs %>% filter(party == party_id)
    
    # Create a formatted summary table
    summary_table <- data.frame(
      Category = c(
        "Party Name", "Email", "Total Guests",
        "Friday Guests", "Saturday Guests", "Sunday Guests",
        "Camping", "Standard Lodging",
        "Friday Cost", "Saturday Cost", "Sunday Cost", "Total Cost"
      ),
      Value = c(
        party_info$party_name, party_info$party_email, party_info$total_guests,
        party_info$staying_friday, party_info$staying_saturday, party_info$staying_sunday,
        party_info$camping, party_info$standard_lodging,
        paste0("$", party_info$total_friday_cost), 
        paste0("$", party_info$total_saturday_cost), 
        paste0("$", party_info$total_sunday_cost), 
        paste0("$", party_info$grand_total)
      )
    )
    
    # Write to CSV
    write_csv(summary_table, filename)
    cat("Generated cost summary for party:", party_name, "\n")
    
    # Also create a text version for easy access
    text_filename <- file.path(output_dir, paste0(
      "cost_summary_", 
      gsub("[^a-zA-Z0-9]", "_", party_name), 
      ".txt"
    ))
    
    writeLines(party_detail$summary, text_filename)
  }
  
  cat("Generated", length(results$party_details), "cost summaries in", output_dir, "\n")
}

# Function to generate simplified emails with cleaner format
generate_simplified_emails <- function(results, output_dir = "wedding_emails") {
  # Create directory if it doesn't exist
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  cat("Ensuring email directory exists:", output_dir, "\n")
  
  # Simplified email template
  email_template <- "
To: %EMAIL%
Subject: Your Wedding Accommodation Details - Cyrena & Jon's Wedding

Dear %PARTY_NAME%,

We're excited to have you join us for our wedding! Here's a summary of your accommodation details:

Party: %PARTY_NAME%
Number of Guests: %GUEST_COUNT%

Your Stay:
%STAY_DETAILS%

Total Cost: $%TOTAL_COST%

Payment Options:
1. Venmo: @wedding-account
2. Zelle: payments@wedding-email.com
3. Check: Mail to Wedding Couple, 123 Wedding Lane, Wedding City, WS 12345

Please make your payment by May 15, 2025.

Looking forward to celebrating with you!

Best wishes,
Cyrena & Jon
"
  
  # Generate an email for each party
  for (party_detail in results$party_details) {
    # Get party details
    party_id <- party_detail$party
    party_name <- party_detail$party_name
    party_email <- party_detail$email
    
    # Get party costs info
    party_costs <- results$party_costs %>% filter(party == party_id)
    total_cost <- party_costs$grand_total
    guest_count <- party_costs$total_guests
    
    # Create a simplified stay summary
    stay_details <- ""
    if(party_costs$staying_friday > 0) {
      stay_details <- paste0(stay_details, 
                             "- Friday Night: ", party_costs$staying_friday, " guests\n")
    }
    if(party_costs$staying_saturday > 0) {
      stay_details <- paste0(stay_details, 
                             "- Saturday Night: ", party_costs$staying_saturday, " guests\n")
    }
    if(party_costs$staying_sunday > 0) {
      stay_details <- paste0(stay_details, 
                             "- Sunday Night: ", party_costs$staying_sunday, " guests\n")
    }
    if(party_costs$camping > 0) {
      stay_details <- paste0(stay_details, 
                             "- Accommodation Type: Camping\n")
    } else if(party_costs$standard_lodging > 0) {
      stay_details <- paste0(stay_details, 
                             "- Accommodation Type: Standard Lodging\n")
    }
    
    # Create email content
    email_content <- email_template
    email_content <- gsub("%PARTY_NAME%", party_name, email_content)
    email_content <- gsub("%EMAIL%", party_email, email_content)
    email_content <- gsub("%GUEST_COUNT%", guest_count, email_content)
    email_content <- gsub("%STAY_DETAILS%", stay_details, email_content)
    email_content <- gsub("%TOTAL_COST%", total_cost, email_content)
    
    # Create a unique filename
    filename <- file.path(output_dir, paste0(
      "email_", 
      gsub("[^a-zA-Z0-9]", "_", party_name), 
      ".txt"
    ))
    
    # Write email to file
    write(email_content, filename)
  }
  
  cat("Generated", length(results$party_details), "simplified email templates in", output_dir, "\n")
}

# Original function to generate a complete email for each party
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
  
  # Also generate simplified emails
  generate_simplified_emails(results, output_dir)
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
  
  # Generate CSVs instead of PDFs - using simple paths
  generate_cost_pdfs(results, costs_dir)
  
  # Generate emails - using simple paths 
  generate_cost_emails(results, emails_dir)
  
  return(results)
}

# Example usage:
# results <- run_cost_summary("path/to/guestlist.csv")