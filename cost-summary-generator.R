# Wedding Cost Summary Generator
# This script generates detailed cost breakdowns for each guest/party
# With improved formatting to show only applicable dates and charges

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
  
  # Create a cost breakdown by party with more details
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
      
      # Cost breakdowns by category - Guest charges only
      total_friday_guest_lodging = sum(friday_guest_lodging_charge, na.rm = TRUE),
      total_friday_guest_meals = sum(friday_guest_meals_charge, na.rm = TRUE),
      total_friday_guest_charge = sum(friday_guest_charge, na.rm = TRUE),
      
      total_saturday_guest_lodging = sum(saturday_guest_lodging_charge, na.rm = TRUE),
      total_saturday_guest_meals = sum(saturday_guest_meals_charge, na.rm = TRUE),
      total_saturday_guest_charge = sum(saturday_guest_charge, na.rm = TRUE),
      
      total_sunday_guest_charge = sum(sunday_guest_charge, na.rm = TRUE),
      
      total_guest_lodging = sum(friday_guest_lodging_charge + saturday_guest_lodging_charge, na.rm = TRUE),
      total_guest_meals = sum(friday_guest_meals_charge + saturday_guest_meals_charge + sunday_guest_charge, na.rm = TRUE),
      grand_total_guest_charge = sum(total_guest_charge, na.rm = TRUE),
      
      # Additional financial information for hosts (not included in guest invoices)
      total_ifc_charge = sum(total_cost, na.rm = TRUE),
      total_host_charge = sum(total_host_charge, na.rm = TRUE)
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
      arrange(desc(total_guest_charge))
    
    # Create a text summary - showing ONLY relevant dates (what the guest is paying for)
    summary_text <- paste0(
      "Party: ", party_info$party_name, "\n",
      "Guests: ", party_info$guest_names, "\n",
      "Email: ", party_info$party_email, "\n",
      "Total Guests: ", party_info$total_guests, "\n",
      "Total Guest Charge: $", party_info$grand_total_guest_charge, "\n\n",
      "Accommodation Summary:\n"
    )
    
    # Add only relevant nights where guests are staying
    if (party_info$staying_friday > 0) {
      summary_text <- paste0(summary_text,
                             "- Friday night: ", party_info$staying_friday, " guests",
                             " ($", party_info$total_friday_guest_charge, ")\n")
    }
    
    if (party_info$staying_saturday > 0) {
      summary_text <- paste0(summary_text,
                             "- Saturday night: ", party_info$staying_saturday, " guests",
                             " ($", party_info$total_saturday_guest_charge, ")\n")
    }
    
    if (party_info$staying_sunday > 0) {
      summary_text <- paste0(summary_text,
                             "- Sunday night: ", party_info$staying_sunday, " guests",
                             " ($", party_info$total_sunday_guest_charge, ")\n")
    }
    
    # Add accommodation type if any guests are staying
    if (party_info$staying_friday > 0 || party_info$staying_saturday > 0 || party_info$staying_sunday > 0) {
      if (party_info$camping > 0) {
        summary_text <- paste0(summary_text, "- Camping: ", party_info$camping, " guests\n")
      }
      
      if (party_info$standard_lodging > 0) {
        summary_text <- paste0(summary_text, "- Standard lodging: ", party_info$standard_lodging, " guests\n")
      }
      
      summary_text <- paste0(summary_text, "\n")
    }
    
    # Add guest charge breakdown - only for applicable dates
    summary_text <- paste0(summary_text, "Guest Charge Breakdown:\n")
    
    if (party_info$staying_friday > 0) {
      summary_text <- paste0(summary_text,
                             "- Friday: $", party_info$total_friday_guest_charge, "\n",
                             "  - Lodging: $", party_info$total_friday_guest_lodging, "\n",
                             "  - Meals: $", party_info$total_friday_guest_meals, "\n")
    }
    
    if (party_info$staying_saturday > 0) {
      summary_text <- paste0(summary_text,
                             "- Saturday: $", party_info$total_saturday_guest_charge, "\n", 
                             "  - Lodging: $", party_info$total_saturday_guest_lodging, "\n",
                             "  - Meals: $", party_info$total_saturday_guest_meals, "\n")
    }
    
    if (party_info$staying_sunday > 0) {
      summary_text <- paste0(summary_text,
                             "- Sunday: $", party_info$total_sunday_guest_charge, "\n")
    }
    
    summary_text <- paste0(summary_text, "\n")
    
    # Add individual guest details
    summary_text <- paste0(summary_text, "Individual Guest Details:\n")
    
    # Add details for each guest - only showing applicable dates
    guest_details <- paste(sapply(1:nrow(party_guests), function(i) {
      guest <- party_guests[i, ]
      
      # Construct the staying string - only include nights the guest is staying
      staying_days <- c()
      if(guest$is_staying_friday) staying_days <- c(staying_days, "Friday")
      if(guest$is_staying_saturday) staying_days <- c(staying_days, "Saturday")
      if(guest$is_staying_sunday) staying_days <- c(staying_days, "Sunday")
      
      staying_str <- if(length(staying_days) > 0) paste(staying_days, collapse = ", ") else "Not staying overnight"
      
      # Construct accommodation type - only if guest is staying
      accommodation_type <- if(guest$is_staying_friday || guest$is_staying_saturday || guest$is_staying_sunday) {
        if(guest$is_camping) "Camping" else "Standard Lodging"
      } else {
        "Not staying overnight"
      }
      
      # Create guest detail string
      guest_str <- paste0(
        i, ". ", guest$first_name, " ", guest$last_name, "\n",
        "   - Staying: ", staying_str, "\n",
        "   - Accommodation Type: ", accommodation_type, "\n"
      )
      
      # Add meal information - only for applicable nights
      if (length(staying_days) > 0) {
        guest_str <- paste0(guest_str, "   - Included Meals:\n")
        
        # Only add meal info for nights the guest is staying
        if(guest$is_staying_friday) {
          guest_str <- paste0(guest_str, "     * Friday: Dinner, Saturday: Breakfast\n")
        }
        if(guest$is_staying_saturday) {
          guest_str <- paste0(guest_str, "     * Saturday: Lunch, Dinner, Sunday: Breakfast\n")
        }
        if(guest$is_staying_sunday) {
          guest_str <- paste0(guest_str, "     * Sunday: Dinner, Monday: Breakfast (special catering)\n")
        }
      } else {
        guest_str <- paste0(guest_str, "   - No overnight meals included\n")
      }
      
      # Add cost information - only for applicable nights
      if(guest$is_staying_friday) {
        guest_str <- paste0(guest_str, 
                            "   - Friday Guest Charge: $", guest$friday_guest_charge, 
                            " (Lodging: $", guest$friday_guest_lodging_charge, 
                            ", Meals: $", guest$friday_guest_meals_charge, ")\n")
      }
      
      if(guest$is_staying_saturday) {
        guest_str <- paste0(guest_str, 
                            "   - Saturday Guest Charge: $", guest$saturday_guest_charge, 
                            " (Lodging: $", guest$saturday_guest_lodging_charge, 
                            ", Meals: $", guest$saturday_guest_meals_charge, ")\n")
      }
      
      if(guest$is_staying_sunday) {
        guest_str <- paste0(guest_str, 
                            "   - Sunday Guest Charge: $", guest$sunday_guest_charge, "\n")
      }
      
      # Add total guest charge
      guest_str <- paste0(guest_str, 
                          "   - Total Guest Charge: $", guest$total_guest_charge,
                          " (Lodging: $", (guest$friday_guest_lodging_charge + guest$saturday_guest_lodging_charge), 
                          ", Meals: $", (guest$friday_guest_meals_charge + guest$saturday_guest_meals_charge + guest$sunday_guest_charge), ")\n")
      
      return(guest_str)
    }), collapse = "\n")
    
    # Combine all text
    full_summary <- paste0(summary_text, guest_details)
    
    return(list(
      party = p,
      party_name = party_info$party_name,
      guest_names = party_info$guest_names,
      email = party_info$party_email,
      summary = full_summary,
      staying_friday = party_info$staying_friday > 0,
      staying_saturday = party_info$staying_saturday > 0,
      staying_sunday = party_info$staying_sunday > 0,
      total_guest_charge = party_info$grand_total_guest_charge
    ))
  })
  
  return(list(
    party_costs = party_costs,
    party_details = party_details
  ))
}

# Function to generate CSV-based cost summaries for each party
generate_cost_pdfs <- function(results, output_dir = "wedding_costs") {
  # Create directory if it doesn't exist
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  cat("Ensuring output directory exists:", output_dir, "\n")
  
  # Generate a CSV for each party
  for (party_detail in results$party_details) {
    # Skip parties with $0 charge
    if (party_detail$total_guest_charge == 0) {
      cat("Skipping $0 charge party:", party_detail$party_name, "\n")
      next
    }
    
    # Get party details
    party_id <- party_detail$party
    party_name <- party_detail$party_name
    
    # Create a unique filename
    filename <- file.path(output_dir, paste0(
      "cost_summary_", 
      gsub("[^a-zA-Z0-9]", "_", party_name), 
      ".csv"
    ))
    
    # Create a party info table
    party_info <- data.frame(
      Category = c(
        "Party Name", 
        "Email", 
        "Total Guests",
        "Total Guest Charge"
      ),
      Value = c(
        party_detail$party_name, 
        party_detail$email, 
        length(strsplit(party_detail$guest_names, ", ")[[1]]),
        paste0("$", format(party_detail$total_guest_charge, big.mark = ","))
      )
    )
    
    # Create a night breakdown table - only for applicable nights
    night_info <- data.frame(
      Night = character(),
      Status = character(),
      Charge = character(),
      stringsAsFactors = FALSE
    )
    
    if (party_detail$staying_friday) {
      night_info <- rbind(night_info, data.frame(
        Night = "Friday, June 20",
        Status = "Staying",
        Charge = "See breakdown below",
        stringsAsFactors = FALSE
      ))
    }
    
    if (party_detail$staying_saturday) {
      night_info <- rbind(night_info, data.frame(
        Night = "Saturday, June 21",
        Status = "Staying",
        Charge = "See breakdown below",
        stringsAsFactors = FALSE
      ))
    }
    
    if (party_detail$staying_sunday) {
      night_info <- rbind(night_info, data.frame(
        Night = "Sunday, June 22",
        Status = "Staying",
        Charge = "See breakdown below",
        stringsAsFactors = FALSE
      ))
    }
    
    # Write to CSV
    write_csv(party_info, filename)
    
    if (nrow(night_info) > 0) {
      write_csv(night_info, file.path(output_dir, paste0(
        "nights_", 
        gsub("[^a-zA-Z0-9]", "_", party_name), 
        ".csv"
      )))
    }
    
    # Also create a text version for easy access
    text_filename <- file.path(output_dir, paste0(
      "cost_summary_", 
      gsub("[^a-zA-Z0-9]", "_", party_name), 
      ".txt"
    ))
    
    writeLines(party_detail$summary, text_filename)
    
    # Generate an elegant invoice PDF for the party
    invoice_filename <- file.path(output_dir, paste0(
      "invoice_", 
      gsub("[^a-zA-Z0-9]", "_", party_name), 
      ".txt"  # Note: In production, this would be .pdf
    ))
    
    # For this example, we'll create a text representation of what would be in the PDF
    invoice_content <- generate_invoice_content(party_detail)
    writeLines(invoice_content, invoice_filename)
    
    cat("Generated cost summary for party:", party_name, "\n")
  }
  
  cat("Generated", length(results$party_details), "cost summaries in", output_dir, "\n")
}

# Helper function to generate invoice content 
generate_invoice_content <- function(party_detail) {
  # This would normally generate a PDF, but for this example we'll create formatted text
  
  # Create header
  invoice_text <- paste0(
    "===============================================================\n",
    "                     ISABELLA FREEDMAN CENTER\n",
    "                   JON & CYRENA'S WEDDING WEEKEND\n",
    "                         JUNE 20-23, 2025\n",
    "===============================================================\n\n",
    
    "INVOICE FOR: ", party_detail$party_name, "\n",
    "EMAIL: ", party_detail$email, "\n",
    "GUESTS: ", party_detail$guest_names, "\n\n",
    
    "===============================================================\n",
    "                        ACCOMMODATION DETAILS\n",
    "===============================================================\n\n"
  )
  
  # Add only the applicable nights - showing nothing for nights not staying
  if (party_detail$staying_friday) {
    invoice_text <- paste0(invoice_text,
                           "FRIDAY, JUNE 20:\n",
                           "  Standard accommodation includes:\n",
                           "  - Friday dinner\n",
                           "  - Saturday breakfast\n",
                           "  - Overnight accommodation\n\n")
  }
  
  if (party_detail$staying_saturday) {
    invoice_text <- paste0(invoice_text,
                           "SATURDAY, JUNE 21:\n",
                           "  Standard accommodation includes:\n",
                           "  - Saturday lunch\n",
                           "  - Saturday dinner\n",
                           "  - Sunday breakfast\n",
                           "  - Overnight accommodation\n\n")
  }
  
  if (party_detail$staying_sunday) {
    invoice_text <- paste0(invoice_text,
                           "SUNDAY, JUNE 22:\n",
                           "  Standard accommodation includes:\n",
                           "  - Sunday dinner\n",
                           "  - Monday breakfast\n",
                           "  - Overnight accommodation\n\n")
  }
  
  # Add payment information
  invoice_text <- paste0(invoice_text,
                         "===============================================================\n",
                         "                       PAYMENT INFORMATION\n",
                         "===============================================================\n\n",
                         "TOTAL DUE: $", format(party_detail$total_guest_charge, big.mark = ","), "\n\n",
                         "Payment Options:\n",
                         "1. Venmo: @wedding-account\n",
                         "2. Zelle: payments@wedding-email.com\n",
                         "3. Check: Mail to Wedding Couple, 123 Wedding Lane, Wedding City, WS 12345\n\n",
                         "Please make your payment by May 15, 2025.\n\n",
                         "For questions, contact: wedding@example.com\n",
                         "===============================================================\n"
  )
  
  return(invoice_text)
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

%STAY_DETAILS%

Total Guest Charge: $%TOTAL_COST%

Payment Options:
1. Venmo: @wedding-account
2. Zelle: payments@wedding-email.com
3. Check: Mail to Wedding Couple, 123 Wedding Lane, Wedding City, WS 12345

Please make your payment by May 15, 2025.

Looking forward to celebrating with you!

Best wishes,
Cyrena & Jon
"
  
  # Generate an email for each party with a charge > 0
  for (party_detail in results$party_details) {
    # Skip parties with $0 charge
    if (party_detail$total_guest_charge == 0) {
      cat("Skipping $0 charge party for email:", party_detail$party_name, "\n")
      next
    }
    
    # Get party details
    party_id <- party_detail$party
    party_name <- party_detail$party_name
    party_email <- party_detail$email
    
    # Get guest count
    guest_count <- length(strsplit(party_detail$guest_names, ", ")[[1]])
    
    # Create a simplified stay summary - only for applicable nights
    stay_details <- "Your Stay:\n"
    
    if (party_detail$staying_friday) {
      stay_details <- paste0(stay_details, 
                             "- Friday Night (June 20)\n")
    }
    
    if (party_detail$staying_saturday) {
      stay_details <- paste0(stay_details, 
                             "- Saturday Night (June 21)\n")
    }
    
    if (party_detail$staying_sunday) {
      stay_details <- paste0(stay_details, 
                             "- Sunday Night (June 22)\n")
    }
    
    stay_details <- paste0(stay_details, 
                           "\nDetails of your stay, including meals included with your accommodation, can be found in the attached invoice.\n")
    
    # Create email content
    email_content <- email_template
    email_content <- gsub("%PARTY_NAME%", party_name, email_content)
    email_content <- gsub("%EMAIL%", party_email, email_content)
    email_content <- gsub("%GUEST_COUNT%", guest_count, email_content)
    email_content <- gsub("%STAY_DETAILS%", stay_details, email_content)
    email_content <- gsub("%TOTAL_COST%", format(party_detail$total_guest_charge, big.mark = ","), email_content)
    
    # Create a unique filename
    filename <- file.path(output_dir, paste0(
      "email_", 
      gsub("[^a-zA-Z0-9]", "_", party_name), 
      ".txt"
    ))
    
    # Write email to file
    write(email_content, filename)
    
    cat("Generated email for party:", party_name, "\n")
  }
  
  cat("Generated emails for", sum(sapply(results$party_details, function(pd) pd$total_guest_charge > 0)), "parties in", output_dir, "\n")
}

# Original function to generate a complete email for each party
generate_cost_emails <- function(results, output_dir = "wedding_emails") {
  # Force directory creation
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
Jon & Cyrena
"
  
  # Generate an email for each party
  for (party_detail in results$party_details) {
    # Skip parties with $0 charge
    if (party_detail$total_guest_charge == 0) {
      cat("Skipping $0 charge party for detailed email:", party_detail$party_name, "\n")
      next
    }
    
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
      "detailed_email_", 
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
  
  cat("Generated", sum(sapply(results$party_details, function(pd) pd$total_guest_charge > 0)), "detailed cost emails in", output_dir, "\n")
  
  # Also generate simplified emails
  generate_simplified_emails(results, output_dir)
}

# Main function to run the cost summary generation
run_cost_summary <- function(file_path) {
  # Print current working directory for debugging
  cat("Current working directory for cost summary:", getwd(), "\n")
  
  # Force directory creation right at the start
  dir.create("wedding_costs", showWarnings = FALSE, recursive = TRUE)
  dir.create("wedding_emails", showWarnings = FALSE, recursive = TRUE)
  
  # Ensure we're using simple directory paths, not nested ones
  costs_dir <- "wedding_costs"
  emails_dir <- "wedding_emails"
  
  # Generate cost summaries
  results <- generate_cost_summary(file_path)
  
  # Generate CSVs and invoices - using simple paths
  generate_cost_pdfs(results, costs_dir)
  
  # Generate emails - using simple paths 
  generate_cost_emails(results, emails_dir)
  
  return(results)
}

# Example usage:
# results <- run_cost_summary("path/to/guestlist.csv")