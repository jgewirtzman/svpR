# rates.R - Centralized rate management with explicit handling for under-21 guests
library(dplyr)
library(readr)
library(stringr)

# Function to load and standardize rates
load_rates <- function(file_path = "charge_rates.csv") {
  # Load rates with error handling
  if (file.exists(file_path)) {
    rates <- read_csv(file_path, 
                      col_types = cols(.default = col_character()),
                      na = c("", "NA", "N/A"))
    
    # Standardize column names
    names(rates) <- names(rates) %>%
      str_to_lower() %>%
      str_replace_all(" ", "_") %>%
      str_replace_all("[^a-z0-9_]", "")
    
    # Clean up night values to use just the day name
    rates <- rates %>%
      mutate(night = ifelse(grepl("Sunday", night), "Sunday", night))
    
    # Ensure numeric columns
    numeric_cols <- c("ifc_rate", "gratuity", "expected_guest_count", 
                      "total_ifc_revenue", "listed_standard_guest_rate", 
                      "listed_standard_guest_meal_rate", "per_guest_charge", 
                      "total_guest_charge", "total_host_charge")
    
    rates <- rates %>%
      mutate(across(all_of(numeric_cols), as.numeric))
    
    cat("Loaded rates from file:", file_path, "\n")
    print(rates)
    
    return(rates)
  } else {
    message("Rate information file not found: ", file_path)
    default_rates <- create_default_rates()
    print(default_rates)
    return(default_rates)
  }
}

# Function to create default rates if CSV is missing
create_default_rates <- function() {
  # Create a dataframe with default rates
  default_rates <- data.frame(
    night = c(rep("Friday", 5), rep("Saturday", 5), rep("Sunday", 5)),
    category = rep(c("Adults 21+ Room", "Guests 12-21 Room", "Children 5-12 Room", 
                     "Children <5 Room", "Adults 21+ Camping"), 3),
    ifc_rate = c(
      # Friday rates
      225, 225, 175, 0, 150,
      # Saturday rates
      225, 225, 175, 0, 150,
      # Sunday rates
      109, 109, 69, 0, 75
    ),
    gratuity = c(
      # Friday rates
      22.5, 22.5, 17.5, 0, 15,
      # Saturday rates
      22.5, 22.5, 17.5, 0, 15,
      # Sunday rates
      10.9, 10.9, 6.9, 0, 7.5
    ),
    per_guest_charge = c(
      # Friday rates
      108, 0, 0, 0, 72,  # Under 21 guests don't pay
      # Saturday rates
      172, 0, 0, 0, 90,  # Under 21 guests don't pay
      # Sunday rates
      18, 0, 0, 0, 18    # Under 21 guests don't pay
    ),
    stringsAsFactors = FALSE
  )
  
  # Calculate additional fields
  default_rates <- default_rates %>%
    mutate(
      total_cost = ifc_rate + gratuity,
      total_guest_charge = per_guest_charge,
      total_host_charge = total_cost - total_guest_charge
    )
  
  return(default_rates)
}

# Get rate for a specific night/category/field combination with enhanced debugging
get_rate <- function(rates, night, category, field, default = 0) {
  # Convert inputs to character to prevent issues with factors
  night <- as.character(night)
  category <- as.character(category)
  field <- as.character(field)
  
  # Special handling for Sunday lookup
  if (night == "Sunday") {
    # Look for any rates where night contains "Sunday"
    result <- rates %>%
      filter(grepl("Sunday", night) & category == !!category) %>%
      pull(!!field)
  } else {
    # Normal lookup for other nights
    result <- rates %>%
      filter(night == !!night & category == !!category) %>%
      pull(!!field)
  }
  
  if (length(result) == 0 || is.na(result[1])) {
    # Debug output for missing rates
    cat("WARNING: No rate found for", night, "/", category, "/", field, "- using default:", default, "\n")
    return(default)
  }
  
  return(as.numeric(result[1]))
}

# Print debug info for a guest
print_guest_debug <- function(guest) {
  cat("Guest:", guest$first_name, guest$last_name, "\n")
  cat("Age Category:", guest$age_category, "\n")
  cat("Camping:", guest$is_camping, "\n")
  cat("Stays: Friday =", guest$is_staying_friday, 
      "Saturday =", guest$is_staying_saturday, 
      "Sunday =", guest$is_staying_sunday, "\n")
}

# Calculate charges for a guest based on their details - FIXED for under 21 guests
calculate_guest_charges <- function(guest, rates) {
  # Enable this for individual guest debugging
  debug_mode <- FALSE
  
  # Only print debug info for under 21 guests if debugging is on
  is_under_21 <- grepl("Children|Guests 12-21", guest$age_category)
  if (debug_mode && is_under_21) {
    print_guest_debug(guest)
  }
  
  # Initialize charge vectors
  nights <- c("Friday", "Saturday", "Sunday")
  stay_flags <- c(guest$is_staying_friday, guest$is_staying_saturday, guest$is_staying_sunday)
  
  # Get the appropriate category based on age and accommodation
  category <- if (guest$is_camping) {
    "Adults 21+ Camping" 
  } else {
    guest$age_category
  }
  
  # Calculate charges for each night
  night_costs <- numeric(3)
  night_guest_charges <- numeric(3)
  night_host_charges <- numeric(3)
  
  for (i in 1:3) {
    # Only calculate if staying this night
    if (stay_flags[i]) {
      # Get rates for this night/category
      ifc_rate <- get_rate(rates, nights[i], category, "ifc_rate")
      gratuity <- get_rate(rates, nights[i], category, "gratuity")
      
      # Total IFC cost for this night
      night_costs[i] <- ifc_rate + gratuity
      
      # For guest charges, use the per_guest_charge directly from rates
      night_guest_charges[i] <- get_rate(rates, nights[i], category, "per_guest_charge")
      
      # EXPLICIT CALCULATION: Host charge is the difference between cost and guest charge
      night_host_charges[i] <- night_costs[i] - night_guest_charges[i]
      
      # Debug output for under 21 guests
      if (debug_mode && is_under_21) {
        cat(nights[i], "Night Calculation:\n")
        cat("- IFC Rate:", ifc_rate, "\n")
        cat("- Gratuity:", gratuity, "\n")
        cat("- Night Cost:", night_costs[i], "\n")
        cat("- Guest Charge:", night_guest_charges[i], "\n")
        cat("- Host Charge:", night_host_charges[i], "\n\n")
      }
    }
  }
  
  # Create results list
  charges <- list(
    friday_cost = night_costs[1],
    saturday_cost = night_costs[2],
    sunday_cost = night_costs[3],
    friday_guest_charge = night_guest_charges[1],
    saturday_guest_charge = night_guest_charges[2],
    sunday_guest_charge = night_guest_charges[3],
    friday_host_charge = night_host_charges[1],
    saturday_host_charge = night_host_charges[2],
    sunday_host_charge = night_host_charges[3],
    total_cost = sum(night_costs),
    total_guest_charge = sum(night_guest_charges),
    total_host_charge = sum(night_host_charges)
  )
  
  # Debug output for under 21 guests
  if (debug_mode && is_under_21) {
    cat("TOTAL CHARGES:\n")
    cat("- Total Cost:", charges$total_cost, "\n")
    cat("- Total Guest Charge:", charges$total_guest_charge, "\n")
    cat("- Total Host Charge:", charges$total_host_charge, "\n\n")
  }
  
  return(charges)
}

# Process all guests in a dataset with extra debugging
process_guest_charges <- function(guests, rates) {
  # Make sure we have age categories
  if (!"age_category" %in% names(guests)) {
    guests$age_category <- "Adults 21+ Room"
  }
  
  # Ensure we have stay information
  stay_cols <- c("is_staying_friday", "is_staying_saturday", "is_staying_sunday", "is_camping")
  for (col in stay_cols) {
    if (!col %in% names(guests)) {
      guests[[col]] <- FALSE
    }
  }
  
  # Output counts by age category for debugging
  cat("\nProcessing guests by age category:\n")
  print(table(guests$age_category))
  
  # Track under 21 host charges for validation
  under_21_host_charges <- 0
  under_21_count <- 0
  
  # Calculate charges for each guest
  for (i in 1:nrow(guests)) {
    charges <- calculate_guest_charges(guests[i,], rates)
    
    # Add charge information to guest data
    for (field in names(charges)) {
      guests[i, field] <- charges[[field]]
    }
    
    # Track under 21 host charges
    if (grepl("Children|Guests 12-21", guests[i,]$age_category)) {
      under_21_count <- under_21_count + 1
      under_21_host_charges <- under_21_host_charges + charges$total_host_charge
    }
  }
  
  # Output under 21 charge summary
  cat("\nUnder 21 guests:", under_21_count, "\n")
  cat("Total under 21 host charges:", under_21_host_charges, "\n\n")
  
  # Add vegetarian and special diet flags for meal planning
  guests <- guests %>%
    mutate(
      is_vegetarian = meal_preferences == "No meat" | 
        meal_preferences == "Opt-in for fish only",
      has_special_diet = !is.na(dietary_restrictions) & 
        dietary_restrictions != ""
    )
  
  # Verify host charges are correctly calculated
  verify_host_charges <- function(g) {
    issues <- sum(g$total_cost != (g$total_guest_charge + g$total_host_charge), na.rm = TRUE)
    if (issues > 0) {
      cat("WARNING:", issues, "guests have inconsistent host charges!\n")
      
      # Show a sample of problem guests
      problem_guests <- g %>%
        filter(total_cost != (total_guest_charge + total_host_charge)) %>%
        select(first_name, last_name, age_category, is_camping, 
               total_cost, total_guest_charge, total_host_charge)
      
      print(head(problem_guests, 5))
    } else {
      cat("All host charges verified correctly.\n")
    }
  }
  
  verify_host_charges(guests)
  
  return(guests)
}