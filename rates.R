# rates.R - Centralized rate management
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
    
    # Ensure numeric columns
    numeric_cols <- c("ifc_rate", "gratuity", "expected_guest_count", 
                      "total_ifc_revenue", "listed_standard_guest_rate", 
                      "listed_standard_guest_meal_rate", "per_guest_charge", 
                      "total_guest_charge", "total_host_charge")
    
    rates <- rates %>%
      mutate(across(all_of(numeric_cols), as.numeric))
    
    return(rates)
  } else {
    message("Rate information file not found: ", file_path)
    return(create_default_rates())
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
      108, 108, 54, 0, 72,
      # Saturday rates
      172, 172, 86, 0, 90,
      # Sunday rates
      18, 18, 9, 0, 18
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

# Get rate for a specific night/category/field combination
# Modified get_rate function to handle Sunday label properly
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
    return(default)
  }
  
  return(as.numeric(result[1]))
}

# Or alternatively, modify the rate loading function to clean up the labels
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
    
    # Convert numeric columns
    numeric_cols <- c("ifc_rate", "gratuity", "expected_guest_count", 
                      "total_ifc_revenue", "listed_standard_guest_rate", 
                      "listed_standard_guest_meal_rate", "per_guest_charge", 
                      "total_guest_charge", "total_host_charge")
    
    rates <- rates %>%
      mutate(across(all_of(numeric_cols), as.numeric))
    
    return(rates)
  } else {
    message("Rate information file not found: ", file_path)
    return(create_default_rates())
  }
}

# Calculate charges for a guest based on their details
calculate_guest_charges <- function(guest, rates) {
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
      guest_charge <- get_rate(rates, nights[i], category, "per_guest_charge")
      
      # Calculate costs
      night_costs[i] <- ifc_rate + gratuity
      night_guest_charges[i] <- guest_charge
      night_host_charges[i] <- night_costs[i] - night_guest_charges[i]
    }
  }
  
  # Return complete charge info
  return(list(
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
  ))
}

# Process all guests in a dataset
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
  
  # Calculate charges for each guest
  for (i in 1:nrow(guests)) {
    charges <- calculate_guest_charges(guests[i,], rates)
    
    # Add charge information to guest data
    for (field in names(charges)) {
      guests[i, field] <- charges[[field]]
    }
  }
  
  return(guests)
}