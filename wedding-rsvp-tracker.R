# Wedding RSVP Tracker and Cost Calculator
# This script processes the guest list CSV and generates reports for:
# 1. Party names and emails
# 2. Accommodation counts and costs
# 3. Meal counts for guests (both on-site and off-site)
# 4. Age categories for room and accommodation planning
# 5. Financial calculations for guests, IFC, and hosts

# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# Function to read in guest list data
read_guest_list <- function(file_path) {
  # Read the CSV file
  guests <- read_csv(file_path, 
                     col_types = cols(.default = col_character()),
                     na = c("", "NA", "N/A"))
  
  # Clean column names (remove spaces, etc.)
  names(guests) <- names(guests) %>%
    str_to_lower() %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("[^a-z0-9_]", "")
  
  # Print column names for debugging
  cat("Available columns in the dataset:\n")
  cat(paste(names(guests), collapse = ", "), "\n")
  
  return(guests)
}

# Read in financial rate information
read_rate_information <- function(file_path = "charge_rates.csv") {
  # Check if the file exists
  if (!file.exists(file_path)) {
    warning("Rate information file not found: ", file_path)
    return(NULL)
  }
  
  # Read the CSV file
  rates <- read_csv(file_path, 
                    col_types = cols(.default = col_character()),
                    na = c("", "NA", "N/A"))
  
  # Clean column names
  names(rates) <- names(rates) %>%
    str_to_lower() %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("[^a-z0-9_]", "")
  
  # Convert numeric columns
  numeric_cols <- c("ifc_rate", "gratuity", "expected_guest_count", "total_ifc_revenue", 
                    "listed_standard_guest_rate", "listed_standard_guest_meal_rate", 
                    "per_guest_charge", "total_guest_charge", "total_host_charge")
  
  rates <- rates %>%
    mutate(across(all_of(numeric_cols), as.numeric))
  
  return(rates)
}

# Helper function to detect and handle various column name formats
find_column_by_pattern <- function(df, patterns) {
  for (pattern in patterns) {
    matching_cols <- grep(pattern, names(df), ignore.case = TRUE, value = TRUE)
    if (length(matching_cols) > 0) {
      return(matching_cols[1])
    }
  }
  return(NULL)
}

# Function to generate party-level summary
generate_party_summary <- function(guests) {
  # Find the email column - handle cases where email might have different names
  email_cols <- grep("email", names(guests), ignore.case = TRUE, value = TRUE)
  
  # Check if we found any email columns
  if (length(email_cols) == 0) {
    warning("No email column found in the data")
    guests$email_column <- NA  # Create a dummy column
    email_col_name <- "email_column"
  } else {
    # Use the first email column found
    email_col_name <- email_cols[1]
    # Rename it to a standard name for use in the function
    guests$email_column <- guests[[email_col_name]]
  }
  
  # Create descriptive party names based on guest names
  guests <- guests %>%
    group_by(party) %>%
    mutate(
      party_name = case_when(
        n() == 1 ~ paste0(first_name, " ", last_name),
        n() == 2 ~ paste0(first(first_name), " ", first(last_name), " & ", last(first_name), " ", last(last_name)),
        TRUE ~ paste0(first(last_name), " Family (", n(), " guests)")
      )
    ) %>%
    ungroup()
  
  # Group by party and get email
  party_summary <- guests %>%
    group_by(party) %>%
    summarize(
      party_name = first(party_name),
      party_email = first(na.omit(email_column)),
      total_guests = n(),
      guest_names = paste(paste(first_name, last_name), collapse = ", "),
      wedding_attending = sum(wedding_rsvp == "Joyfully Accept", na.rm = TRUE),
      wedding_declining = sum(wedding_rsvp == "Regretfully Decline", na.rm = TRUE),
      wedding_no_response = sum(is.na(wedding_rsvp)),
      friday_attending = sum(fridayshabbat_rsvp == "Joyfully Accept", na.rm = TRUE),
      saturday_attending = sum(saturday_onsite_rsvp == "Yes, I will join on Saturday" | 
                                 saturday_offsite_rsvp == "Yes, I will join for lunch only" |
                                 saturday_offsite_rsvp == "Yes, I will join for dinner only" |
                                 saturday_offsite_rsvp == "Yes, I will join for lunch and dinner", 
                               na.rm = TRUE)
    )
  
  return(party_summary)
}

# Function to assign age categories to guests based on age data
assign_age_categories <- function(guests, age_data_path = "Wedding Budget Invite List.csv") {
  # If an age data file is provided, use it
  if (!is.null(age_data_path) && file.exists(age_data_path)) {
    # Read the age data file
    age_data <- read_csv(age_data_path,
                         col_types = cols(.default = col_character()),
                         na = c("", "NA", "N/A"))
    
    # Clean column names
    names(age_data) <- names(age_data) %>%
      str_to_lower() %>%
      str_replace_all(" ", "_")
    
    # Extract name and age information
    if (all(c("first_name", "last_name", "age") %in% names(age_data))) {
      # Clean the first_name and last_name columns in both datasets to improve matching
      age_info <- age_data %>%
        select(first_name, last_name, age) %>%
        filter(!is.na(first_name) & !is.na(last_name)) %>%
        mutate(
          first_name = trimws(first_name),
          last_name = trimws(last_name)
        )
      
      guests <- guests %>%
        mutate(
          first_name = trimws(first_name),
          last_name = trimws(last_name)
        )
      
      # Map age categories to our defined categories
      age_info <- age_info %>%
        mutate(
          age_category = case_when(
            age == "Adult" ~ "Adults 21+ Room",
            age == "Child (13+)" ~ "Guests 12-21 Room",
            age == "Child (5-12)" ~ "Children 5-12 Room",
            age == "Child (0-4)" ~ "Children <5 Room",
            TRUE ~ "Unknown"
          )
        )
      
      # Merge with guests based on first and last name
      guests <- guests %>%
        left_join(age_info %>% select(first_name, last_name, age_category),
                  by = c("first_name", "last_name"))
      
      # Fill in missing age categories and log the unmatched guests
      unmatched <- guests %>% filter(is.na(age_category))
      if(nrow(unmatched) > 0) {
        cat("Could not match age categories for", nrow(unmatched), "guests:\n")
        print(unmatched %>% select(first_name, last_name))
      }
      
      guests <- guests %>%
        mutate(
          age_category = ifelse(is.na(age_category), "Adults 21+ Room", age_category)
        )
      
      cat("Age categories assigned from file:", age_data_path, "\n")
    } else {
      warning("Age data file does not contain required columns: first_name, last_name, and age")
      # Use default assignment
      guests$age_category <- "Adults 21+ Room"
    }
  } else {
    # Use default assignment
    guests$age_category <- "Adults 21+ Room"
    warning("No age data file provided. All guests categorized as adults by default.")
  }
  
  # Update camping adults AFTER age categories are assigned
  if ("is_camping" %in% names(guests)) {
    guests <- guests %>%
      mutate(
        age_category = ifelse(
          age_category == "Adults 21+ Room" & is_camping,
          "Adults 21+ Camping",
          age_category
        )
      )
  }
  
  return(guests)
}

# Function to count guests by age category
count_by_age_category <- function(guests) {
  # Count guests by age category and accommodation
  age_counts <- guests %>%
    group_by(age_category) %>%
    summarize(
      total_guests = n(),
      friday_count = sum(is_staying_friday, na.rm = TRUE),
      saturday_count = sum(is_staying_saturday, na.rm = TRUE),
      sunday_count = sum(is_staying_sunday, na.rm = TRUE),
      total_guest_charge = sum(total_guest_charge, na.rm = TRUE),
      total_host_charge = sum(total_host_charge, na.rm = TRUE),
      total_cost = sum(total_cost, na.rm = TRUE)
    ) %>%
    arrange(desc(total_guests))
  
  return(age_counts)
}

# Function to calculate accommodation costs with financial breakdown
calculate_accommodation_costs <- function(guests, age_data_path = NULL, rates_path = "charge_rates.csv") {
  # Source any helpers we need
  if (exists("ensure_numeric_costs")) {
    source("helpers.R")
  }
  
  # Load rates data
  rates_data <- NULL
  if (file.exists(rates_path)) {
    rates_data <- read_csv(rates_path, 
                           col_types = cols(.default = col_character()),
                           na = c("", "NA", "N/A"))
    
    # Standardize column names
    names(rates_data) <- names(rates_data) %>%
      str_to_lower() %>%
      str_replace_all(" ", "_") %>%
      str_replace_all("[^a-z0-9_]", "")
    
    # Ensure numeric columns
    numeric_cols <- c("ifc_rate", "gratuity", "expected_guest_count", 
                      "total_ifc_revenue", "listed_standard_guest_rate", 
                      "listed_standard_guest_meal_rate", "per_guest_charge", 
                      "total_guest_charge", "total_host_charge")
    
    rates_data <- rates_data %>%
      mutate(across(all_of(numeric_cols), as.numeric))
    
    cat("Loaded rates from:", rates_path, "\n")
  } else {
    cat("Rates file not found, using default values\n")
  }
  
  # First determine stay information
  possible_friday_cols <- c("friday_ifc_stay_1", "ifc_stay_1", "friday_ifc_stay")
  possible_saturday_cols <- c("saturday_ifc_stay_1", "saturday_ifc_stay", "ifc_stay_1")
  possible_sunday_cols <- c("sunday_ifc_stay_1", "sunday_ifc_stay")
  possible_camping_cols <- c("lodgingcamping_weekend", "lodgingcamping_sat_only")
  
  # Process stay data
  guests <- guests %>%
    mutate(
      # Determine if staying each night
      is_staying_friday = rowSums(across(any_of(possible_friday_cols), 
                                         ~ . == "Yes", 
                                         .names = "col_{.col}"), 
                                  na.rm = TRUE) > 0,
      
      is_staying_saturday = rowSums(across(any_of(possible_saturday_cols), 
                                           ~ . == "Yes", 
                                           .names = "col_{.col}"),
                                    na.rm = TRUE) > 0,
      
      is_staying_sunday = rowSums(across(any_of(possible_sunday_cols), 
                                         ~ . == "Yes", 
                                         .names = "col_{.col}"),
                                  na.rm = TRUE) > 0,
      
      # Ensure camping is properly detected and is a logical value
      is_camping = as.logical(rowSums(across(any_of(possible_camping_cols),
                                             ~ . == "Camping",
                                             .names = "col_{.col}"),
                                      na.rm = TRUE) > 0)
    )
  
  # Print summary of camping status
  cat("Camping status summary:\n")
  cat("Number of camping guests:", sum(guests$is_camping, na.rm = TRUE), "\n")
  
  # Assign age categories
  if (!is.null(age_data_path) && file.exists(age_data_path)) {
    guests <- assign_age_categories(guests, age_data_path)
  } else if (!"age_category" %in% names(guests)) {
    guests$age_category <- "Adults 21+ Room"
  }
  
  # Update camping adults AFTER age categories are assigned
  guests <- guests %>%
    mutate(
      age_category = ifelse(
        age_category == "Adults 21+ Room" & is_camping,
        "Adults 21+ Camping",
        age_category
      )
    )
  
  # Define default rates in case rates_data is NULL
  FRIDAY_ADULTS_ROOM_COST <- 247.5   # 225 + 22.5
  FRIDAY_TEEN_ROOM_COST <- 247.5     # 225 + 22.5
  FRIDAY_CHILD_ROOM_COST <- 192.5    # 175 + 17.5
  FRIDAY_INFANT_ROOM_COST <- 0       # 0 + 0
  FRIDAY_CAMPING_COST <- 165         # 150 + 15
  
  SATURDAY_ADULTS_ROOM_COST <- 247.5 # 225 + 22.5
  SATURDAY_TEEN_ROOM_COST <- 247.5   # 225 + 22.5
  SATURDAY_CHILD_ROOM_COST <- 192.5  # 175 + 17.5
  SATURDAY_INFANT_ROOM_COST <- 0     # 0 + 0
  SATURDAY_CAMPING_COST <- 165       # 150 + 15
  
  SUNDAY_ADULTS_ROOM_COST <- 119.9   # 109 + 10.9
  SUNDAY_TEEN_ROOM_COST <- 119.9     # 109 + 10.9
  SUNDAY_CHILD_ROOM_COST <- 75.9     # 69 + 6.9
  SUNDAY_INFANT_ROOM_COST <- 0       # 0 + 0
  SUNDAY_CAMPING_COST <- 82.5        # 75 + 7.5
  
  # Adult guest charges (what they pay)
  FRIDAY_ADULTS_ROOM_GUEST <- 108
  FRIDAY_CAMPING_GUEST <- 72
  
  SATURDAY_ADULTS_ROOM_GUEST <- 172
  SATURDAY_CAMPING_GUEST <- 90
  
  SUNDAY_ADULTS_ROOM_GUEST <- 18
  SUNDAY_CAMPING_GUEST <- 18
  
  # Under-21 guest charges - ALWAYS 0
  UNDER_21_GUEST_CHARGE <- 0
  
  # Override with rates from file if available
  if (!is.null(rates_data)) {
    get_rate <- function(night, category, field, default) {
      result <- rates_data %>%
        filter(night == !!night & category == !!category) %>%
        pull(!!field)
      
      if (length(result) == 0 || is.na(result[1])) {
        return(default)
      }
      
      return(as.numeric(result[1]))
    }
    
    # Get rates from file if possible
    friday_rate_found <- any(rates_data$night == "Friday", na.rm = TRUE)
    saturday_rate_found <- any(rates_data$night == "Saturday", na.rm = TRUE)
    sunday_rate_found <- any(rates_data$night == "Sunday", na.rm = TRUE)
    
    if (friday_rate_found) {
      FRIDAY_ADULTS_ROOM_COST <- get_rate("Friday", "Adults 21+ Room", "ifc_rate", 225) + 
        get_rate("Friday", "Adults 21+ Room", "gratuity", 22.5)
      FRIDAY_TEEN_ROOM_COST <- get_rate("Friday", "Guests 12-21 Room", "ifc_rate", 225) + 
        get_rate("Friday", "Guests 12-21 Room", "gratuity", 22.5)
      FRIDAY_CHILD_ROOM_COST <- get_rate("Friday", "Children 5-12 Room", "ifc_rate", 175) + 
        get_rate("Friday", "Children 5-12 Room", "gratuity", 17.5)
      FRIDAY_INFANT_ROOM_COST <- get_rate("Friday", "Children <5 Room", "ifc_rate", 0) + 
        get_rate("Friday", "Children <5 Room", "gratuity", 0)
      FRIDAY_CAMPING_COST <- get_rate("Friday", "Adults 21+ Camping", "ifc_rate", 150) + 
        get_rate("Friday", "Adults 21+ Camping", "gratuity", 15)
      
      FRIDAY_ADULTS_ROOM_GUEST <- get_rate("Friday", "Adults 21+ Room", "per_guest_charge", 108)
      FRIDAY_CAMPING_GUEST <- get_rate("Friday", "Adults 21+ Camping", "per_guest_charge", 72)
    }
    
    if (saturday_rate_found) {
      SATURDAY_ADULTS_ROOM_COST <- get_rate("Saturday", "Adults 21+ Room", "ifc_rate", 225) + 
        get_rate("Saturday", "Adults 21+ Room", "gratuity", 22.5)
      SATURDAY_TEEN_ROOM_COST <- get_rate("Saturday", "Guests 12-21 Room", "ifc_rate", 225) + 
        get_rate("Saturday", "Guests 12-21 Room", "gratuity", 22.5)
      SATURDAY_CHILD_ROOM_COST <- get_rate("Saturday", "Children 5-12 Room", "ifc_rate", 175) + 
        get_rate("Saturday", "Children 5-12 Room", "gratuity", 17.5)
      SATURDAY_INFANT_ROOM_COST <- get_rate("Saturday", "Children <5 Room", "ifc_rate", 0) + 
        get_rate("Saturday", "Children <5 Room", "gratuity", 0)
      SATURDAY_CAMPING_COST <- get_rate("Saturday", "Adults 21+ Camping", "ifc_rate", 150) + 
        get_rate("Saturday", "Adults 21+ Camping", "gratuity", 15)
      
      SATURDAY_ADULTS_ROOM_GUEST <- get_rate("Saturday", "Adults 21+ Room", "per_guest_charge", 172)
      SATURDAY_CAMPING_GUEST <- get_rate("Saturday", "Adults 21+ Camping", "per_guest_charge", 90)
    }
    
    if (sunday_rate_found) {
      SUNDAY_ADULTS_ROOM_COST <- get_rate("Sunday", "Adults 21+ Room", "ifc_rate", 109) + 
        get_rate("Sunday", "Adults 21+ Room", "gratuity", 10.9)
      SUNDAY_TEEN_ROOM_COST <- get_rate("Sunday", "Guests 12-21 Room", "ifc_rate", 109) + 
        get_rate("Sunday", "Guests 12-21 Room", "gratuity", 10.9)
      SUNDAY_CHILD_ROOM_COST <- get_rate("Sunday", "Children 5-12 Room", "ifc_rate", 69) + 
        get_rate("Sunday", "Children 5-12 Room", "gratuity", 6.9)
      SUNDAY_INFANT_ROOM_COST <- get_rate("Sunday", "Children <5 Room", "ifc_rate", 0) + 
        get_rate("Sunday", "Children <5 Room", "gratuity", 0)
      SUNDAY_CAMPING_COST <- get_rate("Sunday", "Adults 21+ Camping", "ifc_rate", 75) + 
        get_rate("Sunday", "Adults 21+ Camping", "gratuity", 7.5)
      
      SUNDAY_ADULTS_ROOM_GUEST <- get_rate("Sunday", "Adults 21+ Room", "per_guest_charge", 18)
      SUNDAY_CAMPING_GUEST <- get_rate("Sunday", "Adults 21+ Camping", "per_guest_charge", 18)
    }
  }
  
  # Now calculate costs based on age category, accommodation type, and nights
  # This approach does not rely on rates.R functions
  guests <- guests %>%
    mutate(
      # Initialize with zero costs
      friday_cost = 0,
      saturday_cost = 0, 
      sunday_cost = 0,
      friday_guest_charge = 0,
      saturday_guest_charge = 0,
      sunday_guest_charge = 0,
      friday_host_charge = 0,
      saturday_host_charge = 0,
      sunday_host_charge = 0,
      
      # Calculate Friday costs
      friday_cost = case_when(
        !is_staying_friday ~ 0,
        age_category == "Adults 21+ Room" ~ FRIDAY_ADULTS_ROOM_COST,
        age_category == "Guests 12-21 Room" ~ FRIDAY_TEEN_ROOM_COST,
        age_category == "Children 5-12 Room" ~ FRIDAY_CHILD_ROOM_COST,
        age_category == "Children <5 Room" ~ FRIDAY_INFANT_ROOM_COST,
        age_category == "Adults 21+ Camping" ~ FRIDAY_CAMPING_COST,
        TRUE ~ 0
      ),
      
      # Friday guest charges - ZERO for under 21
      friday_guest_charge = case_when(
        !is_staying_friday ~ 0,
        age_category == "Adults 21+ Room" ~ FRIDAY_ADULTS_ROOM_GUEST,
        age_category == "Adults 21+ Camping" ~ FRIDAY_CAMPING_GUEST,
        TRUE ~ UNDER_21_GUEST_CHARGE  # All under-21 guest charges are ZERO
      ),
      
      # Friday host charges - difference between cost and guest charge
      friday_host_charge = friday_cost - friday_guest_charge,
      
      # Calculate Saturday costs
      saturday_cost = case_when(
        !is_staying_saturday ~ 0,
        age_category == "Adults 21+ Room" ~ SATURDAY_ADULTS_ROOM_COST,
        age_category == "Guests 12-21 Room" ~ SATURDAY_TEEN_ROOM_COST,
        age_category == "Children 5-12 Room" ~ SATURDAY_CHILD_ROOM_COST,
        age_category == "Children <5 Room" ~ SATURDAY_INFANT_ROOM_COST,
        age_category == "Adults 21+ Camping" ~ SATURDAY_CAMPING_COST,
        TRUE ~ 0
      ),
      
      # Saturday guest charges - ZERO for under 21
      saturday_guest_charge = case_when(
        !is_staying_saturday ~ 0,
        age_category == "Adults 21+ Room" ~ SATURDAY_ADULTS_ROOM_GUEST,
        age_category == "Adults 21+ Camping" ~ SATURDAY_CAMPING_GUEST,
        TRUE ~ UNDER_21_GUEST_CHARGE  # All under-21 guest charges are ZERO
      ),
      
      # Saturday host charges - difference between cost and guest charge
      saturday_host_charge = saturday_cost - saturday_guest_charge,
      
      # Calculate Sunday costs
      sunday_cost = case_when(
        !is_staying_sunday ~ 0,
        age_category == "Adults 21+ Room" ~ SUNDAY_ADULTS_ROOM_COST,
        age_category == "Guests 12-21 Room" ~ SUNDAY_TEEN_ROOM_COST,
        age_category == "Children 5-12 Room" ~ SUNDAY_CHILD_ROOM_COST,
        age_category == "Children <5 Room" ~ SUNDAY_INFANT_ROOM_COST,
        age_category == "Adults 21+ Camping" ~ SUNDAY_CAMPING_COST,
        TRUE ~ 0
      ),
      
      # Sunday guest charges - ZERO for under 21
      sunday_guest_charge = case_when(
        !is_staying_sunday ~ 0,
        age_category == "Adults 21+ Room" ~ SUNDAY_ADULTS_ROOM_GUEST,
        age_category == "Adults 21+ Camping" ~ SUNDAY_CAMPING_GUEST,
        TRUE ~ UNDER_21_GUEST_CHARGE  # All under-21 guest charges are ZERO
      ),
      
      # Sunday host charges - difference between cost and guest charge
      sunday_host_charge = sunday_cost - sunday_guest_charge,
      
      # Calculate totals
      total_cost = friday_cost + saturday_cost + sunday_cost,
      total_guest_charge = friday_guest_charge + saturday_guest_charge + sunday_guest_charge,
      total_host_charge = friday_host_charge + saturday_host_charge + sunday_host_charge
    )
  
  # Add vegetarian and special diet flags for meal planning
  guests <- guests %>%
    mutate(
      is_vegetarian = meal_preferences == "No meat" | 
        meal_preferences == "Opt-in for fish only",
      has_special_diet = !is.na(dietary_restrictions) & 
        dietary_restrictions != ""
    )
  
  # Verify the calculation - especially for under-21 guests
  under_21_guests <- grepl("Children|Guests 12-21", guests$age_category)
  num_under_21 <- sum(under_21_guests)
  
  if (num_under_21 > 0) {
    cat("Found", num_under_21, "guests under 21 years old\n")
    
    # Check for zero guest charges
    zero_guest_charges <- sum(guests[under_21_guests, "total_guest_charge"] == 0)
    cat("Under-21 guests with zero guest charges:", zero_guest_charges, "\n")
    
    # Check for matching host and total charges
    matching_host_charges <- sum(guests[under_21_guests, "total_host_charge"] == guests[under_21_guests, "total_cost"])
    cat("Under-21 guests with host charge = total cost:", matching_host_charges, "\n")
    
    if (zero_guest_charges < num_under_21 || matching_host_charges < num_under_21) {
      cat("WARNING: Some under-21 guests have incorrect charges!\n")
      
      # Force-fix any incorrect charges
      guests[under_21_guests, "friday_guest_charge"] <- 0
      guests[under_21_guests, "saturday_guest_charge"] <- 0
      guests[under_21_guests, "sunday_guest_charge"] <- 0
      guests[under_21_guests, "total_guest_charge"] <- 0
      
      guests[under_21_guests, "friday_host_charge"] <- guests[under_21_guests, "friday_cost"]
      guests[under_21_guests, "saturday_host_charge"] <- guests[under_21_guests, "saturday_cost"]
      guests[under_21_guests, "sunday_host_charge"] <- guests[under_21_guests, "sunday_cost"]
      guests[under_21_guests, "total_host_charge"] <- guests[under_21_guests, "total_cost"]
      
      cat("Fixed all under-21 guest charges\n")
    } else {
      cat("All under-21 guest charges are correct\n")
    }
  }
  
  # Create accommodation summary
  accommodation_summary <- guests %>%
    summarize(
      total_guests = n(),
      friday_count = sum(is_staying_friday, na.rm = TRUE),
      saturday_count = sum(is_staying_saturday, na.rm = TRUE),
      sunday_count = sum(is_staying_sunday, na.rm = TRUE),
      camping_count = sum(is_camping, na.rm = TRUE),
      standard_lodging_count = sum((is_staying_friday | is_staying_saturday) & 
                                     !is_camping, na.rm = TRUE),
      
      # Cost totals
      total_friday_cost = sum(friday_cost, na.rm = TRUE),
      total_saturday_cost = sum(saturday_cost, na.rm = TRUE),
      total_sunday_cost = sum(sunday_cost, na.rm = TRUE),
      grand_total_cost = sum(total_cost, na.rm = TRUE),
      
      # Guest charge totals
      total_friday_guest_charge = sum(friday_guest_charge, na.rm = TRUE),
      total_saturday_guest_charge = sum(saturday_guest_charge, na.rm = TRUE),
      total_sunday_guest_charge = sum(sunday_guest_charge, na.rm = TRUE),
      grand_total_guest_charge = sum(total_guest_charge, na.rm = TRUE),
      
      # Host charge totals
      total_friday_host_charge = sum(friday_host_charge, na.rm = TRUE),
      total_saturday_host_charge = sum(saturday_host_charge, na.rm = TRUE),
      total_sunday_host_charge = sum(sunday_host_charge, na.rm = TRUE),
      grand_total_host_charge = sum(total_host_charge, na.rm = TRUE)
    )
  
  # Age category breakdown
  age_category_summary <- guests %>%
    group_by(age_category) %>%
    summarize(
      total_guests = n(),
      friday_count = sum(is_staying_friday, na.rm = TRUE),
      saturday_count = sum(is_staying_saturday, na.rm = TRUE),
      sunday_count = sum(is_staying_sunday, na.rm = TRUE),
      total_cost = sum(total_cost, na.rm = TRUE),
      total_guest_charge = sum(total_guest_charge, na.rm = TRUE),
      total_host_charge = sum(total_host_charge, na.rm = TRUE)
    )
  
  return(list(
    guest_costs = guests,
    summary = accommodation_summary,
    age_category_summary = age_category_summary
  ))
}

# Function to create a concise summary for quick insights
create_concise_summary <- function(results) {
  # Get key metrics
  rsvp_counts <- results$guests %>%
    summarize(
      total_invited = n(),
      wedding_attending = sum(wedding_rsvp == "Joyfully Accept", na.rm = TRUE),
      wedding_declining = sum(wedding_rsvp == "Regretfully Decline", na.rm = TRUE),
      wedding_no_response = sum(is.na(wedding_rsvp) | wedding_rsvp == ""),
      friday_attending = sum(fridayshabbat_rsvp == "Joyfully Accept", na.rm = TRUE)
    )
  
  # Accommodation summary
  acc_summary <- results$accommodation_summary
  
  # Calculate total by category
  total_lodging <- acc_summary$total_friday_cost + acc_summary$total_saturday_cost
  
  total_meals <- acc_summary$total_sunday_cost
  
  # Create summary data frame
  summary_df <- data.frame(
    Category = c(
      "Total Invited Guests", "RSVP'd Yes", "RSVP'd No", "No Response",
      "Attending Friday", "Staying Friday Night", "Staying Saturday Night", "Staying Sunday Night",
      "Using Standard Lodging", "Camping", 
      "Total Lodging Cost", "Total Meal Cost", "Total IFC Cost",
      "Total Guest Charge", "Total Host Charge"
    ),
    Count = c(
      rsvp_counts$total_invited, rsvp_counts$wedding_attending, 
      rsvp_counts$wedding_declining, rsvp_counts$wedding_no_response,
      rsvp_counts$friday_attending, 
      acc_summary$friday_count, acc_summary$saturday_count, acc_summary$sunday_count,
      acc_summary$standard_lodging_count, acc_summary$camping_count,
      paste0("$", format(total_lodging, big.mark = ",")), 
      paste0("$", format(total_meals, big.mark = ",")),
      paste0("$", format(acc_summary$grand_total_cost, big.mark = ",")),
      paste0("$", format(acc_summary$grand_total_guest_charge, big.mark = ",")),
      paste0("$", format(acc_summary$grand_total_host_charge, big.mark = ","))
    )
  )
  
  return(summary_df)
}

# Function to validate guest data for quality checks
validate_guest_data <- function(guests) {
  issues <- list()
  
  # Check for missing required fields
  missing_name <- is.na(guests$first_name) | guests$first_name == ""
  if (any(missing_name)) {
    issues$missing_names <- which(missing_name)
  }
  
  # Check for email format issues
  email_cols <- grep("email", names(guests), ignore.case = TRUE, value = TRUE)
  if (length(email_cols) > 0) {
    email_col <- email_cols[1]
    email_pattern <- "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"
    invalid_emails <- !is.na(guests[[email_col]]) & 
      guests[[email_col]] != "" & 
      !grepl(email_pattern, guests[[email_col]])
    if (any(invalid_emails)) {
      issues$invalid_emails <- which(invalid_emails)
    }
  }
  
  # Check for response inconsistencies
  if ("wedding_rsvp" %in% names(guests) && "fridayshabbat_rsvp" %in% names(guests)) {
    # Attending Friday but not wedding
    inconsistent_friday <- guests$fridayshabbat_rsvp == "Joyfully Accept" & 
      guests$wedding_rsvp == "Regretfully Decline" & 
      !is.na(guests$fridayshabbat_rsvp) & 
      !is.na(guests$wedding_rsvp)
    if (any(inconsistent_friday)) {
      issues$inconsistent_friday <- which(inconsistent_friday)
    }
  }
  
  return(issues)
}

# Improved function to count meals for all guests
count_meals <- function(guests) {
  # Get list of stay columns to check
  possible_friday_cols <- c("friday_ifc_stay_1", "ifc_stay_1", "friday_ifc_stay")
  possible_saturday_cols <- c("saturday_ifc_stay_1", "saturday_ifc_stay", "ifc_stay_1")
  possible_sunday_cols <- c("sunday_ifc_stay_1", "sunday_ifc_stay")
  
  # IMPORTANT FIX: First define all attendance variables before using them in calculations
  all_guests_with_attendance <- guests %>%
    mutate(
      # General attendance
      attending_wedding = wedding_rsvp == "Joyfully Accept",
      attending_friday = fridayshabbat_rsvp == "Joyfully Accept",
      
      # Saturday meal attendance (for off-site guests) - look for specific responses
      attending_saturday_lunch_only = saturday_offsite_rsvp == "Yes, I will join for lunch only",
      attending_saturday_dinner_only = saturday_offsite_rsvp == "Yes, I will join for dinner only",
      attending_saturday_both_meals = saturday_offsite_rsvp == "Yes, I will join for lunch and dinner"
    )
  
  # Get age categories if available
  has_age_categories <- "age_category" %in% names(all_guests_with_attendance)
  
  if (has_age_categories) {
    meal_summary <- all_guests_with_attendance %>%
      group_by(age_category) %>%
      summarize(
        # Meals for on-site guests
        friday_dinner_onsite = sum(is_staying_friday, na.rm = TRUE),
        saturday_breakfast_onsite = sum(is_staying_friday, na.rm = TRUE),
        saturday_lunch_onsite = sum(is_staying_saturday, na.rm = TRUE),
        saturday_dinner_onsite = sum(is_staying_saturday, na.rm = TRUE),
        sunday_breakfast_onsite = sum(is_staying_saturday, na.rm = TRUE),
        sunday_dinner_onsite = sum(is_staying_sunday, na.rm = TRUE),
        monday_breakfast_onsite = sum(is_staying_sunday, na.rm = TRUE),
        
        # Meals for off-site guests
        friday_dinner_offsite = sum(attending_friday & !is_staying_friday, na.rm = TRUE),
        saturday_lunch_offsite = sum(attending_saturday_lunch_only | attending_saturday_both_meals, na.rm = TRUE),
        saturday_dinner_offsite = sum(attending_saturday_dinner_only | attending_saturday_both_meals, na.rm = TRUE),
        
        # Wedding reception meal (all attending)
        sunday_lunch_total = sum(attending_wedding, na.rm = TRUE)
      )
    
    # Calculate totals across all age categories
    meal_counts <- meal_summary %>%
      ungroup() %>%
      summarize(
        # Meals for on-site guests
        friday_dinner_onsite = sum(friday_dinner_onsite, na.rm = TRUE),
        saturday_breakfast_onsite = sum(saturday_breakfast_onsite, na.rm = TRUE),
        saturday_lunch_onsite = sum(saturday_lunch_onsite, na.rm = TRUE),
        saturday_dinner_onsite = sum(saturday_dinner_onsite, na.rm = TRUE),
        sunday_breakfast_onsite = sum(sunday_breakfast_onsite, na.rm = TRUE),
        sunday_dinner_onsite = sum(sunday_dinner_onsite, na.rm = TRUE),
        monday_breakfast_onsite = sum(monday_breakfast_onsite, na.rm = TRUE),
        
        # Meals for off-site guests
        friday_dinner_offsite = sum(friday_dinner_offsite, na.rm = TRUE),
        saturday_lunch_offsite = sum(saturday_lunch_offsite, na.rm = TRUE),
        saturday_dinner_offsite = sum(saturday_dinner_offsite, na.rm = TRUE),
        
        # Wedding reception meal (all attending)
        sunday_lunch_total = sum(sunday_lunch_total, na.rm = TRUE),
        
        # Total counts (for planning)
        total_friday_dinner = sum(friday_dinner_onsite + friday_dinner_offsite, na.rm = TRUE),
        total_saturday_breakfast = sum(saturday_breakfast_onsite, na.rm = TRUE),
        total_saturday_lunch = sum(saturday_lunch_onsite + saturday_lunch_offsite, na.rm = TRUE),
        total_saturday_dinner = sum(saturday_dinner_onsite + saturday_dinner_offsite, na.rm = TRUE),
        total_sunday_breakfast = sum(sunday_breakfast_onsite, na.rm = TRUE),
        total_sunday_lunch = sum(sunday_lunch_total, na.rm = TRUE),
        total_sunday_dinner = sum(sunday_dinner_onsite, na.rm = TRUE),
        total_monday_breakfast = sum(monday_breakfast_onsite, na.rm = TRUE)
      )
    
    # FIX: Store meal_summary as a list to avoid the error with row count mismatch
    meal_counts$by_age_category <- list(meal_summary)
  } else {
    # Without age categories
    meal_counts <- all_guests_with_attendance %>%
      summarize(
        # Meals for on-site guests
        friday_dinner_onsite = sum(is_staying_friday, na.rm = TRUE),
        saturday_breakfast_onsite = sum(is_staying_friday, na.rm = TRUE),
        saturday_lunch_onsite = sum(is_staying_saturday, na.rm = TRUE),
        saturday_dinner_onsite = sum(is_staying_saturday, na.rm = TRUE),
        sunday_breakfast_onsite = sum(is_staying_saturday, na.rm = TRUE),
        sunday_dinner_onsite = sum(is_staying_sunday, na.rm = TRUE),
        monday_breakfast_onsite = sum(is_staying_sunday, na.rm = TRUE),
        
        # Meals for off-site guests
        friday_dinner_offsite = sum(attending_friday & !is_staying_friday, na.rm = TRUE),
        saturday_lunch_offsite = sum(attending_saturday_lunch_only | attending_saturday_both_meals, na.rm = TRUE),
        saturday_dinner_offsite = sum(attending_saturday_dinner_only | attending_saturday_both_meals, na.rm = TRUE),
        
        # Wedding reception meal (all attending)
        sunday_lunch_total = sum(attending_wedding, na.rm = TRUE),
        
        # Total counts (for planning)
        total_friday_dinner = sum(sum(is_staying_friday, na.rm = TRUE) + 
                                    sum(attending_friday & !is_staying_friday, na.rm = TRUE), na.rm = TRUE),
        total_saturday_breakfast = sum(is_staying_friday, na.rm = TRUE),
        total_saturday_lunch = sum(sum(is_staying_saturday, na.rm = TRUE) + 
                                     sum(attending_saturday_lunch_only | attending_saturday_both_meals, na.rm = TRUE), 
                                   na.rm = TRUE),
        total_saturday_dinner = sum(sum(is_staying_saturday, na.rm = TRUE) + 
                                      sum(attending_saturday_dinner_only | attending_saturday_both_meals, na.rm = TRUE), 
                                    na.rm = TRUE),
        total_sunday_breakfast = sum(is_staying_saturday, na.rm = TRUE),
        total_sunday_lunch = sum(attending_wedding, na.rm = TRUE),
        total_sunday_dinner = sum(is_staying_sunday, na.rm = TRUE),
        total_monday_breakfast = sum(is_staying_sunday, na.rm = TRUE)
      )
  }
  
  # Add dietary breakdowns for each meal
  veg_count <- sum(all_guests_with_attendance$is_vegetarian, na.rm = TRUE)
  
  meal_counts$friday_dinner_vegetarian <- veg_count
  meal_counts$saturday_breakfast_vegetarian <- veg_count
  meal_counts$saturday_lunch_vegetarian <- veg_count
  meal_counts$saturday_dinner_vegetarian <- veg_count
  meal_counts$sunday_breakfast_vegetarian <- veg_count
  meal_counts$sunday_lunch_vegetarian <- veg_count
  meal_counts$sunday_dinner_vegetarian <- veg_count
  meal_counts$monday_breakfast_vegetarian <- veg_count
  
  # Add special diet counts
  special_diets <- sum(all_guests_with_attendance$has_special_diet, na.rm = TRUE)
  
  meal_counts$friday_dinner_special_diet <- special_diets
  meal_counts$saturday_breakfast_special_diet <- special_diets
  meal_counts$saturday_lunch_special_diet <- special_diets
  meal_counts$saturday_dinner_special_diet <- special_diets
  meal_counts$sunday_breakfast_special_diet <- special_diets
  meal_counts$sunday_lunch_special_diet <- special_diets
  meal_counts$sunday_dinner_special_diet <- special_diets
  meal_counts$monday_breakfast_special_diet <- special_diets
  
  return(meal_counts)
}

# Function to count meals by age category
count_meals_by_age <- function(guests) {
  # Get list of stay columns to check
  possible_friday_cols <- c("friday_ifc_stay_1", "ifc_stay_1", "friday_ifc_stay")
  possible_saturday_cols <- c("saturday_ifc_stay_1", "saturday_ifc_stay", "ifc_stay_1")
  possible_sunday_cols <- c("sunday_ifc_stay_1", "sunday_ifc_stay")
  
  # First, ensure all guests have the is_staying variables defined
  if (!("is_staying_friday" %in% names(guests))) {
    guests <- guests %>%
      mutate(
        is_staying_friday = rowSums(across(any_of(possible_friday_cols), ~ . == "Yes"), na.rm = TRUE) > 0,
        is_staying_saturday = rowSums(across(any_of(possible_saturday_cols), ~ . == "Yes"), na.rm = TRUE) > 0,
        is_staying_sunday = rowSums(across(any_of(possible_sunday_cols), ~ . == "Yes"), na.rm = TRUE) > 0
      )
  }
  
  # Next, add the attendance variables
  guests_with_stays <- guests %>%
    mutate(
      # General attendance
      attending_wedding = wedding_rsvp == "Joyfully Accept",
      attending_friday = fridayshabbat_rsvp == "Joyfully Accept",
      
      # Saturday meal attendance (for off-site guests)
      attending_saturday_lunch_only = saturday_offsite_rsvp == "Yes, I will join for lunch only",
      attending_saturday_dinner_only = saturday_offsite_rsvp == "Yes, I will join for dinner only",
      attending_saturday_both_meals = saturday_offsite_rsvp == "Yes, I will join for lunch and dinner",
      
      # Vegetarian flag
      is_vegetarian = meal_preferences == "No meat" | meal_preferences == "Opt-in for fish only",
      
      # Special diet flag
      has_special_diet = !is.na(dietary_restrictions) & dietary_restrictions != ""
    )
  
  # Function to count guests for a specific meal and age category
  count_by_age <- function(data, meal_condition) {
    data %>%
      filter(!!rlang::parse_expr(meal_condition)) %>%
      group_by(age_category) %>%
      summarize(
        count = n(),
        veg_count = sum(is_vegetarian, na.rm = TRUE),
        meat_fish_count = sum(!is_vegetarian, na.rm = TRUE),
        special_diet_count = sum(has_special_diet, na.rm = TRUE),
        .groups = 'drop'
      )
  }
  
  # Create counts for each meal by age category
  friday_dinner <- count_by_age(guests_with_stays, "is_staying_friday | (attending_friday & !is_staying_friday)")
  saturday_breakfast <- count_by_age(guests_with_stays, "is_staying_friday")
  saturday_lunch <- count_by_age(guests_with_stays, "is_staying_saturday | attending_saturday_lunch_only | attending_saturday_both_meals")
  saturday_dinner <- count_by_age(guests_with_stays, "is_staying_saturday | attending_saturday_dinner_only | attending_saturday_both_meals")
  sunday_breakfast <- count_by_age(guests_with_stays, "is_staying_saturday")
  sunday_lunch <- count_by_age(guests_with_stays, "attending_wedding")
  sunday_dinner <- count_by_age(guests_with_stays, "is_staying_sunday")
  monday_breakfast <- count_by_age(guests_with_stays, "is_staying_sunday")
  
  # Create a comprehensive meal planning table
  all_meals <- bind_rows(
    mutate(friday_dinner, meal = "Friday Dinner"),
    mutate(saturday_breakfast, meal = "Saturday Breakfast"),
    mutate(saturday_lunch, meal = "Saturday Lunch"),
    mutate(saturday_dinner, meal = "Saturday Dinner"),
    mutate(sunday_breakfast, meal = "Sunday Breakfast"),
    mutate(sunday_lunch, meal = "Sunday Lunch (Wedding)"),
    mutate(sunday_dinner, meal = "Sunday Dinner"),
    mutate(monday_breakfast, meal = "Monday Breakfast")
  ) %>%
    select(meal, age_category, count, veg_count, meat_fish_count, special_diet_count)
  
  # Also create a long format table for easier aggregation and plotting
  by_age_long <- all_meals %>%
    pivot_longer(cols = c(count, veg_count, meat_fish_count, special_diet_count),
                 names_to = "category",
                 values_to = "value")
  
  # Create a list of guests with dietary restrictions for each meal
  special_diet_guests <- guests_with_stays %>%
    filter(has_special_diet) %>%
    select(
      first_name,
      last_name,
      age_category,
      meal_preferences,
      dietary_restrictions
    )
  
  # Return the meal counts by age and the comprehensive meal plan
  return(list(
    friday_dinner = friday_dinner,
    saturday_breakfast = saturday_breakfast,
    saturday_lunch = saturday_lunch,
    saturday_dinner = saturday_dinner,
    sunday_breakfast = sunday_breakfast,
    sunday_lunch = sunday_lunch,
    sunday_dinner = sunday_dinner,
    monday_breakfast = monday_breakfast,
    all_meals = all_meals,
    by_age_long = by_age_long,
    special_diet_guests = special_diet_guests
  ))
}

# Function to generate a schedule/roster for IFC
generate_ifc_schedule <- function(guests_with_costs) {
  # Create a comprehensive schedule of all stays and meals
  ifc_schedule <- guests_with_costs %>%
    filter(is_staying_friday | is_staying_saturday | is_staying_sunday) %>%
    mutate(
      # Add meal attendance flags
      friday_dinner = is_staying_friday,
      saturday_breakfast = is_staying_friday,
      saturday_lunch = is_staying_saturday,
      saturday_dinner = is_staying_saturday,
      sunday_breakfast = is_staying_saturday,
      sunday_lunch = wedding_rsvp == "Joyfully Accept",
      sunday_dinner = is_staying_sunday,
      monday_breakfast = is_staying_sunday
    ) %>%
    select(
      first_name,
      last_name,
      party,
      age_category,
      is_staying_friday,
      is_staying_saturday,
      is_staying_sunday,
      is_camping,
      friday_dinner,
      saturday_breakfast,
      saturday_lunch,
      saturday_dinner,
      sunday_breakfast,
      sunday_lunch,
      sunday_dinner,
      monday_breakfast,
      meal_preferences,
      dietary_restrictions,
      is_vegetarian,
      has_special_diet,
      # Add financial breakdown
      total_cost,
      total_guest_charge,
      total_host_charge,
      # Add detailed costs
      friday_cost,
      saturday_cost, 
      sunday_cost,
      friday_guest_charge,
      saturday_guest_charge,
      sunday_guest_charge,
      friday_host_charge,
      saturday_host_charge,
      sunday_host_charge
    )
  
  return(ifc_schedule)
}

# Generate IFC roster with all requested details
generate_ifc_roster <- function(guests) {
  # Filter for overnight guests only
  ifc_guests <- guests %>%
    filter(is_staying_friday | is_staying_saturday | is_staying_sunday) %>%
    mutate(
      # Add meal attendance flags
      friday_dinner = is_staying_friday,
      saturday_breakfast = is_staying_friday,
      saturday_lunch = is_staying_saturday,
      saturday_dinner = is_staying_saturday,
      sunday_breakfast = is_staying_saturday,
      sunday_lunch = wedding_rsvp == "Joyfully Accept",
      sunday_dinner = is_staying_sunday,
      monday_breakfast = is_staying_sunday,
      
      # Add accommodation type
      accommodation_type = ifelse(is_camping, "Camping", "Standard Lodging")
    )
  
  # Select all relevant columns for a complete roster
  columns_to_include <- c(
    # Basic guest info
    "first_name", "last_name", "party", 
    
    # Age category if available
    "age_category",
    
    # Stay information
    "is_staying_friday", "is_staying_saturday", "is_staying_sunday", 
    "is_camping", "accommodation_type",
    
    # Meal attendance
    "friday_dinner", "saturday_breakfast", "saturday_lunch", "saturday_dinner",
    "sunday_breakfast", "sunday_lunch", "sunday_dinner", "monday_breakfast",
    
    # Dietary preferences
    "meal_preferences", "dietary_restrictions", "is_vegetarian", "has_special_diet",
    
    # Financial information
    "total_cost", "total_guest_charge", "total_host_charge",
    "friday_cost", "friday_guest_charge", "friday_host_charge",
    "saturday_cost", "saturday_guest_charge", "saturday_host_charge",
    "sunday_cost", "sunday_guest_charge", "sunday_host_charge"
  )
  
  # Only include columns that actually exist in the data
  columns_to_include <- columns_to_include[columns_to_include %in% names(ifc_guests)]
  
  # Create the final roster with all available columns
  ifc_roster <- ifc_guests %>%
    select(all_of(columns_to_include))
  
  return(ifc_roster)
}

# Generate meal roster by age category
# Fixed generate_meal_roster_by_age function
generate_meal_roster_by_age <- function(results) {
  guests <- results$guests
  
  # Add attendance variables if they don't exist
  if (!("attending_wedding" %in% names(guests))) {
    guests <- guests %>%
      mutate(
        # General attendance
        attending_wedding = wedding_rsvp == "Joyfully Accept",
        attending_friday = fridayshabbat_rsvp == "Joyfully Accept",
        
        # Saturday meal attendance (for off-site guests)
        attending_saturday_lunch_only = saturday_offsite_rsvp == "Yes, I will join for lunch only",
        attending_saturday_dinner_only = saturday_offsite_rsvp == "Yes, I will join for dinner only",
        attending_saturday_both_meals = saturday_offsite_rsvp == "Yes, I will join for lunch and dinner"
      )
  }
  
  # Define meal list
  meal_list <- c("friday_dinner", "saturday_breakfast", "saturday_lunch", 
                 "saturday_dinner", "sunday_breakfast", "sunday_lunch", 
                 "sunday_dinner", "monday_breakfast")
  
  meal_names <- c("Friday Dinner", "Saturday Breakfast", "Saturday Lunch", 
                  "Saturday Dinner", "Sunday Breakfast", "Sunday Lunch (Wedding)", 
                  "Sunday Dinner", "Monday Breakfast")
  
  # Create empty results list
  meal_rosters <- list()
  
  # For each meal, find attendees and their details
  for (i in 1:length(meal_list)) {
    meal_code <- meal_list[i]
    meal_name <- meal_names[i]
    
    # Determine which guests attend this meal
    attendees <- switch(meal_code,
                        "friday_dinner" = guests %>% filter(is_staying_friday | 
                                                              (attending_friday & !is_staying_friday)),
                        "saturday_breakfast" = guests %>% filter(is_staying_friday),
                        "saturday_lunch" = guests %>% filter(is_staying_saturday | 
                                                               attending_saturday_lunch_only | 
                                                               attending_saturday_both_meals),
                        "saturday_dinner" = guests %>% filter(is_staying_saturday | 
                                                                attending_saturday_dinner_only | 
                                                                attending_saturday_both_meals),
                        "sunday_breakfast" = guests %>% filter(is_staying_saturday),
                        "sunday_lunch" = guests %>% filter(attending_wedding),
                        "sunday_dinner" = guests %>% filter(is_staying_sunday),
                        "monday_breakfast" = guests %>% filter(is_staying_sunday),
                        guests %>% filter(FALSE) # Empty default
    )
    
    # Group by age category and count dietary preferences
    if (nrow(attendees) > 0) {
      meal_summary <- attendees %>%
        group_by(age_category) %>%
        summarize(
          total_guests = n(),
          vegetarian_count = sum(is_vegetarian, na.rm = TRUE),
          meat_fish_count = sum(!is_vegetarian, na.rm = TRUE),
          special_diet_count = sum(has_special_diet, na.rm = TRUE),
          .groups = 'drop'
        )
      
      # List of guests with special diets for this meal
      special_diets <- attendees %>%
        filter(has_special_diet) %>%
        select(first_name, last_name, age_category, meal_preferences, dietary_restrictions)
      
      meal_rosters[[meal_code]] <- list(
        meal_name = meal_name,
        attendees = attendees,
        summary = meal_summary,
        special_diets = special_diets
      )
    }
  }
  
  return(meal_rosters)
}

# Main function to generate all reports
generate_wedding_reports <- function(file_path, age_data_path = "Wedding Budget Invite List.csv", 
                                     rates_path = "charge_rates.csv") {
  # Read guest list
  guests <- read_guest_list(file_path)
  
  # Generate party summary
  party_summary <- generate_party_summary(guests)
  
  # Calculate accommodation costs (with age categories and financial breakdown)
  accommodation_results <- calculate_accommodation_costs(guests, age_data_path, rates_path)
  
  # Count meals for all guests with improved function
  meal_counts <- count_meals(accommodation_results$guest_costs)
  
  # Count meals by age category
  meal_counts_by_age <- count_meals_by_age(accommodation_results$guest_costs)
  
  # Generate IFC schedule/roster
  ifc_schedule <- generate_ifc_schedule(accommodation_results$guest_costs)
  
  # Generate comprehensive IFC roster
  ifc_roster <- generate_ifc_roster(accommodation_results$guest_costs)
  
  # Run data validation
  data_issues <- validate_guest_data(guests)
  
  # Generate meal rosters by age category
  meal_rosters <- generate_meal_roster_by_age(list(guests = accommodation_results$guest_costs))
  
  # Print summary reports
  cat("\n=== WEDDING RSVP SUMMARY ===\n")
  print(party_summary)
  
  cat("\n=== ACCOMMODATION SUMMARY ===\n")
  print(accommodation_results$summary)
  
  cat("\n=== AGE CATEGORY SUMMARY ===\n")
  print(accommodation_results$age_category_summary)
  
  cat("\n=== MEAL COUNTS (Detailed) ===\n")
  print(meal_counts)
  
  # Print any data quality issues
  if (length(data_issues) > 0) {
    cat("\n=== DATA QUALITY ISSUES ===\n")
    print(data_issues)
  }
  
  # Return all results as a list
  return(list(
    guests = accommodation_results$guest_costs,
    party_summary = party_summary,
    guest_costs = accommodation_results$guest_costs,
    accommodation_summary = accommodation_results$summary,
    age_category_summary = accommodation_results$age_category_summary,
    meal_counts = meal_counts,
    meal_counts_by_age = meal_counts_by_age,
    meal_rosters = meal_rosters,
    ifc_roster = ifc_roster,
    ifc_schedule = ifc_schedule,
    data_issues = data_issues
  ))
}

# Additional function to export results to CSV files
export_reports <- function(results, output_dir = ".") {
  # SIMPLIFIED: Force directory creation
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  cat("Ensuring report directory exists:", output_dir, "\n")
  
  # Export party summary
  write_csv(results$party_summary, file.path(output_dir, "party_summary.csv"))
  
  # Export guest costs
  write_csv(results$guest_costs, file.path(output_dir, "guest_accommodation_costs.csv"))
  
  # Export accommodation summary as a simple CSV
  accommodation_df <- as.data.frame(t(as.matrix(results$accommodation_summary)))
  accommodation_df$metric <- rownames(accommodation_df)
  accommodation_df <- accommodation_df[, c(ncol(accommodation_df), 1:(ncol(accommodation_df)-1))]
  names(accommodation_df)[1] <- "metric"
  write_csv(accommodation_df, file.path(output_dir, "accommodation_summary.csv"))
  
  # Export age category summary
  write_csv(results$age_category_summary, file.path(output_dir, "age_category_summary.csv"))
  
  # Export meal counts as a simple CSV
  meal_df <- as.data.frame(t(as.matrix(results$meal_counts)))
  meal_df$meal <- rownames(meal_df)
  meal_df <- meal_df[, c(ncol(meal_df), 1:(ncol(meal_df)-1))]
  names(meal_df)[1] <- "meal"
  write_csv(meal_df, file.path(output_dir, "meal_counts.csv"))
  
  # Export meal counts by age category
  if (!is.null(results$meal_counts_by_age)) {
    # Create a directory for age-specific reports
    age_dir <- file.path(output_dir, "age_reports")
    dir.create(age_dir, showWarnings = FALSE, recursive = TRUE)
    
    # Export comprehensive meal planning table
    write_csv(results$meal_counts_by_age$all_meals, 
              file.path(age_dir, "meal_plan_by_age.csv"))
    
    # Export long-format table for plotting
    if (!is.null(results$meal_counts_by_age$by_age_long)) {
      write_csv(results$meal_counts_by_age$by_age_long,
                file.path(age_dir, "meal_plan_by_age_long.csv"))
    }
    
    # Export special diet guests
    write_csv(results$meal_counts_by_age$special_diet_guests, 
              file.path(age_dir, "special_diet_guests.csv"))
    
    # Export each meal's age breakdown
    for (meal_name in names(results$meal_counts_by_age)) {
      if (!meal_name %in% c("all_meals", "special_diet_guests", "by_age_long")) {
        write_csv(
          results$meal_counts_by_age[[meal_name]], 
          file.path(age_dir, paste0(meal_name, "_by_age.csv"))
        )
      }
    }
  }
  
  # Export IFC schedule/roster
  write_csv(results$ifc_schedule, file.path(output_dir, "ifc_schedule.csv"))
  
  # Export IFC roster with full details
  if (!is.null(results$ifc_roster)) {
    write_csv(results$ifc_roster, file.path(output_dir, "ifc_roster.csv"))
  }
  
  # Export meal rosters
  if (!is.null(results$meal_rosters)) {
    meal_dir <- file.path(output_dir, "meal_rosters")
    dir.create(meal_dir, showWarnings = FALSE, recursive = TRUE)
    
    for (meal_code in names(results$meal_rosters)) {
      meal_data <- results$meal_rosters[[meal_code]]
      
      # Export attendee list
      write_csv(meal_data$attendees, 
                file.path(meal_dir, paste0(meal_code, "_attendees.csv")))
      
      # Export summary
      write_csv(meal_data$summary, 
                file.path(meal_dir, paste0(meal_code, "_summary.csv")))
      
      # Export special diets if any
      if (nrow(meal_data$special_diets) > 0) {
        write_csv(meal_data$special_diets, 
                  file.path(meal_dir, paste0(meal_code, "_special_diets.csv")))
      }
    }
  }
  
  # Generate and export concise summary report
  concise_summary <- create_concise_summary(results)
  write_csv(concise_summary, file.path(output_dir, "concise_summary.csv"))
  
  # Export age category details for guests staying each night
  age_by_night <- results$guest_costs %>%
    group_by(age_category) %>%
    summarize(
      friday_count = sum(is_staying_friday, na.rm = TRUE),
      saturday_count = sum(is_staying_saturday, na.rm = TRUE),
      sunday_count = sum(is_staying_sunday, na.rm = TRUE),
      total_guests = n()
    ) %>%
    arrange(age_category)
  
  write_csv(age_by_night, file.path(output_dir, "age_by_night.csv"))
  
  # Export detailed list of guests with age categories
  age_detail <- results$guest_costs %>%
    select(
      first_name,
      last_name,
      party,
      age_category,
      is_staying_friday,
      is_staying_saturday,
      is_staying_sunday,
      is_camping,
      meal_preferences,
      dietary_restrictions,
      is_vegetarian,
      has_special_diet,
      total_cost,
      total_guest_charge,
      total_host_charge
    )
  
  write_csv(age_detail, file.path(output_dir, "age_detail.csv"))
  
  # Create a summary text file for age categories
  age_summary_text <- paste0(
    "AGE CATEGORY SUMMARY\n",
    "Wedding: Cyrena & Jon - June 20-23, 2025\n",
    "Generated: ", format(Sys.Date(), "%B %d, %Y"), "\n\n",
    
    "== Age Category Counts ==\n\n",
    paste(capture.output(print(results$age_category_summary)), collapse = "\n"), "\n\n",
    
    "== Age Categories by Night ==\n\n",
    paste(capture.output(print(age_by_night)), collapse = "\n"), "\n\n",
    
    "== Financial Summary by Age Category ==\n\n",
    "Total IFC Cost: $", sum(results$age_category_summary$total_cost), "\n",
    "Total Guest Charges: $", sum(results$age_category_summary$total_guest_charge), "\n",
    "Total Host Charges: $", sum(results$age_category_summary$total_host_charge), "\n\n",
    
    "== Important Notes ==\n\n",
    "1. Age Categories:\n",
    "   - Adults 21+ Room: Adults staying in standard rooms\n",
    "   - Adults 21+ Camping: Adults staying in camping accommodations\n",
    "   - Guests 12-21 Room: Teenagers and young adults in standard rooms\n",
    "   - Children 5-12 Room: School-age children in standard rooms\n",
    "   - Children <5 Room: Young children and infants in standard rooms\n\n",
    
    "2. Age data has been extracted from the Wedding Budget Invite List.\n",
    "   Guests without matching records were categorized as 'Adults 21+ Room' by default.\n\n",
    
    "3. Financial breakdown:\n",
    "   - IFC Cost: Total amount charged by Isabella Freedman Center\n",
    "   - Guest Charge: Amount guests pay\n",
    "   - Host Charge: Amount hosts pay (difference between IFC cost and guest charge)\n\n",
    
    "4. Children under 5 stay free; children 5-12 are charged at 50% of adult rates.\n"
  )
  
  writeLines(age_summary_text, file.path(output_dir, "age_summary.txt"))
  
  # Create a comprehensive meal summary
  meal_summary_text <- paste0(
    "COMPREHENSIVE MEAL PLANNING SUMMARY\n",
    "Wedding: Cyrena & Jon - June 20-23, 2025\n",
    "Generated: ", format(Sys.Date(), "%B %d, %Y"), "\n\n",
    
    "== Meal Counts Summary ==\n\n",
    "Friday Dinner: ", results$meal_counts$total_friday_dinner, " guests\n",
    "Saturday Breakfast: ", results$meal_counts$total_saturday_breakfast, " guests\n",
    "Saturday Lunch: ", results$meal_counts$total_saturday_lunch, " guests\n",
    "Saturday Dinner: ", results$meal_counts$total_saturday_dinner, " guests\n",
    "Sunday Breakfast: ", results$meal_counts$total_sunday_breakfast, " guests\n",
    "Sunday Lunch (Wedding): ", results$meal_counts$total_sunday_lunch, " guests\n",
    "Sunday Dinner: ", results$meal_counts$total_sunday_dinner, " guests\n",
    "Monday Breakfast: ", results$meal_counts$total_monday_breakfast, " guests\n\n",
    
    "== Dietary Requirements ==\n\n",
    "Vegetarian/Fish Only: ", sum(results$guest_costs$is_vegetarian, na.rm = TRUE), " guests\n",
    "Special Dietary Needs: ", sum(results$guest_costs$has_special_diet, na.rm = TRUE), " guests\n\n",
    
    "== Important Notes ==\n\n",
    "1. Meal Schedule:\n",
    "   - Friday Dinner: 6:00-8:00 PM\n",
    "   - Saturday Breakfast: 7:30-9:30 AM\n",
    "   - Saturday Lunch: 12:00-2:00 PM\n",
    "   - Saturday Dinner: 6:00-8:00 PM\n",
    "   - Sunday Breakfast: 7:30-9:30 AM\n",
    "   - Sunday Lunch (Wedding): 1:00-3:00 PM\n",
    "   - Sunday Dinner: 6:00-8:00 PM\n",
    "   - Monday Breakfast: 7:30-9:30 AM\n\n",
    
    "2. All meals include vegetarian options.\n",
    "3. Special dietary needs have been noted and accommodated.\n",
    "4. Detailed meal reports by age category are available in the age_reports directory.\n"
  )
  
  writeLines(meal_summary_text, file.path(output_dir, "meal_summary.txt"))
  
  cat("Reports exported to:", output_dir, "\n")
}