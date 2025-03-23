# Simplified Wedding RSVP Tracker
# A more efficient implementation of the guest tracking system

# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# Function to read and standardize the guest list data
read_guest_list <- function(file_path) {
  # Read the CSV file
  guests <- read_csv(file_path, 
                     col_types = cols(.default = col_character()),
                     na = c("", "NA", "N/A"))
  
  # Clean column names
  names(guests) <- names(guests) %>%
    str_to_lower() %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("[^a-z0-9_]", "")
  
  # Print column names for debugging
  cat("Available columns in the dataset:\n")
  cat(paste(names(guests), collapse = ", "), "\n")
  
  # Define standardized column names for stay information
  possible_friday_cols <- c("friday_ifc_stay_1", "ifc_stay_1", "friday_ifc_stay")
  possible_saturday_cols <- c("saturday_ifc_stay_1", "saturday_ifc_stay", "ifc_stay_1")
  possible_sunday_cols <- c("sunday_ifc_stay_1", "sunday_ifc_stay")
  possible_camping_cols <- c("lodgingcamping_weekend", "lodgingcamping_sat_only")
  
  # Create standardized variables for stay information
  guests <- guests %>%
    mutate(
      # Set standardized attendance and stay flags
      is_attending_wedding = wedding_rsvp == "Joyfully Accept",
      is_attending_friday = fridayshabbat_rsvp == "Joyfully Accept",
      is_attending_saturday_lunch = saturday_onsite_rsvp == "Yes, I will join on Saturday" | 
        saturday_offsite_rsvp == "Yes, I will join for lunch only" |
        saturday_offsite_rsvp == "Yes, I will join for lunch and dinner",
      is_attending_saturday_dinner = saturday_onsite_rsvp == "Yes, I will join on Saturday" | 
        saturday_offsite_rsvp == "Yes, I will join for dinner only" |
        saturday_offsite_rsvp == "Yes, I will join for lunch and dinner",
      
      # Determine overnight stays
      is_staying_friday = rowSums(across(any_of(possible_friday_cols), ~ . == "Yes"), na.rm = TRUE) > 0,
      is_staying_saturday = rowSums(across(any_of(possible_saturday_cols), ~ . == "Yes"), na.rm = TRUE) > 0,
      is_staying_sunday = rowSums(across(any_of(possible_sunday_cols), ~ . == "Yes"), na.rm = TRUE) > 0,
      
      # Determine camping vs. lodging
      is_camping = rowSums(across(any_of(possible_camping_cols), ~ . == "Camping"), na.rm = TRUE) > 0,
      
      # Determine dietary categories
      is_vegetarian = meal_preferences == "No meat" | meal_preferences == "Opt-in for fish only",
      has_special_diet = !is.na(dietary_restrictions) & dietary_restrictions != ""
    )
  
  return(guests)
}

# Read financial rate information
read_rates <- function(file_path = "charge_rates.csv") {
  if (!file.exists(file_path)) {
    warning("Rate information file not found: ", file_path)
    return(NULL)
  }
  
  rates <- read_csv(file_path, 
                    col_types = cols(.default = col_character()),
                    na = c("", "NA", "N/A"))
  
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

# Function to assign age categories
assign_age_categories <- function(guests, age_data_path = "Wedding Budget Invite List.csv") {
  if (!file.exists(age_data_path)) {
    warning("Age data file not found: ", age_data_path)
    guests$age_category <- "Adults 21+ Room"
    return(guests)
  }
  
  # Read age data
  age_data <- read_csv(age_data_path,
                       col_types = cols(.default = col_character()),
                       na = c("", "NA", "N/A"))
  
  # Clean column names
  names(age_data) <- names(age_data) %>%
    str_to_lower() %>%
    str_replace_all(" ", "_")
  
  # Check for required columns
  if (!all(c("first_name", "last_name", "age") %in% names(age_data))) {
    warning("Age data file does not contain required columns: first_name, last_name, and age")
    guests$age_category <- "Adults 21+ Room"
    return(guests)
  }
  
  # Map age categories
  age_info <- age_data %>%
    select(first_name, last_name, age) %>%
    filter(!is.na(first_name) & !is.na(last_name)) %>%
    mutate(
      age_category = case_when(
        age == "Adult" ~ "Adults 21+ Room",
        age == "Child (13+)" ~ "Guests 12-21 Room",
        age == "Child (5-12)" ~ "Children 5-12 Room",
        age == "Child (0-4)" ~ "Children <5 Room",
        TRUE ~ "Unknown"
      )
    )
  
  # Merge with guests
  guests <- guests %>%
    left_join(age_info %>% select(first_name, last_name, age_category),
              by = c("first_name", "last_name")) %>%
    mutate(
      # Default to adult room
      age_category = ifelse(is.na(age_category), "Adults 21+ Room", age_category),
      # Update camping adults
      age_category = ifelse(age_category == "Adults 21+ Room" & is_camping, "Adults 21+ Camping", age_category)
    )
  
  cat("Age categories assigned from file:", age_data_path, "\n")
  return(guests)
}

# Function to generate party-level summary
generate_party_summary <- function(guests) {
  # Find email column
  email_cols <- grep("email", names(guests), ignore.case = TRUE, value = TRUE)
  email_col <- if (length(email_cols) > 0) email_cols[1] else NULL
  
  # Add email column if not found
  if (is.null(email_col)) {
    guests$email_column <- NA
    email_col <- "email_column"
  } else {
    guests$email_column <- guests[[email_col]]
  }
  
  # Create descriptive party names
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
  
  # Generate party summary
  party_summary <- guests %>%
    group_by(party) %>%
    summarize(
      party_name = first(party_name),
      party_email = first(na.omit(email_column)),
      total_guests = n(),
      guest_names = paste(paste(first_name, last_name), collapse = ", "),
      wedding_attending = sum(is_attending_wedding, na.rm = TRUE),
      wedding_declining = sum(wedding_rsvp == "Regretfully Decline", na.rm = TRUE),
      wedding_no_response = sum(is.na(wedding_rsvp)),
      friday_attending = sum(is_attending_friday, na.rm = TRUE),
      saturday_attending = sum(is_attending_saturday_lunch | is_attending_saturday_dinner, na.rm = TRUE),
      friday_staying = sum(is_staying_friday, na.rm = TRUE),
      saturday_staying = sum(is_staying_saturday, na.rm = TRUE),
      sunday_staying = sum(is_staying_sunday, na.rm = TRUE),
      camping = sum(is_camping, na.rm = TRUE)
    )
  
  return(party_summary)
}

# Calculate costs for all guests
# Calculate costs for all guests - FIXED VERSION
calculate_costs <- function(guests, rates_path = "charge_rates.csv") {
  # Get rate information
  rates_data <- read_rates(rates_path)
  
  if (!is.null(rates_data)) {
    cat("Using rate information from file:", rates_path, "\n")
    
    # Extract rates for each category and night
    friday_adult_room_rate <- rates_data %>% 
      filter(night == "Friday", category == "Adults 21+ Room") %>% 
      pull(ifc_rate) %>% 
      as.numeric() %>% 
      max(0, na.rm = TRUE)
    
    friday_adult_camping_rate <- rates_data %>% 
      filter(night == "Friday", category == "Adults 21+ Camping") %>% 
      pull(ifc_rate) %>% 
      as.numeric() %>% 
      max(0, na.rm = TRUE)
    
    friday_teen_rate <- rates_data %>% 
      filter(night == "Friday", category == "Guests 12-21 Room") %>% 
      pull(ifc_rate) %>% 
      as.numeric() %>% 
      max(0, na.rm = TRUE)
    
    friday_child_rate <- rates_data %>% 
      filter(night == "Friday", category == "Children 5-12 Room") %>% 
      pull(ifc_rate) %>% 
      as.numeric() %>% 
      max(0, na.rm = TRUE)
    
    friday_infant_rate <- 0
    
    # Saturday rates
    saturday_adult_room_rate <- rates_data %>% 
      filter(night == "Saturday", category == "Adults 21+ Room") %>% 
      pull(ifc_rate) %>% 
      as.numeric() %>% 
      max(0, na.rm = TRUE)
    
    saturday_adult_camping_rate <- rates_data %>% 
      filter(night == "Saturday", category == "Adults 21+ Camping") %>% 
      pull(ifc_rate) %>% 
      as.numeric() %>% 
      max(0, na.rm = TRUE)
    
    saturday_teen_rate <- rates_data %>% 
      filter(night == "Saturday", category == "Guests 12-21 Room") %>% 
      pull(ifc_rate) %>% 
      as.numeric() %>% 
      max(0, na.rm = TRUE)
    
    saturday_child_rate <- rates_data %>% 
      filter(night == "Saturday", category == "Children 5-12 Room") %>% 
      pull(ifc_rate) %>% 
      as.numeric() %>% 
      max(0, na.rm = TRUE)
    
    saturday_infant_rate <- 0
    
    # Sunday rates
    sunday_adult_room_rate <- rates_data %>% 
      filter(night == "Sunday (no meals option)", category == "Adults 21+ Room") %>% 
      pull(ifc_rate) %>% 
      as.numeric() %>% 
      max(0, na.rm = TRUE)
    
    sunday_adult_camping_rate <- rates_data %>% 
      filter(night == "Sunday (no meals option)", category == "Adults 21+ Camping") %>% 
      pull(ifc_rate) %>% 
      as.numeric() %>% 
      max(0, na.rm = TRUE)
    
    sunday_teen_rate <- rates_data %>% 
      filter(night == "Sunday (no meals option)", category == "Guests 12-21 Room") %>% 
      pull(ifc_rate) %>% 
      as.numeric() %>% 
      max(0, na.rm = TRUE)
    
    sunday_child_rate <- rates_data %>% 
      filter(night == "Sunday (no meals option)", category == "Children 5-12 Room") %>% 
      pull(ifc_rate) %>% 
      as.numeric() %>% 
      max(0, na.rm = TRUE)
    
    sunday_infant_rate <- 0
    
    # Guest charges
    friday_adult_room_guest_charge <- rates_data %>% 
      filter(night == "Friday", category == "Adults 21+ Room") %>% 
      pull(per_guest_charge) %>% 
      as.numeric() %>% 
      max(0, na.rm = TRUE)
    
    friday_adult_camping_guest_charge <- rates_data %>% 
      filter(night == "Friday", category == "Adults 21+ Camping") %>% 
      pull(per_guest_charge) %>% 
      as.numeric() %>% 
      max(0, na.rm = TRUE)
    
    saturday_adult_room_guest_charge <- rates_data %>% 
      filter(night == "Saturday", category == "Adults 21+ Room") %>% 
      pull(per_guest_charge) %>% 
      as.numeric() %>% 
      max(0, na.rm = TRUE)
    
    saturday_adult_camping_guest_charge <- rates_data %>% 
      filter(night == "Saturday", category == "Adults 21+ Camping") %>% 
      pull(per_guest_charge) %>% 
      as.numeric() %>% 
      max(0, na.rm = TRUE)
    
    sunday_adult_room_guest_charge <- 18
    sunday_adult_camping_guest_charge <- 18
    
    # Calculate costs for each guest
    guests <- guests %>%
      mutate(
        # Calculate IFC costs (what IFC charges for each guest)
        friday_ifc_lodging = case_when(
          is_staying_friday & !is_camping & age_category == "Adults 21+ Room" ~ friday_adult_room_rate,
          is_staying_friday & is_camping & age_category == "Adults 21+ Camping" ~ friday_adult_camping_rate,
          is_staying_friday & age_category == "Guests 12-21 Room" ~ friday_teen_rate,
          is_staying_friday & age_category == "Children 5-12 Room" ~ friday_child_rate,
          is_staying_friday & age_category == "Children <5 Room" ~ friday_infant_rate,
          TRUE ~ 0
        ),
        
        friday_ifc_meals = case_when(
          is_staying_friday & age_category == "Adults 21+ Room" ~ 22.5,
          is_staying_friday & age_category == "Adults 21+ Camping" ~ 22.5,
          is_staying_friday & age_category == "Guests 12-21 Room" ~ 22.5,
          is_staying_friday & age_category == "Children 5-12 Room" ~ 11.25,
          is_staying_friday & age_category == "Children <5 Room" ~ 0,
          TRUE ~ 0
        ),
        
        saturday_ifc_lodging = case_when(
          is_staying_saturday & !is_camping & age_category == "Adults 21+ Room" ~ saturday_adult_room_rate,
          is_staying_saturday & is_camping & age_category == "Adults 21+ Camping" ~ saturday_adult_camping_rate,
          is_staying_saturday & age_category == "Guests 12-21 Room" ~ saturday_teen_rate,
          is_staying_saturday & age_category == "Children 5-12 Room" ~ saturday_child_rate,
          is_staying_saturday & age_category == "Children <5 Room" ~ saturday_infant_rate,
          TRUE ~ 0
        ),
        
        saturday_ifc_meals = case_when(
          is_staying_saturday & age_category == "Adults 21+ Room" ~ 22.5,
          is_staying_saturday & age_category == "Adults 21+ Camping" ~ 22.5,
          is_staying_saturday & age_category == "Guests 12-21 Room" ~ 22.5,
          is_staying_saturday & age_category == "Children 5-12 Room" ~ 11.25,
          is_staying_saturday & age_category == "Children <5 Room" ~ 0,
          TRUE ~ 0
        ),
        
        sunday_ifc_meals = case_when(
          is_staying_sunday & age_category == "Adults 21+ Room" ~ sunday_adult_room_rate,
          is_staying_sunday & age_category == "Adults 21+ Camping" ~ sunday_adult_camping_rate,
          is_staying_sunday & age_category == "Guests 12-21 Room" ~ sunday_teen_rate,
          is_staying_sunday & age_category == "Children 5-12 Room" ~ sunday_child_rate,
          is_staying_sunday & age_category == "Children <5 Room" ~ sunday_infant_rate,
          TRUE ~ 0
        ),
        
        # Guest charges
        friday_guest_charge = case_when(
          is_staying_friday & !is_camping & age_category == "Adults 21+ Room" ~ friday_adult_room_guest_charge,
          is_staying_friday & is_camping & age_category == "Adults 21+ Camping" ~ friday_adult_camping_guest_charge,
          is_staying_friday & age_category == "Guests 12-21 Room" ~ 0,
          is_staying_friday & age_category == "Children 5-12 Room" ~ 0,
          is_staying_friday & age_category == "Children <5 Room" ~ 0,
          TRUE ~ 0
        ),
        
        saturday_guest_charge = case_when(
          is_staying_saturday & !is_camping & age_category == "Adults 21+ Room" ~ saturday_adult_room_guest_charge,
          is_staying_saturday & is_camping & age_category == "Adults 21+ Camping" ~ saturday_adult_camping_guest_charge,
          is_staying_saturday & age_category == "Guests 12-21 Room" ~ 0,
          is_staying_saturday & age_category == "Children 5-12 Room" ~ 0,
          is_staying_saturday & age_category == "Children <5 Room" ~ 0,
          TRUE ~ 0
        ),
        
        sunday_guest_charge = case_when(
          is_staying_sunday & age_category == "Adults 21+ Room" ~ sunday_adult_room_guest_charge,
          is_staying_sunday & age_category == "Adults 21+ Camping" ~ sunday_adult_camping_guest_charge,
          is_staying_sunday & age_category == "Guests 12-21 Room" ~ 0,
          is_staying_sunday & age_category == "Children 5-12 Room" ~ 0,
          is_staying_sunday & age_category == "Children <5 Room" ~ 0,
          TRUE ~ 0
        ),
        
        # Calculate daily totals
        friday_ifc_cost = friday_ifc_lodging + friday_ifc_meals,
        saturday_ifc_cost = saturday_ifc_lodging + saturday_ifc_meals,
        
        # Calculate host charges (whatever IFC charges minus what guests pay)
        friday_host_charge = friday_ifc_cost - friday_guest_charge,
        saturday_host_charge = saturday_ifc_cost - saturday_guest_charge,
        sunday_host_charge = sunday_ifc_meals - sunday_guest_charge,
        
        # Calculate totals
        total_ifc_cost = friday_ifc_cost + saturday_ifc_cost + sunday_ifc_meals,
        total_guest_charge = friday_guest_charge + saturday_guest_charge + sunday_guest_charge,
        total_host_charge = friday_host_charge + saturday_host_charge + sunday_host_charge
      )
  } else {
    # Default cost values if rate file not found
    cat("Using default cost constants...\n")
    guests <- guests %>%
      mutate(
        # IFC costs
        friday_ifc_lodging = ifelse(is_staying_friday & !is_camping, 225, 
                                    ifelse(is_staying_friday & is_camping, 150, 0)),
        friday_ifc_meals = ifelse(is_staying_friday, 22.5, 0),
        saturday_ifc_lodging = ifelse(is_staying_saturday & !is_camping, 225, 
                                      ifelse(is_staying_saturday & is_camping, 150, 0)),
        saturday_ifc_meals = ifelse(is_staying_saturday, 22.5, 0),
        sunday_ifc_meals = ifelse(is_staying_sunday, 109, 0),
        
        # Guest charges
        friday_guest_charge = ifelse(is_staying_friday & !is_camping, 108, 
                                     ifelse(is_staying_friday & is_camping, 72, 0)),
        saturday_guest_charge = ifelse(is_staying_saturday & !is_camping, 172, 
                                       ifelse(is_staying_saturday & is_camping, 90, 0)),
        sunday_guest_charge = ifelse(is_staying_sunday, 18, 0),
        
        # Daily totals for IFC
        friday_ifc_cost = friday_ifc_lodging + friday_ifc_meals,
        saturday_ifc_cost = saturday_ifc_lodging + saturday_ifc_meals,
        
        # Host charges
        friday_host_charge = friday_ifc_cost - friday_guest_charge,
        saturday_host_charge = saturday_ifc_cost - saturday_guest_charge,
        sunday_host_charge = sunday_ifc_meals - sunday_guest_charge,
        
        # Total costs
        total_ifc_cost = friday_ifc_cost + saturday_ifc_cost + sunday_ifc_meals,
        total_guest_charge = friday_guest_charge + saturday_guest_charge + sunday_guest_charge,
        total_host_charge = friday_host_charge + saturday_host_charge + sunday_host_charge
      )
  }
  
  return(guests)
}

# Count meals by standard categories
count_standard_meals <- function(guests) {
  meal_counts <- guests %>%
    summarize(
      # On-site guests by meal
      friday_dinner_onsite = sum(is_staying_friday, na.rm = TRUE),
      saturday_breakfast_onsite = sum(is_staying_friday, na.rm = TRUE),
      saturday_lunch_onsite = sum(is_staying_saturday, na.rm = TRUE),
      saturday_dinner_onsite = sum(is_staying_saturday, na.rm = TRUE),
      sunday_breakfast_onsite = sum(is_staying_saturday, na.rm = TRUE),
      sunday_dinner_onsite = sum(is_staying_sunday, na.rm = TRUE),
      monday_breakfast_onsite = sum(is_staying_sunday, na.rm = TRUE),
      
      # Off-site guests by meal
      friday_dinner_offsite = sum(is_attending_friday & !is_staying_friday, na.rm = TRUE),
      saturday_lunch_offsite = sum(is_attending_saturday_lunch & !is_staying_saturday, na.rm = TRUE),
      saturday_dinner_offsite = sum(is_attending_saturday_dinner & !is_staying_saturday, na.rm = TRUE),
      
      # Wedding meal (all guests)
      sunday_lunch_wedding = sum(is_attending_wedding, na.rm = TRUE),
      
      # Total meal counts
      total_friday_dinner = sum(is_staying_friday | (is_attending_friday & !is_staying_friday), na.rm = TRUE),
      total_saturday_breakfast = sum(is_staying_friday, na.rm = TRUE),
      total_saturday_lunch = sum(is_staying_saturday | (is_attending_saturday_lunch & !is_staying_saturday), na.rm = TRUE),
      total_saturday_dinner = sum(is_staying_saturday | (is_attending_saturday_dinner & !is_staying_saturday), na.rm = TRUE),
      total_sunday_breakfast = sum(is_staying_saturday, na.rm = TRUE),
      total_sunday_lunch = sum(is_attending_wedding, na.rm = TRUE),
      total_sunday_dinner = sum(is_staying_sunday, na.rm = TRUE),
      total_monday_breakfast = sum(is_staying_sunday, na.rm = TRUE),
      
      # Dietary counts (applied to all meals)
      vegetarian_count = sum(is_vegetarian, na.rm = TRUE),
      special_diet_count = sum(has_special_diet, na.rm = TRUE)
    )
  
  return(meal_counts)
}

# Count meals by age category
count_meals_by_age <- function(guests) {
  meals_by_age <- guests %>%
    group_by(age_category) %>%
    summarize(
      # Friday meals
      friday_dinner = sum(is_staying_friday | (is_attending_friday & !is_staying_friday), na.rm = TRUE),
      
      # Saturday meals
      saturday_breakfast = sum(is_staying_friday, na.rm = TRUE),
      saturday_lunch = sum(is_staying_saturday | (is_attending_saturday_lunch & !is_staying_saturday), na.rm = TRUE),
      saturday_dinner = sum(is_staying_saturday | (is_attending_saturday_dinner & !is_staying_saturday), na.rm = TRUE),
      
      # Sunday meals
      sunday_breakfast = sum(is_staying_saturday, na.rm = TRUE),
      sunday_lunch = sum(is_attending_wedding, na.rm = TRUE),
      sunday_dinner = sum(is_staying_sunday, na.rm = TRUE),
      
      # Monday breakfast
      monday_breakfast = sum(is_staying_sunday, na.rm = TRUE),
      
      # Dietary counts
      vegetarian_count = sum(is_vegetarian, na.rm = TRUE),
      special_diet_count = sum(has_special_diet, na.rm = TRUE)
    ) %>%
    arrange(desc(sunday_lunch))
  
  # Convert to a tidy format for each meal
  meal_list <- c("friday_dinner", "saturday_breakfast", "saturday_lunch", "saturday_dinner", 
                 "sunday_breakfast", "sunday_lunch", "sunday_dinner", "monday_breakfast")
  
  meal_names <- c("Friday Dinner", "Saturday Breakfast", "Saturday Lunch", "Saturday Dinner", 
                  "Sunday Breakfast", "Sunday Lunch (Wedding)", "Sunday Dinner", "Monday Breakfast")
  
  meals_by_age_long <- meals_by_age %>%
    pivot_longer(cols = all_of(meal_list), 
                 names_to = "meal_code", 
                 values_to = "count") %>%
    mutate(meal = meal_names[match(meal_code, meal_list)]) %>%
    select(meal, age_category, count, vegetarian_count, special_diet_count)
  
  return(list(
    by_age_and_meal = meals_by_age,
    by_age_long = meals_by_age_long
  ))
}

# Generate IFC schedule/roster
generate_ifc_roster <- function(guests) {
  # Filter for overnight guests only
  ifc_guests <- guests %>%
    filter(is_staying_friday | is_staying_saturday | is_staying_sunday) %>%
    mutate(
      # Add meal attendance flags
      friday_dinner = is_staying_friday,
      saturday_breakfast = is_staying_friday,
      saturday_lunch = is_staying_saturday | is_attending_saturday_lunch,
      saturday_dinner = is_staying_saturday | is_attending_saturday_dinner,
      sunday_breakfast = is_staying_saturday,
      sunday_lunch = is_attending_wedding,
      sunday_dinner = is_staying_sunday,
      monday_breakfast = is_staying_sunday,
      # Add accommodation type
      accommodation_type = ifelse(is_camping, "Camping", "Standard Lodging")
    ) %>%
    select(
      first_name,
      last_name,
      party,
      age_category,
      is_staying_friday,
      is_staying_saturday,
      is_staying_sunday,
      accommodation_type,
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
      # Financial information
      total_ifc_cost,
      total_guest_charge,
      total_host_charge
    )
  
  return(ifc_guests)
}

# Create a summary of accommodation data
create_accommodation_summary <- function(guests) {
  accommodation_summary <- guests %>%
    summarize(
      total_guests = n(),
      friday_count = sum(is_staying_friday, na.rm = TRUE),
      saturday_count = sum(is_staying_saturday, na.rm = TRUE),
      sunday_count = sum(is_staying_sunday, na.rm = TRUE),
      camping_count = sum(is_camping, na.rm = TRUE),
      standard_lodging_count = sum((is_staying_friday | is_staying_saturday | is_staying_sunday) & !is_camping, na.rm = TRUE),
      
      # Cost totals
      total_friday_cost = sum(friday_ifc_cost, na.rm = TRUE),
      total_saturday_cost = sum(saturday_ifc_cost, na.rm = TRUE),
      total_sunday_cost = sum(sunday_ifc_meals, na.rm = TRUE),
      grand_total_cost = sum(total_ifc_cost, na.rm = TRUE),
      
      # Guest charges
      total_friday_guest_charge = sum(friday_guest_charge, na.rm = TRUE),
      total_saturday_guest_charge = sum(saturday_guest_charge, na.rm = TRUE),
      total_sunday_guest_charge = sum(sunday_guest_charge, na.rm = TRUE),
      grand_total_guest_charge = sum(total_guest_charge, na.rm = TRUE),
      
      # Host charges
      total_friday_host_charge = sum(friday_host_charge, na.rm = TRUE),
      total_saturday_host_charge = sum(saturday_host_charge, na.rm = TRUE),
      total_sunday_host_charge = sum(sunday_host_charge, na.rm = TRUE),
      grand_total_host_charge = sum(total_host_charge, na.rm = TRUE)
    )
  
  return(accommodation_summary)
}

# Validate guest data for quality issues
validate_guest_data <- function(guests) {
  issues <- list()
  
  # Check for missing names
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

# Main function to generate all reports
generate_wedding_reports <- function(file_path, age_data_path = "Wedding Budget Invite List.csv", 
                                     rates_path = "charge_rates.csv") {
  # Step 1: Read guest list with standardized variables
  guests <- read_guest_list(file_path)
  
  # Step 2: Assign age categories
  guests <- assign_age_categories(guests, age_data_path)
  
  # Step 3: Calculate all costs
  guests <- calculate_costs(guests, rates_path)
  
  # Step 4: Generate party summary
  party_summary <- generate_party_summary(guests)
  
  # Step 5: Create accommodation summary
  accommodation_summary <- create_accommodation_summary(guests)
  
  # Step 6: Count meals
  meal_counts <- count_standard_meals(guests)
  meal_counts_by_age <- count_meals_by_age(guests)
  
  # Step 7: Generate IFC roster
  ifc_roster <- generate_ifc_roster(guests)
  
  # Step 8: Validate guest data
  data_issues <- validate_guest_data(guests)
  
  # Print summary reports
  cat("\n=== WEDDING RSVP SUMMARY ===\n")
  print(party_summary)
  
  cat("\n=== ACCOMMODATION SUMMARY ===\n")
  print(accommodation_summary)
  
  cat("\n=== MEAL COUNTS (Detailed) ===\n")
  print(meal_counts)
  
  # Print any data quality issues
  if (length(data_issues) > 0) {
    cat("\n=== DATA QUALITY ISSUES ===\n")
    print(data_issues)
  }
  
  # Return all results
  return(list(
    guests = guests,
    party_summary = party_summary,
    accommodation_summary = accommodation_summary,
    meal_counts = meal_counts,
    meal_counts_by_age = meal_counts_by_age,
    ifc_roster = ifc_roster,
    data_issues = data_issues
  ))
}

# Function to export results to CSV files
export_reports <- function(results, output_dir = ".") {
  # Create directory if it doesn't exist
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  cat("Ensuring report directory exists:", output_dir, "\n")
  
  # Export guest data
  write_csv(results$guests, file.path(output_dir, "guest_details.csv"))
  
  # Export party summary
  write_csv(results$party_summary, file.path(output_dir, "party_summary.csv"))
  
  # Export accommodation summary
  accommodation_df <- as.data.frame(t(as.matrix(results$accommodation_summary)))
  accommodation_df$metric <- rownames(accommodation_df)
  accommodation_df <- accommodation_df[, c(ncol(accommodation_df), 1:(ncol(accommodation_df)-1))]
  names(accommodation_df)[1] <- "metric"
  write_csv(accommodation_df, file.path(output_dir, "accommodation_summary.csv"))
  
  # Export meal counts
  meal_df <- as.data.frame(t(as.matrix(results$meal_counts)))
  meal_df$meal <- rownames(meal_df)
  meal_df <- meal_df[, c(ncol(meal_df), 1:(ncol(meal_df)-1))]
  names(meal_df)[1] <- "metric"
  write_csv(meal_df, file.path(output_dir, "meal_counts.csv"))
  
  # Export meal counts by age category
  age_dir <- file.path(output_dir, "age_reports")
  dir.create(age_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Export age categories by meal
  write_csv(results$meal_counts_by_age$by_age_and_meal, file.path(age_dir, "meals_by_age.csv"))
  
  # Export long-format meal data by age
  write_csv(results$meal_counts_by_age$by_age_long, file.path(age_dir, "meals_by_age_long.csv"))
  
  # Export IFC roster
  write_csv(results$ifc_roster, file.path(output_dir, "ifc_roster.csv"))
  
  # Export guest list with dietary preferences for special meals
  special_diet_guests <- results$guests %>%
    filter(has_special_diet) %>%
    select(
      first_name, 
      last_name, 
      age_category, 
      meal_preferences, 
      dietary_restrictions
    )
  
  write_csv(special_diet_guests, file.path(age_dir, "special_diet_guests.csv"))
  
  # Create a summary text file
  summary_text <- paste0(
    "WEDDING REPORT SUMMARY\n",
    "Wedding: Cyrena & Jon - June 20-23, 2025\n",
    "Generated: ", format(Sys.Date(), "%B %d, %Y"), "\n\n",
    
    "== Attendance Summary ==\n",
    "Total Guests: ", results$accommodation_summary$total_guests, "\n",
    "Wedding Attending: ", sum(results$guests$is_attending_wedding, na.rm = TRUE), "\n",
    "Friday Attending: ", sum(results$guests$is_attending_friday, na.rm = TRUE), "\n\n",
    
    "== Accommodation Summary ==\n",
    "Friday Night: ", results$accommodation_summary$friday_count, "\n",
    "Saturday Night: ", results$accommodation_summary$saturday_count, "\n",
    "Sunday Night: ", results$accommodation_summary$sunday_count, "\n",
    "Camping: ", results$accommodation_summary$camping_count, "\n",
    "Standard Lodging: ", results$accommodation_summary$standard_lodging_count, "\n\n",
    
    "== Financial Summary ==\n",
    "Total IFC Cost: $", format(results$accommodation_summary$grand_total_cost, big.mark = ","), "\n",
    "Total Guest Charges: $", format(results$accommodation_summary$grand_total_guest_charge, big.mark = ","), "\n",
    "Total Host Charges: $", format(results$accommodation_summary$grand_total_host_charge, big.mark = ","), "\n\n",
    
    "== Meal Counts ==\n",
    "Friday Dinner: ", results$meal_counts$total_friday_dinner, "\n",
    "Saturday Breakfast: ", results$meal_counts$total_saturday_breakfast, "\n",
    "Saturday Lunch: ", results$meal_counts$total_saturday_lunch, "\n",
    "Saturday Dinner: ", results$meal_counts$total_saturday_dinner, "\n",
    "Sunday Breakfast: ", results$meal_counts$total_sunday_breakfast, "\n",
    "Sunday Lunch (Wedding): ", results$meal_counts$total_sunday_lunch, "\n",
    "Sunday Dinner: ", results$meal_counts$total_sunday_dinner, "\n",
    "Monday Breakfast: ", results$meal_counts$total_monday_breakfast, "\n\n",
    
    "== Dietary Information ==\n",
    "Vegetarian Meals: ", results$meal_counts$vegetarian_count, "\n",
    "Special Dietary Needs: ", results$meal_counts$special_diet_count, "\n\n",
    
    "Reports have been generated in the following directory: ", output_dir, "\n"
  )
  
  writeLines(summary_text, file.path(output_dir, "summary.txt"))
  
  cat("Reports exported to:", output_dir, "\n")
}

# Function to generate meal planning report
generate_meal_planning_report <- function(results, output_file = "meal_planning_report.csv") {
  # Create directory
  output_dir <- dirname(output_file)
  if (output_dir != "." && output_dir != "") {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  # Get meal counts
  meal_counts <- results$meal_counts
  
  # Create meal summary
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
    ),
    Vegetarian = rep(meal_counts$vegetarian_count, 8),
    Special_Diet = rep(meal_counts$special_diet_count, 8)
  )
  
  # Get special diet guests
  special_diet_guests <- results$guests %>%
    filter(has_special_diet) %>%
    select(
      first_name,
      last_name,
      age_category,
      meal_preferences,
      dietary_restrictions
    )
  
  # Write CSV files
  csv_base_name <- tools::file_path_sans_ext(output_file)
  
  # Write meal summary
  write_csv(meal_summary, paste0(csv_base_name, "_meal_summary.csv"))
  
  # Write meal counts by age
  write_csv(results$meal_counts_by_age$by_age_long, paste0(csv_base_name, "_by_age.csv"))
  
  # Write special diet guests
  write_csv(special_diet_guests, paste0(csv_base_name, "_special_diets.csv"))
  
  # Create summary text
  summary_text <- paste0(
    "MEAL PLANNING REPORT\n",
    "Wedding: Cyrena & Jon - June 20-23, 2025\n",
    "Generated: ", format(Sys.Date(), "%B %d, %Y"), "\n\n",
    
    "== Meal Planning Summary ==\n\n",
    paste(capture.output(print(meal_summary)), collapse = "\n"), "\n\n",
    
    "== Special Dietary Requirements ==\n\n",
    "Vegetarian Meals Needed: ", meal_counts$vegetarian_count, "\n",
    "Special Dietary Needs: ", meal_counts$special_diet_count, "\n\n",
    
    "== Important Notes ==\n\n",
    "1. Meal Inclusion by Stay:\n",
    "   - Friday night guests: Friday dinner, Saturday breakfast\n",
    "   - Saturday night guests: Saturday lunch, dinner, Sunday breakfast\n",
    "   - Sunday night guests: Special catering for Sunday dinner, Monday breakfast\n",
    "   - All wedding guests: Sunday lunch (wedding meal)\n\n",
    
    "2. Dietary Information:\n",
    "   - Non-meat options should be available at all meals\n",
    "   - Please review the detailed dietary restrictions in the special_diets.csv file\n\n",
    
    "3. Catering Planning:\n",
    "   - Sunday lunch (wedding reception) is the largest meal requiring service for all guests\n",
    "   - Sunday dinner and Monday breakfast are special catering for overnight guests only\n"
  )
  
  # Write summary
  writeLines(summary_text, paste0(csv_base_name, "_summary.txt"))
  
  cat("Meal planning report generated:", output_file, "\n")
  
  return(list(
    meal_summary = meal_summary,
    meal_counts_by_age = results$meal_counts_by_age,
    special_diet_guests = special_diet_guests
  ))
}

# Function to generate IFC report
generate_ifc_report <- function(results, output_file = "ifc_report.csv") {
  # Create directory
  output_dir <- dirname(output_file)
  if (output_dir != "." && output_dir != "") {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  # Get IFC roster
  ifc_roster <- results$ifc_roster
  
  # Create night summary
  night_summary <- data.frame(
    Night = c("Friday, June 20", "Saturday, June 21", "Sunday, June 22"),
    Guests = c(
      sum(ifc_roster$is_staying_friday),
      sum(ifc_roster$is_staying_saturday),
      sum(ifc_roster$is_staying_sunday)
    ),
    Standard_Lodging = c(
      sum(ifc_roster$is_staying_friday & ifc_roster$accommodation_type == "Standard Lodging"),
      sum(ifc_roster$is_staying_saturday & ifc_roster$accommodation_type == "Standard Lodging"),
      sum(ifc_roster$is_staying_sunday & ifc_roster$accommodation_type == "Standard Lodging")
    ),
    Camping = c(
      sum(ifc_roster$is_staying_friday & ifc_roster$accommodation_type == "Camping"),
      sum(ifc_roster$is_staying_saturday & ifc_roster$accommodation_type == "Camping"),
      sum(ifc_roster$is_staying_sunday & ifc_roster$accommodation_type == "Camping")
    )
  )
  
  # Create age category summary
  age_summary <- ifc_roster %>%
    group_by(age_category) %>%
    summarize(
      Total = n(),
      Friday = sum(is_staying_friday),
      Saturday = sum(is_staying_saturday),
      Sunday = sum(is_staying_sunday)
    )
  
  # Create meal summary
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
    Count = c(
      sum(ifc_roster$friday_dinner),
      sum(ifc_roster$saturday_breakfast),
      sum(ifc_roster$saturday_lunch),
      sum(ifc_roster$saturday_dinner),
      sum(ifc_roster$sunday_breakfast),
      sum(ifc_roster$sunday_lunch),
      sum(ifc_roster$sunday_dinner),
      sum(ifc_roster$monday_breakfast)
    ),
    Vegetarian = rep(sum(ifc_roster$is_vegetarian), 8),
    Special_Diet = rep(sum(ifc_roster$has_special_diet), 8)
  )
  
  # Write CSV files
  csv_base_name <- tools::file_path_sans_ext(output_file)
  
  # Write night summary
  write_csv(night_summary, paste0(csv_base_name, "_night_summary.csv"))
  
  # Write age summary
  write_csv(age_summary, paste0(csv_base_name, "_age_summary.csv"))
  
  # Write meal summary
  write_csv(meal_summary, paste0(csv_base_name, "_meal_summary.csv"))
  
  # Write guest roster
  write_csv(ifc_roster, paste0(csv_base_name, "_guest_roster.csv"))
  
  # Create summary text
  summary_text <- paste0(
    "IFC REPORT SUMMARY\n",
    "Wedding: Cyrena & Jon - June 20-23, 2025\n",
    "Generated: ", format(Sys.Date(), "%B %d, %Y"), "\n\n",
    
    "== Guest Stay Summary ==\n\n",
    paste(capture.output(print(night_summary)), collapse = "\n"), "\n\n",
    
    "== Age Category Summary ==\n\n",
    paste(capture.output(print(age_summary)), collapse = "\n"), "\n\n",
    
    "== Meal Summary ==\n\n",
    paste(capture.output(print(meal_summary)), collapse = "\n"), "\n\n",
    
    "== Financial Summary ==\n\n",
    "Total IFC Cost: $", format(sum(ifc_roster$total_ifc_cost), big.mark = ","), "\n",
    "Total Guest Charges: $", format(sum(ifc_roster$total_guest_charge), big.mark = ","), "\n",
    "Total Host Charges: $", format(sum(ifc_roster$total_host_charge), big.mark = ","), "\n\n",
    
    "== Important Notes ==\n\n",
    "1. All guests listed are part of the wedding party for Cyrena & Jon.\n",
    "2. Room assignments should be based on age categories.\n",
    "3. Special dietary needs are noted in the guest roster.\n",
    "4. For questions, contact: wedding@example.com\n"
  )
  
  # Write summary
  writeLines(summary_text, paste0(csv_base_name, "_summary.txt"))
  
  cat("IFC report generated:", output_file, "\n")
  
  return(list(
    night_summary = night_summary,
    age_summary = age_summary,
    meal_summary = meal_summary,
    guest_roster = ifc_roster
  ))
}
  
  