# Wedding Dashboard App with Enhanced Views
# Updated version with improved handling of age categories and host charges

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(knitr)
library(rmarkdown)

# Add this function after your library statements but before the server function
fix_age_category_summary <- function(data) {
  # Only process if we have guest_costs data
  if (is.null(data) || is.null(data$guest_costs)) {
    return(data)
  }
  
  # Create or update age_category_summary with the missing columns
  age_counts <- data$guest_costs %>%
    group_by(age_category) %>%
    summarize(
      total_guests = n(),
      friday_count = sum(is_staying_friday, na.rm = TRUE),
      saturday_count = sum(is_staying_saturday, na.rm = TRUE),
      sunday_count = sum(is_staying_sunday, na.rm = TRUE)
    )
  
  # If age_category_summary already exists, merge in the new columns
  if (!is.null(data$age_category_summary)) {
    # Join with existing age_category_summary to keep cost data
    data$age_category_summary <- data$age_category_summary %>%
      left_join(age_counts, by = "age_category")
  } else {
    # Just use the counts we calculated
    data$age_category_summary <- age_counts
  }
  
  return(data)
}

# Add this observer near the top of your server function
# after the processed_data reactive is defined
observe({
  req(processed_data())
  
  # Apply our fixes
  updated_data <- fix_age_category_summary(processed_data())
  
  # Update the processed data
  processed_data(updated_data)
})


# Function to inspect the data structure and report issues
diagnose_data_structure <- function(data) {
  # Create a text buffer for all diagnostic messages
  log <- character()
  
  log <- c(log, "== DASHBOARD DATA STRUCTURE DIAGNOSIS ==")
  log <- c(log, paste("Timestamp:", Sys.time()))
  
  # Check if data exists
  if (is.null(data)) {
    log <- c(log, "ERROR: Input data is NULL")
    return(paste(log, collapse = "\n"))
  }
  
  # Check top-level components
  log <- c(log, "\n1. TOP LEVEL COMPONENTS:")
  components <- names(data)
  log <- c(log, paste("Components available:", paste(components, collapse = ", ")))
  
  # Check if essential components exist
  required_components <- c("guests", "party_summary", "guest_costs", 
                           "accommodation_summary", "age_category_summary", 
                           "meal_counts")
  
  missing_components <- setdiff(required_components, components)
  if (length(missing_components) > 0) {
    log <- c(log, paste("MISSING COMPONENTS:", paste(missing_components, collapse = ", ")))
  }
  
  # 2. Check Age Category Data
  log <- c(log, "\n2. AGE CATEGORY DATA:")
  
  # Check if guest_costs exists and contains age_category
  if ("guest_costs" %in% components) {
    if ("age_category" %in% names(data$guest_costs)) {
      age_categories <- unique(data$guest_costs$age_category)
      log <- c(log, paste("Age categories in guest_costs:", paste(age_categories, collapse = ", ")))
      
      # Count by age category
      age_counts <- table(data$guest_costs$age_category)
      log <- c(log, "Age category counts:")
      for (i in 1:length(age_counts)) {
        log <- c(log, paste("  -", names(age_counts)[i], ":", age_counts[i]))
      }
    } else {
      log <- c(log, "ERROR: guest_costs exists but has no age_category column")
    }
  }
  
  # Check age_category_summary
  if ("age_category_summary" %in% components) {
    log <- c(log, "\nage_category_summary structure:")
    log <- c(log, paste("  Columns:", paste(names(data$age_category_summary), collapse = ", ")))
    log <- c(log, paste("  Rows:", nrow(data$age_category_summary)))
    
    # Check expected columns
    expected_cols <- c("age_category", "total_guests", "friday_count", "saturday_count", "sunday_count")
    missing_cols <- setdiff(expected_cols, names(data$age_category_summary))
    if (length(missing_cols) > 0) {
      log <- c(log, paste("  MISSING COLUMNS:", paste(missing_cols, collapse = ", ")))
    }
    
    # Show first few rows
    if (nrow(data$age_category_summary) > 0) {
      log <- c(log, "\n  Preview of age_category_summary (first 3 rows):")
      preview <- utils::capture.output(print(head(data$age_category_summary, 3)))
      log <- c(log, paste("    ", preview))
    }
  } else {
    log <- c(log, "ERROR: age_category_summary component is missing")
  }
  
  # 3. Check Accommodation Data
  log <- c(log, "\n3. ACCOMMODATION DATA:")
  
  if ("accommodation_summary" %in% components) {
    # Convert to data frame if it's not already
    if (!is.data.frame(data$accommodation_summary)) {
      log <- c(log, "  NOTE: accommodation_summary is not a data frame, it's a:", class(data$accommodation_summary)[1])
      
      # Try to show what it contains
      summary_str <- utils::capture.output(print(data$accommodation_summary))
      log <- c(log, "  Contents:")
      log <- c(log, paste("    ", summary_str))
    } else {
      log <- c(log, paste("  Columns:", paste(names(data$accommodation_summary), collapse = ", ")))
      
      # Check for the night counts
      night_cols <- c("friday_count", "saturday_count", "sunday_count")
      for (col in night_cols) {
        if (col %in% names(data$accommodation_summary)) {
          log <- c(log, paste("  ", col, "=", data$accommodation_summary[[col]]))
        } else {
          log <- c(log, paste("  ERROR:", col, "is missing"))
        }
      }
    }
  } else {
    log <- c(log, "ERROR: accommodation_summary component is missing")
  }
  
  # 4. Check IFC Roster 
  log <- c(log, "\n4. IFC ROSTER DATA:")
  
  # Check if ifc_roster exists
  if ("ifc_roster" %in% components) {
    ifc_data <- data$ifc_roster
    log <- c(log, paste("  ifc_roster has", nrow(ifc_data), "rows"))
  } else if ("ifc_schedule" %in% components) {
    ifc_data <- data$ifc_schedule
    log <- c(log, paste("  ifc_schedule has", nrow(ifc_data), "rows (ifc_roster missing)"))
  } else {
    log <- c(log, "  ERROR: Neither ifc_roster nor ifc_schedule found")
    return(paste(log, collapse = "\n"))
  }
  
  # Check cost columns
  cost_cols <- c("total_cost", "total_guest_charge", "total_host_charge")
  for (col in cost_cols) {
    if (col %in% names(ifc_data)) {
      log <- c(log, paste("  ", col, "data type:", class(ifc_data[[col]])[1]))
      
      # Check for NAs
      na_count <- sum(is.na(ifc_data[[col]]))
      if (na_count > 0) {
        log <- c(log, paste("    WARNING:", na_count, "NA values in", col))
      }
      
      # Check for non-numeric data if it's character
      if (is.character(ifc_data[[col]])) {
        non_numeric <- suppressWarnings(is.na(as.numeric(ifc_data[[col]])) & !is.na(ifc_data[[col]]))
        if (any(non_numeric)) {
          log <- c(log, paste("    ERROR:", sum(non_numeric), "non-numeric values in", col))
          log <- c(log, paste("    Example:", ifc_data[[col]][which(non_numeric)[1]]))
        }
      }
    } else {
      log <- c(log, paste("  ERROR:", col, "column is missing"))
    }
  }
  
  # Check for under 21 guests
  if ("age_category" %in% names(ifc_data)) {
    under_21 <- ifc_data[grepl("Children|Guests 12-21", ifc_data$age_category), ]
    log <- c(log, paste("\n  Found", nrow(under_21), "guests under 21"))
    
    if (nrow(under_21) > 0) {
      # Check if they have costs
      for (col in cost_cols) {
        if (col %in% names(under_21)) {
          # Try to convert to numeric if character
          values <- if(is.character(under_21[[col]])) as.numeric(gsub("\\$|,", "", under_21[[col]])) else under_21[[col]]
          
          log <- c(log, paste("    ", col, "for under 21: Range", 
                              min(values, na.rm = TRUE), "to", max(values, na.rm = TRUE), 
                              ", Mean", mean(values, na.rm = TRUE)))
        }
      }
      
      # Show sample of under 21 guests
      log <- c(log, "\n  Sample of under 21 guests:")
      sample_cols <- intersect(c("first_name", "last_name", "age_category", "total_cost", "total_guest_charge", "total_host_charge"), names(under_21))
      sample_data <- under_21[1:min(3, nrow(under_21)), sample_cols, drop = FALSE]
      sample_output <- utils::capture.output(print(sample_data))
      log <- c(log, paste("    ", sample_output))
    }
  } else {
    log <- c(log, "  ERROR: No age_category column in IFC data")
  }
  
  # 5. Check Cost Calculator data
  log <- c(log, "\n5. COST CALCULATOR DATA:")
  
  if ("guest_costs" %in% components) {
    guest_costs <- data$guest_costs
    
    # Check for key columns
    key_cols <- c("first_name", "last_name", "age_category", "is_staying_friday", 
                  "is_staying_saturday", "is_staying_sunday", "is_camping",
                  "total_cost", "total_guest_charge", "total_host_charge")
    
    missing_cols <- setdiff(key_cols, names(guest_costs))
    if (length(missing_cols) > 0) {
      log <- c(log, paste("  MISSING COLUMNS:", paste(missing_cols, collapse = ", ")))
    }
    
    # Check cost columns data types
    cost_cols <- intersect(c("total_cost", "total_guest_charge", "total_host_charge"), names(guest_costs))
    for (col in cost_cols) {
      log <- c(log, paste("  ", col, "data type:", class(guest_costs[[col]])[1]))
      
      # Check for NAs
      na_count <- sum(is.na(guest_costs[[col]]))
      if (na_count > 0) {
        log <- c(log, paste("    WARNING:", na_count, "NA values in", col))
      }
    }
    
    # Count guests by stay
    stay_count <- sum(guest_costs$is_staying_friday | guest_costs$is_staying_saturday | guest_costs$is_staying_sunday, na.rm = TRUE)
    log <- c(log, paste("  Total guests staying:", stay_count))
    
    # Count under 21 guests with stay
    if ("age_category" %in% names(guest_costs)) {
      under_21_staying <- guest_costs[grepl("Children|Guests 12-21", guest_costs$age_category) & 
                                        (guest_costs$is_staying_friday | guest_costs$is_staying_saturday | guest_costs$is_staying_sunday), ]
      log <- c(log, paste("  Under 21 guests staying:", nrow(under_21_staying)))
    }
  } else {
    log <- c(log, "ERROR: guest_costs component is missing")
  }
  
  log <- c(log, "\n== END DIAGNOSIS ==")
  return(paste(log, collapse = "\n"))
}


# UI
ui <- dashboardPage(
  dashboardHeader(title = "Wedding RSVP Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Quick Insights", tabName = "insights", icon = icon("lightbulb")),
      # New comprehensive party view
      menuItem("Parties", tabName = "parties", icon = icon("users")),
      # Enhanced accommodations view
      menuItem("Accommodations", tabName = "accommodation", icon = icon("bed")),
      # Enhanced meals view with detailed schedules
      menuItem("Meal Planning", tabName = "meals", icon = icon("utensils")),
      # New IFC roster view
      menuItem("IFC Roster", tabName = "ifc_roster", icon = icon("clipboard-list")),
      menuItem("Cost Calculator", tabName = "costs", icon = icon("calculator"))
    ),
    fileInput("guestlist", "Upload Guest List CSV",
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    downloadButton("downloadReports", "Download All Reports")
  ),
  
  dashboardBody(
    # Add mobile responsiveness
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
      tags$style(HTML("
        @media (max-width: 768px) {
          .content-wrapper { margin-left: 0 !important; }
          .main-sidebar { display: none; }
        }
        
        /* Make sure tables don't overflow */
        .table-responsive {
          overflow-x: auto;
        }
        
        /* Improve value box styling */
        .small-box {
          margin-bottom: 15px;
        }
        
        /* Fix width issues in tables */
        .dataTable {
          width: 100% !important;
        }
        
        /* Make sure columns don't get cut off */
        .dataTables_wrapper {
          overflow-x: auto;
        }
        
        /* Add styling for tab headers */
        .tab-header {
          font-size: 1.2em;
          margin-bottom: 15px;
          padding-bottom: 5px;
          border-bottom: 1px solid #ddd;
        }
        
        /* Add styling for action buttons */
        .action-button {
          margin: 5px;
        }
      "))
    ),
    
    tabItems(
      # Overview tab (existing)
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_guests", width = 3),
                valueBoxOutput("wedding_attending", width = 3),
                valueBoxOutput("friday_attending", width = 3),
                valueBoxOutput("saturday_attending", width = 3)
              ),
              fluidRow(
                box(title = "RSVP Status", status = "primary", width = 6,
                    plotlyOutput("rsvp_status_plot")
                ),
                box(title = "Accommodations", status = "info", width = 6,
                    plotlyOutput("accommodation_plot")
                )
              ),
              fluidRow(
                box(title = "RSVP Timeline", status = "success", width = 12,
                    plotlyOutput("rsvp_timeline")
                )
              )
      ),
      
      # Quick Insights tab (existing with enhancements)
      tabItem(tabName = "insights",
              fluidRow(
                box(title = "RSVP Status", status = "primary", width = 4,
                    tableOutput("rsvp_status_table")
                ),
                box(title = "Accommodation Summary", status = "info", width = 4,
                    tableOutput("accommodation_summary_table")
                ),
                box(title = "Meal Requirements", status = "warning", width = 4,
                    tableOutput("meal_summary_table")
                )
              ),
              fluidRow(
                box(title = "Age Categories", status = "success", width = 6,
                    tableOutput("age_category_table")
                ),
                box(title = "Cost Summary", status = "danger", width = 6,
                    tableOutput("cost_summary_table")
                )
              ),
              fluidRow(
                box(title = "Data Quality Issues", status = "success", width = 12,
                    DTOutput("data_issues_table")
                )
              )
      ),
      
      # NEW: Comprehensive Party List tab
      tabItem(tabName = "parties",
              fluidRow(
                box(title = "Party Overview", status = "primary", width = 12,
                    p("This table shows all guest parties with attendance status, accommodation choices, and other details."),
                    downloadButton("downloadPartyData", "Download Party Data", class = "action-button"),
                    hr(),
                    DTOutput("comprehensive_party_table")
                )
              ),
              fluidRow(
                box(title = "Individual Guest Details", status = "info", width = 12,
                    p("Select a party above to see detailed information for all guests in that party."),
                    hr(),
                    DTOutput("party_guests_table")
                )
              )
      ),
      
      # Enhanced Accommodations tab
      tabItem(tabName = "accommodation",
              fluidRow(
                valueBoxOutput("total_staying", width = 3),
                valueBoxOutput("camping_count", width = 3),
                valueBoxOutput("lodging_count", width = 3),
                valueBoxOutput("nights_booked", width = 3)
              ),
              fluidRow(
                box(title = "Accommodations by Night and Age Category", status = "primary", width = 12,
                    plotlyOutput("stays_by_night_category_plot")
                )
              ),
              fluidRow(
                box(title = "Accommodation Details", status = "primary", width = 12,
                    p("Complete list of guests staying at the Isabella Freedman Center, with accommodation type and nights."),
                    downloadButton("downloadAccommodationData", "Download Accommodation Data", class = "action-button"),
                    hr(),
                    DTOutput("accommodation_table")
                )
              ),
              fluidRow(
                box(title = "Stay Distribution", status = "info", width = 12,
                    plotlyOutput("stay_distribution")
                )
              )
      ),
      
      # Enhanced Meals tab with detailed meal schedule
      tabItem(tabName = "meals",
              fluidRow(
                box(title = "Meal Summary", status = "primary", width = 12,
                    plotlyOutput("meal_summary_plot")
                )
              ),
              fluidRow(
                tabBox(
                  title = "Meal Details by Day",
                  id = "meal_tabs",
                  width = 12,
                  tabPanel("Friday", 
                           fluidRow(
                             column(6, 
                                    h4("Friday Dinner", class = "tab-header"),
                                    DTOutput("friday_dinner_table")
                             ),
                             column(6,
                                    h4("Friday Dinner Summary", class = "tab-header"),
                                    tableOutput("friday_dinner_summary"),
                                    hr(),
                                    plotlyOutput("friday_dinner_plot")
                             )
                           )
                  ),
                  tabPanel("Saturday", 
                           fluidRow(
                             h4("Saturday Breakfast", class = "tab-header"),
                             column(6, DTOutput("saturday_breakfast_table")),
                             column(6, tableOutput("saturday_breakfast_summary"))
                           ),
                           hr(),
                           fluidRow(
                             h4("Saturday Lunch", class = "tab-header"),
                             column(6, DTOutput("saturday_lunch_table")),
                             column(6, tableOutput("saturday_lunch_summary"))
                           ),
                           hr(),
                           fluidRow(
                             h4("Saturday Dinner", class = "tab-header"),
                             column(6, DTOutput("saturday_dinner_table")),
                             column(6, tableOutput("saturday_dinner_summary"))
                           )
                  ),
                  tabPanel("Sunday", 
                           fluidRow(
                             h4("Sunday Breakfast", class = "tab-header"),
                             column(6, DTOutput("sunday_breakfast_table")),
                             column(6, tableOutput("sunday_breakfast_summary"))
                           ),
                           hr(),
                           fluidRow(
                             h4("Sunday Lunch (Wedding)", class = "tab-header"),
                             column(6, DTOutput("sunday_lunch_table")),
                             column(6, tableOutput("sunday_lunch_summary"))
                           ),
                           hr(),
                           fluidRow(
                             h4("Sunday Dinner", class = "tab-header"),
                             column(6, DTOutput("sunday_dinner_table")),
                             column(6, tableOutput("sunday_dinner_summary"))
                           )
                  ),
                  tabPanel("Monday", 
                           fluidRow(
                             h4("Monday Breakfast", class = "tab-header"),
                             column(6, DTOutput("monday_breakfast_table")),
                             column(6, tableOutput("monday_breakfast_summary"))
                           )
                  ),
                  tabPanel("Meal Preferences", 
                           fluidRow(
                             h4("Dietary Requirements", class = "tab-header"),
                             column(6, plotlyOutput("meal_preferences_plot")),
                             column(6, plotlyOutput("dietary_restrictions_plot"))
                           ),
                           hr(),
                           fluidRow(
                             h4("Guests with Special Dietary Requirements", class = "tab-header"),
                             DTOutput("dietary_table")
                           )
                  )
                )
              )
      ),
      
      # NEW: IFC Roster tab
      tabItem(tabName = "ifc_roster",
              fluidRow(
                box(title = "IFC Schedule Overview", status = "primary", width = 12,
                    p("Complete roster of all guests staying at Isabella Freedman Center with meal schedule, accommodation type, and financial breakdown."),
                    downloadButton("downloadIFCData", "Download IFC Roster", class = "action-button"),
                    hr(),
                    DTOutput("ifc_roster_table")
                )
              ),
              fluidRow(
                box(title = "Accommodation Costs Summary", status = "warning", width = 6,
                    tableOutput("ifc_costs_summary")
                ),
                box(title = "Meal Attendance Summary", status = "info", width = 6,
                    tableOutput("ifc_meals_summary")
                )
              ),
              fluidRow(
                tabBox(
                  title = "IFC Details by Category",
                  id = "ifc_tabs",
                  width = 12,
                  tabPanel("Stay Distribution", 
                           fluidRow(
                             column(6, plotlyOutput("ifc_stay_distribution")),
                             column(6, plotlyOutput("ifc_age_distribution"))
                           )
                  ),
                  tabPanel("Financial Breakdown", 
                           fluidRow(
                             column(6, plotlyOutput("ifc_cost_breakdown")),
                             column(6, tableOutput("ifc_financial_summary"))
                           )
                  )
                )
              )
      ),
      
      # Cost Calculator tab (existing with enhancements)
      tabItem(tabName = "costs",
              fluidRow(
                valueBoxOutput("total_cost", width = 4),
                valueBoxOutput("guest_charge", width = 4),
                valueBoxOutput("host_charge", width = 4)
              ),
              fluidRow(
                box(title = "Cost Breakdown by Night", status = "primary", width = 6,
                    plotlyOutput("cost_breakdown")
                ),
                box(title = "Cost Breakdown by Age Category", status = "info", width = 6,
                    plotlyOutput("age_cost_breakdown")
                )
              ),
              fluidRow(
                box(title = "Individual Guest Costs", status = "info", width = 12,
                    downloadButton("downloadCostData", "Download Cost Data", class = "action-button"),
                    hr(),
                    DTOutput("guest_costs_table")
                )
              )
      )
    )
  ),
  skin = "blue",
  title = "Wedding Dashboard"
)

# Server
server <- function(input, output, session) {
  
    # Add diagnostic button to the dashboard
  output$diagnosticOutput <- renderPrint({
    # This will capture any diagnostic output
    req(input$runDiagnostic)
    if (input$runDiagnostic > 0) {
      req(processed_data())
      diagnose_data_structure(processed_data())
    }
  })
  
  # Add this to your dashboard UI (inside dashboardBody or in any tab):
  # actionButton("runDiagnostic", "Run Diagnostics"),
  # verbatimTextOutput("diagnosticOutput")
  
  # Create a diagnostic observer that runs once when data is loaded
  observe({
    req(processed_data())
    
    # Write diagnostics to a file so they don't get lost in the console
    diagnostics <- diagnose_data_structure(processed_data())
    
    # Print to console
    cat(diagnostics)
    
    # Also save to a file
    writeLines(diagnostics, "dashboard_diagnostics.txt")
    
    # Only run once
    if (!is.null(processed_data())) {
      return(invisible(NULL))
    }
  })
  # Source our new functions
  source("rates.R")
  source("helpers.R")
  
  # Storage for processed data
  processed_data <- reactiveVal(NULL)
  
  # Create a synthetic timeline for RSVP responses (for demonstration)
  create_timeline_data <- function(guests) {
    # Get all guests who have responded
    responded_guests <- guests %>%
      filter(!is.na(wedding_rsvp)) %>%
      mutate(response_type = wedding_rsvp)
    
    total_responses <- nrow(responded_guests)
    
    if (total_responses > 0) {
      # Create a synthetic timeline (last 30 days)
      end_date <- Sys.Date()
      start_date <- end_date - 30
      
      # Distribute responses across the timeline
      set.seed(123) # For reproducibility
      date_range <- seq(start_date, end_date, by = "day")
      
      # Create a distribution that's heavier toward recent dates
      weights <- seq(1, 10, length.out = length(date_range))
      response_dates <- sample(date_range, total_responses, replace = TRUE, prob = weights)
      
      # Create a data frame with the timeline
      timeline_data <- data.frame(
        response_date = response_dates,
        response_type = responded_guests$response_type
      ) %>%
        arrange(response_date) %>%
        group_by(response_date, response_type) %>%
        summarize(daily_count = n(), .groups = 'drop') %>%
        group_by(response_type) %>%
        mutate(cumulative = cumsum(daily_count)) %>%
        ungroup()
      
      return(timeline_data)
    } else {
      return(NULL)
    }
  }
  
  # Add debugging code to check age categories and camping status
  observe({
    req(processed_data())
    
    # Output information to the console for debugging
    cat("\n===== DEBUGGING AGE CATEGORIES =====\n")
    
    # Check if age_category column exists
    if ("age_category" %in% names(processed_data()$guests)) {
      cat("Age categories found in guest data:\n")
      print(table(processed_data()$guests$age_category))
      
      # Check for camping guests with age categories
      if ("is_camping" %in% names(processed_data()$guests)) {
        camping_guests <- processed_data()$guests %>% 
          filter(is_camping) %>% 
          select(first_name, last_name, age_category)
        
        cat("\nCamping guests with age categories:\n")
        if (nrow(camping_guests) > 0) {
          print(camping_guests)
        } else {
          cat("No camping guests found.\n")
        }
      }
      
      # Check age category assignment from the Budget Invite List
      cat("\nSummary of guest counts by age category:\n")
      print(processed_data()$age_category_summary)
    } else {
      cat("No age_category column found in processed_data()$guests\n")
    }
    
    cat("\n===== END DEBUGGING =====\n")
  })
  
  # Process data once when file is uploaded
  observeEvent(input$guestlist, {
    req(input$guestlist)
    
    # Show progress notification
    withProgress(message = "Processing guest data...", {
      
      # Step 1: Read the file
      file_path <- input$guestlist$datapath
      setProgress(0.2, detail = "Reading file...")
      
      # Process the guest data and ensure age categories and costs are calculated
      setProgress(0.4, detail = "Calculating costs...")
      results <- generate_wedding_reports(file_path)
      
      # Ensure numeric cost fields
      setProgress(0.7, detail = "Finalizing calculations...")
      results$guests <- ensure_numeric_costs(results$guests)
      results$guest_costs <- ensure_numeric_costs(results$guest_costs)
      
      # Store the processed data
      setProgress(0.9, detail = "Storing results...")
      processed_data(results)
    })
  })
  
  # Store selected party for detailed view
  selected_party <- reactiveVal(NULL)
  
  # Update selected party when party is clicked
  observeEvent(input$comprehensive_party_table_rows_selected, {
    req(input$comprehensive_party_table_rows_selected)
    req(processed_data()$party_summary)
    
    selected_row <- input$comprehensive_party_table_rows_selected
    selected_party(processed_data()$party_summary$party[selected_row])
  })
  
  # Overview tab outputs
  output$total_guests <- renderValueBox({
    req(processed_data())
    valueBox(
      nrow(processed_data()$guests),
      "Total Invited Guests",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$wedding_attending <- renderValueBox({
    req(processed_data())
    attending_count <- sum(processed_data()$guests$wedding_rsvp == "Joyfully Accept", na.rm = TRUE)
    valueBox(
      attending_count,
      "Attending Wedding",
      icon = icon("glass-cheers"),
      color = "green"
    )
  })
  
  output$friday_attending <- renderValueBox({
    req(processed_data())
    friday_count <- sum(processed_data()$guests$fridayshabbat_rsvp == "Joyfully Accept", na.rm = TRUE)
    valueBox(
      friday_count,
      "Attending Friday",
      icon = icon("star-of-david"),
      color = "yellow"
    )
  })
  
  output$saturday_attending <- renderValueBox({
    req(processed_data())
    saturday_count <- sum(processed_data()$guests$saturday_onsite_rsvp == "Yes, I will join on Saturday" | 
                            processed_data()$guests$saturday_offsite_rsvp == "Yes, I will join for lunch only" |
                            processed_data()$guests$saturday_offsite_rsvp == "Yes, I will join for dinner only" |
                            processed_data()$guests$saturday_offsite_rsvp == "Yes, I will join for lunch and dinner", 
                          na.rm = TRUE)
    valueBox(
      saturday_count,
      "Attending Saturday",
      icon = icon("calendar-day"),
      color = "orange"
    )
  })
  
  output$rsvp_status_plot <- renderPlotly({
    req(processed_data())
    
    # Count RSVPs
    wedding_rsvp_counts <- processed_data()$guests %>%
      group_by(response = ifelse(is.na(wedding_rsvp), "No Response", wedding_rsvp)) %>%
      summarize(count = n()) %>%
      arrange(desc(count))
    
    # Create plot
    p <- plot_ly(wedding_rsvp_counts, labels = ~response, values = ~count, type = 'pie',
                 textinfo = 'label+percent', 
                 marker = list(colors = c('#2E8B57', '#DC3545', '#6C757D')))
    p <- p %>% layout(title = "Wedding RSVP Status")
    p
  })
  
  output$accommodation_plot <- renderPlotly({
    req(processed_data())
    
    # Extract data from accommodation summary
    acc_summary <- processed_data()$accommodation_summary
    
    # Use pre-calculated summary values
    nights_data <- data.frame(
      night = c("Friday", "Saturday", "Sunday"),
      count = c(acc_summary$friday_count, acc_summary$saturday_count, acc_summary$sunday_count)
    )
    
    # Create plot
    p <- plot_ly(nights_data, x = ~night, y = ~count, type = 'bar',
                 marker = list(color = c('#5DADE2', '#F4D03F', '#58D68D')))
    p <- p %>% layout(title = "Guests Staying Each Night",
                      xaxis = list(title = ""),
                      yaxis = list(title = "Number of Guests"))
    p
  })
  
  output$rsvp_timeline <- renderPlotly({
    req(processed_data())
    
    # Generate timeline data from the guests data
    timeline_data <- create_timeline_data(processed_data()$guests)
    
    if (!is.null(timeline_data) && nrow(timeline_data) > 0) {
      # Create the plot
      p <- plot_ly() %>%
        add_trace(
          data = subset(timeline_data, response_type == "Joyfully Accept"),
          x = ~response_date, 
          y = ~cumulative, 
          type = 'scatter', 
          mode = 'lines+markers',
          name = 'Accepted', 
          line = list(color = '#28a745'),
          marker = list(color = '#28a745')
        )
      
      if ("Regretfully Decline" %in% timeline_data$response_type) {
        p <- p %>% add_trace(
          data = subset(timeline_data, response_type == "Regretfully Decline"),
          x = ~response_date, 
          y = ~cumulative, 
          type = 'scatter', 
          mode = 'lines+markers',
          name = 'Declined', 
          line = list(color = '#dc3545'),
          marker = list(color = '#dc3545')
        )
      }
      
      p <- p %>% layout(
        title = "RSVP Response Timeline",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Cumulative Responses"),
        hovermode = "x unified"
      )
      
      return(p)
    } else {
      # Create placeholder if no responses
      plot_ly() %>%
        layout(
          title = "RSVP Timeline (No Responses Yet)",
          annotations = list(
            x = 0.5, y = 0.5, 
            text = "Waiting for RSVP responses to visualize timeline",
            showarrow = FALSE
          )
        )
    }
  })
  
  # Quick Insights tab outputs
  output$rsvp_status_table <- renderTable({
    req(processed_data())
    
    data.frame(
      Category = c("Total Invited", "Accepted", "Declined", "No Response"),
      Count = c(
        nrow(processed_data()$guests),
        sum(processed_data()$guests$wedding_rsvp == "Joyfully Accept", na.rm = TRUE),
        sum(processed_data()$guests$wedding_rsvp == "Regretfully Decline", na.rm = TRUE),
        sum(is.na(processed_data()$guests$wedding_rsvp) | processed_data()$guests$wedding_rsvp == "", na.rm = TRUE)
      )
    )
  })
  
  output$accommodation_summary_table <- renderTable({
    req(processed_data())
    
    # Use pre-calculated accommodation summary
    acc_summary <- processed_data()$accommodation_summary
    
    data.frame(
      Category = c(
        "Friday Night Guests", 
        "Saturday Night Guests", 
        "Sunday Night Guests",
        "Standard Lodging",
        "Camping"
      ),
      Count = c(
        acc_summary$friday_count,
        acc_summary$saturday_count,
        acc_summary$sunday_count,
        acc_summary$standard_lodging_count,
        acc_summary$camping_count
      )
    )
  })
  
  output$meal_summary_table <- renderTable({
    req(processed_data())
    
    # Use pre-calculated meal counts
    meal_counts <- processed_data()$meal_counts
    
    data.frame(
      Meal = c(
        "Friday Dinner",
        "Saturday Breakfast",
        "Saturday Lunch",
        "Saturday Dinner",
        "Sunday Breakfast",
        "Wedding Meal (Sun)",
        "Sunday Dinner",
        "Monday Breakfast"
      ),
      Count = c(
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
  })
  
  # Age Category summary table - using pre-calculated data
  output$age_category_table <- renderTable({
    req(processed_data())
    
    # The problem is that age_category_summary doesn't have the expected columns
    # Let's recalculate them from guest_costs
    processed_data()$guest_costs %>%
      group_by(age_category) %>%
      summarize(
        "Total Guests" = n(),
        "Friday" = sum(is_staying_friday, na.rm = TRUE),
        "Saturday" = sum(is_staying_saturday, na.rm = TRUE),
        "Sunday" = sum(is_staying_sunday, na.rm = TRUE)
      )
  })
  
  output$cost_summary_table <- renderTable({
    req(processed_data())
    
    # Use pre-calculated accommodation summary
    acc_summary <- processed_data()$accommodation_summary
    
    data.frame(
      Category = c(
        "Friday Costs",
        "Saturday Costs",
        "Sunday Costs",
        "Total Costs",
        "Total Guest Charges",
        "Total Host Charges"
      ),
      Amount = c(
        paste0("$", format(acc_summary$total_friday_cost, big.mark = ",")),
        paste0("$", format(acc_summary$total_saturday_cost, big.mark = ",")),
        paste0("$", format(acc_summary$total_sunday_cost, big.mark = ",")),
        paste0("$", format(acc_summary$grand_total_cost, big.mark = ",")),
        paste0("$", format(acc_summary$grand_total_guest_charge, big.mark = ",")),
        paste0("$", format(acc_summary$grand_total_host_charge, big.mark = ","))
      )
    )
  })
  
  output$data_issues_table <- renderDT({
    req(processed_data())
    
    issues <- processed_data()$data_issues
    
    if (length(issues) > 0) {
      issue_df <- data.frame(
        Issue_Type = character(),
        Row_Numbers = character(),
        Description = character(),
        stringsAsFactors = FALSE
      )
      
      if (!is.null(issues$missing_names)) {
        issue_df <- rbind(issue_df, data.frame(
          Issue_Type = "Missing Names",
          Row_Numbers = paste(issues$missing_names, collapse = ", "),
          Description = "Guests without first names",
          stringsAsFactors = FALSE
        ))
      }
      
      if (!is.null(issues$invalid_emails)) {
        issue_df <- rbind(issue_df, data.frame(
          Issue_Type = "Invalid Emails",
          Row_Numbers = paste(issues$invalid_emails, collapse = ", "),
          Description = "Emails with invalid format",
          stringsAsFactors = FALSE
        ))
      }
      
      if (!is.null(issues$inconsistent_friday)) {
        issue_df <- rbind(issue_df, data.frame(
          Issue_Type = "Inconsistent Responses",
          Row_Numbers = paste(issues$inconsistent_friday, collapse = ", "),
          Description = "Attending Friday but declined wedding",
          stringsAsFactors = FALSE
        ))
      }
      
      datatable(issue_df,
                options = list(pageLength = 5, dom = 't'),
                rownames = FALSE)
    } else {
      datatable(data.frame(
        Message = "No data quality issues found!",
        stringsAsFactors = FALSE
      ), options = list(dom = 't'), rownames = FALSE)
    }
  })
  
  # NEW: Comprehensive Party Table outputs
  output$comprehensive_party_table <- renderDT({
    req(processed_data())
    
    # Select only the most important columns for display
    party_display <- processed_data()$party_summary %>%
      select(
        party,
        party_name, 
        total_guests, 
        wedding_attending, 
        wedding_declining,
        wedding_no_response,
        friday_attending, 
        saturday_attending,
        party_email
      ) %>%
      rename(
        "Party ID" = party,
        "Party Name" = party_name,
        "Total Guests" = total_guests,
        "Wedding - Yes" = wedding_attending,
        "Wedding - No" = wedding_declining,
        "Wedding - No Reply" = wedding_no_response,
        "Friday - Yes" = friday_attending,
        "Saturday - Yes" = saturday_attending,
        "Email" = party_email
      )
    
    datatable(
      party_display,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      selection = 'single',
      rownames = FALSE
    )
  })
  
  # Show detailed guest information for selected party
  output$party_guests_table <- renderDT({
    req(processed_data())
    req(selected_party())
    
    party_id <- selected_party()
    
    # Get all guests in the selected party
    party_guests <- processed_data()$guests %>%
      filter(party == party_id) %>%
      select(
        first_name,
        last_name,
        wedding_rsvp,
        fridayshabbat_rsvp,
        saturday_onsite_rsvp,
        saturday_offsite_rsvp,
        meal_preferences,
        dietary_restrictions,
        is_staying_friday,
        is_staying_saturday, 
        is_staying_sunday,
        is_camping,
        total_guest_charge
      ) %>%
      rename(
        "First Name" = first_name,
        "Last Name" = last_name,
        "Wedding RSVP" = wedding_rsvp,
        "Friday RSVP" = fridayshabbat_rsvp,
        "Saturday Onsite" = saturday_onsite_rsvp,
        "Saturday Offsite" = saturday_offsite_rsvp,
        "Meal Preference" = meal_preferences,
        "Dietary Restrictions" = dietary_restrictions,
        "Friday Stay" = is_staying_friday,
        "Saturday Stay" = is_staying_saturday,
        "Sunday Stay" = is_staying_sunday,
        "Camping" = is_camping,
        "Guest Charge" = total_guest_charge
      )
    
    datatable(
      party_guests,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Accommodations tab outputs
  output$total_staying <- renderValueBox({
    req(processed_data())
    
    # Use pre-calculated stats
    acc_summary <- processed_data()$accommodation_summary
    
    # Calculate total overnight guests
    staying_count <- acc_summary$standard_lodging_count + acc_summary$camping_count
    
    valueBox(
      staying_count,
      "Total Overnight Guests",
      icon = icon("hotel"),
      color = "blue"
    )
  })
  
  output$camping_count <- renderValueBox({
    req(processed_data())
    
    # Use pre-calculated camping count
    acc_summary <- processed_data()$accommodation_summary
    
    valueBox(
      acc_summary$camping_count,
      "Camping",
      icon = icon("campground"),
      color = "green"
    )
  })
  
  output$lodging_count <- renderValueBox({
    req(processed_data())
    
    # Use pre-calculated standard lodging count
    acc_summary <- processed_data()$accommodation_summary
    
    valueBox(
      acc_summary$standard_lodging_count,
      "Standard Lodging",
      icon = icon("bed"),
      color = "purple"
    )
  })
  
  output$nights_booked <- renderValueBox({
    req(processed_data())
    
    # Use pre-calculated stats
    acc_summary <- processed_data()$accommodation_summary
    
    # Calculate total nights booked
    total_nights <- acc_summary$friday_count + acc_summary$saturday_count + acc_summary$sunday_count
    
    valueBox(
      total_nights,
      "Total Nights Booked",
      icon = icon("moon"),
      color = "navy"
    )
  })
  
  
  output$stays_by_night_category_plot <- renderPlotly({
    req(processed_data())
    
    # Create a data frame with stays by night and age category
    stays_data <- processed_data()$guest_costs %>%
      filter(is_staying_friday | is_staying_saturday | is_staying_sunday) %>%
      # Reshape data to long format for easier plotting
      pivot_longer(
        cols = c(is_staying_friday, is_staying_saturday, is_staying_sunday),
        names_to = "stay_night",
        values_to = "is_staying"
      ) %>%
      # Clean up the night names
      mutate(
        night = case_when(
          stay_night == "is_staying_friday" ~ "Friday",
          stay_night == "is_staying_saturday" ~ "Saturday",
          stay_night == "is_staying_sunday" ~ "Sunday"
        )
      ) %>%
      # Only keep records where the guest is staying
      filter(is_staying) %>%
      # Count by night and age category
      group_by(night, age_category) %>%
      summarize(count = n(), .groups = 'drop') %>%
      # Ensure nights are in the correct order
      mutate(night = factor(night, levels = c("Friday", "Saturday", "Sunday")))
    
    # Define a pleasing color palette for age categories
    color_palette <- c(
      "Adults 21+ Room" = "#3498db",      # Blue
      "Adults 21+ Camping" = "#2ecc71",   # Green
      "Guests 12-21 Room" = "#9b59b6",    # Purple
      "Children 5-12 Room" = "#e74c3c",   # Red
      "Children <5 Room" = "#f39c12"      # Orange
    )
    
    # Create the stacked bar chart
    p <- plot_ly(
      data = stays_data,
      x = ~night,
      y = ~count,
      color = ~age_category,
      colors = color_palette,
      type = "bar"
    ) %>%
      layout(
        title = "Guest Stays by Night and Age Category",
        xaxis = list(title = "Night", categoryorder = "array", categoryarray = c("Friday", "Saturday", "Sunday")),
        yaxis = list(title = "Number of Guests"),
        barmode = "stack",
        legend = list(title = list(text = "Age Category")),
        hovermode = "closest"
      )
    
    return(p)
  })
  
  # You can also add this helper function to create a data table showing the same information
  output$stays_by_night_category_table <- renderTable({
    req(processed_data())
    
    # Create a summary table of stays by night and age category
    stays_summary <- processed_data()$guest_costs %>%
      group_by(age_category) %>%
      summarize(
        `Friday` = sum(is_staying_friday, na.rm = TRUE),
        `Saturday` = sum(is_staying_saturday, na.rm = TRUE), 
        `Sunday` = sum(is_staying_sunday, na.rm = TRUE),
        `Total` = n()
      ) %>%
      arrange(desc(`Total`))
    
    # Add a totals row
    totals <- data.frame(
      age_category = "Total",
      Friday = sum(stays_summary$Friday),
      Saturday = sum(stays_summary$Saturday),
      Sunday = sum(stays_summary$Sunday),
      Total = sum(stays_summary$Total)
    )
    
    bind_rows(stays_summary, totals)
  })
  

  
  output$accommodation_table <- renderDT({
    req(processed_data())
    
    # Select only relevant columns for display
    accommodation_display <- processed_data()$guest_costs %>%
      filter(is_staying_friday | is_staying_saturday | is_staying_sunday) %>%
      select(
        first_name,
        last_name,
        age_category,
        is_staying_friday,
        is_staying_saturday,
        is_staying_sunday,
        is_camping,
        total_cost,
        total_guest_charge,
        total_host_charge
      ) %>%
      rename(
        "First Name" = first_name,
        "Last Name" = last_name,
        "Age Category" = age_category,
        "Friday" = is_staying_friday,
        "Saturday" = is_staying_saturday,
        "Sunday" = is_staying_sunday,
        "Camping" = is_camping,
        "Total Cost" = total_cost,
        "Guest Charge" = total_guest_charge,
        "Host Charge" = total_host_charge
      )
    
    datatable(
      accommodation_display,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  })
  
  output$stay_distribution <- renderPlotly({
    req(processed_data())
    
    # Use pre-calculated guest costs for accurate stay patterns
    stay_data <- processed_data()$guest_costs %>%
      filter(is_staying_friday | is_staying_saturday | is_staying_sunday) %>%
      summarize(
        `Friday Only` = sum(is_staying_friday & !is_staying_saturday & !is_staying_sunday, na.rm = TRUE),
        `Saturday Only` = sum(!is_staying_friday & is_staying_saturday & !is_staying_sunday, na.rm = TRUE),
        `Friday & Saturday` = sum(is_staying_friday & is_staying_saturday & !is_staying_sunday, na.rm = TRUE),
        `Saturday & Sunday` = sum(!is_staying_friday & is_staying_saturday & is_staying_sunday, na.rm = TRUE),
        `All Three Nights` = sum(is_staying_friday & is_staying_saturday & is_staying_sunday, na.rm = TRUE),
        `Friday & Sunday` = sum(is_staying_friday & !is_staying_saturday & is_staying_sunday, na.rm = TRUE),
        `Sunday Only` = sum(!is_staying_friday & !is_staying_saturday & is_staying_sunday, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = everything(), names_to = "stay_pattern", values_to = "count") %>%
      filter(count > 0)  # Only show patterns with guests
    
    # Create plot
    p <- plot_ly(stay_data, x = ~stay_pattern, y = ~count, type = 'bar',
                 marker = list(color = '#5DADE2'))
    p <- p %>% layout(title = "Stay Pattern Distribution",
                      xaxis = list(title = ""),
                      yaxis = list(title = "Number of Guests"))
    p
  })
  
  # Meal Planning tab outputs
  output$meal_summary_plot <- renderPlotly({
    req(processed_data())
    
    # Use pre-calculated meal counts
    meal_counts <- processed_data()$meal_counts
    
    # Define chronological meal order
    meal_order <- c(
      "Friday Dinner", 
      "Saturday Breakfast", 
      "Saturday Lunch", 
      "Saturday Dinner", 
      "Sunday Breakfast",
      "Sunday Lunch (Wedding)",
      "Sunday Dinner",
      "Monday Breakfast"
    )
    
    # Format data for plotting
    meal_data <- data.frame(
      Meal = meal_order,
      On_Site = c(
        meal_counts$friday_dinner_onsite,
        meal_counts$saturday_breakfast_onsite,
        meal_counts$saturday_lunch_onsite,
        meal_counts$saturday_dinner_onsite,
        meal_counts$sunday_breakfast_onsite,
        0, # For visualization purposes
        meal_counts$sunday_dinner_onsite,
        meal_counts$monday_breakfast_onsite
      ),
      Off_Site = c(
        meal_counts$friday_dinner_offsite,
        0,
        meal_counts$saturday_lunch_offsite,
        meal_counts$saturday_dinner_offsite,
        0,
        0,
        0,
        0
      ),
      Wedding_Reception = c(
        0, 0, 0, 0, 0, meal_counts$total_sunday_lunch, 0, 0
      )
    )
    
    # Convert to long format for stacked bar chart
    meal_data_long <- meal_data %>%
      pivot_longer(cols = c("On_Site", "Off_Site", "Wedding_Reception"), 
                   names_to = "Guest_Type", values_to = "Count") %>%
      filter(Count > 0) %>%  # Only show meals with guests
      # Explicitly set the meal factor levels in chronological order
      mutate(Meal = factor(Meal, levels = meal_order))
    
    # Create plot with ordered x-axis
    p <- plot_ly(meal_data_long, x = ~Meal, y = ~Count, color = ~Guest_Type, type = 'bar',
                 colors = c("On_Site" = "#3498DB", "Off_Site" = "#E74C3C", "Wedding_Reception" = "#2ECC71"))
    
    p <- p %>% layout(title = "Meal Attendance by Guest Type (Chronological)",
                      xaxis = list(
                        title = "",
                        categoryorder = "array",
                        categoryarray = meal_order
                      ),
                      yaxis = list(title = "Number of Guests"),
                      barmode = 'stack',
                      legend = list(title = list(text = "Guest Type")))
    p
  })
  
  # Helper function to create meal attendance tables with improved handling
  create_meal_table <- function(meal_name) {
    req(processed_data())
    
    # Use pre-calculated meal rosters if available
    if (!is.null(processed_data()$meal_rosters) && 
        meal_name %in% names(processed_data()$meal_rosters)) {
      
      meal_attendees <- processed_data()$meal_rosters[[meal_name]]$attendees
      
    } else {
      # Define the filter condition based on meal name
      filter_condition <- switch(
        meal_name,
        "friday_dinner" = expr(is_staying_friday | (fridayshabbat_rsvp == "Joyfully Accept" & !is_staying_friday)),
        "saturday_breakfast" = expr(is_staying_friday),
        "saturday_lunch" = expr(is_staying_saturday | saturday_offsite_rsvp == "Yes, I will join for lunch only" | saturday_offsite_rsvp == "Yes, I will join for lunch and dinner"),
        "saturday_dinner" = expr(is_staying_saturday | saturday_offsite_rsvp == "Yes, I will join for dinner only" | saturday_offsite_rsvp == "Yes, I will join for lunch and dinner"),
        "sunday_breakfast" = expr(is_staying_saturday),
        "sunday_lunch" = expr(wedding_rsvp == "Joyfully Accept"),
        "sunday_dinner" = expr(is_staying_sunday),
        "monday_breakfast" = expr(is_staying_sunday),
        expr(FALSE) # Default empty condition
      )
      
      # Filter guests based on meal attendance
      meal_attendees <- processed_data()$guests %>%
        filter(!!filter_condition)
    }
    
    # Create the display table with proper flags
    meal_display <- meal_attendees %>%
      select(
        first_name,
        last_name,
        age_category,
        meal_preferences,
        dietary_restrictions,
        is_vegetarian,
        has_special_diet
      ) %>%
      mutate(
        is_vegetarian = ifelse(is.na(is_vegetarian), 
                               meal_preferences == "No meat" | meal_preferences == "Opt-in for fish only", 
                               is_vegetarian),
        has_special_diet = ifelse(is.na(has_special_diet),
                                  !is.na(dietary_restrictions) & dietary_restrictions != "",
                                  has_special_diet)
      ) %>%
      rename(
        "First Name" = first_name,
        "Last Name" = last_name,
        "Age Category" = age_category,
        "Meal Preference" = meal_preferences,
        "Dietary Restrictions" = dietary_restrictions,
        "Vegetarian" = is_vegetarian,
        "Special Diet" = has_special_diet
      )
    
    return(meal_display)
  }
  
  # Helper function to create meal summary
  create_meal_summary <- function(meal_name) {
    req(processed_data())
    
    # Use pre-calculated meal summaries if available
    if (!is.null(processed_data()$meal_counts_by_age) && 
        meal_name %in% names(processed_data()$meal_counts_by_age)) {
      
      return(processed_data()$meal_counts_by_age[[meal_name]])
      
    } else {
      # Get the meal attendee table
      meal_table <- create_meal_table(meal_name)
      
      # Calculate summary by age category
      meal_summary <- meal_table %>%
        group_by(`Age Category`) %>%
        summarize(
          Count = n(),
          Vegetarian = sum(Vegetarian, na.rm = TRUE),
          `Meat/Fish` = sum(!Vegetarian, na.rm = TRUE),
          `Special Diet` = sum(`Special Diet`, na.rm = TRUE)
        )
      
      return(meal_summary)
    }
  }
  
  # Generate all meal tables using the helper function
  output$friday_dinner_table <- renderDT({
    datatable(create_meal_table("friday_dinner"), 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$saturday_breakfast_table <- renderDT({
    datatable(create_meal_table("saturday_breakfast"), 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$saturday_lunch_table <- renderDT({
    datatable(create_meal_table("saturday_lunch"), 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$saturday_dinner_table <- renderDT({
    datatable(create_meal_table("saturday_dinner"), 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$sunday_breakfast_table <- renderDT({
    datatable(create_meal_table("sunday_breakfast"), 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$sunday_lunch_table <- renderDT({
    datatable(create_meal_table("sunday_lunch"), 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$sunday_dinner_table <- renderDT({
    datatable(create_meal_table("sunday_dinner"), 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$monday_breakfast_table <- renderDT({
    datatable(create_meal_table("monday_breakfast"), 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  # Generate meal summaries using the helper function
  output$friday_dinner_summary <- renderTable({
    create_meal_summary("friday_dinner")
  })
  
  output$friday_dinner_plot <- renderPlotly({
    req(processed_data())
    summary_data <- create_meal_summary("friday_dinner")
    
    if (nrow(summary_data) > 0) {
      plot_data <- summary_data %>%
        select(Category = `Age Category`, Count, Vegetarian, `Meat/Fish`, `Special Diet`) %>%
        pivot_longer(cols = c(Count, Vegetarian, `Meat/Fish`, `Special Diet`), 
                     names_to = "Metric", values_to = "Value")
      
      p <- plot_ly(plot_data, x = ~Category, y = ~Value, color = ~Metric, type = 'bar',
                   colors = c("Count" = "#3498DB", "Vegetarian" = "#2ECC71", 
                              "Meat/Fish" = "#E74C3C", "Special Diet" = "#F39C12"))
      p <- p %>% layout(title = "Friday Dinner Attendance",
                        xaxis = list(title = ""),
                        yaxis = list(title = "Number of Guests"),
                        barmode = 'group')
      return(p)
    } else {
      # Simple message if no data
      plot_ly() %>%
        layout(
          title = "Friday Dinner Attendance",
          annotations = list(
            x = 0.5, y = 0.5, 
            text = "No attendance data available",
            showarrow = FALSE
          )
        )
    }
  })
  
  # Generate remaining meal summaries
  output$saturday_breakfast_summary <- renderTable({
    create_meal_summary("saturday_breakfast")
  })
  
  output$saturday_lunch_summary <- renderTable({
    create_meal_summary("saturday_lunch")
  })
  
  output$saturday_dinner_summary <- renderTable({
    create_meal_summary("saturday_dinner")
  })
  
  output$sunday_breakfast_summary <- renderTable({
    create_meal_summary("sunday_breakfast")
  })
  
  output$sunday_lunch_summary <- renderTable({
    create_meal_summary("sunday_lunch")
  })
  
  output$sunday_dinner_summary <- renderTable({
    create_meal_summary("sunday_dinner")
  })
  
  output$monday_breakfast_summary <- renderTable({
    create_meal_summary("monday_breakfast")
  })
  
  output$meal_preferences_plot <- renderPlotly({
    req(processed_data())
    
    # Count meal preferences from pre-calculated guest data
    preferences <- processed_data()$guests %>%
      filter(!is.na(meal_preferences)) %>%
      group_by(preference = meal_preferences) %>%
      summarize(count = n()) %>%
      arrange(desc(count))
    
    # Create plot
    p <- plot_ly(preferences, x = ~preference, y = ~count, type = 'bar',
                 marker = list(color = '#9C27B0'))
    p <- p %>% layout(title = "Meal Preferences Distribution",
                      xaxis = list(title = ""),
                      yaxis = list(title = "Number of Guests"))
    p
  })
  
  output$dietary_restrictions_plot <- renderPlotly({
    req(processed_data())
    
    # Count dietary restrictions from pre-calculated guest data
    dietary <- processed_data()$guests %>%
      filter(!is.na(dietary_restrictions) & dietary_restrictions != "") %>%
      group_by(restriction = dietary_restrictions) %>%
      summarize(count = n()) %>%
      arrange(desc(count))
    
    if (nrow(dietary) > 0) {
      # Create plot
      p <- plot_ly(dietary, x = ~restriction, y = ~count, type = 'bar',
                   marker = list(color = '#E67E22'))
      p <- p %>% layout(title = "Dietary Restrictions",
                        xaxis = list(title = ""),
                        yaxis = list(title = "Number of Guests"))
      return(p)
    } else {
      # Create empty plot with message
      plot_ly() %>%
        layout(
          title = "Dietary Restrictions",
          annotations = list(
            x = 0.5, y = 0.5, 
            text = "No dietary restrictions recorded",
            showarrow = FALSE
          )
        )
    }
  })
  
  output$dietary_table <- renderDT({
    req(processed_data())
    
    # Extract dietary restrictions from pre-calculated guest data
    dietary_data <- processed_data()$guests %>%
      filter(!is.na(dietary_restrictions) & dietary_restrictions != "") %>%
      select(first_name, last_name, age_category, meal_preferences, dietary_restrictions) %>%
      rename(
        "First Name" = first_name,
        "Last Name" = last_name,
        "Age Category" = age_category,
        "Meal Preference" = meal_preferences,
        "Dietary Restrictions" = dietary_restrictions
      )
    
    if (nrow(dietary_data) > 0) {
      datatable(dietary_data,
                options = list(
                  pageLength = 10,
                  scrollX = TRUE,
                  autoWidth = TRUE
                ),
                rownames = FALSE)
    } else {
      datatable(data.frame(
        Message = "No special dietary requirements found!",
        stringsAsFactors = FALSE
      ), options = list(dom = 't'), rownames = FALSE)
    }
  })
  
  # IFC Roster tab outputs
  output$ifc_roster_table <- renderDT({
    req(processed_data())
    
    # Get IFC roster from pre-calculated data
    ifc_data <- processed_data()$ifc_roster
    
    # If not available, create from guest costs
    if (is.null(ifc_data)) {
      ifc_data <- processed_data()$guest_costs %>%
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
          accommodation_type = ifelse(is_camping, "Camping", "Standard Lodging")
        )
    }
    
    # Format for display - ensure all cost columns are treated as numeric
    roster_display <- ifc_data %>%
      mutate(across(matches("cost|charge"), as.numeric)) %>%
      select(
        first_name,
        last_name,
        age_category,
        friday_dinner,
        saturday_breakfast, 
        saturday_lunch,
        saturday_dinner,
        sunday_breakfast,
        sunday_lunch,
        sunday_dinner,
        monday_breakfast,
        is_camping,
        total_cost,
        total_guest_charge,
        total_host_charge
      ) %>%
      # Format currency values
      mutate(
        total_cost = if_else(!is.na(total_cost), 
                             paste0("$", format(round(total_cost, 2), nsmall=2)), 
                             NA_character_),
        total_guest_charge = if_else(!is.na(total_guest_charge), 
                                     paste0("$", format(round(total_guest_charge, 2), nsmall=2)), 
                                     NA_character_),
        total_host_charge = if_else(!is.na(total_host_charge), 
                                    paste0("$", format(round(total_host_charge, 2), nsmall=2)), 
                                    NA_character_)
      ) %>%
      rename(
        "First Name" = first_name,
        "Last Name" = last_name,
        "Age Category" = age_category,
        "Friday Dinner" = friday_dinner,
        "Saturday Breakfast" = saturday_breakfast,
        "Saturday Lunch" = saturday_lunch,
        "Saturday Dinner" = saturday_dinner,
        "Sunday Breakfast" = sunday_breakfast,
        "Sunday Lunch" = sunday_lunch,
        "Sunday Dinner" = sunday_dinner,
        "Monday Breakfast" = monday_breakfast,
        "Camping" = is_camping,
        "Total Cost" = total_cost,
        "Guest Charge" = total_guest_charge,
        "Host Charge" = total_host_charge
      )
    
    datatable(
      roster_display,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  })
  
  output$ifc_costs_summary <- renderTable({
    req(processed_data())
    
    # Use pre-calculated accommodation summary
    acc_summary <- processed_data()$accommodation_summary
    
    # Create a more detailed cost summary
    data.frame(
      Category = c(
        "Total IFC Charge", 
        "Guest Payments", 
        "Host Subsidy",
        "Friday Total",
        "Saturday Total", 
        "Sunday Total"
      ),
      Amount = c(
        paste0("$", format(acc_summary$grand_total_cost, big.mark = ",")),
        paste0("$", format(acc_summary$grand_total_guest_charge, big.mark = ",")),
        paste0("$", format(acc_summary$grand_total_host_charge, big.mark = ",")),
        paste0("$", format(acc_summary$total_friday_cost, big.mark = ",")),
        paste0("$", format(acc_summary$total_saturday_cost, big.mark = ",")),
        paste0("$", format(acc_summary$total_sunday_cost, big.mark = ","))
      )
    )
  })
  
  output$ifc_meals_summary <- renderTable({
    req(processed_data())
    
    # Use pre-calculated meal counts
    meal_counts <- processed_data()$meal_counts
    
    # Create meal attendance summary
    data.frame(
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
      Attendees = c(
        meal_counts$total_friday_dinner,
        meal_counts$total_saturday_breakfast,
        meal_counts$total_saturday_lunch,
        meal_counts$total_saturday_dinner,
        meal_counts$total_sunday_breakfast,
        meal_counts$total_sunday_lunch,
        meal_counts$total_sunday_dinner,
        meal_counts$total_monday_breakfast
      ),
      Vegetarian = c(
        meal_counts$friday_dinner_vegetarian,
        meal_counts$saturday_breakfast_vegetarian,
        meal_counts$saturday_lunch_vegetarian,
        meal_counts$saturday_dinner_vegetarian,
        meal_counts$sunday_breakfast_vegetarian,
        meal_counts$sunday_lunch_vegetarian,
        meal_counts$sunday_dinner_vegetarian,
        meal_counts$monday_breakfast_vegetarian
      ),
      `Special Diet` = c(
        meal_counts$friday_dinner_special_diet,
        meal_counts$saturday_breakfast_special_diet,
        meal_counts$saturday_lunch_special_diet,
        meal_counts$saturday_dinner_special_diet,
        meal_counts$sunday_breakfast_special_diet,
        meal_counts$sunday_lunch_special_diet,
        meal_counts$sunday_dinner_special_diet,
        meal_counts$monday_breakfast_special_diet
      )
    )
  })
  
  output$ifc_stay_distribution <- renderPlotly({
    req(processed_data())
    
    # Use pre-calculated accommodation summary
    acc_summary <- processed_data()$accommodation_summary
    
    # Create night distribution
    nights_data <- data.frame(
      Night = c("Friday", "Saturday", "Sunday"),
      Count = c(acc_summary$friday_count, acc_summary$saturday_count, acc_summary$sunday_count)
    )
    
    # Create plot
    p <- plot_ly(nights_data, x = ~Night, y = ~Count, type = 'bar',
                 marker = list(color = c('#5DADE2', '#F4D03F', '#58D68D')))
    p <- p %>% layout(title = "Guests Staying Each Night",
                      xaxis = list(title = ""),
                      yaxis = list(title = "Number of Guests"))
    p
  })
  
  output$ifc_age_distribution <- renderPlotly({
    req(processed_data())
    
    # Use pre-calculated age category summary
    if ("age_category_summary" %in% names(processed_data())) {
      # Create plot
      age_counts <- processed_data()$age_category_summary %>%
        select(Category = age_category, Count = total_guests)
      
      p <- plot_ly(age_counts, x = ~Category, y = ~Count, type = 'bar',
                   marker = list(color = '#9C27B0'))
      p <- p %>% layout(title = "Guests by Age Category",
                        xaxis = list(title = ""),
                        yaxis = list(title = "Number of Guests"))
      return(p)
    } else {
      # Create placeholder if no age data
      plot_ly() %>%
        layout(
          title = "Guests by Age Category",
          annotations = list(
            x = 0.5, y = 0.5, 
            text = "No age category data available",
            showarrow = FALSE
          )
        )
    }
  })
  
  output$ifc_cost_breakdown <- renderPlotly({
    req(processed_data())
    
    # Use pre-calculated accommodation summary
    acc_summary <- processed_data()$accommodation_summary
    
    # Prepare data for visualization
    cost_data <- data.frame(
      category = c("Friday", "Saturday", "Sunday"),
      guest_charge = c(
        acc_summary$total_friday_guest_charge,
        acc_summary$total_saturday_guest_charge,
        acc_summary$total_sunday_guest_charge
      ),
      host_charge = c(
        acc_summary$total_friday_host_charge,
        acc_summary$total_saturday_host_charge,
        acc_summary$total_sunday_host_charge
      )
    )
    
    # Convert to long format for stacked bar chart
    cost_long <- cost_data %>%
      pivot_longer(cols = c("guest_charge", "host_charge"), 
                   names_to = "charge_type", values_to = "amount") %>%
      mutate(charge_type = ifelse(charge_type == "guest_charge", "Guest Charge", "Host Charge"))
    
    # Create plot
    p <- plot_ly(cost_long, x = ~category, y = ~amount, color = ~charge_type, type = 'bar',
                 colors = c("Guest Charge" = "#3498DB", "Host Charge" = "#E74C3C"))
    p <- p %>% layout(title = "Cost Breakdown by Night",
                      xaxis = list(title = ""),
                      yaxis = list(title = "Amount ($)"),
                      barmode = 'stack')
    p
  })
  
  output$ifc_financial_summary <- renderTable({
    req(processed_data())
    
    # Use pre-calculated age category summary for financial breakdown
    if ("age_category" %in% names(processed_data()$guest_costs)) {
      # By age category
      financial_by_type <- processed_data()$age_category_summary %>%
        select(
          Category = age_category,
          `IFC Total` = total_cost,
          `Guest Charges` = total_guest_charge,
          `Host Charges` = total_host_charge,
          `Number of Guests` = total_guests
        )
      
      # Add totals row
      totals <- financial_by_type %>%
        summarize(
          Category = "TOTAL",
          `IFC Total` = sum(`IFC Total`, na.rm = TRUE),
          `Guest Charges` = sum(`Guest Charges`, na.rm = TRUE),
          `Host Charges` = sum(`Host Charges`, na.rm = TRUE),
          `Number of Guests` = sum(`Number of Guests`, na.rm = TRUE)
        )
      
      financial_by_type <- bind_rows(financial_by_type, totals)
      
      # Format currency values
      financial_by_type <- financial_by_type %>%
        mutate(
          `IFC Total` = paste0("$", format(`IFC Total`, big.mark = ",")),
          `Guest Charges` = paste0("$", format(`Guest Charges`, big.mark = ",")),
          `Host Charges` = paste0("$", format(`Host Charges`, big.mark = ","))
        )
      
      return(financial_by_type)
    } else {
      # Simple overall summary if no age data
      acc_summary <- processed_data()$accommodation_summary
      
      financial_summary <- data.frame(
        Category = "Total",
        `IFC Total` = paste0("$", format(acc_summary$grand_total_cost, big.mark = ",")),
        `Guest Charges` = paste0("$", format(acc_summary$grand_total_guest_charge, big.mark = ",")),
        `Host Charges` = paste0("$", format(acc_summary$grand_total_host_charge, big.mark = ",")),
        `Number of Guests` = sum(processed_data()$guest_costs$is_staying_friday | 
                                   processed_data()$guest_costs$is_staying_saturday | 
                                   processed_data()$guest_costs$is_staying_sunday, 
                                 na.rm = TRUE)
      )
      
      return(financial_summary)
    }
  })
  
  # Cost Calculator tab outputs
  output$total_cost <- renderValueBox({
    req(processed_data())
    
    # Use pre-calculated total cost
    acc_summary <- processed_data()$accommodation_summary
    
    valueBox(
      paste0("$", format(acc_summary$grand_total_cost, big.mark = ",")),
      "Total Accommodation Cost",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$guest_charge <- renderValueBox({
    req(processed_data())
    
    # Use pre-calculated guest charge
    acc_summary <- processed_data()$accommodation_summary
    
    valueBox(
      paste0("$", format(acc_summary$grand_total_guest_charge, big.mark = ",")),
      "Total Guest Charges",
      icon = icon("credit-card"),
      color = "blue"
    )
  })
  
  output$host_charge <- renderValueBox({
    req(processed_data())
    
    # Use pre-calculated host charge
    acc_summary <- processed_data()$accommodation_summary
    
    valueBox(
      paste0("$", format(acc_summary$grand_total_host_charge, big.mark = ",")),
      "Total Host Charges",
      icon = icon("hand-holding-usd"),
      color = "red"
    )
  })
  
  output$cost_breakdown <- renderPlotly({
    req(processed_data())
    
    # Use pre-calculated accommodation summary
    acc_summary <- processed_data()$accommodation_summary
    
    # Format for plotting
    plot_data <- data.frame(
      category = c("Friday", "Saturday", "Sunday"),
      guest_charge = c(
        acc_summary$total_friday_guest_charge,
        acc_summary$total_saturday_guest_charge,
        acc_summary$total_sunday_guest_charge
      ),
      host_charge = c(
        acc_summary$total_friday_host_charge,
        acc_summary$total_saturday_host_charge,
        acc_summary$total_sunday_host_charge
      )
    )
    
    # Convert to long format for grouped bar chart
    cost_long <- plot_data %>%
      pivot_longer(cols = c("guest_charge", "host_charge"), 
                   names_to = "charge_type", values_to = "amount") %>%
      mutate(charge_type = ifelse(charge_type == "guest_charge", "Guest Charge", "Host Charge"))
    
    # Create plot
    p <- plot_ly(cost_long, x = ~category, y = ~amount, color = ~charge_type, type = 'bar',
                 colors = c("Guest Charge" = "#3498DB", "Host Charge" = "#E74C3C"))
    p <- p %>% layout(title = "Cost Breakdown by Night",
                      xaxis = list(title = ""),
                      yaxis = list(title = "Amount ($)"),
                      barmode = 'group')
    p
  })
  
  output$age_cost_breakdown <- renderPlotly({
    req(processed_data())
    
    # Check if age_category is in the data
    if ("age_category" %in% names(processed_data()$guest_costs)) {
      # Use pre-calculated age category summary
      age_cost_data <- processed_data()$age_category_summary %>%
        select(
          Category = age_category,
          `Guest Charge` = total_guest_charge,
          `Host Charge` = total_host_charge
        ) %>%
        pivot_longer(cols = c(`Guest Charge`, `Host Charge`), 
                     names_to = "Charge_Type", values_to = "Amount")
      
      # Create plot
      p <- plot_ly(age_cost_data, x = ~Category, y = ~Amount, color = ~Charge_Type, type = 'bar',
                   colors = c("Guest Charge" = "#3498DB", "Host Charge" = "#E74C3C"))
      p <- p %>% layout(title = "Cost Breakdown by Age Category",
                        xaxis = list(title = ""),
                        yaxis = list(title = "Amount ($)"),
                        barmode = 'group')
      p
    } else {
      # Create a placeholder if no age categories available
      plot_ly() %>%
        layout(
          title = "Cost Breakdown by Age Category",
          annotations = list(
            x = 0.5, y = 0.5, 
            text = "No age category data available",
            showarrow = FALSE
          )
        )
    }
  })
  
  output$guest_costs_table <- renderDT({
    req(processed_data())
    
    # Include all guests with any stay
    costs_display <- processed_data()$guest_costs %>%
      filter(is_staying_friday | is_staying_saturday | is_staying_sunday) %>%
      # Convert all cost columns to numeric
      mutate(across(matches("cost|charge"), as.numeric)) %>%
      # Format for display
      mutate(
        friday_cost = if_else(!is.na(friday_cost), 
                              paste0("$", format(round(friday_cost, 2), nsmall=2)), 
                              "$0.00"),
        saturday_cost = if_else(!is.na(saturday_cost), 
                                paste0("$", format(round(saturday_cost, 2), nsmall=2)), 
                                "$0.00"),
        sunday_cost = if_else(!is.na(sunday_cost), 
                              paste0("$", format(round(sunday_cost, 2), nsmall=2)), 
                              "$0.00"),
        total_cost = if_else(!is.na(total_cost), 
                             paste0("$", format(round(total_cost, 2), nsmall=2)), 
                             "$0.00"),
        total_guest_charge = if_else(!is.na(total_guest_charge), 
                                     paste0("$", format(round(total_guest_charge, 2), nsmall=2)), 
                                     "$0.00"),
        total_host_charge = if_else(!is.na(total_host_charge), 
                                    paste0("$", format(round(total_host_charge, 2), nsmall=2)), 
                                    "$0.00")
      ) %>%
      select(
        first_name,
        last_name,
        age_category,
        is_staying_friday,
        is_staying_saturday, 
        is_staying_sunday,
        is_camping,
        friday_cost,
        saturday_cost, 
        sunday_cost,
        total_cost,
        total_guest_charge,
        total_host_charge
      ) %>%
      rename(
        "First Name" = first_name,
        "Last Name" = last_name,
        "Age Category" = age_category,
        "Friday" = is_staying_friday,
        "Saturday" = is_staying_saturday,
        "Sunday" = is_staying_sunday,
        "Camping" = is_camping,
        "Friday Cost" = friday_cost,
        "Saturday Cost" = saturday_cost,
        "Sunday Cost" = sunday_cost,
        "Total Cost" = total_cost,
        "Guest Charge" = total_guest_charge,
        "Host Charge" = total_host_charge
      )
    
    datatable(costs_display,
              options = list(
                pageLength = 15,
                scrollX = TRUE,
                autoWidth = TRUE
              ),
              rownames = FALSE)
  })
  
  # Download handlers
  output$downloadPartyData <- downloadHandler(
    filename = function() {
      paste("party-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processed_data()$party_summary, file, row.names = FALSE)
    }
  )
  
  output$downloadAccommodationData <- downloadHandler(
    filename = function() {
      paste("accommodation-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      accommodation_data <- processed_data()$guest_costs %>%
        filter(is_staying_friday | is_staying_saturday | is_staying_sunday) %>%
        select(
          first_name, last_name, age_category, party,
          is_staying_friday, is_staying_saturday, is_staying_sunday,
          is_camping, total_cost, total_guest_charge, total_host_charge
        )
      write.csv(accommodation_data, file, row.names = FALSE)
    }
  )
  
  output$downloadIFCData <- downloadHandler(
    filename = function() {
      paste("ifc-roster-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Get guests staying at IFC
      ifc_data <- processed_data()$guest_costs %>%
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
          accommodation_type = ifelse(is_camping, "Camping", "Standard Lodging")
        )
      
      write.csv(ifc_data, file, row.names = FALSE)
    }
  )
  
  output$downloadCostData <- downloadHandler(
    filename = function() {
      paste("cost-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Only include guests with costs
      cost_data <- processed_data()$guest_costs %>%
        filter(as.numeric(total_cost) > 0) %>%
        mutate(
          # Ensure numeric cost fields
          friday_cost = as.numeric(friday_cost),
          saturday_cost = as.numeric(saturday_cost),
          sunday_cost = as.numeric(sunday_cost),
          total_cost = as.numeric(total_cost),
          total_guest_charge = as.numeric(total_guest_charge),
          total_host_charge = as.numeric(total_host_charge)
        ) %>%
        select(
          first_name, last_name, age_category, party,
          is_staying_friday, is_staying_saturday, is_staying_sunday,
          is_camping, friday_cost, saturday_cost, sunday_cost,
          total_cost, total_guest_charge, total_host_charge
        )
      
      write.csv(cost_data, file, row.names = FALSE)
    }
  )
  
  # Main download handler for all reports
  output$downloadReports <- downloadHandler(
    filename = function() {
      paste("wedding-reports-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      # Create a temporary directory
      temp_dir <- tempdir()
      report_dir <- file.path(temp_dir, "wedding_reports")
      
      # Force directory creation
      dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)
      
      # Export reports directly from pre-calculated data
      tryCatch({
        # Source the export_reports function if needed
        if (!exists("export_reports")) {
          source("wedding-rsvp-tracker.R")
        }
        
        # Export reports using pre-calculated data
        export_reports(processed_data(), report_dir)
        
      }, error = function(e) {
        # Write error information to a log file
        writeLines(paste("Error exporting reports:", conditionMessage(e)),
                   file.path(report_dir, "export_error_log.txt"))
        
        # Fallback to basic exports
        write.csv(processed_data()$party_summary, 
                  file.path(report_dir, "party_summary.csv"))
        
        write.csv(processed_data()$guest_costs, 
                  file.path(report_dir, "guest_costs.csv"))
        
        write.csv(as.data.frame(processed_data()$accommodation_summary), 
                  file.path(report_dir, "accommodation_summary.csv"))
      })
      
      # Create a zip file
      zip_file <- file.path(temp_dir, "wedding_reports.zip")
      
      tryCatch({
        # Use utils::zip as a fallback
        utils::zip(zipfile = zip_file, 
                   files = list.files(report_dir, full.names = TRUE),
                   flags = "-r9Xj")
        
        file.copy(zip_file, file)
        
      }, error = function(e) {
        # In case of any error, at least provide a notice
        writeLines(
          paste("Error creating zip:", conditionMessage(e), 
                "\nIndividual files can be downloaded from the app interface."),
          file.path(report_dir, "ERROR.txt")
        )
        
        # Copy at least one file to satisfy the download handler
        file.copy(list.files(report_dir, full.names = TRUE)[1], file)
      })
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
