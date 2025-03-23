# Wedding Dashboard App with Enhanced Views
# Updated version with IFC roster, meal schedule, and comprehensive party tables

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

# Source required functions
source("wedding-rsvp-tracker.R")
source("cost-summary-generator.R")

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
                box(title = "Accommodation by Age Category", status = "primary", width = 6,
                    plotlyOutput("age_accommodation_plot")
                ),
                box(title = "Accommodation by Night", status = "warning", width = 6,
                    plotlyOutput("night_accommodation_plot")
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
  
  # Reactive function to process data once CSV is uploaded
  results <- reactive({
    req(input$guestlist)
    
    file_path <- input$guestlist$datapath
    generate_wedding_reports(file_path)
  })
  
  # Store selected party for detailed view
  selected_party <- reactiveVal(NULL)
  
  # Update selected party when party is clicked
  observeEvent(input$comprehensive_party_table_rows_selected, {
    req(input$comprehensive_party_table_rows_selected)
    req(results()$party_summary)
    
    selected_row <- input$comprehensive_party_table_rows_selected
    selected_party(results()$party_summary$party[selected_row])
  })
  
  # Overview tab outputs
  output$total_guests <- renderValueBox({
    req(results())
    valueBox(
      nrow(results()$guests),
      "Total Invited Guests",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$wedding_attending <- renderValueBox({
    req(results())
    attending_count <- sum(results()$guests$wedding_rsvp == "Joyfully Accept", na.rm = TRUE)
    valueBox(
      attending_count,
      "Attending Wedding",
      icon = icon("glass-cheers"),
      color = "green"
    )
  })
  
  output$friday_attending <- renderValueBox({
    req(results())
    friday_count <- sum(results()$guests$fridayshabbat_rsvp == "Joyfully Accept", na.rm = TRUE)
    valueBox(
      friday_count,
      "Attending Friday",
      icon = icon("star-of-david"),
      color = "yellow"
    )
  })
  
  output$saturday_attending <- renderValueBox({
    req(results())
    saturday_count <- sum(results()$guests$saturday_onsite_rsvp == "Yes, I will join on Saturday" | 
                            results()$guests$saturday_offsite_rsvp == "Yes, I will join for lunch only" |
                            results()$guests$saturday_offsite_rsvp == "Yes, I will join for dinner only" |
                            results()$guests$saturday_offsite_rsvp == "Yes, I will join for lunch and dinner", 
                          na.rm = TRUE)
    valueBox(
      saturday_count,
      "Attending Saturday",
      icon = icon("calendar-day"),
      color = "orange"
    )
  })
  
  output$rsvp_status_plot <- renderPlotly({
    req(results())
    
    # Count RSVPs
    wedding_rsvp_counts <- results()$guests %>%
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
    req(results())
    
    # Extract data
    acc_summary <- results()$accommodation_summary
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
    req(results())
    
    # Generate timeline data from the guests data
    timeline_data <- create_timeline_data(results()$guests)
    
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
    req(results())
    
    data.frame(
      Category = c("Total Invited", "Accepted", "Declined", "No Response"),
      Count = c(
        nrow(results()$guests),
        sum(results()$guests$wedding_rsvp == "Joyfully Accept", na.rm = TRUE),
        sum(results()$guests$wedding_rsvp == "Regretfully Decline", na.rm = TRUE),
        sum(is.na(results()$guests$wedding_rsvp) | results()$guests$wedding_rsvp == "", na.rm = TRUE)
      )
    )
  })
  
  output$accommodation_summary_table <- renderTable({
    req(results())
    
    acc_summary <- results()$accommodation_summary
    
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
    req(results())
    
    meal_counts <- results()$meal_counts
    
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
  
  # NEW: Age Category summary table
  output$age_category_table <- renderTable({
    req(results())
    req(results()$age_category_summary)
    
    results()$age_category_summary %>%
      select(age_category, total_guests, friday_count, saturday_count, sunday_count) %>%
      rename(
        "Age Category" = age_category,
        "Total Guests" = total_guests,
        "Friday" = friday_count,
        "Saturday" = saturday_count,
        "Sunday" = sunday_count
      )
  })
  
  output$cost_summary_table <- renderTable({
    req(results())
    
    acc_summary <- results()$accommodation_summary
    
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
    req(results())
    
    issues <- results()$data_issues
    
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
    req(results())
    
    # Select only the most important columns for display
    party_display <- results()$party_summary %>%
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
    req(results())
    req(selected_party())
    
    party_id <- selected_party()
    
    # Get all guests in the selected party
    party_guests <- results()$guests %>%
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
    req(results())
    acc_summary <- results()$accommodation_summary
    staying_count <- sum(acc_summary$friday_count > 0 | acc_summary$saturday_count > 0 | acc_summary$sunday_count > 0)
    valueBox(
      staying_count,
      "Total Overnight Guests",
      icon = icon("hotel"),
      color = "blue"
    )
  })
  
  output$camping_count <- renderValueBox({
    req(results())
    valueBox(
      results()$accommodation_summary$camping_count,
      "Camping",
      icon = icon("campground"),
      color = "green"
    )
  })
  
  output$lodging_count <- renderValueBox({
    req(results())
    valueBox(
      results()$accommodation_summary$standard_lodging_count,
      "Standard Lodging",
      icon = icon("bed"),
      color = "purple"
    )
  })
  
  output$nights_booked <- renderValueBox({
    req(results())
    acc_summary <- results()$accommodation_summary
    total_nights <- acc_summary$friday_count + acc_summary$saturday_count + acc_summary$sunday_count
    valueBox(
      total_nights,
      "Total Nights Booked",
      icon = icon("moon"),
      color = "navy"
    )
  })
  
  # Continued from previous file
  output$age_accommodation_plot <- renderPlotly({
    req(results())
    req(results()$age_category_summary)
    
    # Prepare data
    age_data <- results()$age_category_summary %>%
      select(age_category, total_guests) %>%
      rename(Category = age_category, Count = total_guests)
    
    # Create plot
    p <- plot_ly(age_data, x = ~Category, y = ~Count, type = 'bar',
                 marker = list(color = '#5DADE2'))
    p <- p %>% layout(title = "Accommodation by Age Category",
                      xaxis = list(title = ""),
                      yaxis = list(title = "Number of Guests"))
    p
  })
  
  # Accommodation by night with age breakdown
  output$night_accommodation_plot <- renderPlotly({
    req(results())
    req(results()$age_category_summary)
    
    # Extract data for each night by age category
    age_night_data <- results()$age_category_summary %>%
      select(age_category, friday_count, saturday_count, sunday_count) %>%
      rename(Category = age_category, Friday = friday_count, Saturday = saturday_count, Sunday = sunday_count) %>%
      pivot_longer(cols = c(Friday, Saturday, Sunday), names_to = "Night", values_to = "Count")
    
    # Create plot
    p <- plot_ly(age_night_data, x = ~Night, y = ~Count, color = ~Category, type = 'bar',
                 colors = c('#1F77B4', '#FF7F0E', '#2CA02C', '#D62728', '#9467BD'))
    p <- p %>% layout(title = "Accommodation by Night and Age Category",
                      xaxis = list(title = ""),
                      yaxis = list(title = "Number of Guests"),
                      barmode = 'stack')
    p
  })
  
  output$accommodation_table <- renderDT({
    req(results())
    
    # Select only relevant columns for display
    accommodation_display <- results()$guests %>%
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
    req(results())
    
    # Prepare data for visualization
    stay_data <- results()$guests %>%
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
    req(results())
    
    # Prepare data
    meal_counts <- results()$meal_counts
    
    # Format data for plotting
    meal_data <- data.frame(
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
      filter(Count > 0)  # Only show meals with guests
    
    # Create plot
    p <- plot_ly(meal_data_long, x = ~Meal, y = ~Count, color = ~Guest_Type, type = 'bar',
                 colors = c("On_Site" = "#3498DB", "Off_Site" = "#E74C3C", "Wedding_Reception" = "#2ECC71"))
    p <- p %>% layout(title = "Meal Attendance by Guest Type",
                      xaxis = list(title = ""),
                      yaxis = list(title = "Number of Guests"),
                      barmode = 'stack',
                      legend = list(title = list(text = "Guest Type")))
    p
  })
  
  # Helper function to create meal attendance tables
  create_meal_table <- function(meal_name, guest_data) {
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
    meal_guests <- guest_data %>%
      filter(!!filter_condition) %>%
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
        is_vegetarian = ifelse(is.na(is_vegetarian), FALSE, is_vegetarian),
        has_special_diet = ifelse(is.na(has_special_diet), FALSE, has_special_diet)
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
    
    return(meal_guests)
  }
  
  # Helper function to create meal summary tables
  create_meal_summary <- function(meal_name, guest_data) {
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
    meal_guests <- guest_data %>%
      filter(!!filter_condition)
    
    # Calculate meal summary statistics
    meal_summary <- meal_guests %>%
      summarize(
        Total_Guests = n(),
        Vegetarian_Count = sum(is_vegetarian, na.rm = TRUE),
        Meat_Fish_Count = sum(!is_vegetarian, na.rm = TRUE),
        Special_Diet_Count = sum(has_special_diet, na.rm = TRUE)
      )
    
    # Calculate by age category if available
    if ("age_category" %in% names(meal_guests)) {
      age_summary <- meal_guests %>%
        group_by(Age_Category = age_category) %>%
        summarize(
          Count = n(),
          Vegetarian = sum(is_vegetarian, na.rm = TRUE),
          `Meat/Fish` = sum(!is_vegetarian, na.rm = TRUE),
          `Special Diet` = sum(has_special_diet, na.rm = TRUE)
        )
      
      # Combine summaries
      return(list(overall = meal_summary, by_age = age_summary))
    } else {
      # Just return overall summary
      return(list(overall = meal_summary, by_age = NULL))
    }
  }
  
  # Generate all meal tables
  output$friday_dinner_table <- renderDT({
    req(results())
    datatable(create_meal_table("friday_dinner", results()$guests), 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$saturday_breakfast_table <- renderDT({
    req(results())
    datatable(create_meal_table("saturday_breakfast", results()$guests), 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$saturday_lunch_table <- renderDT({
    req(results())
    datatable(create_meal_table("saturday_lunch", results()$guests), 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$saturday_dinner_table <- renderDT({
    req(results())
    datatable(create_meal_table("saturday_dinner", results()$guests), 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$sunday_breakfast_table <- renderDT({
    req(results())
    datatable(create_meal_table("sunday_breakfast", results()$guests), 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$sunday_lunch_table <- renderDT({
    req(results())
    datatable(create_meal_table("sunday_lunch", results()$guests), 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$sunday_dinner_table <- renderDT({
    req(results())
    datatable(create_meal_table("sunday_dinner", results()$guests), 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$monday_breakfast_table <- renderDT({
    req(results())
    datatable(create_meal_table("monday_breakfast", results()$guests), 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  # Generate all meal summaries
  output$friday_dinner_summary <- renderTable({
    req(results())
    summary_data <- create_meal_summary("friday_dinner", results()$guests)
    if (!is.null(summary_data$by_age)) {
      summary_data$by_age
    } else {
      data.frame(
        Category = "Total",
        Count = summary_data$overall$Total_Guests,
        Vegetarian = summary_data$overall$Vegetarian_Count,
        `Meat/Fish` = summary_data$overall$Meat_Fish_Count,
        `Special Diet` = summary_data$overall$Special_Diet_Count
      )
    }
  })
  
  output$friday_dinner_plot <- renderPlotly({
    req(results())
    summary_data <- create_meal_summary("friday_dinner", results()$guests)
    
    if (!is.null(summary_data$by_age)) {
      plot_data <- summary_data$by_age %>%
        rename(Category = Age_Category) %>%
        select(Category, Count, Vegetarian, `Meat/Fish`, `Special Diet`) %>%
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
      # Simple summary if no age data
      plot_data <- data.frame(
        Category = c("Total", "Vegetarian", "Meat/Fish", "Special Diet"),
        Count = c(
          summary_data$overall$Total_Guests,
          summary_data$overall$Vegetarian_Count,
          summary_data$overall$Meat_Fish_Count,
          summary_data$overall$Special_Diet_Count
        )
      )
      
      p <- plot_ly(plot_data, x = ~Category, y = ~Count, type = 'bar',
                   marker = list(color = '#3498DB'))
      p <- p %>% layout(title = "Friday Dinner Attendance",
                        xaxis = list(title = ""),
                        yaxis = list(title = "Number of Guests"))
      return(p)
    }
  })
  
  # Generate remaining meal summaries using the same pattern
  output$saturday_breakfast_summary <- renderTable({
    req(results())
    summary_data <- create_meal_summary("saturday_breakfast", results()$guests)
    if (!is.null(summary_data$by_age)) {
      summary_data$by_age
    } else {
      data.frame(
        Category = "Total",
        Count = summary_data$overall$Total_Guests,
        Vegetarian = summary_data$overall$Vegetarian_Count,
        `Meat/Fish` = summary_data$overall$Meat_Fish_Count,
        `Special Diet` = summary_data$overall$Special_Diet_Count
      )
    }
  })
  
  output$saturday_lunch_summary <- renderTable({
    req(results())
    summary_data <- create_meal_summary("saturday_lunch", results()$guests)
    if (!is.null(summary_data$by_age)) {
      summary_data$by_age
    } else {
      data.frame(
        Category = "Total",
        Count = summary_data$overall$Total_Guests,
        Vegetarian = summary_data$overall$Vegetarian_Count,
        `Meat/Fish` = summary_data$overall$Meat_Fish_Count,
        `Special Diet` = summary_data$overall$Special_Diet_Count
      )
    }
  })
  
  output$saturday_dinner_summary <- renderTable({
    req(results())
    summary_data <- create_meal_summary("saturday_dinner", results()$guests)
    if (!is.null(summary_data$by_age)) {
      summary_data$by_age
    } else {
      data.frame(
        Category = "Total",
        Count = summary_data$overall$Total_Guests,
        Vegetarian = summary_data$overall$Vegetarian_Count,
        `Meat/Fish` = summary_data$overall$Meat_Fish_Count,
        `Special Diet` = summary_data$overall$Special_Diet_Count
      )
    }
  })
  
  output$sunday_breakfast_summary <- renderTable({
    req(results())
    summary_data <- create_meal_summary("sunday_breakfast", results()$guests)
    if (!is.null(summary_data$by_age)) {
      summary_data$by_age
    } else {
      data.frame(
        Category = "Total",
        Count = summary_data$overall$Total_Guests,
        Vegetarian = summary_data$overall$Vegetarian_Count,
        `Meat/Fish` = summary_data$overall$Meat_Fish_Count,
        `Special Diet` = summary_data$overall$Special_Diet_Count
      )
    }
  })
  
  output$sunday_lunch_summary <- renderTable({
    req(results())
    summary_data <- create_meal_summary("sunday_lunch", results()$guests)
    if (!is.null(summary_data$by_age)) {
      summary_data$by_age
    } else {
      data.frame(
        Category = "Total",
        Count = summary_data$overall$Total_Guests,
        Vegetarian = summary_data$overall$Vegetarian_Count,
        `Meat/Fish` = summary_data$overall$Meat_Fish_Count,
        `Special Diet` = summary_data$overall$Special_Diet_Count
      )
    }
  })
  
  output$sunday_dinner_summary <- renderTable({
    req(results())
    summary_data <- create_meal_summary("sunday_dinner", results()$guests)
    if (!is.null(summary_data$by_age)) {
      summary_data$by_age
    } else {
      data.frame(
        Category = "Total",
        Count = summary_data$overall$Total_Guests,
        Vegetarian = summary_data$overall$Vegetarian_Count,
        `Meat/Fish` = summary_data$overall$Meat_Fish_Count,
        `Special Diet` = summary_data$overall$Special_Diet_Count
      )
    }
  })
  
  output$monday_breakfast_summary <- renderTable({
    req(results())
    summary_data <- create_meal_summary("monday_breakfast", results()$guests)
    if (!is.null(summary_data$by_age)) {
      summary_data$by_age
    } else {
      data.frame(
        Category = "Total",
        Count = summary_data$overall$Total_Guests,
        Vegetarian = summary_data$overall$Vegetarian_Count,
        `Meat/Fish` = summary_data$overall$Meat_Fish_Count,
        `Special Diet` = summary_data$overall$Special_Diet_Count
      )
    }
  })
  
  output$meal_preferences_plot <- renderPlotly({
    req(results())
    
    # Count meal preferences
    preferences <- results()$guests %>%
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
    req(results())
    
    # Count dietary restrictions
    dietary <- results()$guests %>%
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
    req(results())
    
    # Extract dietary restrictions
    dietary_data <- results()$guests %>%
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
    req(results())
    
    # Check if we have an IFC roster or schedule in the results
    ifc_data <- if (!is.null(results()$ifc_roster)) {
      results()$ifc_roster
    } else if (!is.null(results()$ifc_schedule)) {
      results()$ifc_schedule
    } else {
      # Create a roster from the guest data if none exists
      results()$guests %>%
        filter(is_staying_friday | is_staying_saturday | is_staying_sunday) %>%
        mutate(
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
    
    # Format the roster for display
    roster_display <- ifc_data %>%
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
    req(results())
    
    # Calculate accommodation costs for IFC guests
    ifc_guests <- results()$guests %>%
      filter(is_staying_friday | is_staying_saturday | is_staying_sunday)
    
    # Calculate summary
    cost_summary <- ifc_guests %>%
      summarize(
        Total_Cost = sum(total_cost, na.rm = TRUE),
        Guest_Charges = sum(total_guest_charge, na.rm = TRUE),
        Host_Charges = sum(total_host_charge, na.rm = TRUE),
        Friday_Cost = sum(friday_cost, na.rm = TRUE),
        Saturday_Cost = sum(saturday_cost, na.rm = TRUE),
        Sunday_Cost = sum(sunday_cost, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = everything(), names_to = "Category", values_to = "Amount") %>%
      mutate(Amount = paste0("$", format(Amount, big.mark = ",")))
    
    cost_summary
  })
  
  # Continued from previous file
  output$ifc_meals_summary <- renderTable({
    req(results())
    
    # Extract meal counts for IFC guests
    ifc_guests <- results()$guests %>%
      filter(is_staying_friday | is_staying_saturday | is_staying_sunday)
    
    # Calculate meal attendance
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
      Attendees = c(
        sum(ifc_guests$is_staying_friday, na.rm = TRUE),
        sum(ifc_guests$is_staying_friday, na.rm = TRUE),
        sum(ifc_guests$is_staying_saturday, na.rm = TRUE),
        sum(ifc_guests$is_staying_saturday, na.rm = TRUE),
        sum(ifc_guests$is_staying_saturday, na.rm = TRUE),
        sum(ifc_guests$wedding_rsvp == "Joyfully Accept", na.rm = TRUE),
        sum(ifc_guests$is_staying_sunday, na.rm = TRUE),
        sum(ifc_guests$is_staying_sunday, na.rm = TRUE)
      )
    )
    
    # Add vegetarian counts if available
    if ("is_vegetarian" %in% names(ifc_guests)) {
      meal_summary$Vegetarian <- c(
        sum(ifc_guests$is_staying_friday & ifc_guests$is_vegetarian, na.rm = TRUE),
        sum(ifc_guests$is_staying_friday & ifc_guests$is_vegetarian, na.rm = TRUE),
        sum(ifc_guests$is_staying_saturday & ifc_guests$is_vegetarian, na.rm = TRUE),
        sum(ifc_guests$is_staying_saturday & ifc_guests$is_vegetarian, na.rm = TRUE),
        sum(ifc_guests$is_staying_saturday & ifc_guests$is_vegetarian, na.rm = TRUE),
        sum(ifc_guests$wedding_rsvp == "Joyfully Accept" & ifc_guests$is_vegetarian, na.rm = TRUE),
        sum(ifc_guests$is_staying_sunday & ifc_guests$is_vegetarian, na.rm = TRUE),
        sum(ifc_guests$is_staying_sunday & ifc_guests$is_vegetarian, na.rm = TRUE)
      )
    }
    
    # Add special diet counts if available
    if ("has_special_diet" %in% names(ifc_guests)) {
      meal_summary$`Special Diet` <- c(
        sum(ifc_guests$is_staying_friday & ifc_guests$has_special_diet, na.rm = TRUE),
        sum(ifc_guests$is_staying_friday & ifc_guests$has_special_diet, na.rm = TRUE),
        sum(ifc_guests$is_staying_saturday & ifc_guests$has_special_diet, na.rm = TRUE),
        sum(ifc_guests$is_staying_saturday & ifc_guests$has_special_diet, na.rm = TRUE),
        sum(ifc_guests$is_staying_saturday & ifc_guests$has_special_diet, na.rm = TRUE),
        sum(ifc_guests$wedding_rsvp == "Joyfully Accept" & ifc_guests$has_special_diet, na.rm = TRUE),
        sum(ifc_guests$is_staying_sunday & ifc_guests$has_special_diet, na.rm = TRUE),
        sum(ifc_guests$is_staying_sunday & ifc_guests$has_special_diet, na.rm = TRUE)
      )
    }
    
    meal_summary
  })
  
  output$ifc_stay_distribution <- renderPlotly({
    req(results())
    
    # Extract IFC guests
    ifc_guests <- results()$guests %>%
      filter(is_staying_friday | is_staying_saturday | is_staying_sunday)
    
    # Prepare data for visualization
    stay_nights <- data.frame(
      Night = c("Friday", "Saturday", "Sunday"),
      Count = c(
        sum(ifc_guests$is_staying_friday, na.rm = TRUE),
        sum(ifc_guests$is_staying_saturday, na.rm = TRUE),
        sum(ifc_guests$is_staying_sunday, na.rm = TRUE)
      )
    )
    
    # Create plot
    p <- plot_ly(stay_nights, x = ~Night, y = ~Count, type = 'bar',
                 marker = list(color = c('#5DADE2', '#F4D03F', '#58D68D')))
    p <- p %>% layout(title = "Guests Staying Each Night",
                      xaxis = list(title = ""),
                      yaxis = list(title = "Number of Guests"))
    p
  })
  
  output$ifc_age_distribution <- renderPlotly({
    req(results())
    
    # Check if age category data is available
    if ("age_category" %in% names(results()$guests)) {
      # Extract IFC guests
      ifc_guests <- results()$guests %>%
        filter(is_staying_friday | is_staying_saturday | is_staying_sunday)
      
      # Prepare data for visualization
      age_counts <- ifc_guests %>%
        count(age_category) %>%
        rename(Category = age_category, Count = n)
      
      # Create plot
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
    req(results())
    
    # Extract IFC guests
    ifc_guests <- results()$guests %>%
      filter(is_staying_friday | is_staying_saturday | is_staying_sunday)
    
    # Prepare data for visualization
    cost_data <- data.frame(
      category = c("Friday", "Saturday", "Sunday"),
      IFC_Cost = c(
        sum(ifc_guests$friday_cost, na.rm = TRUE),
        sum(ifc_guests$saturday_cost, na.rm = TRUE),
        sum(ifc_guests$sunday_cost, na.rm = TRUE)
      ),
      Guest_Charge = c(
        sum(ifc_guests$friday_guest_charge, na.rm = TRUE),
        sum(ifc_guests$saturday_guest_charge, na.rm = TRUE),
        sum(ifc_guests$sunday_guest_charge, na.rm = TRUE)
      ),
      Host_Charge = c(
        sum(ifc_guests$friday_host_charge, na.rm = TRUE),
        sum(ifc_guests$saturday_host_charge, na.rm = TRUE),
        sum(ifc_guests$sunday_host_charge, na.rm = TRUE)
      )
    )
    
    # Convert to long format for stacked bar chart
    cost_long <- cost_data %>%
      pivot_longer(cols = c("Guest_Charge", "Host_Charge"), 
                   names_to = "Charge_Type", values_to = "Amount")
    
    # Create plot
    p <- plot_ly(cost_long, x = ~category, y = ~Amount, color = ~Charge_Type, type = 'bar',
                 colors = c("Guest_Charge" = "#3498DB", "Host_Charge" = "#E74C3C"))
    p <- p %>% layout(title = "Cost Breakdown by Night and Type",
                      xaxis = list(title = ""),
                      yaxis = list(title = "Amount ($)"),
                      barmode = 'stack')
    p
  })
  
  output$ifc_financial_summary <- renderTable({
    req(results())
    
    # Extract IFC guests
    ifc_guests <- results()$guests %>%
      filter(is_staying_friday | is_staying_saturday | is_staying_sunday)
    
    # Calculate financial summary by type
    if ("age_category" %in% names(ifc_guests)) {
      # By age category if available
      financial_by_type <- ifc_guests %>%
        group_by(Category = age_category) %>%
        summarize(
          `IFC Total` = sum(total_cost, na.rm = TRUE),
          `Guest Charges` = sum(total_guest_charge, na.rm = TRUE),
          `Host Charges` = sum(total_host_charge, na.rm = TRUE),
          `Number of Guests` = n()
        ) %>%
        arrange(Category)
      
      # Add totals row
      totals <- financial_by_type %>%
        summarize(
          Category = "TOTAL",
          `IFC Total` = sum(`IFC Total`),
          `Guest Charges` = sum(`Guest Charges`),
          `Host Charges` = sum(`Host Charges`),
          `Number of Guests` = sum(`Number of Guests`)
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
      financial_summary <- ifc_guests %>%
        summarize(
          Category = "Total",
          `IFC Total` = paste0("$", format(sum(total_cost, na.rm = TRUE), big.mark = ",")),
          `Guest Charges` = paste0("$", format(sum(total_guest_charge, na.rm = TRUE), big.mark = ",")),
          `Host Charges` = paste0("$", format(sum(total_host_charge, na.rm = TRUE), big.mark = ",")),
          `Number of Guests` = n()
        )
      
      return(financial_summary)
    }
  })
  
  # Cost Calculator tab outputs
  output$total_cost <- renderValueBox({
    req(results())
    valueBox(
      paste0("$", format(results()$accommodation_summary$grand_total_cost, big.mark = ",")),
      "Total Accommodation Cost",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$guest_charge <- renderValueBox({
    req(results())
    valueBox(
      paste0("$", format(results()$accommodation_summary$grand_total_guest_charge, big.mark = ",")),
      "Total Guest Charges",
      icon = icon("credit-card"),
      color = "blue"
    )
  })
  
  output$host_charge <- renderValueBox({
    req(results())
    valueBox(
      paste0("$", format(results()$accommodation_summary$grand_total_host_charge, big.mark = ",")),
      "Total Host Charges",
      icon = icon("hand-holding-usd"),
      color = "red"
    )
  })
  
  output$cost_breakdown <- renderPlotly({
    req(results())
    
    # Prepare data
    cost_data <- data.frame(
      category = c("Friday", "Saturday", "Sunday"),
      cost = c(
        results()$accommodation_summary$total_friday_cost,
        results()$accommodation_summary$total_saturday_cost,
        results()$accommodation_summary$total_sunday_cost
      ),
      guest_charge = c(
        results()$accommodation_summary$total_friday_guest_charge,
        results()$accommodation_summary$total_saturday_guest_charge,
        results()$accommodation_summary$total_sunday_guest_charge
      ),
      host_charge = c(
        results()$accommodation_summary$total_friday_host_charge,
        results()$accommodation_summary$total_saturday_host_charge,
        results()$accommodation_summary$total_sunday_host_charge
      )
    )
    
    # Convert to long format for grouped bar chart
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
                      barmode = 'group')
    p
  })
  
  output$age_cost_breakdown <- renderPlotly({
    req(results())
    req(results()$age_category_summary)
    
    # Prepare data
    age_cost_data <- results()$age_category_summary %>%
      select(age_category, total_guest_charge, total_host_charge) %>%
      rename(Category = age_category, `Guest Charge` = total_guest_charge, `Host Charge` = total_host_charge) %>%
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
  })
  
  output$guest_costs_table <- renderDT({
    req(results())
    
    # Create a cleaner display for the costs table
    costs_display <- results()$guest_costs %>%
      filter(total_cost > 0) %>% # Only show guests with costs
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
      mutate(
        friday_cost = paste0("$", friday_cost),
        saturday_cost = paste0("$", saturday_cost),
        sunday_cost = paste0("$", sunday_cost),
        total_cost = paste0("$", total_cost),
        total_guest_charge = paste0("$", total_guest_charge),
        total_host_charge = paste0("$", total_host_charge)
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
      write.csv(results()$party_summary, file, row.names = FALSE)
    }
  )
  
  output$downloadAccommodationData <- downloadHandler(
    filename = function() {
      paste("accommodation-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      accommodation_data <- results()$guests %>%
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
      # Check if we have an IFC roster or schedule in the results
      ifc_data <- if (!is.null(results()$ifc_roster)) {
        results()$ifc_roster
      } else if (!is.null(results()$ifc_schedule)) {
        results()$ifc_schedule
      } else {
        # Create a roster from the guest data if none exists
        results()$guests %>%
          filter(is_staying_friday | is_staying_saturday | is_staying_sunday) %>%
          mutate(
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
      
      write.csv(ifc_data, file, row.names = FALSE)
    }
  )
  
  output$downloadCostData <- downloadHandler(
    filename = function() {
      paste("cost-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Only include guests with costs
      cost_data <- results()$guest_costs %>%
        filter(total_cost > 0) %>%
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
      
      # Export reports to the temporary directory
      export_reports(results(), report_dir)
      
      # Generate meal planning report
      generate_meal_planning_report(results(), file.path(report_dir, "meal_planning_report.csv"))
      
      # Generate IFC report
      generate_ifc_report(results(), file.path(report_dir, "ifc_report.csv"))
      
      # Also generate the simplified IFC summary
      generate_ifc_summary(results(), file.path(report_dir, "ifc_summary.csv"))
      
      # Create a zip file using zip package if available
      zip_file <- file.path(temp_dir, "wedding_reports.zip")
      
      tryCatch({
        if (requireNamespace("zip", quietly = TRUE)) {
          zip::zip(zipfile = zip_file, files = report_dir, recurse = TRUE)
          file.copy(zip_file, file)
        } else {
          # Fallback - create a notice about the zip package
          writeLines(
            "Note: The zip package would provide better compression. Please install with install.packages('zip').",
            file.path(report_dir, "README.txt")
          )
          # Use basic R utils zip function
          utils::zip(file, dir(report_dir, full.names = TRUE))
        }
      }, error = function(e) {
        # In case of any error, at least provide the files
        writeLines(
          paste("Error creating zip:", conditionMessage(e), 
                "\nIndividual files can be downloaded from the app interface."),
          file.path(report_dir, "ERROR.txt")
        )
        file.copy(list.files(report_dir, full.names = TRUE)[1], file)
      })
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)