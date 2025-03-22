# Wedding RSVP Dashboard
# A simple dashboard to visualize RSVP status and accommodation data

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(readr)

# Source the RSVP tracker functions
# source("wedding-rsvp-tracker.R")  # Uncomment to source the previous file

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Wedding RSVP Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Quick Insights", tabName = "insights", icon = icon("lightbulb")),
      menuItem("Party List", tabName = "party", icon = icon("users")),
      menuItem("Accommodations", tabName = "accommodation", icon = icon("bed")),
      menuItem("Meals", tabName = "meals", icon = icon("utensils")),
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
          .content-wrapper { margin-left: 0 !important; # Run the application (uncomment when ready)
# shinyApp(ui = ui, server = server)
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
      "))
    ),
    
    tabItems(
      # Overview tab
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
      
      # Quick Insights tab
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
                box(title = "Cost Summary", status = "danger", width = 12,
                    tableOutput("cost_summary_table")
                )
              ),
              fluidRow(
                box(title = "Data Quality Issues", status = "success", width = 12,
                    DTOutput("data_issues_table")
                )
              )
      ),
      
      # Party List tab
      tabItem(tabName = "party",
              fluidRow(
                box(title = "Party List", status = "primary", width = 12,
                    DTOutput("party_table")
                )
              )
      ),
      
      # Accommodations tab
      tabItem(tabName = "accommodation",
              fluidRow(
                valueBoxOutput("total_staying", width = 4),
                valueBoxOutput("camping_count", width = 4),
                valueBoxOutput("lodging_count", width = 4)
              ),
              fluidRow(
                box(title = "Accommodation Details", status = "primary", width = 12,
                    DTOutput("accommodation_table")
                )
              ),
              fluidRow(
                box(title = "Stay Distribution", status = "info", width = 12,
                    plotlyOutput("stay_distribution")
                )
              )
      ),
      
      # Meals tab
      tabItem(tabName = "meals",
              fluidRow(
                box(title = "Meal Summary", status = "primary", width = 12,
                    plotlyOutput("meal_summary_plot")
                )
              ),
              fluidRow(
                box(title = "Saturday Meals for Off-Site Guests", status = "info", width = 6,
                    plotlyOutput("saturday_meals_plot")
                ),
                box(title = "Meal Preferences", status = "warning", width = 6,
                    plotlyOutput("meal_preferences_plot")
                )
              ),
              fluidRow(
                box(title = "Dietary Restrictions", status = "danger", width = 12,
                    DTOutput("dietary_table")
                )
              )
      ),
      
      # Cost Calculator tab
      tabItem(tabName = "costs",
              fluidRow(
                valueBoxOutput("total_cost", width = 4),
                valueBoxOutput("friday_cost", width = 4),
                valueBoxOutput("saturday_cost", width = 4)
              ),
              fluidRow(
                box(title = "Cost Breakdown", status = "primary", width = 12,
                    plotlyOutput("cost_breakdown")
                )
              ),
              fluidRow(
                box(title = "Individual Guest Costs", status = "info", width = 12,
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
        "Saturday Meals",
        "Wedding Meal (Sun)",
        "Sunday Dinner"
      ),
      Count = c(
        meal_counts$total_friday_dinner,
        meal_counts$total_saturday_lunch,
        meal_counts$total_sunday_lunch,
        meal_counts$total_sunday_dinner
      )
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
        "Total Costs"
      ),
      Amount = c(
        paste0("$", format(acc_summary$total_friday_cost, big.mark = ",")),
        paste0("$", format(acc_summary$total_saturday_cost, big.mark = ",")),
        paste0("$", format(acc_summary$total_sunday_cost, big.mark = ",")),
        paste0("$", format(acc_summary$grand_total_cost, big.mark = ","))
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
  
  # Party List tab outputs
  output$party_table <- renderDT({
    req(results())
    
    # Select only the most important columns for display
    party_display <- results()$party_summary %>%
      select(
        party_name, 
        total_guests, 
        wedding_attending, 
        friday_attending, 
        saturday_attending,
        party_email
      ) %>%
      rename(
        "Party Name" = party_name,
        "Total Guests" = total_guests,
        "Wedding" = wedding_attending,
        "Friday" = friday_attending,
        "Saturday" = saturday_attending,
        "Email" = party_email
      )
    
    datatable(
      party_display,
      options = list(
        pageLength = 15,
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
    staying_count <- max(acc_summary$friday_count, acc_summary$saturday_count, acc_summary$sunday_count)
    valueBox(
      staying_count,
      "Total Staying at IFC",
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
  
  output$accommodation_table <- renderDT({
    req(results())
    
    # Select only relevant columns for display
    accommodation_display <- results()$guest_costs %>%
      select(
        first_name,
        last_name,
        is_staying_friday,
        is_staying_saturday,
        is_staying_sunday,
        is_camping,
        friday_cost,
        saturday_cost,
        sunday_cost,
        total_cost
      ) %>%
      rename(
        "First Name" = first_name,
        "Last Name" = last_name,
        "Friday" = is_staying_friday,
        "Saturday" = is_staying_saturday,
        "Sunday" = is_staying_sunday,
        "Camping" = is_camping,
        "Friday Cost" = friday_cost,
        "Saturday Cost" = saturday_cost,
        "Sunday Cost" = sunday_cost,
        "Total Cost" = total_cost
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
    stay_data <- results()$guest_costs %>%
      summarize(
        `Friday Only` = sum(is_staying_friday & !is_staying_saturday & !is_staying_sunday, na.rm = TRUE),
        `Saturday Only` = sum(!is_staying_friday & is_staying_saturday & !is_staying_sunday, na.rm = TRUE),
        `Friday & Saturday` = sum(is_staying_friday & is_staying_saturday & !is_staying_sunday, na.rm = TRUE),
        `Saturday & Sunday` = sum(!is_staying_friday & is_staying_saturday & is_staying_sunday, na.rm = TRUE),
        `All Three Nights` = sum(is_staying_friday & is_staying_saturday & is_staying_sunday, na.rm = TRUE),
        `Other Combinations` = sum((is_staying_friday | is_staying_saturday | is_staying_sunday), na.rm = TRUE) - 
          sum(is_staying_friday & !is_staying_saturday & !is_staying_sunday, na.rm = TRUE) -
          sum(!is_staying_friday & is_staying_saturday & !is_staying_sunday, na.rm = TRUE) -
          sum(is_staying_friday & is_staying_saturday & !is_staying_sunday, na.rm = TRUE) -
          sum(!is_staying_friday & is_staying_saturday & is_staying_sunday, na.rm = TRUE) -
          sum(is_staying_friday & is_staying_saturday & is_staying_sunday, na.rm = TRUE)
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
  
  # Meals tab outputs
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
  
  output$saturday_meals_plot <- renderPlotly({
    req(results())
    
    # Count Saturday RSVP responses for off-site guests
    saturday_responses <- results()$guests %>%
      filter(!is.na(saturday_offsite_rsvp)) %>%
      group_by(response = saturday_offsite_rsvp) %>%
      summarize(count = n()) %>%
      filter(count > 0)
    
    # Create plot
    p <- plot_ly(saturday_responses, labels = ~response, values = ~count, type = 'pie',
                 textinfo = 'label+percent', 
                 marker = list(colors = c('#F1C40F', '#E67E22', '#3498DB', '#E74C3C')))
    p <- p %>% layout(title = "Saturday Meal Choices (Off-Site Guests)")
    p
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
  
  output$dietary_table <- renderDT({
    req(results())
    
    # Extract dietary restrictions
    dietary_data <- results()$guests %>%
      select(first_name, last_name, meal_preferences, dietary_restrictions) %>%
      filter(!is.na(dietary_restrictions) & dietary_restrictions != "")
    
    datatable(dietary_data,
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                autoWidth = TRUE
              ),
              rownames = FALSE)
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
  
  output$friday_cost <- renderValueBox({
    req(results())
    valueBox(
      paste0("$", format(results()$accommodation_summary$total_friday_cost, big.mark = ",")),
      "Friday Cost",
      icon = icon("dollar-sign"),
      color = "blue"
    )
  })
  
  output$saturday_cost <- renderValueBox({
    req(results())
    valueBox(
      paste0("$", format(results()$accommodation_summary$total_saturday_cost, big.mark = ",")),
      "Saturday Cost",
      icon = icon("dollar-sign"),
      color = "purple"
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
      )
    )
    
    # Create plot
    p <- plot_ly(cost_data, x = ~category, y = ~cost, type = 'bar',
                 marker = list(color = c('#3498DB', '#F1C40F', '#2ECC71')))
    p <- p %>% layout(title = "Cost Breakdown by Night",
                      xaxis = list(title = ""),
                      yaxis = list(title = "Total Cost ($)"))
    p
  })
  
  output$guest_costs_table <- renderDT({
    req(results())
    
    # Create a cleaner display for the costs table
    costs_display <- results()$guest_costs %>%
      select(
        first_name,
        last_name,
        is_staying_friday,
        is_staying_saturday, 
        is_staying_sunday,
        is_camping,
        friday_cost,
        saturday_cost,
        sunday_cost,
        total_cost
      ) %>%
      mutate(
        friday_cost = paste0("$", friday_cost),
        saturday_cost = paste0("$", saturday_cost),
        sunday_cost = paste0("$", sunday_cost),
        total_cost = paste0("$", total_cost)
      ) %>%
      rename(
        "First Name" = first_name,
        "Last Name" = last_name,
        "Friday" = is_staying_friday,
        "Saturday" = is_staying_saturday,
        "Sunday" = is_staying_sunday,
        "Camping" = is_camping,
        "Friday Cost" = friday_cost,
        "Saturday Cost" = saturday_cost,
        "Sunday Cost" = sunday_cost,
        "Total Cost" = total_cost
      )
    
    datatable(costs_display,
              options = list(
                pageLength = 15,
                scrollX = TRUE,
                autoWidth = TRUE
              ),
              rownames = FALSE)
  })
  
  # Download handler for all reports
  output$downloadReports <- downloadHandler(
    filename = function() {
      paste("wedding-reports-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      # Create a temporary directory
      temp_dir <- tempdir()
      report_dir <- file.path(temp_dir, "wedding_reports")
      
      # SIMPLIFIED: Force directory creation
      dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)
      
      # Export reports to the temporary directory
      export_reports(results(), report_dir)
      
      # Generate meal planning report (use CSV instead of PDF)
      generate_meal_planning_report(results(), file.path(report_dir, "meal_planning_report.csv"))
      
      # Generate IFC report (use CSV instead of PDF)
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