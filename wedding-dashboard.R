# Wedding RSVP Dashboard
# A simple dashboard to visualize RSVP status and accommodation data
# Install required packages if not already installed
# install.packages(c("shiny", "shinydashboard", "DT", "ggplot2", "plotly"))

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
  )
)

# Server
server <- function(input, output, session) {
  
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
  
  # Party List tab outputs
  output$party_table <- renderDT({
    req(results())
    datatable(results()$party_summary,
              options = list(pageLength = 10),
              rownames = FALSE)
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
    datatable(results()$guest_costs,
              options = list(pageLength = 10),
              rownames = FALSE)
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
              options = list(pageLength = 10),
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
    
    datatable(results()$guest_costs %>%
                mutate(
                  friday_cost = paste0("$", friday_cost),
                  saturday_cost = paste0("$", saturday_cost),
                  sunday_cost = paste0("$", sunday_cost),
                  total_cost = paste0("$", total_cost)
                ),
              options = list(pageLength = 10),
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
      
      # Generate meal planning report
      generate_meal_planning_report(results(), file.path(report_dir, "meal_planning_report.pdf"))
      
      # Create a zip file
      zip_file <- file.path(temp_dir, "wedding_reports.zip")
      zip::zip(zipfile = zip_file, files = report_dir, recurse = TRUE)
      
      # Copy the zip file to the download location
      file.copy(zip_file, file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)