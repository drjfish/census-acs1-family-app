# Load necessary libraries
library(shiny)
library(plotly)
library(dplyr)
library(scales)  # For comma formatting

# Load the data
data <- read.csv("marital_status_data.csv")
data2 <- read.csv("marital_status_children_data.csv")

# Define UI
ui <- fluidPage(
  # Main title
  titlePanel("U.S. Families Data"),
  
  # Custom CSS for title size and subtitle styling
  tags$head(
    tags$style(HTML("
      .title-subheading { 
        font-size: 18px; 
        font-weight: normal; 
      }
    "))
  ),
  
  tabsetPanel(
    id = "main-tabs",
    
    # begin section: Marital Status Tab
    tabPanel(HTML("<b style='font-size: 16px;'>Marital Status</b>"),
      fluidPage(
        titlePanel(div("Marital Status Distribution by Age Group", class = "title-subheading")),
        sidebarLayout(
          sidebarPanel(
            width = 2,  # Resize sidebar to width 2
            # Year selection only for the "Single Year" tab
            conditionalPanel(
              condition = "input.tab_selected == 'Single Year'",
              uiOutput("year_selector")  # Dynamic UI for year selection
            ),
            # Populate the "State" dropdown with "All States" and all unique states from the dataset
            selectInput("state", "Select State", choices = c("All States", unique(data$state)), selected = "All States"),
            # Display Mode selection
            radioButtons("display_mode", "Display Mode", choices = c("Absolute", "Relative"), selected = "Absolute")
          ),
          mainPanel(
            tabsetPanel(
              id = "tab_selected",
              tabPanel("Single Year", plotlyOutput("maritalStatusPlot")),
              tabPanel("Trend Over Time", plotlyOutput("trendPlot"))
            )
          )
        )
      )
    ),
    # end section: Marital Status Tab

  # begin section: Children Data Tab
  tabPanel(HTML("<b style='font-size: 16px;'>Children Data</b>"), 
    fluidPage(
      titlePanel(div("Children Data Visualization", class = "title-subheading")),
      sidebarLayout(
        sidebarPanel(
          width = 2,
          # Conditional display for the year selector based on active tab
          conditionalPanel(
            condition = "input.children_tab_selected == 'Single Year'",
            uiOutput("children_year_selector")
          ),
          # State selector
          selectInput("children_state", "Select State", choices = c("All States", unique(data2$state)), selected = "All States"),
          # Display mode selector (Absolute or Relative)
          radioButtons("children_display_mode", "Display Mode", choices = c("Absolute", "Relative"), selected = "Absolute")
        ),
        mainPanel(
          tabsetPanel(
            id = "children_tab_selected",  # ID for the tabsetPanel to allow conditional display
            # Single Year visualization tab
            tabPanel("Single Year", plotlyOutput("childrenSingleYearPlot")),
            # Trend Over Time visualization tab with animation (no year selector here)
            tabPanel("Trend Over Time", plotlyOutput("childrenTrendPlot"))
          )
        )
      )
    )
  ),
  # end section: Children Data Tab

  )
)

# Define Server
server <- function(input, output, session) {
  
  # begin section: Marital Status Server Logic
  # Dynamically generate year selection based on data for the "Single Year" tab
  output$year_selector <- renderUI({
    selectInput("year", "Select Year", choices = sort(unique(data$year), decreasing = TRUE), selected = max(data$year))
  })

  output$maritalStatusPlot <- renderPlotly({
    # Ensure input$year is not NULL before filtering by year
    if (is.null(input$year)) return(NULL)
    
    # Filter data based on selected state and selected year
    filtered_data <- data %>%
      filter(year == input$year) %>%
      filter(if (input$state != "All States") state == input$state else TRUE) %>%
      group_by(age, marital_status) %>%
      summarise(total_estimate = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
      arrange(age)

    # Set y-axis value and hover text based on display mode
    if (input$display_mode == "Relative") {
      filtered_data <- filtered_data %>%
        group_by(age) %>%
        mutate(total_by_age = sum(total_estimate)) %>%
        ungroup() %>%
        mutate(percentage = (total_estimate / total_by_age) * 100)
      y_value <- ~percentage
      hover_text <- ~paste("Marital Status:", marital_status,
                           "<br>Percentage:", round(percentage, 1), "%")
      y_axis_title <- "Percentage (%)"
    } else {
      y_value <- ~total_estimate
      hover_text <- ~paste("Marital Status:", marital_status,
                           "<br>Estimate:", comma(total_estimate))
      y_axis_title <- "Population Estimate"
    }
    
    # Plot for the selected year
    plot <- plot_ly(
      data = filtered_data,
      x = ~age,
      y = y_value,
      color = ~marital_status,
      type = "bar",
      text = hover_text,
      hoverinfo = "text",
      textposition = "none"
    ) %>%
      layout(
        title = paste("Marital Status Distribution in", input$year, "for State", input$state),
        xaxis = list(title = "Age Group"),
        yaxis = list(title = y_axis_title),
        barmode = "stack",
        margin = list(t = 80)  # Add top margin for spacing
      )
    
    plot
  })
  
  output$trendPlot <- renderPlotly({
    # Filter data based on selected state and include all years
    filtered_data <- data %>%
      filter(if (input$state != "All States") state == input$state else TRUE) %>%
      group_by(year, age, marital_status) %>%
      summarise(total_estimate = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
      arrange(age, year)

    # Set y-axis value and hover text based on display mode
    if (input$display_mode == "Relative") {
      filtered_data <- filtered_data %>%
        group_by(year, age) %>%
        mutate(total_by_age_year = sum(total_estimate)) %>%
        ungroup() %>%
        mutate(percentage = (total_estimate / total_by_age_year) * 100)
      y_value <- ~percentage
      hover_text <- ~paste("Year:", year, "<br>Marital Status:", marital_status,
                           "<br>Percentage:", round(percentage, 1), "%")
      y_axis_title <- "Percentage (%)"
    } else {
      y_value <- ~total_estimate
      hover_text <- ~paste("Year:", year, "<br>Marital Status:", marital_status,
                           "<br>Estimate:", comma(total_estimate))
      y_axis_title <- "Population Estimate"
    }

    # Plot for trend over time with marital status as stacked bars for each year
    plot <- plot_ly(
      data = filtered_data,
      x = ~age,
      y = y_value,
      color = ~marital_status,  # Color by marital status for stacking
      type = "bar",
      text = hover_text,
      hoverinfo = "text",
      textposition = "none",
      frame = ~year  # Keep animation by year for dynamic trend visualization
    ) %>%
      layout(
        title = paste("Marital Status Trend Over Time for State", input$state),
        xaxis = list(title = "Age Group"),
        yaxis = list(title = y_axis_title),
        barmode = "stack",  # Stack marital status bars within each year
        margin = list(t = 80)  # Add top margin for spacing
      ) %>%
      animation_opts(frame = 1000, easing = "linear", redraw = FALSE)

    plot
  })
  # end section: Marital Status Server Logic

###################################################################################################################################

  # begin section: Children Data Server Logic

  # Render year selector for Children Data
  output$children_year_selector <- renderUI({
    selectInput("children_year", "Select Year", choices = sort(unique(data2$year), decreasing = TRUE), selected = max(data2$year))
  })

  # Render plot for Single Year Children Data
  output$childrenSingleYearPlot <- renderPlotly({
    # Check if the year input is NULL before proceeding
    req(input$children_year, input$children_state)

    # Filter data based on selected year and state
    filtered_data <- data2 %>%
      filter(year == input$children_year) %>%
      filter(if (input$children_state != "All States") state == input$children_state else TRUE) %>%
      group_by(child_age, child_parent) %>%
      summarise(total_estimate = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
      arrange(child_age)

    # Set y-axis value and hover text based on display mode
    if (input$children_display_mode == "Relative") {
      filtered_data <- filtered_data %>%
        group_by(child_age) %>%
        mutate(total_by_age = sum(total_estimate)) %>%
        ungroup() %>%
        mutate(percentage = (total_estimate / total_by_age) * 100)
      y_value <- ~percentage
      hover_text <- ~paste("Parental Situation:", child_parent,
                          "<br>Percentage:", round(percentage, 1), "%")
      y_axis_title <- "Percentage (%)"
    } else {
      y_value <- ~total_estimate
      hover_text <- ~paste("Parental Situation:", child_parent,
                          "<br>Estimate:", scales::comma(total_estimate))
      y_axis_title <- "Population Estimate"
    }
    
    # Plot for Children Data (Single Year)
    plot <- plot_ly(
      data = filtered_data,
      x = ~child_age,
      y = y_value,
      color = ~child_parent,
      type = "bar",
      text = hover_text,
      hoverinfo = "text",
      textposition = "none"
    ) %>%
      layout(
        title = paste("Children Data in", input$children_year, "for State", input$children_state),
        xaxis = list(title = "Child Age Group"),
        yaxis = list(title = y_axis_title),
        barmode = "stack",
        margin = list(t = 80)  # Add top margin for spacing
      )
    
    plot
  })

  # Render plot for Trend Over Time Children Data
  output$childrenTrendPlot <- renderPlotly({
    # Filter data based on selected state and include all years
    filtered_data <- data2 %>%
      filter(if (input$children_state != "All States") state == input$children_state else TRUE) %>%
      group_by(year, child_age, child_parent) %>%
      summarise(total_estimate = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
      arrange(child_age, year)

    # Set y-axis value and hover text based on display mode
    if (input$children_display_mode == "Relative") {
      filtered_data <- filtered_data %>%
        group_by(year, child_age) %>%
        mutate(total_by_age_year = sum(total_estimate)) %>%
        ungroup() %>%
        mutate(percentage = (total_estimate / total_by_age_year) * 100)
      y_value <- ~percentage
      hover_text <- ~paste("Year:", year, "<br>Parental Situation:", child_parent,
                          "<br>Percentage:", round(percentage, 1), "%")
      y_axis_title <- "Percentage (%)"
      y_axis_range <- list(range = c(0, 100))  # Set y-axis range to 0-100% for relative display
    } else {
      y_value <- ~total_estimate
      hover_text <- ~paste("Year:", year, "<br>Parental Situation:", child_parent,
                          "<br>Estimate:", scales::comma(total_estimate))
      y_axis_title <- "Population Estimate"
      y_axis_range <- NULL  # No fixed range for absolute display
    }

    # Plot for Trend Over Time with animation
    plot <- plot_ly(
      data = filtered_data,
      x = ~child_age,
      y = y_value,
      color = ~child_parent,
      type = "bar",
      text = hover_text,
      hoverinfo = "text",
      textposition = "none",
      frame = ~year  # Animation by year
    ) %>%
      layout(
        title = paste("Children Data Trend Over Time for State", input$children_state),
        xaxis = list(title = "Child Age Group"),
        yaxis = list(title = y_axis_title, range = y_axis_range),
        barmode = "stack",
        margin = list(t = 80)  # Add top margin for spacing
      ) %>%
      animation_opts(frame = 1000, easing = "linear", redraw = FALSE)

    plot
  })

  # end section: Children Data Server Logic
  
}

# Run the application
shinyApp(ui = ui, server = server)