##### Nova Scotia Ocean Temperature Dashboard
##### Author: Scottie Szewczyk
##### Tidy Tuesday

library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)

# Load the ocean_temperatures Tidy Tuesday dataset (from March 31, 2026).
# This dataset was originally downloaded from the tidytuesdayR package
# using the code found at https://github.com/rfordatascience/tidytuesday/tree/main/data/2026/2026-03-31.
ocean_temperature <- read_csv("ocean_temperature.csv")


########## UI ##########
ui <- fluidPage(
  
  titlePanel("Nova Scotia Ocean Temperature Dashboard"),
  
  div(
    style = "margin-bottom: 20px; font-size: 18px",
    p("This dashboard visualizes ocean temperature measurements at different depths off the coast of Nova Scotia, Canada from 2018-2025."),

    p("- Use the", strong("Daily Temperatures"), "tab to explore temperature variation within a specific month and year."),
    p("- Use the", strong("Monthly Temperatures Animation"), "tab to observe how temperature patterns change across years."),
    tags$hr(style = "border-top: 3px solid #000000;"),
    p(em("The original data was collected through the Centre for Marine Applied Research's Coastal Monitoring Program and
      can be downloaded from the Nova Scotia Open Data Portal. The dataset was curated for Tidy Tuesday by Danielle Dempsey and Rachel Woodside,
      Centre for Marine Applied Research. More information can be found at ",
      tags$a(
      href = "https://github.com/rfordatascience/tidytuesday/tree/main/data/2026/2026-03-31",
      "https://github.com/rfordatascience/tidytuesday/tree/main/data/2026/2026-03-31",
      target = "_blank"
      )
    )
  ),
    p(em("Please note that this dashboard is only for demonstration purposes. It is not intended to convey up-to-date scientific information."))
  ),
  
  tabsetPanel(
    
    ##### TAB 1: Daily Temperatures
    
    tabPanel("Daily Temperatures",
             
             sidebarLayout(
               sidebarPanel(
                 selectInput("year", "Select Year",
                             choices = sort(unique(year(ocean_temperature$date))),
                             selected = year(Sys.Date())),
                 
                 selectInput("month", "Select Month",
                             choices = setNames(1:12, month.name),
                             selected = month(Sys.Date()))
               ),
               
               mainPanel(
                 h3(textOutput("daily_title")),
                 div(
                   style = "padding: 15px; border-radius: 8px; margin-bottom: 20px;",
                   p("This heatmap shows the mean", strong("daily"), "temperature at each of the 7 depths (in meters): 2, 5, 10, 15, 20, 30, 40.
                      The x-axis represents the day of the month;",
                     "the y-axis represents water depth in meters (please note that shallow depths are at the top);",
                     "and the color indicates temperature in degrees Celsius,",
                     "ranging from blue (cooler) to red (warmer). Colorless cells indicate that no data was collected on those respective dates."),
                   p(tags$u("Dragging the cursor across the heatmap will show more information on each cell."))
                 ),
                 p(
                   textOutput("daily_alt_text"),
                   style = "font-size: 14px; color: #cccccc; margin-bottom: 15px;"
                 ),
                 plotlyOutput("heatmap_plot", width = "100%", height = "70vh")
               )
            )
    ),
    
    ##### TAB 2: Monthly Temperatures Animation
    
    tabPanel("Monthly Temperatures Animation",
             
             mainPanel(
               
               div(
                 style = "padding: 15px; border-radius: 8px; margin-bottom: 20px;",
                 h3(p("Monthly Average Temperatures by Year")),
                 p("This animation shows the mean", strong("monthly"), "temperature at each depth for each year."),
                 p("Press the play button to observe how temperature patterns evolve over time.")
               ),
               plotlyOutput("yearly_animation", width = "100%", height = "70vh")
             )
    ),

  )
)



########## SERVER ##########
server <- function(input, output, session) {
  
  
  # Create a global color scale for heatmap temperatures.
  temp_min <- min(ocean_temperature$mean_temperature_degree_c, na.rm = TRUE)
  temp_max <- max(ocean_temperature$mean_temperature_degree_c, na.rm = TRUE)
  
  ##### TAB 1: Daily Temperatures
  
  # Create reactive title for Tab 1. 
  output$daily_title <- renderText({
    paste(
      "Daily temperatures for",
      month.name[as.integer(input$month)],
      input$year
    )
  })
  
  # Create reactive filtered data.
  filtered_data <- reactive({
    ocean_temperature %>%
      filter(
        year(date) == input$year,
        month(date) == input$month
      )
  })
  
  
  # Plot reactive heatmap of daily temperatures.
  output$heatmap_plot <- renderPlotly({
    
    # Call the reactive filtered dataset.
    data_filtered <- filtered_data()
    
    p <- ggplot(
      data_filtered,
      aes(
        x = date,
        y = factor(sensor_depth_at_low_tide_m),
        fill = mean_temperature_degree_c,
        text = paste(
          "Date:", date,
          "<br>Depth:", sensor_depth_at_low_tide_m, "m",
          "<br>Mean Temp.:", round(mean_temperature_degree_c, 4), "°C"
        )
      )
    ) +
      
      geom_tile() +
      
      # Define color scale based on global limits
      scale_fill_gradientn(
        colors = c("blue", "cyan", "yellow", "red"),
        limits = c(temp_min, temp_max)
      ) +
      
      # Flip y-axis so that shallower (smaller) depths are at the top.
      scale_y_discrete(limits = rev) +
      
      labs(
        x = "",
        y = "Depth (m)",
        fill = "Temperature (°C)"
      ) +
      
      theme_minimal()
    
    # Create tooltip with plotly.
    ggplotly(p, tooltip = "text")
    
  })
  
  
  
  ##### TAB 2: Monthly Temperatures Animation
  
  output$yearly_animation <- renderPlotly({
    
    # Create reactive filtered data.
    data_anim <- ocean_temperature %>%
      mutate(
        year = year(date),
        month = month(date)
      ) %>%
      group_by(year, sensor_depth_at_low_tide_m, month) %>%
      summarise(
        avg_temp = mean(mean_temperature_degree_c, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      complete(
        year,
        month = 1:12,
        sensor_depth_at_low_tide_m
      )
    
    # Create animated heatmap with Plotly.
    plot_ly(
      data = data_anim,
      x = ~factor(month, levels = 1:12, labels = month.name),
      y = ~factor(sensor_depth_at_low_tide_m),
      z = ~avg_temp,
      frame = ~year,
      type = "heatmap",
      colorscale = list(
        c(0.0, "blue"),
        c(0.33, "cyan"),
        c(0.66, "yellow"),
        c(1.0, "red")
      ),
      colorbar = list(title = "Temperature (°C)"),
      zmin = temp_min,
      zmax = temp_max,
      text = ~paste(
        "Date:", month.name[month], year,
        "<br>Depth:", sensor_depth_at_low_tide_m, "m",
        "<br>Mean Temp.:", round(avg_temp, 4), "°C"
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(
          title = "Depth (m)",
          autorange = "reversed"
        )
      ) %>%
      animation_opts(
        frame = 1000,
        transition = 0
      ) %>%
      animation_slider(
        currentvalue = list(
          prefix = "Year: ",
          font = list(size = 16, color = "black")
        )
      )
  })
}

# Run app
shinyApp(ui, server)