library(shiny)
library(tidyverse)
library(dplyr)
library(readr)
library(lubridate)
library(geosphere)
library(leaflet)
library(plotly)
library(htmltools)
library(htmlwidgets)

#Data loading and modification
# Load data
file_paths <- list.files("data", pattern = "\\.csv$", full.names = TRUE)
dat <- lapply(seq_along(file_paths), function(i) {
  data <- read.csv(file_paths[i])
  data$datetime <- as.POSIXct(paste(data$date, data$time), format = "%Y-%m-%d %H:%M:%S")
  data$run <- i
  return(data)
})
dat <- do.call(rbind, dat)

# Calculate distance difference
dat <- dat %>%
  mutate(dist_diff = c(0, sqrt(diff(dat$lng)^2 + diff(dat$lat)^2)*111.32))

new_run_starts <- which(dat$run != lag(dat$run, default = dat$run[1]))
dat$dist_diff[new_run_starts] <- 0

# Calculate duration per run
per_run <- dat %>%
  group_by(run) %>%
  summarize(min_duration = difftime(max(datetime), min(datetime), units = "mins"),
            duration = {
              dur <- as.numeric(difftime(max(datetime), min(datetime), units = "mins"))
              if (dur >= 60) {
                hours <- floor(dur) / 60
                minutes <- round(dur) %% 60
                paste(round(hours,0), "hr", minutes, "min", sep = " ")
              }
              else {
                paste(round(dur,2), "min")
              }
            },
            distance = round(sum(dist_diff),2)
  )

# Calculate pace
per_run <- per_run %>%
  mutate(pace = round(distance/as.numeric(min_duration),2))

#Add date to per_run
per_run <- per_run %>%
  mutate(date = as.Date(unique(dat$date)))

#Dates ran vs not ran
all_dates <- seq(as.Date("2022-02-27"), as.Date("2024-03-01"), by = "day")
dates_ran <- as.Date(unique(dat$date))
dates_notran <- as.Date(all_dates[!all_dates %in% dates_ran])


# UI
ui <- fluidPage(
  titlePanel("Jog4Health"),

  # Define UI for two tabs: Maps and Overview
  tabsetPanel(
    tabPanel(
      icon = icon("map-marker-alt"),
      title = "Map",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   dateInput("date", "Select Date:", value = Sys.Date(), min = "2022-02-27", max = "2024-03-01",
                             format = "yyyy-mm-dd", datesdisabled = as.character(as.Date(dates_notran))),
                   actionButton("submit", "Show Run")
                 ),
                 mainPanel(
                   textOutput("header"),
                   verbatimTextOutput("run_info"),  # Add this line to display run summary
                   leafletOutput("map")
                 )
               )
             )
    ),
    tabPanel(
      icon = icon("road"),
      title = "Distance",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   selectInput("year", "Select Year:", choices = unique(year(dat$datetime)))
                 ),
                 mainPanel(
                   #textOutput("overview"),
                   verbatimTextOutput("distance_summary"),
                   plotlyOutput("distance_plot")
                 )
               )
             )
    ),
    tabPanel(
      icon = icon("stopwatch"),
      title = "Pace",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   selectInput("year_pace", "Select Year:", choices = unique(year(dat$datetime)))
                 ),
                 mainPanel(
                   verbatimTextOutput("pace_summary"),
                   plotlyOutput("pace_plot")
                 )
               )
             )
    )
  )
)




# Server logic
server <- function(input, output) {

  # Function to generate map for a given date
  output$map <- renderLeaflet({
    req(input$submit)
    selected_date <- input$date
    day_run <- unique(dat$run[dat$date == selected_date])
    run_data <- dat[dat$run == day_run, ]

    start_icon <- makeIcon(
      iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
      iconWidth = 20, iconHeight = 30
    )
    end_icon <- makeIcon(
      iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-red.png",
      iconWidth = 20, iconHeight = 30
    )

    map_run <- leaflet(run_data) %>%
      addTiles() %>%
      addPolylines(lng = ~lng, lat = ~lat, color = "blue") %>%
      addMarkers(data = run_data[1, ], popup = ~"Start", icon = start_icon) %>%
      addMarkers(data = run_data[nrow(run_data), ], popup = ~"Finish", icon = end_icon)
    #addMarkers(~lng, ~lat, popup = paste("Time:", run_data$time, "<br>",
    #  "Elevation:", run_data$elevation, "<br>"))
  })

  # Function to display summary statistics about each run
  output$header <- renderText({
    req(input$submit)
    selected_date <- input$date
    day_run <- unique(dat$run[dat$date == selected_date])
    run_data <- dat[dat$run == day_run, ]

    header <- paste(weekdays(selected_date), "Run!")
    return(header)

  })
  output$run_info <- renderText({
    req(input$submit)
    selected_date <- input$date
    day_run <- unique(dat$run[dat$date == selected_date])
    run_data <- dat[dat$run == day_run, ]


    run_info <- paste(
      " Distance:", per_run$distance[per_run$run == day_run], "km\n",
      "Duration:", per_run$duration[per_run$run == day_run],"\n",
      "Pace:", per_run$pace[per_run$run == day_run], "km/min",
      collapse = "\n")

    return(run_info)
  })

  # Function to display overview
  output$overview <- renderPrint({
    total_dist <- sum(per_run$distance)
    sum_dur <- as.numeric(sum(per_run$min_duration))
    total_time <- paste(floor(sum_dur/60), "hr", round(sum_dur%%60,2), "min", sep = " ")
    ave_pace <- round(mean(per_run$pace),2)

    overview_text <- paste("Total distance:", total_dist,"km\n",
                           "Total time:", total_time, "\n",
                           "Average Pace:", ave_pace, "km/min")
    return(overview_text)
  })

  output$distance_plot <- renderPlotly({
    req(input$year)
    filtered_dat <- per_run[year(per_run$date) == input$year, ]

    # Calculate average pace and average distance
    avg_distance <- mean(filtered_dat$distance)

    # Create a plotly line plot
    plot <- plot_ly(data = filtered_dat, x = ~date, y = ~distance, type = 'scatter', mode = 'lines') %>%
      add_trace(x = filtered_dat$date, y = rep(avg_distance, nrow(filtered_dat)), type = 'scatter', mode = 'lines', line = list(color = 'red', dash = 'dash'), name = 'Average Distance') %>%
      layout(
             xaxis = list(title = "Date"),
             yaxis = list(title = "Distance (km)"), showlegend = F) %>%
      config(scrollZoom = TRUE, displayModeBar = TRUE, modeBarButtonsToRemove = list('sendDataToCloud', 'autoScale2d', 'resetScale2d', 'toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian'))

    return(plot)
  })

  output$pace_plot <- renderPlotly({
    req(input$year_pace)
    filtered_dat <- per_run[year(per_run$date) == input$year_pace, ]

    # Calculate average pace
    avg_pace <- mean(filtered_dat$pace)

    # Create a plotly line plot for pace
    plot <- plot_ly(data = filtered_dat, x = ~date, y = ~pace, type = 'scatter', mode = 'lines') %>%
      add_trace(x = filtered_dat$date, y = rep(avg_pace, nrow(filtered_dat)), type = 'scatter', mode = 'lines', line = list(color = 'red', dash = 'dash'), name = 'Average Pace') %>%
      layout(
             xaxis = list(title = "Date"),
             yaxis = list(title = "Pace (km/min)"), showlegend = F) %>%
      config(scrollZoom = TRUE, displayModeBar = TRUE, modeBarButtonsToRemove = list('sendDataToCloud', 'autoScale2d', 'resetScale2d', 'toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian'))

    return(plot)
  })

  # Function to display distance summary
  output$distance_summary <- renderText({
    req(input$year)
    filtered_dat <- per_run[year(per_run$date) == input$year, ]

    total_distance <- sum(filtered_dat$distance)
    max_distance <- max(filtered_dat$distance)
    max_distance_date <- filtered_dat$date[which.max(filtered_dat$distance)]
    min_distance <- min(filtered_dat$distance)
    min_distance_date <- filtered_dat$date[which.min(filtered_dat$distance)]

    summary_text <- paste(
      "Distance ran in", input$year, "Analysis\n",
      "Total distance ran:", total_distance, "km\n",
      "Maximum distance ran:", max_distance, "km (on", max_distance_date, ")\n",
      "Minimum distance ran:", min_distance, "km (on", min_distance_date, ")\n"
    )

    return(summary_text)
  })

  # Function to display pace summary
  output$pace_summary <- renderText({
    req(input$year_pace)
    filtered_dat <- per_run[year(per_run$date) == input$year_pace, ]

    avg_pace <- round(mean(filtered_dat$pace),2)
    max_pace <- max(filtered_dat$pace)
    max_pace_date <- filtered_dat$date[which.max(filtered_dat$pace)]
    min_pace <- min(filtered_dat$pace)
    min_pace_date <- filtered_dat$date[which.min(filtered_dat$pace)]

    summary_text <- paste(
      "Pace in", input$year_pace, "Analysis\n",
      "Average pace:", avg_pace, "km/min\n",
      "Maximum pace:", max_pace, "km/min (on", max_pace_date, ")\n",
      "Minimum pace:", min_pace, "km/min (on", min_pace_date, ")\n"
    )

    return(summary_text)
  })


}

# Run the application
shinyApp(ui = ui, server = server)
