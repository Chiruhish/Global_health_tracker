library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(maps)
library(shinydashboard)
library(plotly)



# Loading the datasets
global_hunger_data <- read.csv("global-hunger-index.csv")
underweight_data <- read.csv("share-of-children-underweight.csv")


world_coords <- map_data("world")

# Drop the unwanted column from global hunger data
global_hunger_data <- global_hunger_data %>% select(-X411773.annotations)

# Data cleaning function
clean_data <- function(data, world_coords) {
  country_coords <- world_coords %>%
    group_by(region) %>%
    summarise(long = mean(long), lat = mean(lat), .groups = 'drop')
  
  data <- data %>%
    left_join(country_coords, by = c("Entity" = "region"))
  
  data <- data %>%
    filter(!is.na(data[, 4]))
  
  data <- data %>%
    filter(!is.na(lat) & !is.na(long) & lat != 0 & long != 0)
  
  data[, 4] <- as.numeric(as.character(data[, 4]))
  data$Year <- as.numeric(as.character(data$Year))
  data$Entity <- as.character(data$Entity)
  
  data <- data %>%
    filter(!is.na(lat) & !is.na(long) & lat != 0 & long != 0 & !is.na(data[, 4]) & !is.na(Year))
  
  return(data)
}

# Cleaning the datasets
global_hunger_data <- clean_data(global_hunger_data, world_coords)
underweight_data <- clean_data(underweight_data, world_coords)


# Defining UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Global Health Tracker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Global Hunger Index", tabName = "hunger", icon = icon("dashboard")),
      menuItem("Underweight Children", tabName = "underweight", icon = icon("child")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "hunger",
              fluidRow(
                box(
                  title = "Cumulative Global Hunger Index",
                  width = 4,
                  selectInput("selected_country_hunger", "Select Country", choices = unique(global_hunger_data$Entity)),
                  plotlyOutput("cumulativePlotHunger")
                ),
                box(
                  title = "Global Hunger Index Map",
                  width = 8,
                  leafletOutput("mapPlotHunger")
                )
              ),
              fluidRow(
                box(
                  title = "Global Hunger Index by Country",
                  width = 12,
                  plotlyOutput("barPlotHunger")
                )
              )
      ),
      tabItem(tabName = "underweight",
              fluidRow(
                box(
                  title = "Share of Children Underweight",
                  width = 4,
                  selectInput("selected_country_underweight", "Select Country", choices = unique(underweight_data$Entity)),
                  plotlyOutput("cumulativePlotUnderweight")
                ),
                box(
                  title = "Underweight Children Map",
                  width = 8,
                  leafletOutput("mapPlotUnderweight")
                )
              ),
              fluidRow(
                box(
                  title = "Underweight Children by Country",
                  width = 12,
                  plotlyOutput("barPlotUnderweight")
                )
              )
      ),
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "About this Tracker",
                  width = 12,
                  h3("Purpose of the Tracker"),
                  p("This dashboard provides an overview of global health data, focusing on hunger and underweight children. The aim is to raise awareness about these critical issues, highlight the regions most affected, and provide insights for policymakers, researchers, and the general public."),
                  h3("Using the Dashboard"),
                  p("The dashboard contains three main sections: Global Hunger Index, Underweight Children, and Stunting in Children (future work). Each section has interactive elements to explore the data in detail."),
                  p("1. **Global Hunger Index**:"),
                  p("   - **Cumulative Plot**: Shows the global hunger index over the years for selected countries. Use the dropdown menu to select a country and see its trend."),
                  p("   - **Map**: Displays the global hunger index on a map. The size and color of the circles represent the severity."),
                  p("   - **Bar Plot**: Compares the global hunger index of the top 10 countries."),
                  p("2. **Underweight Children**:"),
                  p("   - **Cumulative Plot**: Shows the share of underweight children over the years for selected countries. Use the dropdown menu to select a country and see its trend."),
                  p("   - **Map**: Displays the share of underweight children on a map. The size and color of the circles represent the severity."),
                  p("   - **Bar Plot**: Compares the share of underweight children of the top 10 countries."),
                  h3("Interpreting the Graphs"),
                  p("The graphs and plots in this dashboard provide a visual representation of the data, making it easier to understand trends and comparisons. The color and size of the markers on the maps help indicate the severity of the issues."),
                  p("The interactive elements, such as dropdown menus and hover effects, allow users to explore the data in more detail and gain insights into specific countries or regions."),
                  h3("Data Sources"),
                  p("The data used in this dashboard is sourced from [Kaggle](https://www.kaggle.com/datasets/whenamancodes/the-global-hunger-index).")
                )
              )
      )
    )
  )
)


server <- function(input, output, session) {
  
  
  filtered_data_hunger <- reactive({
    global_hunger_data %>% filter(Entity == input$selected_country_hunger)
  })
  
  filtered_data_underweight <- reactive({
    underweight_data %>% filter(Entity == input$selected_country_underweight)
  })
  
  # Cumulative plot for Global Hunger Index
  output$cumulativePlotHunger <- renderPlotly({
    plot_ly(filtered_data_hunger(), x = ~Year, y = ~global_hunger_index, type = 'scatter', mode = 'lines+markers', color = ~Entity) %>%
      layout(title = paste("Cumulative Global Hunger Index for", input$selected_country_hunger), xaxis = list(title = "Year"), yaxis = list(title = "Global Hunger Index"))
  })
  
  # Bar plot for Global Hunger Index
  output$barPlotHunger <- renderPlotly({
    top10_data <- global_hunger_data %>%
      group_by(Entity) %>%
      summarise(latest_gi = max(global_hunger_index, na.rm = TRUE)) %>%
      arrange(desc(latest_gi)) %>%
      head(10)
    
    
    top10_data$Entity <- factor(top10_data$Entity, levels = top10_data$Entity)
    
    plot_ly(top10_data, x = ~latest_gi, y = ~reorder(Entity, latest_gi), type = 'bar', orientation = 'h', colors = "Blues") %>%
      layout(title = "Top 10 Countries by Global Hunger Index", xaxis = list(title = "Global Hunger Index"), yaxis = list(title = "Country"))
  })
  
  # Map plot for Global Hunger Index
  output$mapPlotHunger <- renderLeaflet({
    leaflet(global_hunger_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%  # Change the tile provider for better visualization
      addCircleMarkers(lng = ~long, lat = ~lat, 
                       radius = ~global_hunger_index / 10, 
                       color = ~colorFactor("YlOrRd", domain = global_hunger_data$global_hunger_index)(global_hunger_index),
                       fillOpacity = 0.7,
                       popup = ~paste("<b>Country:</b>", Entity, "<br><b>Global Hunger Index:</b>", global_hunger_index))
  })
  
  # Cumulative plot for Underweight Children
  output$cumulativePlotUnderweight <- renderPlotly({
    plot_ly(filtered_data_underweight(), x = ~Year, y = ~`Share.of.children.underweight`, type = 'scatter', mode = 'lines+markers', color = ~Entity) %>%
      layout(title = paste("Share of Children Underweight for", input$selected_country_underweight), xaxis = list(title = "Year"), yaxis = list(title = "Underweight Children (%)"))
  })
  
  # Bar plot for Underweight Children
  output$barPlotUnderweight <- renderPlotly({
    top10_data <- underweight_data %>%
      group_by(Entity) %>%
      summarise(latest_uw = max(`Share.of.children.underweight`, na.rm = TRUE)) %>%
      arrange(desc(latest_uw)) %>%
      head(10)
    
    
    top10_data$Entity <- factor(top10_data$Entity, levels = top10_data$Entity)
    
    plot_ly(top10_data, x = ~latest_uw, y = ~reorder(Entity, latest_uw), type = 'bar', orientation = 'h', colors = "Blues") %>%
      layout(title = "Top 10 Countries by Underweight Children", xaxis = list(title = "Underweight Children (%)"), yaxis = list(title = "Country"))
  })
  
  # Map plot for Underweight Children
  output$mapPlotUnderweight <- renderLeaflet({
    leaflet(underweight_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%  # Change the tile provider for better visualization
      addCircleMarkers(lng = ~long, lat = ~lat, 
                       radius = ~`Share.of.children.underweight` / 10, 
                       color = ~colorFactor("YlOrRd", domain = underweight_data$`Share.of.children.underweight`)(`Share.of.children.underweight`),
                       fillOpacity = 0.7,
                       popup = ~paste("<b>Country:</b>", Entity, "<br><b>Underweight Children (%):</b>", `Share.of.children.underweight`))
  })
}


shinyApp(ui = ui, server = server)