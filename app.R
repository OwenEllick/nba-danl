# Load required libraries
library(shiny)
library(tidyverse)
library(viridis)
library(showtext)
library(plotly)# For interactive plots

font_add_google("Roboto", "roboto")
font_add_google("Alegreya", "alegreya")
showtext_auto()

# Replace this with your actual NBA project data
nba_players <- read.csv("http://bcdanl.github.io/data/nba_players.csv")  # Assuming a CSV file with columns: Team, Player, Points, Assists, Rebounds

# Define UI for the Shiny app
ui <- fluidPage(
  
  # Application title
  titlePanel("NBA Player Statistics"),
  
  # Sidebar for selecting a team
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "team",
        label = "Select a Team:",
        choices = unique(nba_players$Team),
        selected = unique(nba_players$Team)[1]
      )
    ),
    
    # Main panel for displaying the plot and selected value
    mainPanel(
      plotlyOutput("teamPlot"),  # Interactive Plotly plot
      textOutput("selectedValue")  # Text output for selected value
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output) {
  
  # Reactive expression to filter data for the selected team
  filtered_data <- reactive({
    nba_players %>%
      filter(Team == input$team) %>%
      pivot_longer(cols = c(PTS, AST, TRB), 
                   names_to = "Statistic", 
                   values_to = "Value")
  })
  
  # Render the interactive plot
  output$teamPlot <- renderPlotly({
    plot <- ggplot(filtered_data(), aes(x = PlayerName, y = Value, fill = Statistic)) +
      geom_bar(stat = "identity", position = "dodge", color = "black") +
      scale_fill_viridis_d(
        name = " Player Statistics",
        option = "D"
      ) +
      labs(
        title = paste("Player Statistics for", input$team),
        subtitle = "Player Points, Assists, and Rebounds per Game",
        x = "Player",
        y = "Stat Value",
        fill = "Statistic"
      ) +
      theme_minimal(base_family = "roboto") +
      theme(
        text = element_text(family = "roboto", size = 14),
        
        plot.title = element_text(family = "alegreya",hjust = 0.5, size = rel(1.5), face = "bold", margin = margin(10,0,20,0), color = "#2C3E50"),
        plot.subtitle = element_text(family = "alegreya", face = "italic", size = rel(1.2), hjust = 0.5, 
                                     margin = margin(5, 0, 15, 0), color = "#34495E"),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(angle = 45,hjust = 1 , face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        plot.background = element_rect(fill = "#f9f9f9", color = NA),
        panel.grid.major = element_line(color = "#eaeaea"),
        panel.grid.minor = element_blank()
      )
    
    ggplotly(plot, tooltip = c("x", "y", "fill")) %>% 
      layout(clickmode = "event+select")  # Enable click events
  })
  
  # Display the selected value when a bar is clicked
  output$selectedValue <- renderText({
    event_data <- event_data("plotly_click")
    if (is.null(event_data)) {
      return("Click on a bar to see details.")
    }
    
    paste0("You selected: ", 
           event_data$key, 
           " (Player: ", event_data$x, 
           ", Value: ", event_data$y, 
           ", Statistic: ", event_data$curveNumber + 1, ")")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)