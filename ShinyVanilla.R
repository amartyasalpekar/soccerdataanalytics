library(tidyverse) 
library(StatsBombR) #1 
Comp <- FreeCompetitions() %>% filter(competition_id==223 & season_id==282) #2 

Matches <- FreeMatches(Comp) #3 
StatsBombData <- free_allevents(MatchesDF = Matches, Parallel = T) #4 

StatsBombData = allclean(StatsBombData) #5

shots_goal = StatsBombData %>% group_by(team.name) %>%
  summarise(shots = sum(type.name == "Shot", na.rm = T), goals = sum(shot.outcome.name == "Goal", na.rm = T))

shots_goal_per_game = StatsBombData %>% group_by(team.name) %>%
  summarise(shots = sum(type.name == "Shot", na.rm = T)/n_distinct(match_id), goals = sum(shot.outcome.name == "Goal", na.rm = T)/n_distinct(match_id))%>%
  arrange(desc(shots))




library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Define UI
ui <- fluidPage(
  titlePanel("Shots and Goals per Game Copa America 2023-24"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Select Team:", choices = NULL, selected = NULL, multiple = TRUE)
    ),
    
    mainPanel(
      plotlyOutput("shotsGoalsPlot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive expression to filter data based on selected teams
  filteredData <- reactive({
    data <- shots_goal_per_game
    if (!is.null(input$team)) {
      data <- data %>% filter(team.name %in% input$team)
    }
    data %>% mutate(team.name = fct_reorder(team.name, shots, .desc = T))
  })
  
  # Update selectInput choices based on available teams
  observe({
    updateSelectInput(session, "team", choices = unique(shots_goal_per_game$team.name))
  })
  
  # Render the interactive plot using plotly
  output$shotsGoalsPlot <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = team.name)) +
      geom_bar(aes(y = shots, text = paste("Shots:", shots)), stat = "identity", fill = "blue", alpha = 0.7) +
      geom_bar(aes(y = goals, text = paste("Goals:", goals)), stat = "identity", fill = "red", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Shots and Goals per Game", x = "Team", y = "Average per Game") +
      coord_flip()
    
    ggplotly(p, tooltip = "text")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)