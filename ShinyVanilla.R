library(tidyverse) 
library(StatsBombR) #1 
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(SBpitch)

Comp <- FreeCompetitions() %>% filter(competition_id==223 & season_id==282) #2 

Matches <- FreeMatches(Comp) #3 
StatsBombData <- free_allevents(MatchesDF = Matches, Parallel = T) #4 

StatsBombData = allclean(StatsBombData) #5

shots_goal = StatsBombData %>% group_by(team.name) %>%
  summarise(shots = sum(type.name == "Shot", na.rm = T), goals = sum(shot.outcome.name == "Goal", na.rm = T))



# Danger Passes: Passes that were played in the 15 second window before the shot was attempted.
# I am calculating the danger passes played by Copa America Winners - Argentina and heat map to understand which third of the field were Argentina most dangerous in. 

argentina_events <- StatsBombData%>%filter(team.name == "Argentina")

danger_passes <- data.frame()

# Iterate over each period
for (period in 1:2) {
  
  # Filter the passes and shots for the current period
  passes <- argentina_events %>%
    filter(type.name == "Pass", is.na(pass.outcome.name), period == period) %>%
    select(x = location.x, y = location.y, end_x = pass.end_location.x, end_y = pass.end_location.y, minute, second, player.name) %>%
    mutate(pass_time_seconds = minute * 60 + second)
  
  shots <- argentina_events %>%
    filter(type.name == "Shot", period == period) %>%
    select(minute, second) %>%
    mutate(shot_time_seconds = minute * 60 + second)
  
  shot_window <- 15
  
  # Adjust the shot start time for edge cases
  shots <- shots %>%
    mutate(shot_start = pmax(shot_time_seconds - shot_window, ifelse(period == 1, 0, 2700))) # 2700 seconds is the start of the second half (45 * 60)
  
  # Identify danger passes
  pass_to_shot <- sapply(passes$pass_time_seconds, function(pass_time) {
    any(sapply(seq_len(nrow(shots)), function(i) {
      pass_time > shots$shot_start[i] && pass_time < shots$shot_time_seconds[i]
    }))
  })
  
  danger_passes_period <- passes[pass_to_shot, ]
  
  # Combine the results
  danger_passes <- bind_rows(danger_passes, danger_passes_period)
}

## James Rodirguez passes and assists. 

james_pass <- StatsBombData%>%filter(type.name == "Pass" & is.na(pass.outcome.name), player.id == 5695)%>%
  filter(pass.end_location.x>=102 & pass.end_location.y<=62 & pass.end_location.y>=18)


ui <- navbarPage(
  "Copa America 2023-24 Analysis",
  
  tabPanel("Shots and Goals",
           sidebarLayout(
             sidebarPanel(
               selectInput("team", "Select Team:", choices = NULL, selected = NULL, multiple = TRUE)
             ),
             
             mainPanel(
               plotlyOutput("shotsGoalsPlot")
             )
           )
  ),
  
  tabPanel("James Rodriguez Passes",
           mainPanel(
             plotOutput("jamesPassesPlot")
           )
  )
)

server <- function(input, output, session) {
  # Reactive expression to filter data based on selected teams
  filteredData <- reactive({
    data <- shots_goal
    if (!is.null(input$team)) {
      data <- data %>% filter(team.name %in% input$team)
    }
    data %>% mutate(team.name = fct_reorder(team.name, shots))
  })
  
  # Update selectInput choices based on available teams
  observe({
    updateSelectInput(session, "team", choices = unique(shots_goal$team.name))
  })
  
  # Render the interactive plot using plotly
  output$shotsGoalsPlot <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = team.name)) +
      geom_bar(aes(y = shots, text = paste("Shots:", shots)), stat = "identity", fill = "blue", alpha = 0.7) +
      geom_bar(aes(y = goals, text = paste("Goals:", goals)), stat = "identity", fill = "red", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Shots and Goals", x = "Team", y = "Total") +
      coord_flip()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Render the James Rodriguez plot
  output$jamesPassesPlot <- renderPlot({
    james_pass <- StatsBombData %>%
      filter(type.name == "Pass" & is.na(pass.outcome.name) & player.id == 5695) %>%
      filter(pass.end_location.x >= 102 & pass.end_location.y <= 62 & pass.end_location.y >= 18)
    
    create_Pitch() + 
      geom_segment(data = james_pass, aes(x = location.x, y = location.y, 
                                          xend = pass.end_location.x, yend = pass.end_location.y),
                   lineend = "round", size = 0.5, colour = "#000000", 
                   arrow = arrow(length = unit(0.07, "inches"), ends = "last", type = "open")) + 
      labs(title = "James Rodriguez, Completed Box Passes", 
           subtitle = "Copa America 2023-24") + 
      scale_y_reverse() + 
      coord_fixed(ratio = 105/100)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
