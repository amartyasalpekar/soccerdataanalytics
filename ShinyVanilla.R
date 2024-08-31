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

james_pass <- StatsBombData%>%filter(type.name == "Pass" & is.na(pass.outcome.name), player.id == 5695)

##Final Third Passes 

passes_final_third <- StatsBombData%>%filter(type.name == "Pass", pass.end_location.x > 80)%>%
  filter(!pass.outcome.name %in% c("Injury Clearance", "Unknown"))


passes_final_third <- passes_final_third %>%
  mutate(pass_success = ifelse(is.na(pass.outcome.name), "Successful", "Unsuccessful"))

passes_final_third_count <- passes_final_third %>%
  group_by(player.name) %>%
  summarise(
    total_passes = n(),
    successful_passes = sum(pass_success == "Successful"),
    unsuccessful_passes = sum(pass_success == "Unsuccessful")
  ) %>%
  ungroup()

passes_final_third_count <- passes_final_third_count%>%filter(total_passes > 83)


passes_final_third_count <- passes_final_third_count %>%
  mutate(is_james = ifelse(player.name == "James David Rodríguez Rubio", TRUE, FALSE))

## Copa America Final Argentina 

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
  
  tabPanel("Copa America 2023-24 Top Players Analysis",
           mainPanel(
             plotOutput("jamesPassesPlot"),
             plotOutput("finalThirdPassesPlot")
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
  
  observe({
    updateSelectInput(session, "team", choices = unique(shots_goal$team.name))
  })
  
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
    
    create_Pitch() + 
      geom_segment(data = james_pass, aes(x = location.x, y = location.y,
                                          xend = pass.end_location.x, yend = pass.end_location.y,
                                          colour = pass.goal_assist),
                   lineend = "round", size = 0.5, 
                   arrow = arrow(length = unit(0.07, "inches"), ends = "last", type = "open")) +
      scale_colour_manual(values = c("FALSE" = "#000000", "TRUE" = "#FF0000"), 
                          labels = c("FALSE" = "Other Passes", "TRUE" = "Assists"), # More descriptive legend labels
                          name = "Pass Type") + # Updated legend title
      labs(title = "Golden Ball Winner - James Rodriguez, All Passes with Assists Highlighted", 
           subtitle = "Copa America 2023-24") + 
      scale_y_reverse() + 
      coord_fixed(ratio = 105/100) + 
      theme_minimal() + 
      theme(
        legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10)    
      )
  })
  
  output$finalThirdPassesPlot <- renderPlot({
    ggplot(passes_final_third_count, aes(x = reorder(player.name, -total_passes))) +
      geom_bar(aes(y = successful_passes, fill = "Successful Passes"), stat = "identity", position = "dodge", color = "black") +
      geom_bar(aes(y = unsuccessful_passes, fill = "Unsuccessful Passes"), stat = "identity", position = "dodge", color = "black") +
      scale_fill_manual(values = c("Successful Passes" = "green", "Unsuccessful Passes" = "red")) +
      labs(title = "James Rodríguez vs. Top 10 Final Third Passers",
           x = "Player",
           y = "Number of Passes") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_bar(data = passes_final_third_count %>% filter(player.name == "James David Rodríguez Rubio"),
               aes(y = successful_passes, fill = "Successful Passes"), color = "black", stat = "identity", position = "dodge", fill = "green") +
      geom_bar(data = passes_final_third_count %>% filter(player.name == "James David Rodríguez Rubio"),
               aes(y = unsuccessful_passes, fill = "Unsuccessful Passes"), color = "black", stat = "identity", position = "dodge", fill = "red") +
      labs(fill = "Pass Type")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
