library(tidyverse) 
library(StatsBombR) #1 
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(SBpitch)
library(ggthemes)
library(ggrepel)

Comp <- FreeCompetitions() %>% filter(competition_id==223 & season_id==282) #2 

Matches <- FreeMatches(Comp) #3 
StatsBombData <- free_allevents(MatchesDF = Matches, Parallel = T) #4 

StatsBombData = allclean(StatsBombData) #5

shots_goal = StatsBombData %>% group_by(team.name) %>%
  summarise(shots = sum(type.name == "Shot", na.rm = T), goals = sum(shot.outcome.name == "Goal", na.rm = T))



# # Danger Passes: Passes that were played in the 15 second window before the shot was attempted.
# # I am calculating the danger passes played by Copa America Winners - Argentina and heat map to understand which third of the field were Argentina most dangerous in. 
# 
# argentina_events <- StatsBombData%>%filter(team.name == "Argentina")
# 
# danger_passes <- data.frame()
# 
# # Iterate over each period
# for (period in 1:2) {
#   
#   # Filter the passes and shots for the current period
#   passes <- argentina_events %>%
#     filter(type.name == "Pass", is.na(pass.outcome.name), period == period) %>%
#     select(x = location.x, y = location.y, end_x = pass.end_location.x, end_y = pass.end_location.y, minute, second, player.name) %>%
#     mutate(pass_time_seconds = minute * 60 + second)
#   
#   shots <- argentina_events %>%
#     filter(type.name == "Shot", period == period) %>%
#     select(minute, second) %>%
#     mutate(shot_time_seconds = minute * 60 + second)
#   
#   shot_window <- 15
#   
#   # Adjust the shot start time for edge cases
#   shots <- shots %>%
#     mutate(shot_start = pmax(shot_time_seconds - shot_window, ifelse(period == 1, 0, 2700))) # 2700 seconds is the start of the second half (45 * 60)
#   
#   # Identify danger passes
#   pass_to_shot <- sapply(passes$pass_time_seconds, function(pass_time) {
#     any(sapply(seq_len(nrow(shots)), function(i) {
#       pass_time > shots$shot_start[i] && pass_time < shots$shot_time_seconds[i]
#     }))
#   })
#   
#   danger_passes_period <- passes[pass_to_shot, ]
#   
#   # Combine the results
#   danger_passes <- bind_rows(danger_passes, danger_passes_period)
# }

## James Rodirguez passes and assists - Golden Ball. 

james_pass <- StatsBombData%>%filter(type.name == "Pass" & is.na(pass.outcome.name), player.id == 5695)

## Key Passes vs Assists per 90 minutes 

key_passes <- StatsBombData %>%
  filter(!is.na(pass.assisted_shot_id)) %>%
  group_by(player.id, player.name, team.name) %>%
  summarise(key_passes = n())

assists <- StatsBombData %>%
  filter(pass.goal_assist == TRUE) %>%
  group_by(player.id, player.name, team.name) %>%
  summarise(assists = n())

player_minutes <- minutes_data %>% group_by(player.id) %>% summarise(minutes = sum(MinutesPlayed))

player_stats <- left_join(assists, player_minutes)
player_stats <-left_join(player_stats, key_passes)

player_stats <- player_stats%>%mutate(nineties = minutes/90)

player_stats <- player_stats%>%mutate(assists_per90 = assists/nineties, key_passes_per90 = key_passes/nineties)
player_stats <- player_stats%>%filter(minutes >= 180)
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

### Lautaro Martinez - Golden Boot 

lautaro_shots = StatsBombData%>%filter(type.name == "Shot", player.id ==11456)

shotmapxgcolors <- c("#192780", "#2a5d9f", "#40a7d0", "#87cdcf", "#e7f8e6", "#f4ef95", "#FDE960", "#FCDC5F", "#F5B94D", "#F0983E", "#ED8A37", "#E66424", "#D54F1B", "#DC2608", "#BF0000", "#7F0000", "#5F0000")

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
             plotOutput("finalThirdPassesPlot"),
             plotOutput("keyPassesAssistsPlot"),
             plotOutput("lautaroShotMapPlot")
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
  
  output$lautaroShotMapPlot <- renderPlot({
    create_Pitch() + 
      geom_point(data = lautaro_shots, aes(x = location.x, y = location.y, 
                                           fill = shot.statsbomb_xg, 
                                           color = ifelse(shot.outcome.name == "Goal", "Goal", "Miss")), 
                 size = 3, shape = 21, alpha = 0.7) +  # Use shape = 21 for filled points with an outline
      scale_fill_gradientn(colors = shotmapxgcolors, name = "Expected Goals (xG)") + 
      scale_color_manual(values = c("Goal" = "green", "Miss" = "red"), name = "Shot Outcome") +
      labs(title = "Golden Boot Winner - Lautaro Martínez, All Shots",
           subtitle = "Copa America 2023-24") + 
      scale_y_reverse() + 
      coord_fixed(ratio = 105/100) + 
      theme_minimal() + 
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10),
        legend.position = "right",           # Keep legend on the right
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        axis.title = element_blank(),        # Remove axis titles
        axis.text = element_blank(),         # Remove axis text
        axis.ticks = element_blank(),        # Remove axis ticks
        panel.grid = element_blank(),        # Remove grid lines
        plot.margin = unit(c(1, 1, 1, 1), "cm")  # Adjust plot margins
      )
  })
  output$keyPassesAssistsPlot <- renderPlot({
    ggplot(player_stats, aes(x = key_passes_per90, y = assists_per90, color = "Team")) +
      geom_point(size = 5, aes(fill = team.name), shape = 21, stroke = 1) + 
      geom_text_repel(aes(label = player.name), size = 3, box.padding = 0.5, point.padding = 0.5) + 
      scale_color_manual(values = c("Argentina" = "#56B2B9", "Bolivia" = "#F0E442", 
                                    "Brazil" = "#009E73", "Chile" = "#D55E00", 
                                    "Colombia" = "#FDF100", "Costa Rica" = "#56B4E9", 
                                    "Ecuador" = "#CC79A7", "Jamaica" = "#000000", 
                                    "Mexico" = "#E69F00", "Panama" = "#0072B2", 
                                    "Paraguay" = "#56B4E9", "Peru" = "#F0E442", 
                                    "United States" = "#D55E00", "Uruguay" = "#009E73", 
                                    "Venezuela" = "#CC79A7", "Canada" = "#0072B2")) + 
      scale_fill_manual(values = c("Argentina" = "#0072B2", "Bolivia" = "#F0E442", 
                                   "Brazil" = "#009E73", "Chile" = "#D55E00", 
                                   "Colombia" = "#E69F00", "Costa Rica" = "#56B4E9", 
                                   "Ecuador" = "#CC79A7", "Jamaica" = "#000000", 
                                   "Mexico" = "#E69F00", "Panama" = "#0072B2", 
                                   "Paraguay" = "#56B4E9", "Peru" = "#F0E442", 
                                   "United States" = "#D55E00", "Uruguay" = "#009E73", 
                                   "Venezuela" = "#CC79A7", "Canada" = "#0072B2")) +
      scale_x_continuous(limits = c(0, 4), breaks = seq(0, 5, by = 1)) +  # Extend x-axis range to 5
      scale_y_continuous(breaks = seq(0, 1.4, by = 0.2), limits = c(0, 2)) +  # Set y-axis interval to 0.2 and limit to 2
      labs(title = "Copa America 2023-24",
           subtitle = "Chance creation (Assists & Key passes per 90')",
           x = "Key passes per 90 minutes",
           y = "Assists per 90 minutes", 
           color = "Team") + 
    
      theme_minimal(base_size = 10) +
      theme(legend.position = "right",
            plot.margin = unit(c(1, 1, 1, 2), "cm")) +  # Increase the right margin
      theme(panel.grid.major = element_line(color = "grey90"),
            panel.grid.minor = element_line(color = "grey95"),
            panel.background = element_rect(fill = "white", color = NA))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
