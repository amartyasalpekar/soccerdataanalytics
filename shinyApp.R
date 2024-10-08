library(tidyverse) 
library(StatsBombR) #1 
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(SBpitch)
library(ggthemes)
library(ggrepel)
library(ggsoccer)

Comp <- FreeCompetitions() %>% filter(competition_id==223 & season_id==282) #2 

Matches <- FreeMatches(Comp) #3 
StatsBombData <- free_allevents(MatchesDF = Matches, Parallel = T) #4 

StatsBombData = allclean(StatsBombData) #5

shots_goal = StatsBombData %>% group_by(team.name) %>%
  summarise(shots = sum(type.name == "Shot", na.rm = T), goals = sum(shot.outcome.name == "Goal", na.rm = T))


# Danger Passes: Passes that were played in the 15 second window before the shot was attempted.
# I am calculating the danger passes played by Copa America Winners - Argentina and heat map to understand which third of the field were Argentina most dangerous in.

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

##RADAR PLOT

#minutes played


player_minutes = get.minutesplayed(StatsBombData) 
player_minutes = player_minutes %>% group_by(player.id) %>% 
  summarise(minutes = sum(MinutesPlayed)) #3

player_minutes <- player_minutes%>%filter(minutes>180)

xg_by_player <- StatsBombData %>%
  filter(type.name == "Shot") %>%
  group_by(player.name, player.id) %>%
  summarise(total_xg = sum(shot.statsbomb_xg, na.rm = TRUE)) %>%
  arrange(desc(total_xg))

#passes_final_third_count -> already available
passes_final_third <- StatsBombData%>%filter(type.name == "Pass", pass.end_location.x > 80)%>%
  filter(!pass.outcome.name %in% c("Injury Clearance", "Unknown"))


passes_final_third <- passes_final_third %>%
  mutate(pass_success = ifelse(is.na(pass.outcome.name), "Successful", "Unsuccessful"))

passes_final_third_count <- passes_final_third %>%
  group_by(player.id, player.name) %>%
  summarise(
    total_passes = n(),
    successful_passes = sum(pass_success == "Successful"),
    unsuccessful_passes = sum(pass_success == "Unsuccessful")
  ) %>%
  ungroup()


passes_final_third_count <- passes_final_third_count %>%
  mutate(is_james = ifelse(player.name == "James David Rodríguez Rubio", TRUE, FALSE))


aerial_duels <- StatsBombData %>%
  filter(clearance.aerial_won == TRUE | shot.aerial_won == TRUE) %>%
  group_by(player.name, player.id) %>%
  summarise(total_aerial_duels_won = n()) %>%
  arrange(desc(total_aerial_duels_won))

ground_duels_won <- StatsBombData %>%
  filter(duel.outcome.name %in% c("Won", "Success", "Success in Play")) %>%
  group_by(player.id, player.name, team.name) %>%
  summarise(total_ground_duels_won = n(), .groups = 'drop') %>%
  arrange(desc(total_ground_duels_won))

goals_by_player <- StatsBombData %>%
  filter(shot.outcome.name == "Goal") %>%
  group_by(player.id, player.name, team.name) %>%
  summarise(total_goals = n(), .groups = 'drop') %>%
  arrange(desc(total_goals)) 

##key_passes and assists available
key_passes <- StatsBombData %>%
  filter(!is.na(pass.assisted_shot_id)) %>%
  group_by(player.id, player.name, team.name) %>%
  summarise(key_passes = n())

assists <- StatsBombData %>%
  filter(pass.goal_assist == TRUE) %>%
  group_by(player.id, player.name, team.name) %>%
  summarise(assists = n())

## successful dribbles
dribbles <- StatsBombData %>%
  filter(dribble.outcome.name == "Complete") %>%
  group_by(player.id, player.name, team.name) %>%
  summarise(dribbles = n())


final_stats <- player_minutes %>%
  # Join with xg_by_player to get player.name
  left_join(xg_by_player %>% select(player.id, player.name), by = "player.id") %>%
  left_join(xg_by_player, by = c("player.id", "player.name")) %>%
  left_join(aerial_duels, by = c("player.id", "player.name")) %>%
  left_join(ground_duels_won, by = c("player.id", "player.name")) %>%
  left_join(goals_by_player, by = c("player.id", "player.name")) %>%
  left_join(passes_final_third_count, by = c("player.id", "player.name")) %>%
  left_join(key_passes, by = c("player.id", "player.name")) %>%
  left_join(assists, by = c("player.id", "player.name")) %>%
  left_join(dribbles, by = c("player.id", "player.name")) %>%
  # Replace NA values with 0 for numerical columns
  replace_na(list(
    total_xg = 0,
    total_aerial_duels_won = 0,
    total_ground_duels_won = 0,
    total_goals = 0,
    key_passes = 0,
    assists = 0,
    dribbles = 0
  ))

final_stats <- final_stats %>%
  select(-team.name.x, -team.name.y, -team.name.x.x, -team.name.x.x, -team.name.y.y, -successful_passes, -unsuccessful_passes)

final_stats_per90 <- final_stats %>%
  # Ensure minutes is not zero to avoid division by zero
  mutate(minutes = if_else(minutes == 0, 1, minutes)) %>%
  mutate(
    xg_per90 = (total_xg / minutes) * 90,
    aerial_duels_per90 = (total_aerial_duels_won / minutes) * 90,
    ground_duels_per90 = (total_ground_duels_won / minutes) * 90,
    goals_per90 = (total_goals / minutes) * 90,
    key_passes_per90 = (key_passes / minutes) * 90,
    assists_per90 = (assists / minutes) * 90,
    dribbles_per90 = (dribbles/minutes) * 90
  )%>%
  select(
    player.id, player.name, minutes,
    xg_per90, aerial_duels_per90, ground_duels_per90, goals_per90, key_passes_per90, assists_per90, dribbles_per90
  )

player_names <- StatsBombData %>%
  select(player.id, player.name) %>%
  distinct(player.id, .keep_all = TRUE)


player_positions <- StatsBombData %>%
  select(player.id, player.name, position.id) %>%
  distinct()

attackers_ids <- c(17, 21, 22, 23, 24, 25)
midfielders_ids <- c(9, 10, 11, 12, 13, 14, 15, 16, 18)
defenders_ids <- c(2, 3, 4, 5, 6, 7, 8)

final_stats_with_positions <- final_stats_per90 %>%
  left_join(player_positions, by = "player.id")

attackers <- final_stats_with_positions %>%
  filter(position.id %in% attackers_ids) %>%
  distinct(player.id, .keep_all = TRUE)

midfielders <- final_stats_with_positions %>%
  filter(position.id %in% midfielders_ids) %>%
  distinct(player.id, .keep_all = TRUE)

defenders <- final_stats_with_positions %>%
  filter(position.id %in% defenders_ids) %>%
  distinct(player.id, .keep_all = TRUE)

attackers_percentiles <- attackers %>%
  mutate(
    xg_percentile = percent_rank(xg_per90),
    aerial_duels_percentile = percent_rank(aerial_duels_per90),
    ground_duels_percentile = percent_rank(ground_duels_per90),
    goals_percentile = percent_rank(goals_per90),
    key_passes_percentile = percent_rank(key_passes_per90),
    assists_percentile = percent_rank(assists_per90),
    dribbles_percentile = percent_rank(dribbles_per90)
  )

midfielders_percentiles <- midfielders %>%
  mutate(
    xg_percentile = percent_rank(xg_per90),
    aerial_duels_percentile = percent_rank(aerial_duels_per90),
    ground_duels_percentile = percent_rank(ground_duels_per90),
    goals_percentile = percent_rank(goals_per90),
    key_passes_percentile = percent_rank(key_passes_per90),
    assists_percentile = percent_rank(assists_per90), 
    dribbles_percentile = percent_rank(dribbles_per90)
  )

defenders_percentiles <- defenders %>%
  mutate(
    xg_percentile = percent_rank(xg_per90),
    aerial_duels_percentile = percent_rank(aerial_duels_per90),
    ground_duels_percentile = percent_rank(ground_duels_per90),
    goals_percentile = percent_rank(goals_per90),
    key_passes_percentile = percent_rank(key_passes_per90),
    assists_percentile = percent_rank(assists_per90),
    dribbles_percentile = percent_rank(dribbles_per90)
  )

attackers_radar_data <- attackers_percentiles %>%
  rename(player.name = player.name.x) %>%
  select(player.name, xg_percentile, aerial_duels_percentile, ground_duels_percentile, 
         goals_percentile, key_passes_percentile, assists_percentile, dribbles_percentile)

midfielders_radar_data <- midfielders_percentiles %>%
  rename(player.name = player.name.x) %>%
  select(player.name, xg_percentile, aerial_duels_percentile, ground_duels_percentile, 
         goals_percentile, key_passes_percentile, assists_percentile, dribbles_percentile)

defenders_radar_data <- defenders_percentiles %>%
  rename(player.name = player.name.x) %>%
  select(player.name, xg_percentile, aerial_duels_percentile, ground_duels_percentile, 
         goals_percentile, key_passes_percentile, assists_percentile, dribbles_percentile)

create_radar_plot <- function(data, selected_players) {
  # Filter the data for the selected players
  player_data <- data %>% filter(player.name %in% selected_players)
  
  # Initialize the plot
  radar_plot <- plot_ly(type = 'scatterpolar', fill = 'toself')
  
  # Add each player's data to the plot
  for (i in 1:nrow(player_data)) {
    radar_plot <- radar_plot %>%
      add_trace(
        r = as.numeric(player_data[i, -1]),  # The player's stats
        theta = colnames(data)[-1],  # The stat names
        name = player_data$player.name[i]
      )
  }
  
  # Set layout options
  radar_plot <- radar_plot %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = TRUE,
          range = c(0, 1)  # Percentiles are between 0 and 1
        )
      ),
      title = "Radar Plot for Selected Players"
    )
  
  return(radar_plot)
}



## James Rodriguez passes and assists - Golden Ball. 

james_pass <- StatsBombData%>%filter(type.name == "Pass" & is.na(pass.outcome.name), player.id == 5695)

## Key Passes vs Assists per 90 minutes 


minutes_data = get.minutesplayed(StatsBombData)

player_minutes <- minutes_data %>% group_by(player.id) %>% summarise(minutes = sum(MinutesPlayed))

player_stats <- left_join(assists, player_minutes)
player_stats <-left_join(player_stats, key_passes)

player_stats <- player_stats%>%mutate(nineties = minutes/90)

player_stats <- player_stats%>%mutate(assists_per90 = assists/nineties, key_passes_per90 = key_passes/nineties)
player_stats <- player_stats%>%filter(minutes >= 180)
##Final Third Passes 

top_passes_final_third_count <- passes_final_third_count%>%filter(total_passes > 83)


# Create the pitch and plot Lautaro's touches, colored by type



### Lautaro Martinez - Golden Boot 

lautaro_shots = StatsBombData%>%filter(type.name == "Shot", player.id ==11456)

lautaro_touches = StatsBombData%>%filter(player.id ==11456, type.name %in% c("Ball Receipt*", "Pass", "Shot", "Carry", "Dribble", "Foul Won", "Miscontrol", "Ball Recovery"))%>%
  filter(location.x >= 102 & location.x <= 120 & location.y>=18 & location.y<=62)

touch_colors <- c("Shot" = "red", "Ball Receipt*" = "blue", "Pass" = "green", "Carry" = "purple", "Dribble" = "yellow", "Foul Won" = "brown", "Miscontrol" = "black", "Ball Recovery" = "pink")

### Defensive Pressures Regions

heatmap <- StatsBombData%>%mutate(location.x = ifelse(location.x>120, 120, location.x), 
                                  location.y = ifelse(location.y>80, 80, location.y), 
                                  location.x = ifelse(location.x<0, 0, location.x), 
                                  location.y = ifelse(location.y<0, 0, location.y))


heatmap$xbin <- cut(heatmap$location.x, breaks = seq(from=0, to=120, by = 20),include.lowest=TRUE ) 
heatmap$ybin <- cut(heatmap$location.y, breaks = seq(from=0, to=80, by = 20),include.lowest=TRUE)

heatmap = heatmap%>% filter(type.name=="Pressure" | duel.type.name=="Tackle" | type.name=="Foul Committed" | type.name=="Interception" | type.name=="Block" ) %>%
  group_by(team.name) %>% mutate(total_DA = n()) %>% 
  group_by(team.name, xbin, ybin) %>% 
  summarise(total_DA = max(total_DA), bin_DA = n(), bin_pct = bin_DA/total_DA, location.x = median(location.x), location.y = median(location.y))%>% 
  group_by(xbin, ybin) %>% mutate(league_ave = mean(bin_pct)) %>% 
  group_by(team.name, xbin, ybin) %>% 
  mutate(diff_vs_ave = bin_pct - league_ave)

library(grid) 
defensiveactivitycolors <- c("#dc2429", "#dc2329", "#df272d", "#df3238", "#e14348", "#e44d51",
                                           "#e35256", "#e76266", "#e9777b", "#ec8589", "#ec898d", "#ef9195", 
                                           "#ef9ea1", "#f0a6a9", "#f2abae", "#f4b9bc", "#f8d1d2", "#f9e0e2",
                                           "#f7e1e3", "#f5e2e4", "#d4d5d8", "#d1d3d8", "#cdd2d6", "#c8cdd3", 
                                           "#c0c7cd", "#b9c0c8", "#b5bcc3", "#909ba5", "#8f9aa5", "#818c98", 
                                           "#798590", "#697785", "#526173", "#435367", "#3a4b60", "#2e4257", 
                                           "#1d3048", "#11263e", "#11273e", "#0d233a", "#020c16")



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
               plotlyOutput("shotsGoalsPlot"),
               plotlyOutput("ShotsGoalsper90")
             )
           )
  ),
  
  tabPanel("Copa America 2023-24 - James Rodriguez",
           mainPanel(
             plotOutput("jamesPassesPlot"),
             plotOutput("finalThirdPassesPlot"),
             plotOutput("keyPassesAssistsPlot"),
           )
  ),
  tabPanel("Copa America 2023-24 - Lautaro Martinez",
           mainPanel(
             plotOutput("lautaroShotMapPlot"),
             plotOutput("lautaroTouchMapPlot")
           )
  ),
  tabPanel("Scouting Reports",
           fluidRow(
             column(
               width = 12,
               selectInput("selected_players", "Select Attackers:", 
                           choices = unique(attackers_radar_data$player.name),
                           selected = unique(attackers_radar_data$player.name)[1],  # Default selection
                           multiple = TRUE)  # Allow multiple selections
             )
           ),
           fluidRow(
             column(
               width = 12,
               plotlyOutput("radarPlotAttackers")
             )
           ),
           fluidRow(
             column(
               width = 12,
               selectInput("selected_midfielders", "Select Midfielders:", 
                           choices = unique(midfielders_radar_data$player.name),
                           selected = unique(midfielders_radar_data$player.name)[1],  # Default selection
                           multiple = TRUE)  # Allow multiple selections
             )
           ),
           fluidRow(
             column(
               width = 12,
               plotlyOutput("radarPlotMidfielders")
             )
           ),
           fluidRow(
             column(
               width = 12,
               selectInput("selected_defenders", "Select Defenders:", 
                           choices = unique(defenders_radar_data$player.name),
                           selected = unique(defenders_radar_data$player.name)[1],  # Default selection
                           multiple = TRUE)  # Allow multiple selections
             )
           ),
           fluidRow(
             column(
               width = 12,
               plotlyOutput("radarPlotDefenders")
             )
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
    ggplot(top_passes_final_third_count, aes(x = reorder(player.name, -total_passes))) +
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
      scale_fill_manual(values = c("Argentina" = "#56B2B9", "Bolivia" = "#F0E442", 
                                   "Brazil" = "#009E73", "Chile" = "#D55E00", 
                                   "Colombia" = "#FDF100", "Costa Rica" = "#56B4E9", 
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
  
  output$radarPlotAttackers <- renderPlotly({
    create_radar_plot(attackers_radar_data, input$selected_players)
  })
  
  output$radarPlotMidfielders <- renderPlotly({
    create_radar_plot(midfielders_radar_data, input$selected_midfielders)
  })
  
  output$radarPlotDefenders <- renderPlotly({
    create_radar_plot(defenders_radar_data, input$selected_defenders)
  })
  
  output$lautaroTouchMapPlot <- renderPlot({
    create_Pitch() + 
      geom_point(data = lautaro_touches, 
                 aes(x = location.x, y = location.y, color = type.name), 
                 size = 3, shape = 16) +  # Adjust point size if needed
      scale_color_manual(values = touch_colors, name = "Touch Type") +  # Map touch types to colors
      labs(title = "Lautaro Martínez's Touches in Opponent's Box", 
           subtitle = "Copa America 2023-24") + 
      scale_y_reverse() +  # Reverse Y axis to match pitch orientation
      coord_fixed(ratio = 105/100) +  # Maintain the correct pitch dimensions
      theme_minimal() +  # Apply a minimal theme for a clean look
      theme(
        panel.grid = element_blank(),  # Remove grid lines
        axis.text = element_blank(),  # Remove axis text
        axis.title = element_blank(),  # Remove axis titles
        axis.ticks = element_blank()  # Remove axis ticks
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
