
#####
#Pulling StatsBomb Free Data Into R

library(tidyverse)
library(StatsBombR)

Comps <- FreeCompetitions()

Comps = Comps %>%
  filter(competition_id=="2" & season_name=="2015/2016")

Matches <- FreeMatches(Comps)

StatsBombData <- free_allevents(MatchesDF = Matches, Parallel = T)

StatsBombData = allclean(StatsBombData)

#####
##Filtering to Team Shots and Goals

#Totals
shots_goals = StatsBombData %>%
  group_by(team.name) %>%
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE),
            goals = sum(shot.outcome.name=="Goal", na.rm = TRUE))
