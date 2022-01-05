library('tidyverse')
library(worldfootballR)
library(reshape2)

years <- list('2018', '2019', '2020', '2021')

vini_stats <- function(x){
  year <- understat_team_players_stats(paste("https://understat.com/team/Real_Madrid/", x, sep = "")) %>%
    select(player_name, games, goals, assists, xG, xA) %>%
    filter(player_name == "Vinícius Júnior") %>%
    mutate(year = x)
  
  return(year) 
}

vini_stats <-lapply(years, vini_stats)
vini_stats <- do.call(rbind, vini_stats)

vini_stats$year <- as.numeric(as.character(vini_stats$year))

vini_stats %>%
  mutate(GApergame = (goals + assists)/ games, xGA = (xG + xA)/ games) %>%
  select(GApergame, xGA, year) %>%
  melt(id = 'year') %>%
  ggplot(aes(x= year, y= value, color = variable)) + geom_line()


