### EDA for ATP data

# Load required packages 
library(tidyverse)
library(stringr)
library(lubridate)
library(gridExtra)
library(data.table)
library(ggplot2)

# Load data
matches <- read_csv("tennis_atp-master/atp_matches_2021.csv")
# Exclude Davis Cup matches
matches <- matches %>% filter(tourney_level!="D")
# Exclude games that had retirees or walkovers
#matches <- matches %>%  filter(!grepl("RET" | "W/O", score))
colnames(matches)
head(matches)

## Clean dataset

# Filling in null values in seed columns
matches$winner_seed[is.na(matches$winner_seed)] = "unseeded"
matches$loser_seed[is.na(matches$loser_seed)] = "unseeded"

# Split the date into month and year components
matches1 <- matches %>% mutate(month=lubridate::month(ymd(tourney_date), label=T, abbr=T))

### Plotting trends

## Surfaces used throughout the year
fig1 <- ggplot(data=matches1 %>% group_by(month, surface),
               aes(month,fill=surface)) +
  geom_bar(stat="count", position="dodge") + 
  labs(y="Number of Matches", x="Month", fill="Surface") + 
  theme_light() +
  theme(legend.position = c(0.9,0.84))

fig1

## Level of tournaments over year
fig2 <- ggplot(data=matches1 %>% group_by(month, tourney_level),
               aes(month,fill=tourney_level)) +
  geom_bar(stat="count", position="dodge") + 
  labs(y="Number of Matches", x="Month", fill="Tournament \n Level") + 
  theme_light() +
  theme(legend.position = c(0.88,0.78))

fig2

## Creating functions to plot individual player trends

# Function to find wins for individual player by surface (source: https://rpubs.com/patenish/atp)
wins_by_surface <- function(player_name){
  
  surface_wins_plot <- matches1 %>% filter(winner_name == player_name ) %>% ggplot() + 
    geom_bar(aes(x = surface, fill = surface)) +
    labs(x = "Month", y = "Match Wins", 
         title = str_c(str_to_title(player_name), "'s Match Wins by Surface (2021)", sep = "")) + 
    theme_light()
  
  grid.arrange(surface_wins_plot)
  
}

# Find wins by surface for the individual players
wins_by_surface("Rafael Nadal")
wins_by_surface("Novak Djokovic")
wins_by_surface("Roger Federer")

# Function to find the ranking of an individual players over the year
rankings_history_full <- function(player_name){
  
  matches2 <- matches1 %>% filter(winner_name == player_name | loser_name == player_name) %>%
    mutate(player_rank = ifelse(winner_name == player_name, as.integer(winner_rank),
                                as.integer(loser_rank)))
  
  rankings_plot <- matches2 %>% ggplot(aes(x = ymd(tourney_date), y = player_rank)) +
    geom_step(color = "blue") + 
    scale_y_reverse() +
    scale_x_date(date_labels = "%b %y") +
    labs(x = "Time", y = "Singles Ranking") + 
    theme_light()
  
  grid.arrange(rankings_plot)
  
}

# Find ranking for the individual players over 2021
rankings_history_full("Rafael Nadal")
rankings_history_full("Novak Djokovic")
rankings_history_full("Roger Federer")
# Not that interesting for highly ranked players, may be more interesting if more years are included
# Will look at Aslan Karatsev (voted ATP Awards Most Improved Player of the Year 2021)
rankings_history_full("Aslan Karatsev")

#####

### Creating plots from https://medium.com/@tobikasali/exploratory-data-analysis-with-r-f0b0a5163ecd

## Summarizing data for individual players
# Creating a players who won the match dataset
winners <- matches1 %>% 
  mutate(bp=l_bpFaced, bpWon=l_bpFaced-l_bpSaved/l_bpFaced,
         per_FSReWon=100*(l_1stIn-l_1stWon)/l_1stIn, 
         per_SSReWon=100*(l_svpt-l_1stIn-l_df-l_2ndWon)/(l_svpt-l_1stIn-l_df)) %>% 
  select(tourney_id:winner_age,score:w_bpFaced,winner_rank,
         winner_rank_points,month:per_SSReWon) %>%
  rename_all(~ stringr::str_replace(., regex("winner_", ignore_case = TRUE), "")) %>%
  rename_all(~ stringr::str_replace(., regex("w_", ignore_case = TRUE), "")) %>%
  add_column(result = 1) %>%
  rename(draw_size = drasize)
  
# Creating a players who lost the match dataset
losers <- matches1 %>% 
  mutate(bp=w_bpFaced, bpWon=w_bpFaced-w_bpSaved,
            per_FSReWon=100*(w_1stIn-w_1stWon)/w_1stIn, 
            per_SSReWon=100*(w_svpt-w_1stIn-w_df-w_2ndWon)/(w_svpt-w_1stIn-w_df)) %>%
  select(tourney_id:match_num,loser_id:minutes,l_ace:l_bpFaced,
         loser_rank:per_SSReWon) %>%
  rename_all(~ stringr::str_replace(., regex("loser_", ignore_case = TRUE), "")) %>%
  rename_all(~ stringr::str_replace(., regex("l_", ignore_case = TRUE), "")) %>%
  add_column(result = 0)

# Combining the winners and losers datasets to give a dataset of players matches
players <- rbind(winners,losers) %>% 
  rename(FSIn="1stIn", FSWon="1stWon", SSWon="2ndWon")
# Column names starting with numbers were rename to prevent issues later



# Calculating the number of games played, matches won and win ratio for each player
players_stat <- players %>% 
  group_by(name) %>% 
  summarise(games=n(),wins=sum(result),win_ratio=wins/games)

# Calculating the same for finals (and therefore finding tournament wins)
pstat_finals <- players %>% 
  group_by(name) %>% 
  filter(round=="F") %>%
  summarise(finals=n(),tourney_wins=sum(result),finals_win_ratio=tourney_wins/finals) 

# Calculating the number of games played, matches won and win ratio for each player by surface
pstat_by_surface <- players %>%
  group_by(name, surface) %>%
  summarise(games=n(),wins=sum(result),win_ratio=wins/games) %>%
  group_by(name) %>%
  mutate(total_wins=sum(wins)) %>%
  ungroup()

pstat_by_surface1 <- pstat_by_surface %>% 
  pivot_wider(names_from=surface,values_from=c(games:win_ratio),
              names_glue = "{surface}_{.value}",values_fill=0) 

# Calculating the number of games played, matches won and win ratio for each player by surface
pstat_by_level <- players %>% 
  group_by(name, tourney_level) %>%
  summarise(games=n(),wins=sum(result),win_ratio=wins/games) %>%
  group_by(name) %>%
  mutate(total_wins=sum(wins)) 

pstat_by_level1 <- pstat_by_level %>%
  pivot_wider(names_from=tourney_level,values_from=c(games:win_ratio),
              names_glue = "{tourney_level}_{.value}",values_fill=0) 

players_stat1 <- left_join(players_stat,pstat_finals,by="name")
players_stat2 <- merge(players_stat1, pstat_by_surface1)
players_stat3 <- merge(players_stat1, pstat_by_level1)



## Plotting data for individual players
# Plotting the number of wins for each of the players in the top 10%
wins_by_player <- ggplot(data=players_stat %>% 
                           filter(wins>quantile(players_stat$wins,probs=0.9)|name=="Rafael Nadal"),
                         aes(y=reorder(name, wins),x=wins, fill=win_ratio)) + 
  geom_col() +
  labs(y = "", x = "Match Wins", fill="Win Ratio") + 
  theme_light()

wins_by_player

# Plotting the number of tournament wins for individual players
tourney_wins <- ggplot(data=players_stat1 %>% filter(tourney_wins>0),
                         aes(y=reorder(name, tourney_wins),x=tourney_wins, fill=wins)) + 
  geom_col() +
  labs(y = "", x = "Tournament Wins", fill="Wins") + 
  theme_light()

tourney_wins

# Plotting wins by surface
top10pc <- players_stat %>% filter(wins>quantile(players_stat$wins,probs=0.9)|name=="Rafael Nadal")
wins_by_surface1 <- ggplot(data=pstat_by_surface %>% filter(name%in%top10pc$name),
                           aes(y=reorder(name, total_wins),x=wins, fill=surface)) + 
  geom_col() +
  labs(y = "", x = "Match Wins", fill="Surface")  + 
  theme_light()

wins_by_surface1

# Plotting the number of matches per surface to give some perspective
matches_per_surface <- ggplot(matches, aes(x=surface, fill=surface)) + 
  geom_bar(show.legend = FALSE) +
  theme_light() +
  labs(x = "Surface", y = "Number of Matches") 
matches_per_surface
# fig1 also show the time of the year the different surfaces are played

# Plotting wins by level
wins_by_level <- ggplot(data=pstat_by_level %>% filter(name%in%top10pc$name),
                           aes(y=reorder(name, total_wins),x=wins, fill=tourney_level)) + 
  geom_col() +
  labs(y = "", x = "Match Wins", fill="Tournament \n Level") + 
  theme_light()

wins_by_level

# Plotting the number of matches per level to give some perspective
matches_per_level <- ggplot(matches, aes(x=tourney_level, fill=tourney_level)) + 
  geom_bar(show.legend = FALSE) +
  theme_light() +
  labs(x = "Tournament Level", y = "Number of Matches") 
matches_per_level



## Comparing winners and losers of individual games
players1 <- copy(players)
players1$result[players1$result==1] = "Winner"
players1$result[players1$result==0] = "Loser"


# Break points faced
ggplot(players1, aes(x=bpFaced, fill=result)) + 
  geom_histogram(position="identity",alpha=0.5, binwidth=1) +
  theme_light() +
  labs(x="Break Points Faced", y="Count", fill="Result") +
  theme(legend.position = c(0.9,0.87))
# Box plot
ggplot(players1, aes(x=result, y=bpFaced, fill=factor(result))) + 
  geom_boxplot() +
  theme_light() +
  labs(x="Result", y="Break Points Faced", fill="Result")

# Break points saved
ggplot(players1, aes(x=bpSaved, fill=result)) + 
  geom_histogram(position="identity",alpha=0.5, binwidth=1) +
  theme_light() +
  labs(x="Break Points Saved", y="Count", fill="Result") +
  theme(legend.position = c(0.9,0.87))
# Box plot
ggplot(players1, aes(x=result, y=bpSaved, fill=factor(result))) + 
  geom_boxplot() +
  theme_light() +
  labs(x="Result", y="Break Points Saved", fill="Result")

# Percentage of break point saved
ggplot(players1 %>% mutate(per_bpSaved = 100*bpSaved/bpFaced), 
       aes(per_bpSaved, fill=factor(result))) + 
  geom_histogram(position="identity",alpha=0.5, bins=50) +
  theme_light() +
  labs(x="Percentage of Break Points Saved", y="Count", fill="Result")
# Box plot
ggplot(players1 %>% mutate(per_bpSaved = 100*bpSaved/bpFaced),
       aes(x=result, y=per_bpSaved, fill=factor(result))) + 
  geom_boxplot() +
  theme_light() +
  labs(x="Result", y="Percentage of Break Points Saved", fill="Result")


# Number of aces
ggplot(players1 %>% mutate(per_ace=100*ace/svpt), aes(per_ace, fill=factor(result))) + 
  geom_histogram(position="identity",alpha=0.5, binwidth=1) +
  theme_light() +
  labs(x="Aces", y="Count", fill="Result") +
  theme(legend.position = c(0.9,0.87))
# Box plot
ggplot(players1, aes(x=result,y=ace, fill=factor(result))) + 
  geom_boxplot() +
  theme_light() +
  labs(x="Aces", y="Count", fill="Result")

# Number of double faults
ggplot(players1 %>% mutate(per_df=100*df/svpt), aes(per_df, fill=factor(result))) + 
  geom_histogram(position="identity",alpha=0.5, binwidth=1) +
  theme_light() +
  labs(x="Double Faults", y="Count", fill="Result") +
  theme(legend.position = c(0.9,0.87))
#Box plot
ggplot(players1, aes(x=result,y=df, fill=factor(result))) + 
  geom_boxplot() +
  theme_light() +
  labs(x="Double Faults", y="Count", fill="Result")


# Number of first serves made
ggplot(players1, aes(FSIn, fill=factor(result))) + 
  geom_histogram(position="identity",alpha=0.5, bins=50) +
  theme_light() +
  labs(x="First Serves In", y="Count", fill="Result")
# Box plot
ggplot(players1, aes(x=result,y=FSIn, fill=factor(result))) + 
  geom_boxplot() +
  theme_light() +
  labs(x="First Serves In", y="Count", fill="Result")

# Number of first serve points won
ggplot(players1, aes(FSWon, fill=factor(result))) + 
  geom_histogram(position="identity",alpha=0.5, bins=50) +
  theme_light() +
  labs(x="Points Won on First Serve", y="Count", fill="Result") +
  theme(legend.position = c(0.9,0.87))

# Percentage of points won after making first serve
ggplot(players1 %>% mutate(p_FSWon=100*FSWon/FSIn), aes(p_FSWon, fill=factor(result))) + 
  geom_histogram(position="identity",alpha=0.5, bins=50) +
  theme_light() +
  labs(x="First Serve Winning Percentage", y="Count", fill="Result") +
  theme(legend.position = c(0.1,0.87))
# NOTE: Number of first serves points was very similar between winners and losers
# but the percentage of first serve points is quite different


# Number of second serve points won
ggplot(players1, aes(SSWon, fill=factor(result))) + 
  geom_histogram(position="identity",alpha=0.5, binwidth=1) +
  theme_light() +
  labs(x="Number of second serve points won", y="Count", fill="Result")

# Percentage of points won after making second serve
ggplot(players1 %>% mutate(p_SSWon=100*SSWon/(svpt-FSIn-df)), aes(p_SSWon, fill=factor(result))) + 
  geom_histogram(position="identity",alpha=0.5, bins=50) +
  theme_light() +
  labs(x="Second Serve Winning Percentage", y="Count", fill="Result") +
  theme(legend.position = c(0.9,0.87))


# Number of points per service games
ggplot(players1 %>% mutate(ppg=(FSWon+SSWon)/SvGms), aes(ppg, fill=factor(result))) + 
  geom_histogram(position="identity",alpha=0.5, binwidth=1) +
  theme_light() +
  labs(x="Points won per service game", y="Count", fill="Result")
# Box plot
ggplot(players1, aes(x=result, y=SvGms, fill=factor(result))) + 
  geom_boxplot() +
  theme_light() +
  labs(x="Result", y="Number of serve games", fill="Result")


# Seed of players
ggplot(matches %>% filter(round=="F"), aes(factor(winner_seed, level=c(1,2,3,4,5,6,7,8,21,26,"unseeded")))) + 
  geom_bar(fill=34,color="black") +
  theme_light() +
  labs(x="Seed of Tournament Winners", y="Count")

# Seed of tournament winner by tournament level
seed1 <- ggplot(matches %>% group_by(tourney_level) %>% filter(round=="F"), 
                aes(factor(winner_seed, level=c(1,2,3,4,5,6,7,8,21,26,"unseeded")), fill=tourney_level)) + 
  geom_bar(stat="count", position="dodge",color="black") +
  theme_light() +
  labs(x="Seed of Tournament Winners", y="Count")

seed1


## Return Statistics

# Break Points 
ggplot(players1, aes(bp, fill=factor(result))) + 
  geom_histogram(position="identity",alpha=0.5, binwidth=1) +
  theme_light() +
  labs(x="Break Points", y="Count", fill="Result") +
  theme(legend.position = c(0.9,0.87))
# Box plot
ggplot(players1, aes(x=result, y=bp, fill=factor(result))) + 
  geom_boxplot() +
  theme_light() +
  labs(x="Result", y="Break Points", fill="Result")

# Break point won
ggplot(players1, aes(bpWon, fill=factor(result))) + 
  geom_histogram(position="identity",alpha=0.5, binwidth=1) +
  theme_light() +
  labs(x="Break Points Won", y="Count", fill="Result") +
  theme(legend.position = c(0.9,0.87))
# Box plot
ggplot(players1, aes(x=result, y=bpWon, fill=factor(result))) + 
  geom_boxplot() +
  theme_light() +
  labs(x="Result", y="Break Points Won", fill="Result")

# Percentage of points won on opponents first serve
ggplot(players1, aes(per_FSReWon, fill=factor(result))) + 
  geom_histogram(position="identity",alpha=0.5, bins=50) +
  theme_light() +
  labs(x="First Serve Return Winning Percentage", y="Count", fill="Result") +
  theme(legend.position = c(0.9,0.87))
# Box plot
ggplot(players1, aes(x=result, y=per_FSReWon, fill=factor(result))) + 
  geom_boxplot() +
  theme_light() +
  labs(x="Result", y="First Serve Return Winning Percentage", fill="Result")

# Percentage of points won on opponents second serve
ggplot(players1, aes(per_SSReWon, fill=factor(result))) + 
  geom_histogram(position="identity",alpha=0.5, bins=50) +
  theme_light() +
  labs(x="Second Serve Return Winning Percentage", y="Count", fill="Result") +
  theme(legend.position = c(0.9,0.87))
# Box plot
ggplot(players1, aes(x=result, y=per_SSReWon, fill=factor(result))) + 
  geom_boxplot() +
  theme_light() +
  labs(x="Result", y="Second Serve Return Winning Percentage", fill="Result")




#####

### Further summarising individual player statistics 
# (allow to compare best players with the rest)
avg_stats <- players %>% 
  group_by(name) %>%
  summarise_at(vars(ace:bpFaced),mean,na.rm=TRUE)

# Adding column to help compare the top players with the rest
# (top players are determine by those in the upper quartile for number of wins and win ratio)
top_players <- players_stat %>%
  filter(wins>9 & win_ratio>0.5)

avg_stats <- avg_stats %>% 
  mutate(group = if_else(name%in%top_players$name,"top_players","other_players"))

## Plotting comparisons between top players and other players

# Percentage of break points saved
ggplot(avg_stats %>% mutate(per_bpSaved = 100*bpSaved/bpFaced), 
       aes(x=group, y=per_bpSaved)) + 
  geom_bar(stat = "summary", fun.y = "mean",fill=34,color="Black") +
  theme_light() +
  labs(x="", y="Percentage of Break Points Saved")

# Percentage of points won after making first serve
ggplot(avg_stats %>% mutate(per_FSWon=100*FSWon/FSIn), 
       aes(x=group, y=per_FSWon)) + 
  geom_bar(stat = "summary", fun.y = "mean", fill=34,color="Black") +
  theme_light() +
  labs(x="", y="Percentage of First Serve Point Won")

# Percentage of points won after making second serve
ggplot(avg_stats %>% mutate(per_SSWon=100*SSWon/(svpt-df-FSIn)), 
       aes(x=group, y=per_SSWon)) + 
  geom_bar(stat = "summary", fun.y = "mean", fill=34,color="Black") +
  theme_light() +
  labs(x="", y="Percentage of Second Serve Point Won")

# Percentage of Aces on service
ggplot(avg_stats %>% mutate(per_ace=100*ace/svpt), 
       aes(x=group, y=per_ace)) + 
  geom_bar(stat = "summary", fun.y = "mean", fill=34,color="Black") +
  theme_light() +
  labs(x="", y="Percentage of Aces on Service")

# Percentage of Double Faults on service
ggplot(avg_stats %>% mutate(per_df=100*df/svpt), 
       aes(x=group, y=per_df)) + 
  geom_bar(stat = "summary", fun.y = "mean", fill=34,color="Black") +
  theme_light() +
  labs(x="", y="Percentage of Double Faults on Service")

# Plotting wins by level
ggplot(data=pstat_by_level %>% 
         mutate(group = if_else(name%in%top_players$name,"top_players","other_players")) %>%
         group_by(group) %>%
         mutate(per_wins=prop.table(wins)),
                        aes(y=per_wins,x=group, fill=tourney_level)) + 
  geom_col() +
  scale_y_continuous(label = scales::percent) +
  labs(x = "", y = "Percentage of Wins")  + 
  theme_light()
