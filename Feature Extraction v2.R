# Feature extraction
# Load required packages 
library(tidyverse)
library(stringr)
library(lubridate)
library(gridExtra)
library(data.table)
library(ggplot2)
#library(xlsx)
library(readxl)

# Load data
matches21 <- read_csv("tennis_atp-master/atp_matches_2021.csv")
matches20 <- read_csv("tennis_atp-master/atp_matches_2020.csv")
matches19 <- read_csv("tennis_atp-master/atp_matches_2019.csv")
matches18 <- read_csv("tennis_atp-master/atp_matches_2018.csv")
matches17 <- read_csv("tennis_atp-master/atp_matches_2017.csv")
matches16 <- read_csv("tennis_atp-master/atp_matches_2016.csv")
odds <- read_xlsx("ATP match odds 2021.xlsx")

matches <- rbind(matches16,matches17,matches18,matches19,matches20,matches21) %>%
  arrange(tourney_date, tourney_name, match_num)

dim(matches) # 15717


# Exclude Davis Cup matches
matches <- matches %>% filter(tourney_level!="D")
dim(matches) # 14567 (1150 excluded)
# Exclude games that had retirees or walkovers
matches <- matches %>% filter(!grepl("RET|W/O", score))
dim(matches) # 14099 (468 excluded)
colnames(matches)
head(matches)

# Assign odds to match df
matches <- matches %>% 
  add_column(winner_odds=NA,loser_odds=NA)

# Split the date into month and year components
matches <- matches %>% mutate(month=lubridate::month(ymd(tourney_date), label=F),
                              year = year(ymd(tourney_date)))


# First rename some of the tournament names in odds df to match matches df
odds["Tournament"][odds["Tournament"]=="Miami Open"] <- "Miami Masters"
odds["Tournament"][odds["Tournament"]=="Mutua Madrid Open"] <- "Madrid Masters"
odds["Tournament"][odds["Tournament"]=="Internazionali BNL d'Italia"] <- "Rome Masters"
odds["Tournament"][odds["Tournament"]=="Belgrade Open"] <- "Belgrade 2"
odds["Tournament"][odds["Tournament"]=="French Open"] <- "Roland Garros"
odds["Tournament"][odds["Tournament"]=="Queen's Club Championships"] <- "Queen's Club"
odds["Tournament"][odds["Tournament"]=="Canadian Open"] <- "Canada Masters"
odds["Tournament"][odds["Tournament"]=="US Open"] <- "Us Open"
odds["Tournament"][odds["Tournament"]=="BNP Paribas Open"] <- "Indian Wells Masters"
odds["Tournament"][odds["Tournament"]=="Western & Southern Financial Group Masters"] <- "Cincinnati Masters"
odds["Tournament"][odds["Tournament"]=="BNP Paribas Masters"] <- "Paris Masters"
odds["Tournament"][odds["Tournament"]=="Masters Cup"] <- "Tour Finals"

# Either location or tournament odds variable matches tourney_name matches variable
# Collect the odds variables that match in one column
odds <- odds %>%
  add_column(tourney_name=ifelse((odds$Tournament %in% matches$tourney_name), odds$Tournament, odds$Location))

# Test how many names align
test <- matches %>%
  filter(year==2021, tourney_name %in% odds$tourney_name) 
nrow(test) # 2409
test1 <- matches %>%
  filter(year==2021)
nrow(test1) # 2517
# ~95% of 2021 data should have odds


# Finding out when 2021 data starts
nrow(matches[matches$year<2021,])
# 11582

# Need to make sure element used in the condition are no null
sum(is.na(matches$tourney_name[matches$year==2021])) # 0 NA's
sum(is.na(odds$tourney_name)) # 0 NA's
sum(is.na(matches$winner_rank[matches$year==2021])) # 0 NA's
sum(is.na(matches$loser_rank[matches$year==2021])) # 6 NA's
sum(is.na(odds$WRank)) # 0 NA's
sum(is.na(odds$LRank)) # 6 NA's
# Therefore have to include condition to avoid NA in loser rank variables being included in main condition statement

test2 <- matches[matches$year==2021,]
test2 <- test2[is.na(matches$loser_rank[matches$year==2021]),]
test2.1 <- odds[is.na(odds$LRank),]
# These nulls of are for the same matches in both df's
  

# Matches are identified using tournament name, winner surname and loser surname
for (i in c(11583:nrow(matches))){
  for (j in c(1:nrow(odds))){
    # If statement to exclude null loser rank terms
    if (!is.na(matches$loser_rank[i]) & !is.na(odds$LRank[j])){
      # Identify matches using tournament name, winner rank and loser rank
      if (matches$tourney_name[i] == odds$tourney_name[j] & 
          matches$winner_rank[i] == odds$WRank[j] & 
          matches$loser_rank[i] == odds$LRank[j]){
        matches$winner_odds[i] <- odds$AvgW[j]
        matches$loser_odds[i] <- odds$AvgL[j]
      }
    } else { 
      # For null values of loser rank matches can be identified by tournament name alone
      if (matches$tourney_name[i] == odds$tourney_name[j]){
        matches$winner_odds[i] <- odds$AvgW[j]
        matches$loser_odds[i] <- odds$AvgL[j]
      }   
    }
  }
}

# Check number of values assigned for winner and loser odds
sum(!is.na(matches$winner_odds)) # 2352
sum(!is.na(matches$loser_odds)) # 2352
# Around 165 matches were not assigned odds data as rankings were not consistent across the two dfs


# Filling in null values in seed columns
#matches$winner_seed[is.na(matches$winner_seed)] = "unseeded"
#matches$loser_seed[is.na(matches$loser_seed)] = "unseeded"
# Fill with arbitrary number thats is significantly higher than the lowest seed (32)
matches$winner_seed[is.na(matches$winner_seed)] = 50 # there may be a better choice than 50
matches$loser_seed[is.na(matches$loser_seed)] = 50


# Creating a match id as this will be relevant later for identifying matches
matches <- matches %>% mutate(match_id = paste(tourney_id, match_num, sep = '-'))


# Calculate percentage stats and drop unnecessary features
matches <- matches %>% 
  mutate(w_per1stWon = w_1stWon/w_1stIn, l_per1stWon = l_1stWon/l_1stIn, # 1st serve win %
         w_per1stReWon = 1-l_per1stWon, l_per1stReWon = 1-w_per1stWon,  # 1st serve return win %
         w_per2ndWon = w_2ndWon/(w_svpt-w_1stIn-w_df), l_per2ndWon = l_2ndWon/(l_svpt-l_1stIn-l_df),# 2nd serve win %
         w_per2ndReWon = 1-l_per2ndWon, l_per2ndReWon = 1-w_per2ndWon, # 2nd serve return win %
         w_bpWon = l_bpFaced-l_bpSaved, l_bpWon = w_bpFaced-w_bpSaved, # break points won
         w_per_ace = w_ace/w_svpt, l_per_ace = l_ace/l_svpt, # Aces per service point
         w_per_df = w_df/w_svpt, l_per_df = l_df/l_svpt) %>% # Double faults per service point
  select(tourney_id:minutes,w_bpSaved,w_bpFaced,l_bpSaved:l_per_df) # Drop features used to calculate percentage stats


# Create players dataframe to help calculate historical averages
# Creating a dataset for players who won the match 
winners <- matches %>% 
  select(-starts_with("l_"),-starts_with("loser")) %>%
  rename_all(~ stringr::str_replace(., regex("winner_", ignore_case = TRUE), "")) %>%
  rename_all(~ stringr::str_replace(., regex("w_", ignore_case = TRUE), "")) %>%
  add_column(win = 1) %>%
  rename(draw_size = drasize)

# Creating a dataset for players who lost the match 
losers <- matches %>% 
  select(-starts_with("w_"),-starts_with("winner")) %>%
  rename_all(~ stringr::str_replace(., regex("loser_", ignore_case = TRUE), "")) %>%
  rename_all(~ stringr::str_replace(., regex("l_", ignore_case = TRUE), "")) %>%
  add_column(win = 0)

# Combining the winners and losers datasets to give a dataset of players 
players <- rbind(winners,losers) %>% arrange(tourney_date, tourney_name, match_num)
# This was sorted to match the order of the matches dataset which will be important later

# Calculate players' historical averages for match stats as well as number of wins
hist_avg <- players %>%
  group_by(name) %>%
  mutate(per1stWon = ifelse(!is.na(per1stWon), lag(cummean(na.omit(per1stWon))), NA), 
         per1stReWon = ifelse(!is.na(per1stReWon), lag(cummean(na.omit(per1stReWon))), NA),  
         per2ndWon = ifelse(!is.na(per2ndWon), lag(cummean(na.omit(per2ndWon))), NA),    
         per2ndReWon = ifelse(!is.na(per2ndReWon), lag(cummean(na.omit(per2ndReWon))), NA), 
         bpFaced = ifelse(!is.na(bpFaced), lag(cummean(na.omit(bpFaced))), NA),
         bpSaved = ifelse(!is.na(bpSaved), lag(cummean(na.omit(bpSaved))), NA), 
         bpWon = ifelse(!is.na(bpWon), lag(cummean(na.omit(bpWon))), NA), 
         per_ace = ifelse(!is.na(per_ace), lag(cummean(na.omit(per_ace))), NA),  
         per_df = ifelse(!is.na(per_df), lag(cummean(na.omit(per_df))), NA),
         # lag(cummean()) calculates the mean value of all previous games 
         .keep="unused") %>%
  mutate(wins = lag(cumsum(win)))

# Check number of null values
sum(!is.na(hist_avg$per_df))

# Replace match statistics in matches df with historical averages
for (i in c(1:nrow(matches))) {
  matches[i,"w_per1stWon"] <- hist_avg[2*i-1,"per1stWon"]
  matches[i,"w_per1stReWon"] <- hist_avg[2*i-1,"per1stReWon"]
  matches[i,"w_per2ndWon"] <- hist_avg[2*i-1,"per2ndWon"]
  matches[i,"w_per2ndReWon"] <- hist_avg[2*i-1,"per2ndReWon"]
  matches[i,"w_bpFaced"] <- hist_avg[2*i-1,"bpFaced"]
  matches[i,"w_bpSaved"] <- hist_avg[2*i-1,"bpSaved"]
  matches[i,"w_bpWon"] <- hist_avg[2*i-1,"bpWon"]
  matches[i,"w_per_ace"] <- hist_avg[2*i-1,"per_ace"]
  matches[i,"w_per_df"] <- hist_avg[2*i-1,"per_df"]
  matches[i,"w_wins"] <- hist_avg[2*i-1,"wins"]
  matches[i,"l_per1stWon"] <- hist_avg[2*i,"per1stWon"]
  matches[i,"l_per1stReWon"] <- hist_avg[2*i,"per1stReWon"]
  matches[i,"l_per2ndWon"] <- hist_avg[2*i,"per2ndWon"]
  matches[i,"l_per2ndReWon"] <- hist_avg[2*i,"per2ndReWon"]
  matches[i,"l_bpFaced"] <- hist_avg[2*i,"bpFaced"]
  matches[i,"l_bpSaved"] <- hist_avg[2*i,"bpSaved"]
  matches[i,"l_bpWon"] <- hist_avg[2*i,"bpWon"]
  matches[i,"l_per_ace"] <- hist_avg[2*i,"per_ace"]
  matches[i,"l_per_df"] <- hist_avg[2*i,"per_df"]
  matches[i,"l_wins"] <- hist_avg[2*i,"wins"]
}     

# Assigning players player1/ player2 label
# PLayer 1 is winner
matches1 <- matches %>% 
  rename_all(~ stringr::str_replace(., regex("winner_", ignore_case = TRUE), "p1_")) %>%
  rename_all(~ stringr::str_replace(., regex("w_", ignore_case = TRUE), "p1_")) %>%
  rename_all(~ stringr::str_replace(., regex("loser_", ignore_case = TRUE), "p2_")) %>%
  rename_all(~ stringr::str_replace(., regex("l_", ignore_case = TRUE), "p2_")) %>%
  add_column(result = 1) %>%
  rename(draw_size = drap1_size)
# Player 2 is winner
matches0 <- matches %>% 
  rename_all(~ stringr::str_replace(., regex("winner_", ignore_case = TRUE), "p2_")) %>%
  rename_all(~ stringr::str_replace(., regex("w_", ignore_case = TRUE), "p2_")) %>%
  rename_all(~ stringr::str_replace(., regex("loser_", ignore_case = TRUE), "p1_")) %>%
  rename_all(~ stringr::str_replace(., regex("l_", ignore_case = TRUE), "p1_")) %>%
  add_column(result = 0) %>%
  rename(draw_size = drap2_size)
# This will allow for half of the rows of matches1 to be randomly deleted and replace by the equivalent rows in matches0
# Creating a dataframe with random allocation of player1/player2 labels to winners and losers

# Randomly delete half of the rows of matches1 and concat the equalvent rows of matches0 to replace them
set.seed(42)   
data1 <- matches1[sample(1:nrow(matches1), nrow(matches1)/2), ] 
dim(data1)
data0 <- matches0 %>% filter(!match_id %in% data1$match_id)

# Reorder columns names so the dataframes align
data0 <- data0[,c(colnames(data1))]
# Combine the two dataframes, reordering by in chronological order
data <- rbind(data0, data1) %>% arrange(tourney_date, tourney_name, match_num)

write.csv(data,"ATP_match_prediction2.csv", row.names = FALSE)

