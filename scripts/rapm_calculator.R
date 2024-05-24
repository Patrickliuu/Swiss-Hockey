## ----setup, include=FALSE-------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----echo=FALSE-----------------------------------------------------
library(tidyverse)
library(glmnet)
library(data.table)

should_run_optional_chunk = TRUE

options(scipen=999, digits = 2) # Serves only for better representation


## -------------------------------------------------------------------
file_path <- "../data/input/pbp_events_L2_S2021_STRegular_fixed_coords.csv"

# Extract season info "L1_S20XX" using regular expression
season_info <- sub(".*/pbp_events_(L\\d+_S\\d+)_STRegular_fixed_coords.csv", "\\1", file_path)

data <- read.csv(file = file_path, fileEncoding = "UTF-8-BOM")
backup <- data

# data <- backup


## -------------------------------------------------------------------
event_order <- c("SHOT", "puckPossession" ,"interruption", "FACEOFF")
data <- data %>% arrange(Date, Game.ID, Time, match(Event, event_order))


## -------------------------------------------------------------------
condition <- data$Event == "SHOT" & data$Detail == "GOAL"
data$Score.State <- ifelse(condition, lag(data$Score.State), data$Score.State)
data$Home.Score <- ifelse(condition, lag(data$Home.Score), data$Home.Score)
data$Away.Score <- ifelse(condition, lag(data$Away.Score), data$Away.Score)


## -------------------------------------------------------------------
data$time_since_last_event <- c(NA, diff(data$Seconds))
data$time_since_last_event[data$Seconds == 0] <- 0

data$time_since_last_event <- ifelse(is.na(data$time_since_last_event), 0, data$time_since_last_event)

if(data$Seconds[1] != 0) {
  data$time_since_last_event[1] <- data$Seconds[1]
}


## -------------------------------------------------------------------
even_strength <- c("3-3", "4-4", "5-5") # Variable skaters | Strength.State
power_play <- c("5-4", "4-5", "5-3", "3-5", "4-3", "3-4") # Variable skaters 
home_power_play <- c("5-4", "5-3", "4-3") # Variable skaters
away_power_play <- c("4-5", "3-5", "3-4") # Variable skaters
stoppages <- c("interruption", "penalty") # Eventtypes


## -------------------------------------------------------------------
# Add back to back
teams <- data %>%
  group_by(Date) %>%
  reframe(
    Yesterday_Home_Team = unique(Home.Team),
    Yesterday_Away_Team = unique(Away.Team)
  )

teams <- teams %>%
  group_by(Date) %>%
  summarise_all( ~ paste(., collapse = " - "))

teams$Date <- as.Date(teams$Date) + 1
data$Date <- as.Date(data$Date)

data <- left_join(data, teams)

# Check with mapply(grepl) if team played yesterday
data$Home_Home_Yesterday <- mapply(grepl,
                                   pattern = data$Home.Team,
                                   x = data$Yesterday_Home_Team)
data$Home_Away_Yesterday <- mapply(grepl,
                                   pattern = data$Home.Team,
                                   x = data$Yesterday_Away_Team)
data$Away_Home_Yesterday <- mapply(grepl,
                                   pattern = data$Away.Team,
                                   x = data$Yesterday_Home_Team)
data$Away_Away_Yesterday <- mapply(grepl,
                                   pattern = data$Away.Team,
                                   x = data$Yesterday_Away_Team)


# Create the new columns Home_BTB and Away_BTB
data$Home_BTB <- ifelse((data$Home_Home_Yesterday == 1 |
                           data$Home_Away_Yesterday == 1),
                        1,
                        0)

data$Away_BTB <- ifelse((data$Away_Home_Yesterday == 1 |
                           data$Away_Away_Yesterday == 1),
                        1,
                        0)


## -------------------------------------------------------------------
# Create "MISSING_PLAYER"
data$h1.num <- ifelse(is.na(data$h1.num), "MISSING_PLAYER", data$h1.num)
data$h2.num <- ifelse(is.na(data$h2.num), "MISSING_PLAYER", data$h2.num)
data$h3.num <- ifelse(is.na(data$h3.num), "MISSING_PLAYER", data$h3.num)
data$h4.num <- ifelse(is.na(data$h4.num), "MISSING_PLAYER", data$h4.num)
data$h5.num <- ifelse(is.na(data$h5.num), "MISSING_PLAYER", data$h5.num)
data$h6.num <- ifelse(is.na(data$h6.num), "MISSING_PLAYER", data$h6.num)
data$a1.num <- ifelse(is.na(data$a1.num), "MISSING_PLAYER", data$a1.num)
data$a2.num <- ifelse(is.na(data$a2.num), "MISSING_PLAYER", data$a2.num)
data$a3.num <- ifelse(is.na(data$a3.num), "MISSING_PLAYER", data$a3.num)
data$a4.num <- ifelse(is.na(data$a4.num), "MISSING_PLAYER", data$a4.num)
data$a5.num <- ifelse(is.na(data$a5.num), "MISSING_PLAYER", data$a5.num)
data$a6.num <- ifelse(is.na(data$a6.num), "MISSING_PLAYER", data$a6.num)


## -------------------------------------------------------------------
# Add shift change 
data$shift_change <- ifelse(
  lag(data$h1.num) == data$h1.num &
    lag(data$h2.num) == data$h2.num &
    lag(data$h3.num) == data$h3.num &
    lag(data$h4.num) == data$h4.num &
    lag(data$h5.num) == data$h5.num &
    lag(data$h6.num) == data$h6.num &
    lag(data$a1.num) == data$a1.num &
    lag(data$a2.num) == data$a2.num &
    lag(data$a3.num) == data$a3.num &
    lag(data$a4.num) == data$a4.num &
    lag(data$a5.num) == data$a5.num &
    lag(data$a6.num) == data$a6.num &
    lag(data$Score.State) == data$Score.State &
    lag(data$Home.Goalie) == data$Home.Goalie &
    lag(data$Away.Goalie) == data$Away.Goalie &
    lag(data$Game.ID) == data$Game.ID,
  0,
  1
)
data$shift_change <-
  ifelse(is.na(data$shift_change), 0, data$shift_change)

data$shift_change_index <- cumsum(data$shift_change)


## -------------------------------------------------------------------
data$home_pp_expiry <- ifelse(data$Strength.State %in% even_strength &
                              lag(data$Strength.State) %in% home_power_play & 
                              data$shift_change == 1, 
                              1, 0)

data$home_pp_expiry <- ifelse((lag(data$Event) %in% stoppages & data$Time == lag(data$Time)) | 
                              (lag(lag(data$Event)) %in% stoppages & lag(lag(data$Time)) == data$Time) | 
                              (lag(lag(lag(data$Event))) %in% stoppages & lag(lag(lag(data$Time))) == data$Time),
                              0, data$home_pp_expiry)

data$home_pp_expiry[is.na(data$home_pp_expiry)] <- 0

data$away_pp_expiry <- ifelse(data$Strength.State %in% even_strength & 
                              lag(data$Strength.State) %in% away_power_play & 
                              data$shift_change == 1, 
                              1, 0)

data$away_pp_expiry <- ifelse((lag(data$Event) %in% stoppages & data$Time == lag(data$Time)) | 
                              (lag(lag(data$Event)) %in% stoppages & lag(lag(data$Time)) == data$Time) | 
                              (lag(lag(lag(data$Event))) %in% stoppages & lag(lag(lag(data$Time))) == data$Time),
                              0, data$away_pp_expiry)

data$away_pp_expiry[is.na(data$away_pp_expiry)] <- 0


## -------------------------------------------------------------------
# We define if the zone is defensive and it happend e block it counts as an offensive zone
data$event_zone <- ifelse(data$Zone=="DEFENSIVE" & data$Detail=="BLOCK", "OFFENSIVE", data$Zone)
data$home_zone <- data$Home.Zone

# Here we check if at an event the TEAM is in the OFFENSIVE area
# Looking back just to check the last four data point if the shift change happend before or at the FACEOFF
data$home_ozs <- ifelse((data$Event == "FACEOFF" & data$home_zone == "OFFENSIVE") & (
  (data$shift_change == 1) |
    (data$Seconds == lag(data$Seconds) & lag(data$shift_change == 1)) |
    (data$Seconds == lag(lag(data$Seconds)) & lag(lag((data$shift_change) == 1))) |
    (data$Seconds == lag(lag(lag(data$Seconds))) & lag(lag(lag(data$shift_change))) == 1) |
    (data$Seconds == lag(lag(lag(lag(data$Seconds)))) &
       lag(lag(lag(lag(data$shift_change)))) == 1)
  ),1,0)

data$away_ozs <- ifelse((data$Event=="FACEOFF" & data$home_zone=="DEFENSIVE") & (
  (data$shift_change==1)  |
    (data$Seconds == lag(data$Seconds) & lag(data$shift_change == 1)) |
    (data$Seconds == lag(lag(data$Seconds)) & lag(lag((data$shift_change) == 1))) |
    (data$Seconds == lag(lag(lag(data$Seconds))) & lag(lag(lag(data$shift_change))) == 1) |
    (data$Seconds == lag(lag(lag(lag(data$Seconds)))) &
       lag(lag(lag(lag(data$shift_change)))) == 1)
  ),1,0)

data$nzs <- ifelse((data$Event=="FACEOFF" & data$home_zone=="NEUTRAL") & (
  (data$shift_change==1) |
    (data$Seconds == lag(data$Seconds) & lag(data$shift_change == 1)) |
    (data$Seconds == lag(lag(data$Seconds)) & lag(lag((data$shift_change) == 1))) |
    (data$Seconds == lag(lag(lag(data$Seconds))) & lag(lag(lag(data$shift_change))) == 1) |
    (data$Seconds == lag(lag(lag(lag(data$Seconds)))) &
       lag(lag(lag(lag(data$shift_change)))) == 1)
  ),1,0)

data$home_xGF <- ifelse(data$ev.team==data$Home.Team, data$xG, 0)
data$away_xGF <- ifelse(data$ev.team!=data$Home.Team, data$xG, 0)
data$home_ozs[is.na(data$home_ozs)] <- 0
data$away_ozs[is.na(data$away_ozs)] <- 0
data$tied <- ifelse(data$Home.Score==data$Away.Score, 1, 0)
data$home_lead_1 <- ifelse(data$Home.Score-data$Away.Score==1, 1, 0)
data$home_lead_2 <- ifelse(data$Home.Score-data$Away.Score==2, 1, 0)
data$home_lead_3 <- ifelse(data$Home.Score-data$Away.Score>=3, 1, 0)
data$away_lead_1 <- ifelse(data$Home.Score-data$Away.Score==(-1), 1, 0)
data$away_lead_2 <- ifelse(data$Home.Score-data$Away.Score==(-2), 1, 0)
data$away_lead_3 <- ifelse(data$Home.Score-data$Away.Score<=(-3), 1, 0)
data$Five <- ifelse(data$Strength.State=="5-5", 1, 0)
data$Four <- ifelse(data$Strength.State=="4-4", 1, 0)
data$Three <- ifelse(data$Strength.State=="3-3", 1, 0)
data$home_xGF[is.na(data$home_xGF)] <- 0
data$away_xGF[is.na(data$away_xGF)] <- 0
data$period_1 <- ifelse(data$Period==1, 1, 0)
data$period_2 <- ifelse(data$Period==2, 1, 0)
data$period_3 <- ifelse(data$Period==3, 1, 0)
data$overtime <- ifelse(data$Period==4, 1, 0)
data$Home_BTB[is.na(data$Home_BTB)] <- 0
data$Away_BTB[is.na(data$Away_BTB)] <- 0


## -------------------------------------------------------------------
dataev <- data %>%
  filter(Strength.State %in% even_strength)


## -------------------------------------------------------------------
shifts_grouped_ev <- dataev %>%
  group_by(
    Game.ID,
    shift_change_index,
    Period,
    Score.State,
    h1.num,
    h2.num,
    h3.num,
    h4.num,
    h5.num,
    h6.num,
    a1.num,
    a2.num,
    a3.num,
    a4.num,
    a5.num,
    a6.num,
    Home.Goalie,
    Away.Goalie
  ) %>%
  reframe(
    shift_length = sum(time_since_last_event),
    homexGF = sum(home_xGF),
    awayxGF = sum(away_xGF),
    Home_OZS = max(home_ozs),
    Away_OZS = max(away_ozs),
    NZS = max(nzs),
    Home_Up_1 = max(home_lead_1),
    Home_Up_2 = max(home_lead_2),
    Home_Up_3 = max(home_lead_3),
    Away_Up_1 = max(away_lead_1),
    Away_Up_2 = max(away_lead_2),
    Away_Up_3 = max(away_lead_3),
    Tied = max(tied),
    State_5v5 = max(Five),
    State_4v4 = max(Four),
    State_3v3 = max(Three),
    Period_1 = max(period_1),
    Period_2 = max(period_2),
    Period_3 = max(period_3),
    Overtime = max(overtime),
    Home_BTB = max(Home_BTB),
    Away_BTB = max(Away_BTB),
    Home_PPx = max(home_pp_expiry),
    Away_PPx = max(away_pp_expiry)
  ) %>%
  filter(shift_length > 0)

saveRDS(shifts_grouped_ev, "../data/temp/shifts_grouped_ev.RData")


## ----optional_chunk, eval = should_run_optional_chunk---------------

update_shifts <- function(data) {
  data %>%
    mutate(
      shift_length = ifelse(
        shift_length < 1 & shift_length > 0 & !is.na(lead(shift_length)),
        shift_length + ifelse(!is.na(lead(shift_length)), lead(shift_length), 0),
        shift_length
      ),
      homexGF = ifelse(
        shift_length < 1 & shift_length > 0 & !is.na(lead(shift_length)),
        homexGF + ifelse(!is.na(lead(shift_length)), lead(homexGF), 0),
        homexGF
      ),
      awayxGF = ifelse(
        shift_length < 1 & shift_length > 0 & !is.na(lead(shift_length)),
        awayxGF + ifelse(!is.na(lead(shift_length)), lead(awayxGF), 0),
        awayxGF
      )
    ) %>%
    filter(
      !(shift_length < 1 & shift_length > 0 & is.na(lead(shift_length)))
    )
}

# If their are multiple short shifts generated between the events.
for (i in 1:3) {
  shifts_grouped_ev <- update_shifts(shifts_grouped_ev)
}


## -------------------------------------------------------------------
# Reshape the data to have one row per player per shift
toi_data <- shifts_grouped_ev %>%
  select(h1.num:h6.num,a1.num:a6.num,shift_length)

toi_data <- toi_data %>%
  gather(key = "Player_position", value = "Player", h1.num:h6.num, a1.num:a6.num) %>%
  select(Player, shift_length)

# Calculate TOI for each player
toi <- toi_data %>%
  group_by(Player) %>%
  summarise(TimeOnIce = sum(shift_length, na.rm = TRUE) / 60) %>% # from seconds -> minutes
  arrange(desc(TimeOnIce))


## -------------------------------------------------------------------
team_names <- unique(c(data$Home.Team, data$Away.Team))
data_table <- as.data.table(data)

# Create a table with 'Team', 'Name', and 'Pos'
home_table <- data_table[, {
  .(Team = Home.Team,
    Player = c(get(paste0("h1.num")), get(paste0("h2.num")), get(paste0("h3.num")), get(paste0("h4.num")), get(paste0("h5.num")), get(paste0("h6.num"))),
    Pos = c(get(paste0("h1.pos")), get(paste0("h2.pos")), get(paste0("h3.pos")), get(paste0("h4.pos")), get(paste0("h5.pos")), get(paste0("h6.pos")))
  )
}]

away_table <- data_table[, {
  .(Team = Away.Team,
    Player = c(get(paste0("a1.num")), get(paste0("a2.num")), get(paste0("a3.num")), get(paste0("a4.num")), get(paste0("a5.num")), get(paste0("a6.num"))),
    Pos = c(get(paste0("a1.pos")), get(paste0("a2.pos")), get(paste0("a3.pos")), get(paste0("a4.pos")), get(paste0("a5.pos")), get(paste0("a6.pos")))
  )
}]

playerTeam <- rbind(home_table, away_table)

# Remove goalies and na 
playerTeam <- playerTeam[!(is.na(Pos) | playerTeam$Pos=="G"), ]
playerTeam$Pos <- ifelse(playerTeam$Pos == "LW" | playerTeam$Pos == "RW" | playerTeam$Pos == "C", "W", playerTeam$Pos)

# Remove duplicates
playerTeam <- unique(playerTeam)

# Join playerTeam with time on ice
playerTeamWithToi <- merge(playerTeam, toi, by = "Player", all.x = TRUE)
write_excel_csv(playerTeamWithToi, paste0("../data/temp/players_", season_info, ".csv"))

# Some players had played offensiv and defensive. They are in both lists.
defenders <- playerTeamWithToi[Pos == "D"]
write_excel_csv(defenders, paste0("../data/temp/defenders_", season_info, ".csv"))

strikers <- playerTeamWithToi[Pos == "W"]
write_excel_csv(strikers, paste0("../data/temp/strikers_", season_info, ".csv"))


## -------------------------------------------------------------------
shifts_long <- shifts_grouped_ev %>%
  gather(key = "Player_position", value = "Player", h1.num:h6.num, a1.num:a6.num) %>%
  mutate(
    xG = ifelse(grepl("^h", Player_position), as.numeric(homexGF), as.numeric(awayxGF)),
    IsGoalie = (Player == Home.Goalie) | (Player == Away.Goalie)
  ) %>%
  filter(!IsGoalie, Player != "MISSING_PLAYER") # Herausfiltern der Torhüter und "MISSING_PLAYER"

xG_per_player <- shifts_long %>%
  group_by(Player) %>%
  summarise(TotalxG = sum(xG, na.rm = TRUE)) %>%
  arrange(desc(TotalxG))

view(xG_per_player)


## -------------------------------------------------------------------
actual_xG60 <- left_join(xG_per_player, toi, by = "Player")
actual_xG60 <- actual_xG60 %>%
  mutate(actual_xG_60 = (TotalxG / TimeOnIce) * 60)

view(actual_xG60)
write_excel_csv(actual_xG60, paste0("../data/temp/actual_xG60_per_Player_", season_info, ".csv"))


## -------------------------------------------------------------------
home_offense_all <- shifts_grouped_ev %>%
  rename(offense_1 = h1.num, offense_2 = h2.num, offense_3 = h3.num, offense_4 = h4.num, offense_5 = h5.num, offense_6 = h6.num,
         defense_1 = a1.num, defense_2 = a2.num, defense_3 = a3.num, defense_4 = a4.num, defense_5 = a5.num, defense_6 = a6.num,
         Off_Zonestart = Home_OZS, Def_Zonestart = Away_OZS, Neu_Zonestart = NZS, Up_1 = Home_Up_1, Up_2 = Home_Up_2, Up_3 = Home_Up_3,
         Down_1 = Away_Up_1, Down_2 = Away_Up_2, Down_3 = Away_Up_3, xGF = homexGF, BTB = Home_BTB, Opp_BTB = Away_BTB, PPx = Home_PPx, PKx = Away_PPx) %>%
  select(Game.ID, shift_change_index, Period, Score.State,
         offense_1, offense_2, offense_3, offense_4, offense_5, offense_6, 
         defense_1, defense_2, defense_3, defense_4, defense_5, defense_6, 
         xGF, shift_length, shift_change_index, Tied, State_5v5, State_4v4, State_3v3, 
         Up_1, Up_2, Up_3, Down_1, Down_2, Down_3, Off_Zonestart, Def_Zonestart, Neu_Zonestart,
         offense_goalie = Home.Goalie, defense_goalie = Away.Goalie, Period_1, Period_2, Period_3, Overtime, BTB, Opp_BTB, PPx, PKx) %>%
  mutate(xGF_60 = xGF*3600/shift_length, is_home = 1)

away_offense_all <- shifts_grouped_ev %>%
  rename(offense_1 = a1.num, offense_2 = a2.num, offense_3 = a3.num, offense_4 = a4.num, offense_5 = a5.num, offense_6 = a6.num,
         defense_1 = h1.num, defense_2 = h2.num, defense_3 = h3.num, defense_4 = h4.num, defense_5 = h5.num, defense_6 = h6.num,
         Off_Zonestart = Away_OZS, Def_Zonestart = Home_OZS, Neu_Zonestart = NZS, Up_1 = Away_Up_1, Up_2 = Away_Up_2, Up_3 = Away_Up_3,
         Down_1 = Home_Up_1, Down_2 = Home_Up_2, Down_3 = Home_Up_3, xGF = awayxGF, BTB = Away_BTB, Opp_BTB = Home_BTB, PPx = Away_PPx, PKx = Home_PPx) %>%
  select(Game.ID, shift_change_index, Period, Score.State,
         offense_1, offense_2, offense_3, offense_4, offense_5, offense_6, 
         defense_1, defense_2, defense_3, defense_4, defense_5, defense_6, 
         xGF, shift_length, shift_change_index, Tied, State_5v5, State_4v4, State_3v3, 
         Up_1, Up_2, Up_3, Down_1, Down_2, Down_3, Off_Zonestart, Def_Zonestart, Neu_Zonestart,
         offense_goalie = Away.Goalie, defense_goalie = Home.Goalie, Period_1, Period_2, Period_3, Overtime, BTB, Opp_BTB, PPx, PKx) %>%
  mutate(xGF_60 = xGF*3600/shift_length, is_home = 0)


## -------------------------------------------------------------------
home_offense_all$offense_1 <- ifelse(home_offense_all$offense_goalie==home_offense_all$offense_1, "GOALIE.GUY", home_offense_all$offense_1)
home_offense_all$offense_2 <- ifelse(home_offense_all$offense_goalie==home_offense_all$offense_2, "GOALIE.GUY", home_offense_all$offense_2)
home_offense_all$offense_3 <- ifelse(home_offense_all$offense_goalie==home_offense_all$offense_3, "GOALIE.GUY", home_offense_all$offense_3)
home_offense_all$offense_4 <- ifelse(home_offense_all$offense_goalie==home_offense_all$offense_4, "GOALIE.GUY", home_offense_all$offense_4)
home_offense_all$offense_5 <- ifelse(home_offense_all$offense_goalie==home_offense_all$offense_5, "GOALIE.GUY", home_offense_all$offense_5)
home_offense_all$offense_6 <- ifelse(home_offense_all$offense_goalie==home_offense_all$offense_6, "GOALIE.GUY", home_offense_all$offense_6)

home_offense_all$defense_1 <- ifelse(home_offense_all$defense_goalie==home_offense_all$defense_1, "GOALIE.GUY", home_offense_all$defense_1)
home_offense_all$defense_2 <- ifelse(home_offense_all$defense_goalie==home_offense_all$defense_2, "GOALIE.GUY", home_offense_all$defense_2)
home_offense_all$defense_3 <- ifelse(home_offense_all$defense_goalie==home_offense_all$defense_3, "GOALIE.GUY", home_offense_all$defense_3)
home_offense_all$defense_4 <- ifelse(home_offense_all$defense_goalie==home_offense_all$defense_4, "GOALIE.GUY", home_offense_all$defense_4)
home_offense_all$defense_5 <- ifelse(home_offense_all$defense_goalie==home_offense_all$defense_5, "GOALIE.GUY", home_offense_all$defense_5)
home_offense_all$defense_6 <- ifelse(home_offense_all$defense_goalie==home_offense_all$defense_6, "GOALIE.GUY", home_offense_all$defense_6)

away_offense_all$offense_1 <- ifelse(away_offense_all$offense_goalie==away_offense_all$offense_1, "GOALIE.GUY", away_offense_all$offense_1)
away_offense_all$offense_2 <- ifelse(away_offense_all$offense_goalie==away_offense_all$offense_2, "GOALIE.GUY", away_offense_all$offense_2)
away_offense_all$offense_3 <- ifelse(away_offense_all$offense_goalie==away_offense_all$offense_3, "GOALIE.GUY", away_offense_all$offense_3)
away_offense_all$offense_4 <- ifelse(away_offense_all$offense_goalie==away_offense_all$offense_4, "GOALIE.GUY", away_offense_all$offense_4)
away_offense_all$offense_5 <- ifelse(away_offense_all$offense_goalie==away_offense_all$offense_5, "GOALIE.GUY", away_offense_all$offense_5)
away_offense_all$offense_6 <- ifelse(away_offense_all$offense_goalie==away_offense_all$offense_6, "GOALIE.GUY", away_offense_all$offense_6)

away_offense_all$defense_1 <- ifelse(away_offense_all$defense_goalie==away_offense_all$defense_1, "GOALIE.GUY", away_offense_all$defense_1)
away_offense_all$defense_2 <- ifelse(away_offense_all$defense_goalie==away_offense_all$defense_2, "GOALIE.GUY", away_offense_all$defense_2)
away_offense_all$defense_3 <- ifelse(away_offense_all$defense_goalie==away_offense_all$defense_3, "GOALIE.GUY", away_offense_all$defense_3)
away_offense_all$defense_4 <- ifelse(away_offense_all$defense_goalie==away_offense_all$defense_4, "GOALIE.GUY", away_offense_all$defense_4)
away_offense_all$defense_5 <- ifelse(away_offense_all$defense_goalie==away_offense_all$defense_5, "GOALIE.GUY", away_offense_all$defense_5)
away_offense_all$defense_6 <- ifelse(away_offense_all$defense_goalie==away_offense_all$defense_6, "GOALIE.GUY", away_offense_all$defense_6)


## -------------------------------------------------------------------
combined_shifts <- full_join(home_offense_all, away_offense_all)


## -------------------------------------------------------------------
subsetted_shifts <- subset(combined_shifts, select = -c(Game.ID, shift_change_index,Period, Score.State, xGF, Tied, State_5v5, Period_1:Period_3, Overtime, offense_goalie, defense_goalie))


## -------------------------------------------------------------------
combined_shifts_dummies <- fastDummies::dummy_cols(subsetted_shifts)

memory.limit(5000000000000) # Was passiert hier?

combined_shifts_dummies = subset(combined_shifts_dummies, select = -c(offense_1:defense_6))


## -------------------------------------------------------------------
colnames(combined_shifts_dummies) = gsub("offense_1", "offense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("offense_2", "offense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("offense_3", "offense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("offense_4", "offense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("offense_5", "offense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("offense_6", "offense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("defense_1", "defense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("defense_2", "defense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("defense_3", "defense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("defense_4", "defense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("defense_5", "defense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("defense_6", "defense_", colnames(combined_shifts_dummies))


## -------------------------------------------------------------------
combined_shifts_dummies <- as.data.frame(lapply(split.default(combined_shifts_dummies, names(combined_shifts_dummies)), function(x) Reduce(`+`, x)))


## -------------------------------------------------------------------
combined_shifts_dummies <- combined_shifts_dummies %>% select(-contains("Goalie"))
combined_shifts_dummies <- combined_shifts_dummies %>% select(-contains("Missing"))
combined_shifts_dummies <- combined_shifts_dummies %>% select(-contains("NA"))


## -------------------------------------------------------------------
xGF60 <- as.numeric(c(combined_shifts_dummies$xGF_60))
shift_length <- as.numeric(c(combined_shifts_dummies$shift_length))
saveRDS(xGF60, file=paste0("../data/temp/xGF_60_", season_info, ".RData"))

subsetted_dummies = subset(combined_shifts_dummies, select = -c(shift_length, xGF_60))

RAPM_xGF <- as.matrix(subsetted_dummies)
RAPM_xGF[!is.finite(RAPM_xGF)] <- 0

saveRDS(RAPM_xGF, file=paste0("../data/temp/RAPM_xGF_", season_info, ".RData"))


## -------------------------------------------------------------------
Sparse_RAPM_xGF <- Matrix(RAPM_xGF, sparse = TRUE)

Cross_Validated_Results <- cv.glmnet(x=Sparse_RAPM_xGF, y=xGF60, weights=shift_length, alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE)


## -------------------------------------------------------------------
Run_RAPM <- glmnet(x=Sparse_RAPM_xGF, y=xGF60, weights=shift_length, lambda = Cross_Validated_Results[["lambda.min"]], alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE)
saveRDS(Run_RAPM, file=paste0("../data/temp/Run_RAPM_", season_info, ".RData"))


## -------------------------------------------------------------------
RAPM_coefficients <- as.data.frame(as.matrix(coef(Run_RAPM)))


## -------------------------------------------------------------------
Binded_Coefficients <- cbind(rownames(RAPM_coefficients), RAPM_coefficients) %>%
  rename(Player = `rownames(RAPM_coefficients)`, xGF_60 = s0)

write_excel_csv(Binded_Coefficients,paste0("../data/output/RapmCoefficients_", season_info, ".csv"))

offense_RAPM <- Binded_Coefficients %>%
  filter(grepl("offense", Binded_Coefficients$Player))

offense_RAPM$Player = str_replace_all(offense_RAPM$Player, "offense__", "")

defense_RAPM <- Binded_Coefficients %>%
  filter(grepl("defense", Binded_Coefficients$Player)) %>%
  rename(xGA_60 = xGF_60)

defense_RAPM$Player = str_replace_all(defense_RAPM$Player, "defense__", "")

joined_RAPM <- inner_join(offense_RAPM, defense_RAPM, by="Player")

joined_RAPM$xGPM_60 <- joined_RAPM$xGF_60 - joined_RAPM$xGA_60

joined_RAPM <- joined_RAPM %>%
  arrange(desc(xGPM_60))



## -------------------------------------------------------------------
# Sort for defender and striker
striker <- read_csv(paste0("../data/temp/strikers_", season_info, ".csv"))
defender <- read_csv(paste0("../data/temp/defenders_", season_info, ".csv"))

# Set all striker to missing player
rapm_striker <- merge(striker, joined_RAPM, by = "Player", all = FALSE)
rapm_striker <- rapm_striker[order(-rapm_striker$xGPM_60), ]

# Set all defender to missing player
rapm_defender <- merge(defender, joined_RAPM, by = "Player", all = FALSE)
rapm_defender <- rapm_defender[order(-rapm_defender$xGPM_60), ]

# All player in one table
rapm_joined_full <- merge(playerTeamWithToi, joined_RAPM, by = "Player", all = FALSE)
rapm_joined_full <- rapm_joined_full[order(-rapm_joined_full$xGPM_60), ]


## -------------------------------------------------------------------
# View and save rapm defender
View(rapm_defender)
write_excel_csv(rapm_defender, paste0("../data/output/RapmEvenStrengthDefender_", season_info, ".csv"))

# View and save rapm striker
View(rapm_striker)
write_excel_csv(rapm_striker, paste0("../data/output/RapmEvenStrengthStriker_", season_info, ".csv"))

#View and save rapm joined full
View(rapm_joined_full)
write_excel_csv(rapm_joined_full, paste0("../data/output/RapmEvenStrengthJoined_", season_info, ".csv"))


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Filter defenders with less than 500 minutes of ToI
filtered_defenders <- rapm_defender[rapm_defender$TimeOnIce < 500, ]

# Filter strikers with less than 500 minutes of ToI
filtered_strikers <- rapm_striker[rapm_striker$TimeOnIce < 500, ]
