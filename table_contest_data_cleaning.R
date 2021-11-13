

# Loading Libraries -------------------------------------------------------

library(shiny)
library(tidyverse)
library(janitor)
library(reactable)
library(reactablefmtr)



# Loading Data ------------------------------------------------------------
dat = read_csv("players_22.csv")
# list(dat %>% colnames())

dat$league_name[is.na(dat$league_name)] <- "NC"

dat$league_name %>% unique()

# dat %>% filter(league_name=="NC") %>% View()


# dat %>% dim()
# dat %>% View()
dat <- dat %>% select(
  short_name,
  player_positions,
  overall,
  potential,
  value_eur,
  wage_eur,
  age,
  club_name,
  league_name,
  league_level,
  club_position,
  nationality,
  preferred_foot,
  pace,
  shooting,
  passing,
  dribbling,
  defending,
  physic,
  goalkeeping_diving,
  goalkeeping_handling,
  goalkeeping_kicking,
  goalkeeping_positioning,
  goalkeeping_reflexes,
  goalkeeping_speed,
  player_face_url,
  club_logo_url
)
# dat %>% View()

dat$league_name %>% unique()

dat <- dat %>% filter(
  league_name %in%
    c(
      "French Ligue 1",
      "German 1. Bundesliga",
      "English Premier League",
      "Spain Primera Division" ,
      "Italian Serie A",
      "Indian Super League",
      "NC"
    )
)


# Using case_when for teams having "NA" and assigning with respect
# to teams and also positions of the players --------
# Atlético de Madrid
# Real Madrid CF
# AC Milan
# RC Celta de Vigo
# Real Betis Balompié
#
# U.C. Sampdoria
# TSG Hoffenheim
# Levante Unión Deportiva
# U.S. Sassuolo Calcio
# Torino F.C.
# Montpellier Hérault SC
# RCD Espanyol de Barcelona
# AFC Bournemouth

# dat %>% View()

dat <- dat %>% mutate(
  league_name1 = case_when(
    club_name %in% c(
      "Atlético de Madrid",
      "Real Madrid CF",
      "RC Celta de Vigo",
      "Real Betis Balompié",
      "Levante Unión Deportiva",
      "RCD Espanyol de Barcelona"
    ) ~ "Spain Primera Division",
    club_name %in% c("AC Milan", "U.C. Sampdoria", "U.C. Sampdoria") ~ "Italian Serie A",
    club_name == "TSG Hoffenheim" ~ "German 1. Bundesliga",
    club_name == "AFC Bournemouth" ~ "English Premier League",
    club_name == "Montpellier Hérault SC" ~ "French Ligue 1",
    TRUE ~ league_name
  )
)

# dat %>% View()
dat$league_name1 %>% unique()

dat <- dat %>% filter(league_name != "NC")

# dat$league_name1 %>% unique()
# 
# #position
# 
# dat$club_position %>% unique() %>% list()

dat <- dat %>% mutate(
  player_position = case_when(
    club_position == "GK" ~ "GoalKeeper",
    club_position %in% c("LWB", "RWB", "CB", "RB", "LB", "RCB", "LCB") ~ "Defender",
    club_position %in% c("RW", "LW", "ST", "RS", "LS", "CF", "RF", "LF") ~
      "Forward",
    club_position %in% c(
      "RCM",
      "RDM",
      "CAM",
      "LCM",
      "LDM",
      "CDM",
      "RAM",
      "LAM",
      "LM",
      "RM",
      "CM")~"MidFielder",
  club_position %in% c("RES","SUB")~"Subs / Reserves"
))


# dat$player_position %>% unique()


# dat %>% View()

#Formatting nos to currncy
val<- function(x) {
  dplyr::case_when(
    x < 1e3 ~ as.character(x),
    x < 1e6 ~ paste0(as.character(x/1e3), "K"),
    x < 1e9 ~ paste0(as.character(x/1e6), "M"),
    TRUE ~ "To be implemented..."
  )
}

# dat %>% View()

dat <- dat %>% mutate(
  Value=val(value_eur),
  Wage=val(wage_eur)
)
# 
# dat %>% View()





