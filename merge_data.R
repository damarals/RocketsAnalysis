library(dplyr)
library(readr)
library(fs)
library(purrr)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggthemr)
library(lubridate)
library(glue)

ggthemr("fresh")

paths <- dir_ls("./raw_data/", glob = "*.csv")
data <- map_dfr(paths, read_csv, .id = "path")
abb <- read_delim("abbreviations_NBA.txt", "\t", col_names = c("Abb", "Name"))

# Preprocessamento
data <- data %>%
  mutate(path = str_extract(path, "(?<=\\-)(.*?)(?=\\.)"),
         cl = str_remove(as.character(data$cl), ".{3}$"),
         cl = ms(cl),
         Minute = minute(cl), Second = second(cl),
         Minute = if_else(Minute == 12, 12 - Minute, 
                          if_else(Minute == 0 & Second == 0, 12, 11 - Minute)),
         Second = if_else(Second == 0, 0, 60 - Second),
         Minute = if_else(qrt != "1st", (as.numeric(str_remove(qrt, "st")) - 1) * 12 + Minute, Minute)) %>%
  separate(col = path, into = c("AwayName", "HomeName"), sep = 3) %>%
  rename(ID = id, Quarter = qrt, Time = cl, Description = de, HomeScore = hs, AwayScore = vs) %>%
  inner_join(abb, by = c("HomeName" = "Abb")) %>% rename(HomeNameAll = Name) %>% 
  inner_join(abb, by = c("AwayName" = "Abb")) %>% rename(AwayNameAll = Name) %>%
  select(ID, Quarter, Minute, Second, HomeName, HomeNameAll, HomeScore, AwayName, AwayNameAll, AwayScore, Description, locX, locY) 

data_gp <- data %>% 
  pivot_longer(cols = c(HomeName, AwayName), names_to = "Place", values_to = "Team") %>%
  filter(Team == "HOU") %>%
  mutate(Place = str_remove(Place, "Name"),
         Score = if_else(Place == "Home", HomeScore, AwayScore),
         RivalScore = if_else(Place == "Away", HomeScore, AwayScore)) %>%
  select(ID, Place, HomeNameAll, AwayNameAll, Quarter, Minute, Second, Score, RivalScore, Description, locX, locY) %>%
  distinct(ID, Place, Quarter, Score, .keep_all = T) %>% 
  filter(!(Description == "Start Period" & Quarter != "1st")) %>%
  group_by(ID, Minute) %>% 
  mutate(ScoreMinute = max(Score)) %>% 
  ungroup() %>% distinct(ID, Minute, .keep_all = T) %>%
  select(ID, Place, HomeNameAll, AwayNameAll, Minute, ScoreMinute, RivalScore) %>%
  rename(Score = ScoreMinute) 

data_gp %>% 
  right_join(data_gp %>% 
               distinct(ID, Place, HomeNameAll, AwayNameAll) %>% 
               mutate(Minute = list(0:48)) %>% 
               unnest(Minute)) %>%
  arrange(ID, Minute) %>%
  group_by(ID) %>%
  fill(Score) %>% fill(RivalScore) %>%
  mutate(Home = if_else(Place == "Home", 1, 0)) %>%
  select(ID, HomeNameAll, AwayNameAll, Home, Minute, RivalScore, Score) -> data_gp


data_gmm <- data %>% 
  pivot_longer(cols = c(HomeName, AwayName), names_to = "Place", values_to = "Team") %>%
  filter(Team == "HOU") %>%
  mutate(Place = str_remove(Place, "Name"),
         Score = if_else(Place == "Home", HomeScore, AwayScore)) %>%
  select(ID, Place, Quarter, Minute, Second, Score, Description, locX, locY) %>%
  distinct(ID, Place, Quarter, Score, .keep_all = T) %>% 
  filter(Description != "Start Period")

write_csv(data_gp, "data_gp.csv")
write_csv(data_gmm, "data_gmm.csv")
