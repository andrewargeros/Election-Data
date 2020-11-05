
options(scipen = 999)
library(tidyverse)
library(magrittr)
library(janitor)

df = read_csv("C:/RScripts/Election-Data/Data/1976-2016-president.csv")
elec_votes = read_csv('https://query.data.world/s/fzzaosnoerbl3xhfbhworyghh2v6yc') %>% clean_names('snake')

data = df %>% 
  filter(!is.na(candidate)) %>% 
  mutate(party = case_when(party == 'democratic-farmer-labor' ~ 'democrat',
                           TRUE ~ party)) %>% 
  select(-c(version, notes)) %>% 
  group_by(year, state) %>% 
  mutate(vote_prop = candidatevotes/totalvotes,
         electoral_win = ifelse(vote_prop == max(vote_prop), "WINNER", NA)) %>% 
  left_join(., elec_votes, by = 'state')

data %$% sum(is.na(number_of_electoral_votes))

yearly_res = data %>% 
  mutate(votes_received = ifelse(electoral_win == "WINNER", number_of_electoral_votes, 0)) %>% 
  group_by(year, candidate) %>% 
  summarise(pop_votes = sum(totalvotes),
            elec_votes = sum(votes_received, na.rm = T)) %>% 
  filter(elec_votes > 0) %>% 
  mutate(winner = ifelse(elec_votes >= 270, 'Winner', NA))

proportional_voting = data %>% 
  mutate(ev_votes = round(vote_prop*number_of_electoral_votes)) %>% 
  group_by(year, candidate) %>% 
  summarise(elec_votes = sum(ev_votes, na.rm = T)) %>% 
  filter(elec_votes > 0) %>% 
  group_by(year) %>% 
  mutate(winner = ifelse(elec_votes == max(elec_votes), 'Winner', NA)) %>% 
  mutate(total_evotes = sum(elec_votes))
