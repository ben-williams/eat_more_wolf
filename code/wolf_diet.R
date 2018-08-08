# wolf scat samples
# ben.williams@alaska.gov
# load ----
library(tidyverse)
library(lubridate)
library(readxl)
library(PBSmapping)
library(mapproj)

theme_set(theme_bw())

# data ----
# map data
data("nepacLL")
nepacLL %>% select(group=PID, POS=POS,long=X,lat=Y) -> ak

# load sample data, cleanup and drop some columns
# data need to be in long format - so the spreadsheet as provided needs substantial reworking

read_xlsx('data/scat_diet_samples.xlsx') %>%                           # read data
  dplyr::select(-c(Collector, DateSample, Season, ScatAge, Location, Major,   # drop some columns
                   Minor, SamplesSentWhere, DateSent, Results, Results2, 
                   Comments, X__1, `Total number of prey species`)) %>% 
  filter(Predator=='wolf', `Include in analyses?` == 'yes') %>%               # drop some data
  rename(Area = `Analysis Unit`, Include = `Include in analyses?`) -> dat     # rename
  
dat %>% 
  dplyr::select(SampleNumber:Prey1Ct) %>% 
  rename(Prey = Prey1, count = Prey1Ct) %>% 
  bind_rows(select(dat, c(SampleNumber:Include, Prey = Prey2, count = Prey2Ct))) %>% 
  bind_rows(select(dat, c(SampleNumber:Include, Prey = Prey3, count = Prey3Ct))) %>% 
  bind_rows(select(dat, c(SampleNumber:Include, Prey = Prey4, count = Prey4Ct))) %>% 
  bind_rows(select(dat, c(SampleNumber:Include, Prey = Prey5, count = Prey5Ct))) %>% 
  bind_rows(select(dat, c(SampleNumber:Include, Prey = Prey6, count = Prey6Ct))) %>% 
  bind_rows(select(dat, c(SampleNumber:Include, Prey = Prey7, count = Prey7Ct))) %>% 
  bind_rows(select(dat, c(SampleNumber:Include, Prey = Prey8, count = Prey8Ct))) %>% 
  bind_rows(select(dat, c(SampleNumber:Include, Prey = Prey9, count = Prey9Ct))) %>% 
  filter(!is.na(count)) -> poop

# check data
glimpse(poop)
sapply(poop, unique)

# exploratory map ----
poop %>% 
  filter(Predator=='wolf', Latitude>0) %>% 
  ggplot(aes(Longitude, Latitude, color = Area)) + geom_point() +
  geom_polygon(data=ak, aes(long, lat, group=group), fill=8, color="black", alpha = .3) +
  coord_map(xlim = c(-137, -130), ylim = c(54.7, 59.7)) +
  theme(legend.position = c(.8, .75))

# by area - all years combined ----
poop %>% 
  group_by(Area) %>% 
  mutate(prey_detected = length(unique(Prey)),
         n = sum(length(unique(SampleNumber))),
         n_prey_samples = n()) %>% 
  group_by(Area, Prey) %>% 
  mutate(prey_occurance = length(Prey),
         o_f = prey_occurance / n,
         o_i = prey_occurance / n_prey_samples) %>% 
  dplyr::select(-SampleNumber, -BioYear, -Latitude, -Longitude, -Predator, -Include, -Unit, -count) %>% 
  group_by(Area, Prey) %>% 
  summarise_all(., mean) %>% 
  group_by(Area) %>% 
  do(area=(.)) %>% 
  select(area) %>% 
  purrr::map(identity) %>% 
  unlist(recursive = FALSE) -> df_a

# can call each area individually using df_a$area1...
df_a$area1

# by area and year ----
poop %>% 
  group_by(Area, BioYear) %>% 
  mutate(prey_detected = length(unique(Prey)),
         n = sum(length(unique(SampleNumber))),
         n_prey_samples = n()) %>% 
  group_by(Area, Prey, BioYear) %>% 
  mutate(prey_occurance = length(Prey),
         o_f = prey_occurance / n,
         o_i = prey_occurance / n_prey_samples) %>% 
  dplyr::select(-SampleNumber, -Latitude, -Longitude, -Predator, -Include, -Unit, -count) %>% 
  group_by(Area, Prey, BioYear) %>% 
 summarise_all(., mean) %>% 
  group_by(Area, BioYear) %>% 
  do(area=(.)) %>% 
  select(area) %>% 
  purrr::map(identity) %>% 
  unlist(recursive = FALSE) -> df_ay
 

  