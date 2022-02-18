library(readxl)
library(dplyr)
library(here)

grepl(".xlsx", here::here("data", "Scats sent to OSU 6 July 2021_RESULTS_V2.xlsx"))
readxl::excel_sheets(here::here("data", "Scats sent to OSU 6 July 2021_RESULTS_V2.xlsx"))
read_excel(here::here("data", "Scats sent to OSU 6 July 2021_RESULTS_V2.xlsx"), "SampleData") %>%
  rename_all(function(x) gsub(" ", "_", x)) %>%
  rename_all(tolower) -> sample_data

read_excel(here::here("data", "Scats sent to OSU 6 July 2021_RESULTS_V2.xlsx"), "Illumina Results") %>%
  dplyr::select(-locus) %>%
  rename_all(function(x) gsub(" ", "_", x)) %>%
  rename_all(tolower) %>%
  # filter(sampleid==2467 ) %>%
  group_by(sampleid, location) %>%
  mutate(group_sum = sum(counts),
         group_mean = group_sum / length(unique(rep))) %>%
  group_by(sampleid, scientific_name, location) %>%
  summarise(group_sum = mean(group_sum),
            group_mean = mean(group_mean),
            sum = sum(counts),
            mean = sum/ length(unique(rep))) %>%
  mutate_if(is.numeric, round) %>%
  left_join(., sample_data) %>% View


read_excel(here::here("data", "POW_scat_locs_map_1_20_2022.xlsx")) %>%
  rename_all(function(x) gsub(" ", "_", x)) %>%
  rename_all(tolower) -> locs

glimpse(locs)

locs %>%
  tidyr::pivot_longer(ends_with("ct"), values_to = "count") %>%
  tidyr::pivot_longer(matches("prey[0-9]"), names_to = "prey") %>%
  mutate(name = gsub("ct", "", name),
         count = ifelse(name == prey, count, NA)) %>%
  tidyr::drop_na(count) %>%
  dplyr::select(-name) %>%
  group_by(bioyear)
