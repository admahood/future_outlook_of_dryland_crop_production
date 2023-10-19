# extremne stormes

library(tidyverse)
library(sf)
library(janitor)
storm_events <- 
  list.files("data/storm_events",
             pattern = "csv.gz",
             full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  clean_names()

aoi <- storm_events %>%
  filter(!is.na(damage_crops), damage_crops != "0.00K", #!is.na(BEGIN_LON), 
         state %in% c("COLORADO", "KANSAS", "OKLAHOMA", "TEXAS", "NEW MEXICO", 
                      "NEBRASKA", "SOUTH DAKOTA", "NORTH DAKOTA", "MONTANA",
                      "WYOMING", "MINNESOTA")) %>%
  mutate(cdn = str_sub(damage_crops, 1, nchar(damage_crops)-1)%>% as.numeric(),
         cdm = str_sub(damage_crops, nchar(damage_crops), nchar(damage_crops)),
         cdx = ifelse(cdm == "K", 1000, 1e6),
         crop_damage = cdn * cdx)

yearly_sums <-
  aoi %>%
  filter(event_type %in% c("Drought", "Hail", "Flood", "Thunderstorm Wind")) %>%
  group_by(event_type, year) %>%
  summarise(sum_crop_damage = sum(crop_damage, na.rm = TRUE),
            n_damaging_events = n()) %>%
  ungroup()

ggplot(yearly_sums, aes(x = year, y=log(sum_crop_damage))) +
  geom_point() +
  facet_wrap(~event_type, scales = "free")
ggplot(yearly_sums, aes(x = year, y=n_damaging_events)) +
  geom_point() +
  facet_wrap(~event_type, scales = "free")
aoi %>%
  mutate(date = ym(begin_yearmonth)) %>%
  filter(event_type %in% c("Drought", "Hail", "Flood", "Thunderstorm Wind")) %>%
  ggplot(aes(x=date, y= log(crop_damage))) +
  geom_point(alpha = 0.5) +
  facet_wrap(~event_type)

plot(spatial[0])

# cumulative crop damage by state ~ time

