# get data from JSON
# save to RData
library(tidyjson)
library(tidyverse)
# library(jsonlite)

programs <- tidyjson::read_json(path = "Programs/json/complete.json", format = "json")
# programs <- tidyjson::read_json(path = "Programs/json/2011-12_TO_NOW.json", format = "json")

concerts <- programs %>%
  gather_keys %>% gather_array %>%
  spread_values(id = jstring("id"), 
                program_id = jstring("programID"),
                orchestra = jstring("orchestra"),
                season = jstring("season")) %>%
  enter_object("concerts") %>% gather_array %>%
  spread_values(event_type = jstring("eventType"),
                location = jstring("Location"),
                venue = jstring("Venue"),
                date = jstring("Date"),
                time = jstring("Time")) %>%
  select(-document.id, -key, -array.index)

works <- programs %>% 
  gather_keys %>% gather_array %>%
  spread_values(id = jstring("id"), 
                program_id = jstring("programID")) %>%
  enter_object("works") %>% gather_array %>% 
  spread_values(work_id = jstring("ID"),
                composer = jstring("composerName"),
                title = jstring("workTitle"),
                movement = jstring("movement"),
                conductor = jstring("conductorName")) %>%
  select(-document.id, -key, -array.index)  

performance <- left_join(concerts, works, by = "id") 
  
save(concerts, works, file = "concerts_works.RData")
save(performance, file = "all_programs.RData")
