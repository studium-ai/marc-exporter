library(tidyverse)

caa_sources = read_csv('2024-01-24_caa_sources_table.csv')

studium_places = readxl::read_excel('/Users/ghum-m-ae231206/Library/CloudStorage/OneDrive-SharedLibraries-KULeuven/Suchismita Ghosh - Internship_KUL DH/Studium_Places.xlsx')


caa_imprint_places = caa_264 %>% select(RecordID, `264_a`) %>% group_by(RecordID) %>% summarise(places_in_264 = paste0(unique(`264_a`), collapse = "; "))

caa_places_for_whg = caa_df %>% 
  select(RecordID, main_title, add_title, d1, m1, y1, d2, m2, y2, place_1_fm_id = place_1, place_2_fm_id = place_2) %>% 
  left_join(studium_places %>% 
              select(Place_ID_pk, name) %>% 
              mutate(Place_ID_pk = as.character(Place_ID_pk)), by = c('place_1_fm_id' = 'Place_ID_pk')) %>% 
  left_join(studium_places %>% 
              select(Place_ID_pk, name) %>% 
              mutate(Place_ID_pk = as.character(Place_ID_pk)), by = c('place_2_fm_id' = 'Place_ID_pk')) %>% 
  left_join(caa_imprint_places) %>% 
  left_join(caa_sources %>% mutate(RecordID = as.character(RecordID)) %>% 
              select(RecordID, fm_id))

lov_places_for_whg = lov_df %>% 
  select(uri, RecordID = mms, main_title = title, add_title, d1, m1, y1, d2, m2, y2, place_1_fm_id = place_fm_id1, place_2_fm_id = place_fm_id2)%>% 
  mutate(RecordID = as.character(RecordID)) %>% 
  left_join(studium_places %>% 
              select(Place_ID_pk, name) %>% 
              mutate(Place_ID_pk = as.character(Place_ID_pk)), by = c('place_1_fm_id' = 'Place_ID_pk')) %>% 
  left_join(studium_places %>% 
              select(Place_ID_pk, name) %>% 
              mutate(Place_ID_pk = as.character(Place_ID_pk)), by = c('place_2_fm_id' = 'Place_ID_pk'))%>% 
  left_join(caa_imprint_places) %>% 
  left_join(lov_sources %>% 
              select(uri, fm_id)) %>% select(-uri)


lov_sources = read_csv('2024-01-16_lov_sources_table.csv')
caa_sources = read_csv('2024-02-15_caa_sources_table.csv')

lov_places = lov_omeka_file %>% 
  select(`Item URI`, place = `Dublin Core:Coverage`,RecordID =  `Item Type Metadata:MMS ID`) %>% 
  separate(place, into = c('place1', 'place2', 'place3'), sep = "\\$\\$")


lov_places_for_whg %>% 
  rbind(caa_places_for_whg) %>% 
  left_join(caa_imprint_places, by = 'RecordID') %>% 
  mutate(places_in_264 = coalesce(places_in_264.x, places_in_264.y)) %>% 
  select(-places_in_264.x, -places_in_264.y) %>% View() 
  rename(place_name_in_fm_1 = name.x,place_name_in_fm_2 = name.y) %>% write_csv('caa_places_for_whg.csv')
