next_ps_att_id = 155459
next_ps_id = 143303

theses_ps = theses_700 %>% 
  left_join(theses_dates) %>% 
  inner_join(source_ids, by = c('RecordID' = 'tag_001')) %>% 
  distinct(`700_a`, `700_d`) %>% 
  separate(`700_d`, into = c('f1', 'f2'), sep = '-') %>% 
  mutate(f1 = ifelse(str_detect(f1, "18th"), '1700', f1))

theses_ps_att = theses_700 %>% 
  left_join(theses_dates) %>% 
  inner_join(source_ids, by = c('RecordID' = 'tag_001')) %>% 
  select(name = `700_a`, role = `700_e`, d1, m1, y1, d2, m2, y2, source_id =fm_id, role = `700_e`,RecordID) %>% 
  mutate(event_type = 'publication') %>% 
  mutate(psatt_id = next_ps_att_id:((next_ps_att_id-1) + nrow(.))) %>% 
  left_join(theses_245, by = 'RecordID') %>% 
  select(RecordID,role, name, `245_c` )%>% 
  separate(`245_c`, into = c('p', 'd'), sep = '(?i)defendet|proponit')  %>% 
  mutate(toponym.p = ifelse( role == 'dissertant', str_extract(d, "\\b\\w+(?:-\\w+)?sis\\b"), NA)) %>% 
  mutate(toponym.p2 = ifelse(role == 'dissertant', str_extract(d, "\\b(?<=ex)[^.]*|\\b(?<=de la)([^.]|[^.,])*"), NA))%>% 
  separate(toponym.p2, into = c('toponym.p2', 'discard'), sep = 'Lovanii') %>% select(-discard) %>% 
  mutate(toponym.p3 = ifelse(role == 'dissertant', str_extract(d, "(?i)Namuranus|Hanno\\-Montanus|Hanno\\-montanus"), NA))  %>% 
  mutate(toponym.p = tolower(str_remove_all(toponym.p, "[[:punct:][:space:]]")))%>% 
  mutate(toponym.p2 = tolower(str_remove_all(toponym.p2, "[[:punct:][:space:]]")))%>% 
  mutate(toponym.p3 = tolower(str_remove_all(toponym.p3, "[[:punct:][:space:]]"))) %>% 
  mutate(toponym.p = tolower(trimws(toponym.p, which = 'both')))  %>% 
  mutate(toponym.p2 = tolower(trimws(toponym.p2, which = 'both')))%>% 
  mutate(toponym.p3 = tolower(trimws(toponym.p3, which = 'both')))


theses_ps_att_for_export = theses_ps_att %>% 
  #filter(role == 'dissertant') %>% 
  select(RecordID, name, role,psatt_id) %>% 
  separate(name, into = c('last', 'rest'), sep = ',') %>%
  mutate(rest = str_remove(rest, "\\."))%>% 
  mutate(last = trimws(last), rest = trimws(rest))  %>% 
  separate(rest, into = c(paste0('desc', 1:5)), sep = ' ') %>% 
  mutate(desc1 = trimws(desc1), desc2 = trimws(desc2), desc3= trimws(desc3), desc4 = trimws(desc4), desc5 = trimws(desc5))%>% 
  left_join(all_place_descriptors) %>% select(RecordID, role, psatt_id, desc1, desc2, desc3, desc4, desc5, last) %>% 
  pivot_longer(cols = c(starts_with('desc'), 'last')) %>% 
  filter(!is.na(value)) %>% 
  group_by(psatt_id) %>% 
  summarise(name = paste0(value, collapse = ' '), role = role[1], RecordID = RecordID[1]) %>% 
  left_join(theses_dates) %>% 
  left_join(theses_sources %>% select(RecordID = tag_001, source_fk = fm_id)) 

theses_descriptors_for_export = theses_ps_att %>% filter(role == 'dissertant') %>% 
  select(RecordID, name, role) %>% 
  separate(name, into = c('last', 'rest'), sep = ',') %>% 
  mutate(last = trimws(last), rest = trimws(rest)) %>%
  separate(rest, into = c(paste0('desc', 1:5)), sep = ' ') %>% 
  mutate(desc1 = trimws(desc1), desc2 = trimws(desc2), desc3= trimws(desc3), desc4 = trimws(desc4), desc5 = trimws(desc5))%>% 
  left_join(all_place_descriptors) %>% 
  select(RecordID, desc1, desc2, desc3, desc4, desc5, last) %>% 
  pivot_longer(cols = c(starts_with('desc'), 'last')) %>% 
  left_join(theses_links_mms %>%
              select(RecordID = external_id, source_fm_id = fm_id)) %>% 
  filter(!is.na(value)) %>% 
  left_join(theses_ps_att_for_export %>% select(RecordID, psatt_id)) %>% 
  mutate(type = ifelse(name == 'desc1', 'first',
                       ifelse(name == 'last', 'last', 'middle'))) %>% group_by(psatt_id) %>% 
  mutate(position = seq_along(psatt_id)) %>% select(-name)
  

place_desc_to_place = read_csv('place_descriptors_to_placeID.csv') %>% 
  mutate(descriptor = str_remove(descriptor, "^a|de")) %>% 
  mutate(descriptor = trimws(descriptor)) %>% 
  mutate(descriptor = tolower(str_remove_all(descriptor, "[[:punct:][:space:]]")))%>% 
  distinct(descriptor, place_id_fk) 

for_fuzzy_match = theses_ps_att %>% 
  dplyr::left_join(place_desc_to_place, by = c("toponym.p" = "descriptor")) %>% 
  dplyr::left_join(place_desc_to_place, by = c("toponym.p2" = "descriptor"))%>% 
  dplyr::left_join(place_desc_to_place, by = c("toponym.p3" = "descriptor"))%>% 
  mutate(place_id_fk = coalesce(place_id_fk.x, place_id_fk.y, place_id_fk))  %>% 
  filter(is.na(place_id_fk) & role == 'dissertant') %>% 
  mutate(place_id_to_join = coalesce(toponym.p2, toponym.p, toponym.p3))

library(fuzzyjoin)

for_fuzzy_match %>% filter(!is.na(place_id_to_join)) %>% select(RecordID, place_id_to_join) %>% 
  fuzzyjoin::stringdist_left_join(place_desc_to_place, 
                                  by = c("place_id_to_join" = "descriptor"),
                                  max_dist = 2,
                                  method = 'lv',distance_col = 'dist') %>% 
  arrange(dist) %>% distinct(place_id_to_join, .keep_all = TRUE) %>% View()

theses_ps_att %>% distinct(name, toponym, toponym2) %>% arrange(name) %>% View()

theses_ps_descriptors = theses_ps_att %>% select(name, source_id,psatt_id) 

theses_ps_descriptors %>% separate(name, into = c('last', 'rest'), sep = ',') %>% 
  mutate(last = trimws(last), rest = trimws(rest)) %>% select(psatt_id,source_id, rest, last) 


