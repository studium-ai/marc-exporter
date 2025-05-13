theses_ps_att = theses_700 %>% 
  left_join(theses_dates) %>% 
  inner_join(source_ids, by = c('RecordID' = 'tag_001')) %>% 
  select(name = `700_a`, role = `700_e`, d1, m1, y1, d2, m2, y2, source_id =fm_id, role = `700_e`,RecordID) %>% 
  mutate(event_type = 'publication') %>% 
  mutate(psatt_id = next_ps_att_id:((next_ps_att_id-1) + nrow(.))) %>% 
  left_join(theses_245, by = 'RecordID') %>% 
  select(RecordID,role, name, `245_c`,psatt_id ) %>% 
  separate(`245_c`, into = c('p', 'd'), sep = '(?i)defendet|proponit|defendent|dfendet|defendit|asserebat|ponebat|deefndet')  %>% 
  mutate(d = str_replace_all(d, "æ", "ae")) %>% 
  mutate(d = trimws(d)) %>% 
  separate(d, into = c('d', 'discard'), sep = 'Lovanii') %>% 
  mutate(d = str_replace(d, "S\\.", "S\\&"))%>% 
  mutate(d = str_replace(d, "Sti\\.", "Sti\\&")) %>% 
  mutate(toponym.p = ifelse( role == 'dissertant', str_extract(d, "\\b\\w+(?:-\\w+)?sis\\b"), NA))  %>% 
  mutate(toponym.p2 = ifelse(role == 'dissertant', str_extract(d, "\\b(?<=ex)[^.]*|\\b(?<=de la)([^.]|[^.,])*"), NA))  %>% 
  mutate(toponym.p2 = str_replace(toponym.p2, "S\\&", "S\\."))%>% 
  mutate(toponym.p2 = str_replace(toponym.p2, "Sti\\&", "Sti\\."))%>% 
  select(-discard) %>% 
  mutate(toponym.p3 = ifelse(role == 'dissertant', str_extract(d, "(?i)Namuranus|Hanno\\-Montanus|Hanno\\-montanus|Castro-Solranus"), NA))  %>% 
  mutate(toponym.pl = tolower(str_remove_all(toponym.p, "[[:punct:][:space:]]")))%>% 
  mutate(toponym.p2l = tolower(str_remove_all(toponym.p2, "[[:punct:][:space:]]")))%>% 
  mutate(toponym.p3l = tolower(str_remove_all(toponym.p3, "[[:punct:][:space:]]"))) %>% 
  mutate(toponym.pl = tolower(trimws(toponym.pl, which = 'both')))  %>% 
  mutate(toponym.p2l = tolower(trimws(toponym.p2l, which = 'both')))%>% 
  mutate(toponym.p3l = tolower(trimws(toponym.p3l, which = 'both'))) 

more_than_one_d = theses_ps_att %>% filter(role == 'dissertant') %>% count(RecordID) %>%filter(n>1)

theses_ps_att = theses_ps_att %>% filter(!RecordID %in% more_than_one_d$RecordID)

place_desc_to_place = read_csv('/Users/ghum-m-ae231206/marc-exporter/descriptorsOrigin_PlaceID.csv') %>% 
  mutate(descriptor = str_remove(descriptor, "^a|de")) %>% 
  mutate(descriptor = trimws(descriptor)) %>% 
  mutate(descriptor = tolower(str_remove_all(descriptor, "[[:punct:][:space:]]")))%>% 
  distinct(descriptor, Place_ID)%>% rename(place_id_fk = Place_ID)


export_correct = theses_ps_att %>% 
  mutate(top_used = trimws(coalesce(toponym.p2l, toponym.pl, toponym.p3l))) %>% 
  dplyr::left_join(place_desc_to_place, by = c("top_used" = "descriptor")) %>%
  filter(!is.na(place_id_fk) & role == 'dissertant') %>%
  left_join(theses_245, by = 'RecordID') %>% 
  select(RecordID, orig_text_from_marc = `245_c`, toponym.p,toponym.p2,toponym.p3, top_used,
         place_fm_id_found=place_id_fk) 




missing = theses_ps_att %>% 
  mutate(top_used = trimws(coalesce(toponym.p2l, toponym.pl, toponym.p3l))) %>% 
  dplyr::left_join(place_desc_to_place, by = c("top_used" = "descriptor")) %>%
  filter(is.na(place_id_fk) & role == 'dissertant') 

place_extracted = missing %>% 
  mutate(place_id_to_join = coalesce(toponym.p2l, toponym.pl, toponym.p3l)) %>% 
  filter(!is.na(place_id_to_join)) %>% 
  left_join(place_desc_to_place, by = c('place_id_to_join' = 'descriptor'))

export_place_extracted = place_extracted%>%
  left_join(theses_245, by = 'RecordID') %>%
  select(RecordID, orig_text_from_marc = `245_c`, top_used, place_id_to_join) 

no_place_extracted = missing %>% 
  mutate(place_id_to_join = coalesce(toponym.p2l, toponym.pl, toponym.p3l)) %>% 
  filter(is.na(place_id_to_join))

found2 = no_place_extracted %>% 
  separate(d, into = c('d', 'discard'), sep = 'Lovanii') %>% 
  select(-discard) %>% 
  mutate(d2 = str_remove_all(d, "\\."))  %>% 
  mutate(d2 = str_remove(d2, "in collegio majori|in collegio majori|in collegio maiori|canonicus"))%>% 
  mutate(d2 = str_remove_all(d2, "\\[\\.\\.\\.\\]\\.")) %>% 
  mutate(d2 = str_remove_all(d2, "\\[\\]"))%>% 
  mutate(d2 = str_remove_all(d2, "\\.")) %>% 
  mutate(d2 = trimws(d2)) %>% 
  mutate(last_word = word(d2, -1))  %>% 
  select(-d2) %>% 
  mutate(last_word2 = tolower(str_remove_all(last_word, "[[:punct:][:space:]]"))) %>% 
  dplyr::left_join(place_desc_to_place, by = c("last_word2" = "descriptor")) %>% 
  filter(!is.na(place_id_fk.y)) 

export_found2 = found2 %>%
  left_join(theses_245, by = 'RecordID') %>% 
  select(RecordID, orig_text_from_marc = `245_c`, last_word, last_word2,
         place_fm_id_found=place_id_fk.y) 



missing2 = no_place_extracted %>% 
  separate(d, into = c('d', 'discard'), sep = 'Lovanii') %>% select(-discard) %>% 
  mutate(d2 = str_remove_all(d, "\\."))  %>% 
  mutate(d2 = str_remove(d2, "in collegio majori|in collegio majori|in collegio maiori|canonicus"))%>% 
  mutate(d2 = str_remove_all(d2, "\\[\\.\\.\\.\\]\\.")) %>% 
  mutate(d2 = str_remove_all(d2, "\\[\\]"))%>% 
  mutate(d2 = str_remove_all(d2, "\\.")) %>% 
  mutate(d2 = trimws(d2))%>% 
  mutate(last_word = word(d2, -1)) %>% select(-d2) %>% 
  mutate(last_word2 = tolower(str_remove_all(last_word, "[[:punct:][:space:]]"))) %>% 
  dplyr::left_join(place_desc_to_place, by = c("last_word2" = "descriptor")) %>% 
  filter(is.na(place_id_fk.y)) %>% mutate(standarisation = NA)

export_missing2 = missing2  %>%
  left_join(theses_245, by = 'RecordID')%>% 
  select(RecordID, 
         orig_text_from_marc = `245_c`, 
         last_word,last_word2, 
         place_fm_id_found=place_id_fk.y)

correct_export %>% 
  write_excel_csv(paste0(Sys.Date(), 'correct.csv'), na = '')

export_place_extracted %>% 
  write_excel_csv(paste0(Sys.Date(), 'place_extracted_no_match_found.csv'), na = '')

found2_export %>% write_excel_csv(paste0(Sys.Date(), 'place_found_round2.csv'), na = '')

missing2_export %>% 
  write_excel_csv(paste0(Sys.Date(), 'no_place_found_round2.csv'), na = '')



all_place_descriptors_names = rbind(correct_export %>%select(RecordID, desc = top_used),
                              found2_export %>% select(RecordID, desc = last_word),
                              missing2_export %>% select(RecordID, desc = last_word),
                              place_extracted_export %>% select(RecordID, desc = top_used))
