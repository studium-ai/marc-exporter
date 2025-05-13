next_ps_att_id = 155459

next_ps_id = 143304

next_desc_id = 498108

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
  mutate(ps_id = next_ps_id:((next_ps_id-1) + nrow(.)))%>% 
  left_join(theses_245, by = 'RecordID') %>% 
  select(RecordID,role, name, `245_c`,psatt_id, ps_id)%>% 
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
  select(RecordID, name, role, psatt_id, ps_id) %>% 
  separate(name, into = c('last', 'rest'), sep = ',') %>%
  mutate(rest = str_remove(rest, "\\."))%>% 
  mutate(last = trimws(last), rest = trimws(rest))  %>% 
  separate(rest, into = c(paste0('desc', 1:5)), sep = ' ') %>% 
  mutate(desc1 = trimws(desc1), desc2 = trimws(desc2), desc3= trimws(desc3), desc4 = trimws(desc4), desc5 = trimws(desc5))%>% 
  #left_join(all_place_descriptors) %>% 
  select(RecordID, role, psatt_id,ps_id, desc1, desc2, desc3, desc4, desc5, last) %>% 
  pivot_longer(cols = c(starts_with('desc'), 'last')) %>% 
  filter(!is.na(value)) %>% 
  group_by(psatt_id, ps_id) %>% 
  summarise(name = paste0(value, collapse = ' '), role = role[1], RecordID = RecordID[1]) %>% 
  ungroup() %>% 
  left_join(theses_dates) %>% 
  left_join(theses_sources %>% select(RecordID = tag_001, source_fk = fm_id))  %>% 
  mutate(event_type = 'publication') 

theses_descriptors_for_export = theses_ps_att %>% 
  #filter(role == 'dissertant') %>% 
  select(RecordID, name, role, psatt_id, ps_id) %>% 
  separate(name, into = c('last', 'rest'), sep = ',') %>% 
  mutate(last = trimws(last), rest = trimws(rest)) %>%
  separate(rest, into = c(paste0('desc', 1:5)), sep = ' ') %>% 
  mutate(desc1 = trimws(desc1), desc2 = trimws(desc2), desc3= trimws(desc3), desc4 = trimws(desc4), desc5 = trimws(desc5))%>% 
  left_join(all_place_descriptors, by = 'psatt_id') %>% 
  select(RecordID, desc1, desc2, desc3, desc4, desc5, last, desc, psatt_id, ps_id) %>% 
  pivot_longer(cols = c(desc1, desc2, desc3, desc4, desc5, last, desc)) %>% 
  filter(!is.na(value)) %>% 
  mutate(value = str_remove(value, "\\.$")) %>% mutate(value = trimws(value)) %>% 
  left_join(theses_sources %>%
              select(RecordID = tag_001, source_fm_id = fm_id))  %>% 
 left_join(all_place_descriptors, by = c('psatt_id', c('value' = 'desc'))) %>% 
  mutate(type = ifelse(name == 'desc1', 'first',
                       ifelse(name == 'last', 'last',
                              ifelse(name == 'desc', 'origin', 'middle')))) %>% 
  group_by(psatt_id, ps_id) %>% 
  mutate(position = seq_along(psatt_id)) %>% select(-name) %>% ungroup() %>% 
  mutate(desc_id = next_desc_id:((next_desc_id-1) + nrow(.))) %>% 
  select(desc_id, everything(), -RecordID) %>% 
  mutate(standardised_element = ifelse(type == 'origin', 'no', 'yes')) %>% 
  mutate(place_fm_id = replace_na(place_fm_id, '0'))

places_for_concat = theses_descriptors_for_export %>% 
  filter(descriptor_type == 'origin') %>% 
  select(ps_att_id_fk, descriptor)

# Check with Yanne that we actually decided to add the name descriptors to the concatenated name. 

theses_ps_att_for_export = theses_ps_att_for_export %>% 
  left_join(places_for_concat, by = c('psatt_id' = 'ps_att_id_fk')) %>% 
  mutate(name = ifelse(!is.na(descriptor), paste0(name, " ", descriptor), name)) %>% 
  select(-descriptor)

theses_ps_att_for_export = theses_ps_att_for_export %>% 
  select(ps_id_id_fk = psatt_id, 
         source_id_fk = source_fk,
         ps_id_fk = ps_id,
         descriptors_concat = name, 
         event_type, 
         d1, m1, y1,
         d2, m2, y2, 
         role) %>% 
  mutate(place_id_fk = 2)


# Run missing_namvar_matches.R

theses_descriptors_for_export  = theses_descriptors_for_export %>% 
  mutate(type_for_join = ifelse(type == 'last', 'last',
                                ifelse(type %in% c('first', 'middle'), 'given', 'other'))) %>%
  left_join(all_namvar, by = c(c('value' = 'X3'), 'type_for_join')) %>% 
  select(descriptor_id_pk = desc_id, 
         descriptor = value, 
         namvar_id_fk = X1, 
         position, 
         descriptor_type = type, 
         source_id_fk = source_fm_id,
         ps_att_id_fk = psatt_id,
         nam_id_fk = X2,
         standardised_element, 
         place_id_fk = place_fm_id,
         ps_id_fk = ps_id
         )%>% 
  mutate(namvar_id_fk = replace_na(namvar_id_fk, 0))%>% 
  mutate(nam_id_fk = replace_na(nam_id_fk, 0))

# Check:

theses_descriptors_for_export %>% filter(namvar_id_fk == 0 & descriptor_type != 'origin')




