studium_places = readxl::read_excel('/Users/ghum-m-ae231206/Library/CloudStorage/OneDrive-SharedLibraries-KULeuven/Suchismita Ghosh - Internship_KUL DH/Studium_Places.xlsx')

lov_sources = read_csv('2024-01-16_lov_sources_table.csv')
caa_sources = read_csv('2024-02-15_caa_sources_table.csv')

caa_places = studium_places %>% 
  filter(Place_ID_pk %in% c(lov_sources$place_fm_id1, lov_sources$place_fm_id2, caa_sources$place_1,caa_sources$place_2))

caa_orig_places = readxl::read_excel('/Users/ghum-m-ae231206/Library/CloudStorage/OneDrive-KULeuven/WP 2 Data input - pilot and external datasets/caa_places.xlsx')

caa_alternatives = caa_orig_places %>% group_by(fm_place_id) %>% summarise(alts = paste0(unique(`264_a`), collapse = '; '))

caa_places_all = caa_sources %>% 
  select(RecordID, place_1, y1, y2) %>% 
  mutate(RecordID = as.character(RecordID)) %>% 
  left_join(caa_264, by = c('RecordID'  = 'RecordID'))  %>% 
  select(place_fm_id1 = place_1, `264_a`, y1, y2,RecordID)


lov_places_all = lov_df %>% 
  select(mms, place_fm_id1, y1, y2) %>% 
  mutate(mms = as.character(mms)) %>% 
  left_join(caa_264, by = c('mms'  = 'RecordID'))  %>% 
  select(place_fm_id1, `264_a`, y1, y2, RecordID = mms)


caa_places_for_whg = caa_places_all %>% 
  rbind(lov_places_all) %>% filter(y2 < 1798) %>% 
  left_join(studium_places %>% mutate(Place_ID_pk = as.character(Place_ID_pk)) %>% 
              select(place_fm_id1 = Place_ID_pk, main_name = name)) %>% 
  group_by(place_fm_id1) %>% 
  summarise(main_name = max(main_name), all_names = paste0(unique(`264_a`), collapse = '; '), 
            start = min(y1, na.rm = TRUE), 
            end = max(y1, na.rm = TRUE), 
            source_id = RecordID[1])  %>% 
  mutate(all_names = str_remove_all(all_names, "; NA")) %>% 
  mutate(all_names = trimws(all_names)) 


caa_places_for_whg%>% write_csv('caa_places_for_whg_with_source.csv')
