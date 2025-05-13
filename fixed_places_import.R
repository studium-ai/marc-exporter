imported_place_extracted_not_found = readxl::read_xlsx('/Users/ghum-m-ae231206/Library/CloudStorage/OneDrive-KULeuven/WP 2 Data input - pilot and external datasets/Thesis Sheets/place_extracted_no_match_found.xlsx')
imported_no_place_found = readxl::read_xlsx('/Users/ghum-m-ae231206/Library/CloudStorage/OneDrive-KULeuven/WP 2 Data input - pilot and external datasets/Thesis Sheets/no_place_found_round2.xlsx')
imported_correct = readxl::read_xlsx('/Users/ghum-m-ae231206/Library/CloudStorage/OneDrive-KULeuven/WP 2 Data input - pilot and external datasets/Thesis Sheets/correct.xlsx')
imported_found_round2 = readxl::read_xlsx('/Users/ghum-m-ae231206/Library/CloudStorage/OneDrive-KULeuven/WP 2 Data input - pilot and external datasets/Thesis Sheets/place_found_round2.xlsx')

imp1 = imported_correct %>% 
  select(RecordID, place_fm_id = place_fm_id_found)
imp2 = imported_found_round2 %>% 
  select(RecordID, place_fm_id = place_fm_id_found)
imp3 = imported_place_extracted_not_found %>% 
  select(RecordID, place_fm_id = fm_place_id)
imp4 = imported_no_place_found %>% 
  select(RecordID, place_fm_id = place_fm_id_found)

all_place_descriptors = rbind(imp1, imp2, imp3, imp4) %>% 
  filter(!is.na(place_fm_id)) %>% 
  mutate(RecordID = as.numeric(RecordID)) %>% 
  mutate(RecordID = RecordID + 8) %>% 
  mutate(RecordID = as.character(RecordID)) %>% 
  left_join(export_correct %>% select(RecordID, place_fm_id_found))

for_matching = theses_ps_att_for_export %>% 
  filter(role == 'dissertant') %>% 
  select(RecordID, psatt_id) %>% 
  left_join(all_place_descriptors_names)

all_place_descriptors= all_place_descriptors %>% 
  left_join(for_matching) %>% 
  select(-RecordID) %>% mutate(place_fm_id_found = as.character(place_fm_id_found)) %>% 
  mutate(place_fm_id = coalesce(place_fm_id_found, place_fm_id)) %>% select(-place_fm_id_found)


