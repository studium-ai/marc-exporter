import_file = "20240223092601_the_ingest_alma_theses_oul_digcol_xml"

next_fm_id = 6969

next_project_id = 23

theses_places = theses_518 %>% 
  separate(`518_a`, into = c('place', 'date'), sep = ',') %>% 
  mutate(place_fm_id = ifelse(str_detect(place, "(?i)leuven"), 2, NA)) %>% 
  select(RecordID, place_fm_id) %>% distinct(RecordID, .keep_all = TRUE)

to_remove = theses_518 %>% filter(str_detect(`518_a`, "(?i)Douai")) %>% pull(RecordID)

language = theses_041 %>% 
  mutate(language = ifelse( `041_a` == 'lat', 'Latin', language)) %>% 
  select(RecordID, language)

theses_dates = theses_518 %>%
  separate(`518_a`, into = c('place', 'date'), sep = ',') %>% 
  mutate(date2 = dmy(date)) %>% 
  mutate(date3 = ymd(date)) %>% 
  mutate(date = str_remove_all(date, "\\?")) %>% 
  mutate(date4 = str_extract(date, "[0-9X]{4}$")) %>%
  mutate(y1 = year(date2), m1 = month(date2), d1 = day(date2))%>%
  mutate(y2 = year(date3), m2 = month(date3), d2 = day(date3)) %>% 
  mutate(y1 = coalesce(y1, y2),d1 = coalesce(d1, d2),m1 = coalesce(m1, m2))  %>% 
  mutate(date5 = ifelse(str_detect(date4, "XX(?!XX)$"), paste0(str_replace(date4, "XX", "00")," - ", str_replace(date4, "XX", "99")), date4)) %>% 
  mutate(date5 = ifelse(str_detect(date4, "[0-9]{3}X"), paste0(str_replace(date4, "X(?!X)$", "0")," - ", str_replace(date4, "X(?!X)$", "9")), date5)) %>%
  separate(date5, into = c('y3', 'y4'), sep = '-') %>% 
  mutate(y3 = as.numeric(y3)) %>% mutate(y4 = as.numeric(y4)) %>% 
  mutate(y1 = coalesce(y1, y3)) %>% mutate(y2 = coalesce(y2, y4)) %>% 
  select(RecordID, d1, m1, y1, d2, m2, y2) %>% 
  mutate(d2 = coalesce(d2, d1))%>% 
  mutate(m2 = coalesce(m2, m1))%>% 
  mutate(y2 = coalesce(y2, y1)) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  distinct(RecordID, .keep_all = TRUE)

theses_title = theses_245 %>% 
  mutate(`245_c` = coalesce(`245_c`, `245_b`)) %>% 
  mutate(alt_title = ifelse(!is.na(`245_b`), paste0(`245_c`, "/", `245_b`), `245_c`)) %>% 
  select(RecordID, main_title = `245_a`, alt_title = `245_c`) %>% 
  left_join(theses_246) %>% 
  mutate(alt_title = ifelse(!is.na(`246_a`), paste0(alt_title, "/", `246_a`), alt_title))

theses_sources = theses_001008 %>% 
  filter(!tag_001 %in% to_remove) %>% 
  select(tag_001) %>% 
  left_join(theses_dates, by = c('tag_001' = 'RecordID')) %>% 
  left_join(theses_places, by = c('tag_001' = 'RecordID')) %>% 
  mutate(fm_id = next_fm_id:((next_fm_id-1) + nrow(.))) %>% 
  mutate(import = paste0("Imported from ", import_file))
  

source_ids = theses_sources %>% select(tag_001, fm_id) 



theses_links = theses_856 %>% 
  filter(!RecordID %in% to_remove) %>% 
  select(RecordID, link = `856_u`) %>% 
  mutate(project_id = 23) %>% 
  left_join(source_ids, by = c('RecordID' = 'tag_001')) %>% 
  mutate(import = paste0("Imported from ", import_file))

theses_links_mms = source_ids %>% 
  mutate(project_fm_id = 24) %>% 
  rename(external_id = tag_001) %>% 
  mutate(import = paste0("Imported from ", import_file))


theses_inst_for_linking = theses_710 %>%
  filter(!RecordID %in% to_remove) %>% 
  filter(`710_e` == 'organizing institution') %>% 
  distinct(`710_a`)

theses_inst_for_linking %>% 
  write_csv('theses_inst_for_linking.csv')

theses_inst_linked = read_csv('theses_inst_for_linking_finished.csv')

theses_inst = theses_710 %>% 
  filter(!RecordID %in% to_remove) %>% 
  filter(`710_e` == 'organizing institution')

theses_inst = theses_inst %>% 
  left_join(theses_inst_linked) %>% 
  mutate(import = paste0("Imported from ", import_file))

theses_inst = theses_inst %>% 
  select(RecordID, inst_fm_id, institution_role = `710_e`, import) %>% 
  left_join(source_ids, by = c('RecordID' = 'tag_001'))

theses_sources %>% 
  write_csv(paste0(Sys.Date(),'_the_sources_table.csv'))

theses_links %>% 
  write_csv(paste0(Sys.Date(),'_the_sources_links_table.csv'))

theses_links_mms %>% 
  write_csv(paste0(Sys.Date(),'_the_sources_links_mms_table.csv'))

# Add project LIBIS as 23 and Thesis Sheets as 24

# MAKE MANUAL CHANGE IN FM
# RecordID 9981133700101488 date range is 10 6 1771 11 6 1771
# RecordID 9916000640101488 date range is 15 4 1778 15 5 1778

theses_inst %>% write_csv(paste0(Sys.Date(),'_the_sources_instutitions_table.csv'))

  