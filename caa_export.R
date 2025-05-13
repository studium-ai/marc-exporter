lov_omeka_file = read_csv('/Users/ghum-m-ae231206/Library/CloudStorage/OneDrive-KULeuven/Shared_data_studium/5 Collectio Academica Antiqua (through ALMA in stead of the fraction of Digitized items on Lovaniensia)/20230728_Omeka_Lovaniensia.csv')


next_fm_id = 4147

next_project_id = 22

next_places_id = 9203

# places table

# if we're not going to have new codes for all of them sorted out, then probably have to 
# export list of existing place codes, join these to this list
# upload all missing ones to places table, even if they are obviously the same
# or try and sort out as many as possible at this point
# then use these codes as a concordance.


# Make list of all strings used as place names, export to GS

all_places = caa_264 %>% 
  filter(!RecordID %in% lov_omeka_file$`Item Type Metadata:MMS ID`)%>% 
  count(`264_a`) %>% 
  arrange(desc(n)) 

# find either fm ID or wikidata information, enter on GS
# Import this file

caa_places = readxl::read_xlsx('/Users/ghum-m-ae231206/Library/CloudStorage/OneDrive-KULeuven/WP 2 Data input - pilot and external datasets/caa_places.xlsx')


caa_places %>% 
  filter(`264_a` %in% all_places$`264_a`) %>% 
  filter(is.na(fm_place_id)) %>% 
  filter(is.na(wikidata_id)) %>% 
  filter(is.na(remain_blank)) %>% 
  filter(str_detect(notes, "(?i)find") | str_detect(notes, "(?i)check") | is.na(wikidata_id) & is.na(fm_place_id))%>% 
  left_join(caa_264, by = '264_a') %>% 
  select(`264_a`, RecordID, notes) %>% 
  left_join(caa_856 %>% distinct(RecordID, .keep_all = TRUE), by = 'RecordID') %>% arrange(`264_a`) %>% View()
  write_excel_csv('/Users/ghum-m-ae231206/Library/CloudStorage/OneDrive-KULeuven/WP 2 Data input - pilot and external datasets/missing_or_unsure_places_caa_short.csv')

#  Make list of those with wikidata IDs:

caa_wikidata = caa_places %>% 
  filter(`264_a` %in% all_places$`264_a`) %>% 
  filter(!is.na(wikidata_id) & is.na(fm_place_id)) %>% 
  distinct(wikidata_id)

# Load script to get info from Wikidata:

source('/Users/ghum-m-ae231206/odis-exporter/wikidata_function.R')

wikidata_place_details = lapply(caa_wikidata$wikidata_id, get_one_id)

wikidata_place_details_df = data.table::rbindlist(wikidata_place_details)

# Make tables to be uploaded to FM

# Places:

# Check next places ID

totally_missing_places = caa_places %>% 
  filter(`264_a` %in% all_places$`264_a`)%>% 
  filter(is.na(fm_place_id) & is.na(wikidata_id)) %>% 
  filter(is.na(remain_blank)) %>% 
  mutate(geonames = NA, long = 0, lat = 0, country = NA, countryLabel = NA, countryGeonames = NA, placeLabel = `264_a`, wikidata = NA) %>% 
  select(geonames, long, lat, country, countryLabel, countryGeonames, placeLabel, wikidata)

totally_missing_places %>% left_join(caa_264, by = c('placeLabel' = '264_a')) %>% select(placeLabel, )

wikidata_place_details_df = wikidata_place_details_df %>% 
  separate(coord, into = c('long', 'lat'), sep = ' ') %>% 
  mutate(long = str_remove(long, "Point\\(")) %>% 
  mutate(lat = str_remove(lat, "\\)")) %>% slice(-c(3,38, 52)) %>% 
  distinct(wikidata, .keep_all = TRUE) %>% 
  mutate(place_fm_id = next_places_id:((next_places_id-1) + nrow(.)))

places_table = wikidata_place_details_df %>% 
  select(place_fm_id, long, lat, placeLabel,countryLabel, wikidata) %>% 
  mutate(intNotes = ifelse(!is.na(countryLabel), paste0("Country: ", countryLabel), NA))  %>% 
  mutate(import = "Imported from 20230728_Alma_Caa_all.xml") %>% 
  mutate(wikidata = coalesce(wikidata, placeLabel))

places_table %>% 
  write_csv(paste0(Sys.Date(),'_caa_places_table.csv'), na = "0")


# now edit MARC

places_codes = caa_264 %>% 
  filter(!RecordID %in% lov_omeka_file$`Item Type Metadata:MMS ID`) %>% 
  left_join(caa_places, by = '264_a', na_matches = 'never') %>% 
  mutate(wikidata_id = coalesce(wikidata_id, `264_a`)) %>% 
  left_join(places_table, by = c('wikidata_id' = 'wikidata'), na_matches = 'never')  %>% 
  mutate(place_fm_id = as.character(place_fm_id)) %>% 
  mutate(fm_place_id = coalesce(fm_place_id, place_fm_id)) %>%
  select(RecordID, fm_place_id, fm_place_id_2) %>% 
  distinct(RecordID, fm_place_id, .keep_all = TRUE) %>% 
  group_by(RecordID) %>% 
  mutate(id = row_number()) %>% 
  mutate(id = paste0("place_", id)) %>% 
  pivot_wider(names_from = id, values_from = fm_place_id) %>% ungroup() %>% 
  mutate(fm_place_id_2 = as.character(fm_place_id_2)) %>% 
  mutate(place_2 = coalesce(place_2, fm_place_id_2)) %>% 
  select(RecordID, place_1, place_2) %>% 
  #mutate(place_1 = as.numeric(place_1))%>% 
  #mutate(place_2 = as.numeric(place_2))
  mutate(place_2 = replace_na(place_2, '0'))%>% 
  mutate(place_1 = replace_na(place_1, '0'))


# links:

# Check project IDs

places_links = wikidata_place_details_df %>% filter(!is.na(wikidata)) %>% 
  select(place_fm_id, geonames, wikidata) %>% 
  mutate(geonames = paste0("https://www.geonames.org/",geonames)) %>% 
  mutate(wikidata = paste0("https://www.wikidata.org/wiki/", wikidata)) %>% 
  pivot_longer(2:3)  %>% 
  mutate(project_fk = ifelse(name == 'geonames', 7, 21)) %>% 
  mutate(import = "Imported from 20230728_Alma_Caa_all.xml")

places_links %>% 
  write_csv(paste0(Sys.Date(),'_caa_places_links_table.csv'), na = "0")



alt_title = caa_246  %>% 
  filter(!RecordID %in% lov_omeka_file$`Item Type Metadata:MMS ID`)%>% 
  mutate(add_title = paste0(`246_a`, "/", `246_b`, "/", `246_c`)) %>% 
  mutate(add_title = str_remove_all(add_title, "NA/|NA")) %>% 
  mutate(add_title = str_remove_all(add_title, "/$")) %>% 
  group_by(RecordID) %>% 
  summarise(add_titles = paste0(add_title, collapse = "/"))

dates1 = caa_264  %>% 
  filter(!RecordID %in% lov_omeka_file$`Item Type Metadata:MMS ID`)%>% 
  separate(`264_c`, into = c('date1', 'date2'), sep = '-') %>% 
  mutate(publ_date_uncertain = ifelse(str_detect(date1, "\\?|\\["), 'yes', 'no')) %>%
  mutate(y1 = str_extract(date1, "[0-9]{4}"))%>%
  mutate(y2 = str_extract(date2, "[0-9]{4}")) %>% 
  group_by(RecordID) %>% 
  summarise(y1 = min(y1, na.rm = TRUE), y2 = max(y2, na.rm = TRUE), publ_date_uncertain = max(publ_date_uncertain, na.rm = TRUE), place = `264_a`[1]) %>% 
  mutate(y2 = replace_na(y2, '0'))

dates2 = caa_001008  %>% 
  filter(!tag_001 %in% lov_omeka_file$`Item Type Metadata:MMS ID`)%>% 
  mutate(dates = substr(tag_008,8, 15)) %>% 
  mutate(y1 = str_extract(dates, "[0-9]{4}"))%>% 
  mutate(y2 = str_extract(dates, "[0-9]{4}$")) %>% 
  mutate(y2 = coalesce(y2, y1)) 
  
ISO_639_3 = ISOcodes::ISO_639_3

languages = caa_041 %>% 
  filter(!RecordID %in% lov_omeka_file$`Item Type Metadata:MMS ID`) %>% 
  pivot_longer(starts_with("041")) %>% 
  select(-name) %>% filter(!is.na(value)) %>% 
  left_join(ISO_639_3 %>% 
              select(Part2B, Name), by = c('value' ='Part2B')) %>% 
  mutate(Name = str_remove_all(Name, "\\(.*?\\)")) %>% 
  mutate(Name = trimws(Name)) %>% group_by(RecordID) %>% 
  summarise(language = paste0(unique(Name), collapse = ';'))

caa_df = caa_245 %>% 
  filter(!RecordID %in% lov_omeka_file$`Item Type Metadata:MMS ID`) %>% 
  mutate(fm_id = next_fm_id:((next_fm_id-1) + nrow(.))) %>%
  select(RecordID, fm_id, title_1 = `245_a`, title_2 = `245_b`, title_3 = `245_c`) %>%  
  left_join(alt_title) %>% 
  rename(main_title =  title_1) %>% 
  mutate(add_title = paste0(title_2, "/", title_3, "/", add_titles)) %>% 
  mutate(add_title = str_remove_all(add_title, "NA/|NA")) %>% 
  mutate(add_title = str_remove_all(add_title, "/$")) %>% 
  left_join(dates2, by = c('RecordID' = 'tag_001')) %>% 
  select(RecordID, fm_id, main_title, add_title, y1, y2) %>%
  mutate(d1 = 0, m1 = 0) %>%
  mutate(d2 = 0, m2 = 0) %>%
  left_join(caa_300 %>% filter(!is.na(`300_g`)) %>% 
              distinct(RecordID, .keep_all =  TRUE) %>% select(RecordID, format = `300_g` )) %>% 
  select(RecordID, fm_id, main_title, add_title, d1, m1, y1, d2, m2, y2, format) %>% 
  mutate(y2 = coalesce(y2, y1)) %>% 
  left_join(caa_546) %>% 
  rename(ext_notes = `546_a`) %>% 
  left_join(places_codes, by = 'RecordID')%>% 
  mutate(import = "Imported from 20230728_Alma_Caa_all.xml") %>% 
  left_join(caa_264 %>% distinct(RecordID, .keep_all = TRUE) %>% select(RecordID, orig_place_imprint = `264_a`)) %>% 
  mutate(orig_place_imprint = ifelse(place_1 == 0,orig_place_imprint, "" )) %>% 
  mutate(int_notes =ifelse(place_1 == 0, paste0("264_a_field: ",orig_place_imprint), orig_place_imprint)) %>% left_join(languages, by = 'RecordID') %>% 
  filter(y2<1798 | is.na(y2))

# also need links table

# create LIBIS.be as a project, note down the project ID

source_links = df %>% select(main_title, fm_id, RecordID) %>% left_join(caa_856) 

# projects

source_links = source_links %>% 
  mutate(project_fm_id = ifelse(str_detect(`856_u`, "ustc"), 17, 
                                ifelse(str_detect(`856_u`, "stcv"), 18, NA))) %>% 
  mutate(`856_z` = ifelse(str_detect(`856_u`, "ustc"), 'USTC', 
                                ifelse(str_detect(`856_u`, "stcv"), 'STCV', `856_z`))) %>% 
  mutate(`856_z` = ifelse(str_detect(`856_u`, "sbn") & is.na(`856_z`), "Servizio Bibliotecario Nazionale", `856_z`)) %>% 
  mutate(`856_z` = ifelse(str_detect(`856_y`, "VD16") & is.na(`856_z`), "Verzeichnis der im deutschen Sprachbereich erschienenen Drucke des 16. Jahrhunderts", `856_z`)) %>% 
  mutate(`856_z` = ifelse(str_detect(`856_y`, "VD17") & is.na(`856_z`), "Das Verzeichnis der im deutschen Sprachraum erschienenen Drucke des 17. Jahrhunderts", `856_z`))%>% 
  mutate(project_fm_id = ifelse(str_detect(`856_z`, "Bibliothèque nationale de France"), 15, project_fm_id)) %>% 
  filter(!is.na(`856_u`)) 

projects_table = source_links %>% 
  filter(!is.na(`856_z`)) %>% 
  filter(is.na(project_fm_id)) %>% 
  mutate(`856_z` = str_remove(`856_z`, "(?i)\\((state) a|b\\)")) %>%
  mutate(`856_z` = trimws(`856_z`)) %>% 
  distinct(`856_z`, .keep_all = TRUE) %>% 
  select(`856_z`) %>% 
  mutate(project_fm_id = next_project_id:((next_project_id-1) + nrow(.)))


projects_table %>% 
  write_csv(paste0(Sys.Date(),'_caa_projects_table.csv'), na = "")

projects_codes = source_links %>% 
  mutate(`856_z` = str_remove(`856_z`, "(?i)\\((state) a|b\\)")) %>%
  mutate(`856_z` = trimws(`856_z`)) %>% 
  left_join(projects_table, by ='856_z' ) %>% 
  mutate(project_fm_id = coalesce(project_fm_id.x, project_fm_id.y))

df %>% 
  select(RecordID, fm_id, main_title, add_title,d1, m1, y1, d2, m2, y2,format, place_1, place_2, import, int_notes)   %>% 
  write_csv(paste0(Sys.Date(),'_caa_sources_table.csv'), na = "")

source_links %>% mutate(project_fm_id = projects_codes$project_fm_id) %>% 
  select(fm_id, link = `856_u`, project_fm_id) %>%  
  mutate(import = "Imported from 20230728_Alma_Caa_all.xml")%>% 
  write_csv(paste0(Sys.Date(),'_caa_sources_links_table.csv'), na = "")





languages_add_table = df %>% select(fm_id, language) %>% filter(!is.na(language)) 


languages_add_table %>% 
  write_csv(paste0(Sys.Date(),'_caa_languages_add_table.csv'), na = "")


type = df %>% select(fm_id) %>% mutate(type = 'printed')

type %>% 
  write_csv(paste0(Sys.Date(),'_caa_type_add_table.csv'), na = "")

record_id_source_link = df %>% select(fm_id, RecordID) %>% mutate(project_id = 22)

record_id_source_link %>% 
  write_csv(paste0(Sys.Date(),'_caa_record_id_add_table.csv'), na = "")

missing_from_yanne = read_csv('CAA_without_SourcesLink.csv', col_names = FALSE)

caa_type_record_id = read_csv('2024-02-15_caa_type_record_id_add_table.csv')

caa_type_record_id %>% filter(fm_id %in% missing_from_yanne$X1) %>% select(-type) %>% write_csv('record_id_missing.csv')


caa_type_record_id %>% left_join(record_id_source_link, by = 'fm_id')
