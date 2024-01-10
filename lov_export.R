lov_omeka_file = read_csv('/Users/Yann/Library/CloudStorage/OneDrive-KULeuven/Shared_data_studium/5 Collectio Academica Antiqua (through ALMA in stead of the fraction of Digitized items on Lovaniensia)/20230728_Omeka_Lovaniensia.csv')

next_fm_id = 2647
next_places_id = 9096

source('/Users/Yann/repos/odis-exporter/wikidata_function.R')

lov_places = lov_omeka_file %>% 
  select(`Item URI`, place = `Dublin Core:Coverage`) %>% 
  separate(place, into = c('place1', 'place2', 'place3'), sep = "\\$\\$")

places_to_get_ids = lov_places %>% 
  pivot_longer(starts_with("place")) %>%
  filter(!is.na(value)) %>% 
  count(value) %>% 
  arrange(desc(n))

places_to_get_ids %>% 
  googlesheets4::write_sheet()

# fill in google sheet with place IDs - only after all other previous imports are done and you are working with live version
# Also add wikidata links to missing ones where possible.

# Read in the places concordance:

places_with_fm_id = googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/12_27saZxrqzsOOOBbOICiGDOql8XOToCGcaevGis6X4/edit?usp=sharing',na = 'NA', col_types = 'c')

# isolate those with Wikidata IDs, make new list:

places_with_fm_id_to_add = places_with_fm_id %>% filter(!is.na(wikidata_id))


missing_place_details = lapply(places_with_fm_id_to_add$wikidata_id, get_one_id)

missing_place_details_df = missing_place_details %>% data.table::rbindlist()

# import, get information, create a places table

missing_place_details_df = missing_place_details_df %>% 
  separate(coord, into = c('long', 'lat'), sep = ' ') %>% 
  mutate(long = str_remove(long, "Point\\(")) %>% 
  mutate(lat = str_remove(lat, "\\)")) %>% slice(-4) %>% 
  mutate(place_fm_id = next_places_id:((next_places_id-1) + nrow(.)))

missing_place_details_df%>% 
  write_csv(paste0(Sys.Date(),'_lov_places_table.csv'), na = "0")


# import to filemaker

# add Wikidata as a project

# create links for missing places:

links_table = missing_place_details_df %>% 
  separate(coord, into = c('long', 'lat'), sep = ' ') %>% 
  mutate(long = str_remove(long, "Point\\(")) %>% 
  mutate(lat = str_remove(lat, "\\)")) %>% 
  slice(-4) %>% 
  mutate(places_fk = next_places_id:((next_places_id-1) + nrow(.))) %>% 
  mutate(geonames_link = paste0("https://www.geonames.org/", geonames)) %>% 
  mutate(wikidata_link = paste0("http://www.wikidata.org/entity/", wikidata)) %>% 
  select(geonames_link, wikidata_link, places_fk) %>% 
  pivot_longer(1:2) %>% 
  mutate(project_fk = ifelse(name == 'geonames_link', 7, 16))

# change the file name and import

df = lov_omeka_file  %>% 
  mutate(fm_id = next_fm_id:((next_fm_id-1) + nrow(.)))%>% 
  select(fm_id, uri = `Item URI`, mms = `Item Type Metadata:MMS ID`,
         place = `Dublin Core:Coverage`, 
         y1 =`Dublin Core:Date`, 
         description = `Dublin Core:Description`,
         language = `Dublin Core:Language`,
         title = `Dublin Core:Title`) %>% 
  mutate(language = case_when(
    language == "lat" ~ "Latin",
    language == "fr"  ~ "French",
    language == "dut" ~ "Dutch",
           TRUE ~ as.character(language))) %>% 
  mutate(d1 = 0, m1 = 0, y2 = 0, m2 = 0, d2 = 0) %>% 
  mutate(format = str_extract(description, "\\b\\d{1,2}° ?\\??")) %>% select(-description) %>%
  left_join(places_with_fm_id, by = c('place' = 'value')) %>% select(-n) %>% 
  left_join(missing_place_details_df %>% 
              distinct(wikidata, .keep_all = TRUE) %>% 
              select(place_fm_id, wikidata) %>% mutate(place_fm_id = as.character(place_fm_id)), by = c('wikidata_id' = 'wikidata')) %>% 
  mutate(place_fm_id = coalesce(fm_place_id, place_fm_id))

# create links table:

links = df %>% select(source_id = fm_id, link = uri) %>%
  mutate(project_id = 2) 

# create other links table, from MARC:

links2 = df %>% select(fm_id, mms) %>% mutate(mms = as.character(mms)) %>%
  inner_join(lov_856, by = c('mms' = 'RecordID')) %>% 
  select(source_id = fm_id, link =  `856_u`) %>% mutate(project_id = NA)

rbind(links, links2) %>% 
  write_csv(paste0(Sys.Date(),'_lov_sources_links_table.csv'), na = '0')

df %>% select(-mms) %>% 
  write_csv(paste0(Sys.Date(),'_lov_sources_table.csv'), na = '0')
