next_fm_id = 2071

# places table

# if we're not going to have new codes for all of them sorted out, then probably have to 
# export list of existing place codes, join these to this list
# upload all missing ones to places table, even if they are obviously the same
# or try and sort out as many as possible at this point
# then use these codes as a concordance.


# Make list of all strings used as place names, export to GS

caa_264 %>% count(`264_a`) %>% arrange(desc(n)) %>% googlesheets4::write_sheet()

# find either fm ID or wikidata information, enter on GS
# Import this file

caa_places = googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1xnmcg9YMgT5izCEwTKDGHr3CxcDz5x6elbM3vg1y83U/edit?usp=sharing', col_types = 'c')

#  Make list of those with wikidata IDs:

caa_wikidata = caa_places %>% filter(!is.na(wikidata_id)) %>% distinct(wikidata_id)

# Load script to get info from Wikidata:

source('/Users/Yann/repos/odis-exporter/wikidata_function.R')

wikidata_place_details = lapply(caa_wikidata$wikidata_id, get_one_id)

wikidata_place_details_df = data.table::rbindlist(wikidata_place_details)

# Make tables to be uploaded to FM
# Places:

# Check next places ID

next_places_id = 9093

wikidata_place_details_df = wikidata_place_details_df %>% 
  separate(coord, into = c('long', 'lat'), sep = ' ') %>% 
  mutate(long = str_remove(long, "Point\\(")) %>% 
  mutate(lat = str_remove(lat, "\\)")) %>% slice(-4) %>% 
  mutate(place_fm_id = next_places_id:((next_places_id-1) + nrow(.)))

places_table = wikidata_place_details_df %>% 
  select(place_fm_id, long, lat, countryLabel, wikidata)




# now edit MARC

places_codes = caa_264 %>% 
  left_join(caa_places, by = '264_a', na_matches = 'never') %>% 
  left_join(places_table, by = c('wikidata_id' = 'wikidata'), na_matches = 'never')  %>% 
  mutate(place_fm_id = as.character(place_fm_id)) %>% 
  mutate(fm_place_id = coalesce(fm_place_id, place_fm_id)) %>%
  select(RecordID, fm_place_id) 


# links:

# Check project IDs

places_links = wikidata_place_details_df %>% 
  select(place_fm_id, geonames, wikidata) %>% 
  mutate(geonames = paste0("https://www.geonames.org/",geonames)) %>% 
  mutate(wikidata = paste0("https://www.wikidata.org/wiki/", wikidata)) %>% 
  pivot_longer(2:3)  %>% 
  mutate(project_fk = ifelse(name == 'geonames', 7, 15))

# add institutions codes

# make manual concordance

caa_952 %>% distinct(`952_e`) %>% write_csv('caa_institutions.csv')

# fill in institution names from filemaker records
# make copy of the file with new name
# import and filter to any missing ones

institution_concordance = read_csv('caa_institutions_completed.csv')

# create export for institutions - including subdivision
# might be OK to do manually depending


# final concordance list for institutions

# make a sourcesInstitutions table, with Source ID, institution 

df = caa_245 %>% 
  mutate(fm_id = next_fm_id:((next_fm_id-1) + nrow(.))) %>%
  left_join(caa_100) %>% 
  select( RecordID, fm_id, title_1 = `245_a`) %>%  
  left_join(alt_title) %>% 
  mutate(Title = coalesce(Title, title_1)) %>% select(-title_1) %>% 
  left_join(caa_264) %>%  
  separate(`264_c`, into = c('date1', 'date2'), sep = '-') %>% 
  select(RecordID, fm_id, Title, `sourcesPublication_to_places::name` = `264_a`,date1, date2) %>%
  mutate(d1 = 0, m1 = 0, y1 = str_extract(date1, "[0-9]{4}"))%>%
  mutate(d2 = 0, m2 = 0, y2 = str_extract(date2, "[0-9]{4}")) %>% 
  mutate(publ_date_uncertain = ifelse(str_detect(date1, "\\?"), 'yes', 'no')) %>% 
  left_join(caa_300 %>% filter(!is.na(`300_c`))) %>% 
  select(RecordID, fm_id, Title, `sourcesPublication_to_places::name`, d1, m1, y1, d2, m2, y2, publ_date_uncertain, page_numbers = `300_a`, dim = `300_c`) %>% 
  #mutate(h_w = str_extract_all(dim, "[0-9]{3}"))  %>%
  #mutate(height = sapply(h_w, function(x) ifelse(length(x) >= 1, x[1], NA)),
  # width = sapply(h_w, function(x) ifelse(length(x) >= 2, x[2], NA))) %>% 
  left_join(caa_340 %>% filter(!is.na(`340_e`))) %>% 
  rename(material = `340_e`) %>% select(-`340_a`) %>% 
  mutate(page_numbers = str_extract(page_numbers, "[0-9]{1,}")) %>% 
  left_join(caa_655) %>% rename(type = `655_v`) %>% 
  mutate(y2 = coalesce(y2, y1)) %>% 
  left_join(caa_546) %>% 
  rename(language = `546_a`)  %>% 
  mutate(date_concat = ifelse(y1 != y2, paste0(y1, " - ", y2), y1)) %>% 
  #mutate(format = paste0(height, "h x ", width, "w" )) %>% 
  left_join(places_codes, by = 'RecordID')

institution = df %>% 
  select(fm_id, RecordID) %>% 
  left_join(caa_952) %>% 
  left_join(institution_concordance) 


# also need links table

# create LIBIS.be as a project, note down the project ID

source_links = df %>% select(Title, fm_id, RecordID) %>% left_join(caa_856) %>% mutate(project = 16)

df %>% 
  select(Title, language, d1, m1, y1, d2, m2, y2, date_concat,format = dim, publication_place_fk = place) %>% 
  write_csv('20231212_caa_published_sources_table.csv')

source_links %>% write_csv('20231212_caa_published_sources_links_table.csv')

