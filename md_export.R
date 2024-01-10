next_fm_id = 2071


# add insitutions codes

# make manual concordance

md_952 %>% 
  distinct(`952_e`) %>%
  write_csv('md_institutions.csv')

# fill in institution names from filemaker records
# make copy of the file with new name
# import and filter to any missing ones

institution_concordance = read_csv('md_institutions_completed.csv')

# create export for institutions - including subdivision
# might be OK to do manually depending

# final concordance list for institutions

# make a sourcesInstitutions table, with Source ID, institution 



# Add details from 505 field

details = md_505 %>% mutate(a = paste0(`505_g`, ": ", `505_a`)) %>% 
  group_by(RecordID) %>% 
  summarise(details = paste0(a, collapse = "\n"))

alt_title = md_246 %>% group_by(RecordID) %>% 
  summarise(alt_title = paste0(`246_a`, collapse = "/"))

languages = md_546 %>% 
  group_by(RecordID) %>% 
  summarise(language = paste0(`546_a`, collapse = '/'))


dates = md_264 %>%  
  separate(`264_c`, into = c('date1', 'date2'), sep = '-')  %>%
  mutate(date1 = coalesce(`264_a`, date1)) %>% 
  group_by(RecordID) %>% 
  summarise(date1 = max(date1, na.rm = TRUE), date2 = max(date2, na.rm = TRUE)) %>% 
  mutate(d1 = 0, m1 = 0, y1 = str_extract(date1, "[0-9]{4}"))%>%
  mutate(d2 = 0, m2 = 0, y2 = str_extract(date2, "[0-9]{4}")) %>% 
  mutate(publ_date_uncertain = ifelse(str_detect(date1, "\\?|\\[")|str_detect(date2, "\\?|\\["), 'yes', 'no')) 

df = md_245 %>% 
  mutate(fm_id = next_fm_id:((next_fm_id-1) + nrow(.))) %>%
  left_join(md_100) %>% 
  select(RecordID, fm_id, Title = `245_a`) %>%  
  left_join(alt_title) %>% 
  left_join(dates) %>% 
  select(RecordID, fm_id, Title,alt_title,d1, m1, y1, d2, m2, y2, publ_date_uncertain)%>% 
  left_join(md_300 %>% filter(!is.na(`300_c`))) %>% 
  select(RecordID, fm_id, Title,alt_title, d1, m1, y1, d2, m2, y2, publ_date_uncertain, page_numbers = `300_a`, dim = `300_c`) %>% 
  #mutate(h_w = str_extract_all(dim, "[0-9]{3}"))  %>%
  #mutate(height = sapply(h_w, function(x) ifelse(length(x) >= 1, x[1], NA)),
        # width = sapply(h_w, function(x) ifelse(length(x) >= 2, x[2], NA)))  %>% 
  mutate(page_numbers = str_extract(page_numbers, "[0-9]{1,}")) %>% 
  left_join(md_655) %>% rename(type = `655_v`) %>% 
  left_join(languages) %>% 
  mutate(y2 = coalesce(y2, y1))  %>% 
  mutate(place =2) %>% # place added manually because all the same
  left_join(details, by = 'RecordID') 

# Institution table

df %>% select(RecordID, fm_id) %>% 
  left_join(md_952 %>% select(RecordID,`952_e` )) %>%
  left_join(institution_concordance) %>% select(fm_id, inst_fk) %>% 
  write_csv(paste0(Sys.Date(),'_md_institutions_table.csv'), na = '0')



# also need links table

# create LIBIS.be as a project, note down the project ID

source_links = df %>% 
  select(Title, fm_id, RecordID) %>% 
  left_join(md_856) %>% 
  mutate(project = 21)

df %>% 
  select(fm_id, RecordID, Title,alt_title, language, d1, m1, y1, d2, m2, y2,format = dim, publication_place_fk = place) %>% 
  write_csv(paste0(Sys.Date(),'_md_sources_table.csv'), na = '0')

source_links %>% 
  write_csv(paste0(Sys.Date(),'_md_published_sources_links_table.csv'), na = '0')

record_id_fm_id_concordance = df %>% select(RecordID, fm_id)

record_id_fm_id_concordance %>% 
  write_csv(paste0(Sys.Date(),'_md_mms_fm_id_concordance.csv'), na = '0')
