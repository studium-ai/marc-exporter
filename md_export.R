next_fm_id = 6355

next_project_id = 23

next_places_id = 9232


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


dates2 = md_001008  %>% 
  mutate(dates = substr(tag_008,8, 15)) %>% 
  mutate(y1 = str_extract(dates, "[0-9]{4}"))%>% 
  mutate(y2 = str_extract(dates, "[0-9]{4}$")) %>% 
  mutate(y2 = coalesce(y2, y1)) %>% 
  mutate(d1 = 0, m1 = 0)%>%
  mutate(d2 = 0, m2 = 0) %>% 
  left_join(dates %>% select(RecordID, publ_date_uncertain),by =c('tag_001' = 'RecordID')) %>% 
  mutate(publ_date_uncertain = ifelse(publ_date_uncertain == 'yes', 'yes', "")) %>% 
  mutate(publ_date_uncertain = replace_na(publ_date_uncertain, ""))

type = md_902 %>% 
  distinct(RecordID, .keep_all = TRUE) %>% 
  select(RecordID, `902_r`) %>%
  mutate(type = tolower(`902_r`)) %>% 
  select(-`902_r`) %>% mutate(type = ifelse(type == 'book', 'printed', type))

df = md_245 %>% 
  filter(!RecordID %in% caa_001008$tag_001) %>% 
  mutate(fm_id = next_fm_id:((next_fm_id-1) + nrow(.))) %>%
  left_join(md_100) %>% 
  select(RecordID, fm_id, Title = `245_a`) %>%  
  left_join(alt_title) %>% 
  left_join(dates2, by =c('RecordID' = 'tag_001')) %>% 
  select(RecordID, fm_id, Title,alt_title,d1, m1, y1, d2, m2, y2, publ_date_uncertain)%>% 
  left_join(md_300 %>% filter(!is.na(`300_c`))) %>% 
  select(RecordID, fm_id, Title,alt_title, d1, m1, y1, d2, m2, y2, publ_date_uncertain, page_numbers = `300_a`, dim = `300_c`) %>% 
  #mutate(h_w = str_extract_all(dim, "[0-9]{3}"))  %>%
  #mutate(height = sapply(h_w, function(x) ifelse(length(x) >= 1, x[1], NA)),
        # width = sapply(h_w, function(x) ifelse(length(x) >= 2, x[2], NA)))  %>% 
  mutate(page_numbers = str_extract(page_numbers, "[0-9]{1,}")) %>% 
  left_join(languages) %>% 
  mutate(y2 = coalesce(y2, y1))  %>% 
  mutate(place =2) %>% # place added manually because all the same
  left_join(details, by = 'RecordID') %>% left_join(type) %>% 
  mutate(alt_title = replace_na(alt_title, ""))%>% 
  mutate(import = "Imported from 20230728_113119_md_ingest_alma_lecture_notes.xml") 

# Institution table

df %>% 
  select(RecordID, fm_id) %>% 
  left_join(md_952 %>% select(RecordID,`952_e` )) %>%
  left_join(institution_concordance) %>% View()
  select(fm_id, inst_fk) %>% 
  mutate(institution_role = 'organizing institution') %>% 
  mutate(import = "Imported from 20230728_113119_md_ingest_alma_lecture_notes.xml") %>% 
  write_csv(paste0(Sys.time(),'_md_institutions_table.csv'), na = '0')



# also need links table


df %>% 
  select(fm_id, RecordID, Title,alt_title, language, d1, m1, y1, d2, m2, y2,format = dim, publication_place_fk = place, publ_date_uncertain, type, import) %>%
  write_csv(paste0(Sys.Date(),'_md_sources_table.csv'))


record_id_fm_id_concordance = df %>% select(RecordID, fm_id)

record_id_fm_id_concordance %>% 
  write_csv(paste0(Sys.Date(),'_md_mms_fm_id_concordance.csv'), na = '0')


source_links = df %>% select(fm_id, RecordID) %>% mutate(project =1)


source_links %>% 
  mutate(import = "Imported from 20230728_113119_md_ingest_alma_lecture_notes.xml") %>% 
  write_csv(paste0(Sys.Date(),'_md_source_links_table.csv'), na = '0')


## Call numbers 



md_544 %>% 
  select(RecordID, `544_b`) %>% 
  full_join(md_852 %>% select(RecordID,`852_h`)) %>% 
  mutate(call_no = coalesce(`544_b`,`852_h`)) %>% 
  left_join(df %>% select(fm_id, RecordID)) %>% select(fm_id, call_no) %>% 
  #mutate(call_no = paste0("Call number: ", call_no))%>% 
  mutate(project_id = 1) %>% 
  write_csv(paste0(Sys.Date(),'_md_callno_add_table.csv'))
  
