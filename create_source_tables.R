source('extract_marc.R')

xml_file_path = "/Users/Yann/Library/CloudStorage/OneDrive-KULeuven/Shared_data_studium/9 Thesis sheet/Theses_olduniversity_DIGCOL.xml"
caa_xml_file_path = '/Users/Yann/Library/CloudStorage/OneDrive-KULeuven/Shared_data_studium/5 Collectio Academica Antiqua (through ALMA in stead of the fraction of Digitized items on Lovaniensia)/20230728_Alma_Caa_all.xml'
md_xml_file_path = "/Users/Yann/Library/CloudStorage/OneDrive-KULeuven/Shared_data_studium/4 Magister Dixit (through ALMA of the KU Leuven libraries' Special Collections)/20230728_Alma_Lecturenotes_digital.xml"
lec_xml_file_path = "/Users/Yann/Library/CloudStorage/OneDrive-KULeuven/Shared_data_studium/5 Collectio Academica Antiqua (through ALMA in stead of the fraction of Digitized items on Lovaniensia)/20230728_Alma_Lecturenotes_all.xml"

sources_table = readxl::read_excel('/Users/Yann/Library/CloudStorage/OneDrive-KULeuven/Shared_data_studium/Sources - Yann/draft_sources_tables/sources_table_export.xlsx')
sources_table = sources_table %>% filter(!is.na(Source_ID_pk))


theses_245 = extract_marc21_data('245',xml_file_path)
theses_246 = extract_marc21_data('246',xml_file_path)
theses_264 = extract_marc21_data('264', xml_file_path)
theses_300 = extract_marc21_data('300', xml_file_path)
theses_340 = extract_marc21_data('340', xml_file_path)
theses_655 = extract_marc21_data('655', xml_file_path)
theses_700 = extract_marc21_data('700',xml_file_path)
theses_856 = extract_marc21_data('856',xml_file_path)
theses_546 = extract_marc21_data('546', xml_file_path)

caa_100 = extract_marc21_data('100',caa_xml_file_path)
caa_245 = extract_marc21_data('245',caa_xml_file_path)
caa_246 = extract_marc21_data('246',caa_xml_file_path)
caa_264 = extract_marc21_data('264', caa_xml_file_path)
caa_300 = extract_marc21_data('300', caa_xml_file_path)
caa_340 = extract_marc21_data('340', caa_xml_file_path)
caa_655 = extract_marc21_data('655', caa_xml_file_path)
caa_700 = extract_marc21_data('700',caa_xml_file_path)
caa_856 = extract_marc21_data('856',caa_xml_file_path)
caa_546 = extract_marc21_data('546', caa_xml_file_path)

md_100 = extract_marc21_data('100',md_xml_file_path)
md_245 = extract_marc21_data('245',md_xml_file_path)
md_246 = extract_marc21_data('246',md_xml_file_path)
md_264 = extract_marc21_data('264', md_xml_file_path)
md_300 = extract_marc21_data('300', md_xml_file_path)
md_340 = extract_marc21_data('340', md_xml_file_path)
md_655 = extract_marc21_data('655', md_xml_file_path)
md_700 = extract_marc21_data('700',md_xml_file_path)
md_856 = extract_marc21_data('856',md_xml_file_path)
md_546 = extract_marc21_data('546', md_xml_file_path)


lec_100 = extract_marc21_data('100',lec_xml_file_path)
lec_245 = extract_marc21_data('245',lec_xml_file_path)
lec_246 = extract_marc21_data('246',lec_xml_file_path)
lec_264 = extract_marc21_data('264', lec_xml_file_path)
lec_300 = extract_marc21_data('300', lec_xml_file_path)
lec_340 = extract_marc21_data('340', lec_xml_file_path)
lec_655 = extract_marc21_data('655', lec_xml_file_path)
lec_700 = extract_marc21_data('700',lec_xml_file_path)
lec_856 = extract_marc21_data('856',lec_xml_file_path)
lec_546 = extract_marc21_data('546', lec_xml_file_path)

alt_title = lec_246 %>% group_by(RecordID) %>% 
  summarise(Title = paste0(`246_a`, collapse = "/"))

df = lec_245 %>% 
  left_join(lec_100) %>% 
  select( RecordID, title_1 = `245_a`) %>%  
  left_join(alt_title) %>% 
  mutate(Title = coalesce(Title, title_1)) %>% select(-title_1) %>% 
  left_join(lec_264) %>%  
  separate(`264_c`, into = c('date1', 'date2'), sep = '-') %>% 
  select(RecordID, Title, `sourcesPublication_to_places::name` = `264_a`,date1, date2) %>%
  mutate(d1 = 0, m1 = 0, y1 = str_extract(date1, "[0-9]{4}"))%>%
  mutate(d2 = 0, m2 = 0, y2 = str_extract(date2, "[0-9]{4}")) %>% 
  mutate(publ_date_uncertain = ifelse(str_detect(date1, "\\?"), 'yes', 'no')) %>% 
  left_join(lec_300 %>% filter(!is.na(`300_c`))) %>% 
  select(RecordID, Title, `sourcesPublication_to_places::name`, d1, m1, y1, d2, m2, y2, publ_date_uncertain, page_numbers = `300_a`, dim = `300_c`) %>% 
  mutate(h_w = str_extract_all(dim, "[0-9]{3}")) %>%
  mutate(height = h_w[[1]][1], width = h_w[[1]][2]) %>% 
  left_join(lec_340 %>% filter(!is.na(`340_e`))) %>% 
  rename(material = `340_e`) %>% select(-`340_a`, -dim, -h_w) %>% mutate(page_numbers = str_extract(page_numbers, "[0-9]{1,}")) %>% 
  left_join(lec_655) %>% rename(type = `655_v`) %>% 
  mutate(y2 = coalesce(y2, y1)) %>% 
  left_join(lec_546) %>% 
  rename(language = `546_a`) %>% 
  mutate(date_concat = ifelse(y1 != y2, paste0(y1, " - ", y2), y1)) 
 
df_list = list(sources_table, df)

lecture_notes_sources = df_list %>% data.table::rbindlist(use.names = TRUE, fill = TRUE) %>% select(-RecordID) 

lecture_notes_sources$Source_ID_pk[14:615] = 1571:(1571 + (615-14))

lecture_notes_sources  %>% write_csv('/Users/Yann/Library/CloudStorage/OneDrive-KULeuven/Shared_data_studium/Sources - Yann/draft_sources_tables/caa_lecture_notes_sources.csv')

alt_title = md_246 %>% group_by(RecordID) %>% 
  summarise(Title = paste0(`246_a`, collapse = "/"))

df = md_245 %>% 
  left_join(md_100) %>% 
  select( RecordID, title_1 = `245_a`) %>%  
  left_join(alt_title) %>% 
  mutate(Title = coalesce(Title, title_1)) %>% select(-title_1) %>% 
  left_join(md_264) %>%  
  separate(`264_c`, into = c('date1', 'date2'), sep = '-') %>% 
  select(RecordID, Title, `sourcesPublication_to_places::name` = `264_a`,date1, date2) %>%
  mutate(d1 = 0, m1 = 0, y1 = str_extract(date1, "[0-9]{4}"))%>%
  mutate(d2 = 0, m2 = 0, y2 = str_extract(date2, "[0-9]{4}")) %>% 
  mutate(publ_date_uncertain = ifelse(str_detect(date1, "\\?"), 'yes', 'no')) %>% 
  left_join(md_300 %>% filter(!is.na(`300_c`))) %>% 
  select(RecordID, Title, `sourcesPublication_to_places::name`, d1, m1, y1, d2, m2, y2, publ_date_uncertain, page_numbers = `300_a`, dim = `300_c`) %>% 
  mutate(h_w = str_extract_all(dim, "[0-9]{3}")) %>%
  mutate(height = h_w[[1]][1], width = h_w[[1]][2]) %>% 
  left_join(md_340 %>% filter(!is.na(`340_e`))) %>% 
  rename(material = `340_e`) %>% select(-`340_a`, -dim, -h_w) %>% mutate(page_numbers = str_extract(page_numbers, "[0-9]{1,}")) %>% 
  left_join(md_655) %>% rename(type = `655_v`) %>% 
  mutate(y2 = coalesce(y2, y1)) %>% 
  left_join(md_546) %>% 
  rename(language = `546_a`)  %>% 
  mutate(date_concat = ifelse(y1 != y2, paste0(y1, " - ", y2), y1))


df_list = list(sources_table, df)

md_notes_sources = df_list %>% data.table::rbindlist(use.names = TRUE, fill = TRUE) 

md_notes_sources$Source_ID_pk[5:nrow(md_notes_sources)] = 1560:(1560 + (nrow(md_notes_sources)-5))

md_notes_sources  %>% write_csv('/Users/Yann/Library/CloudStorage/OneDrive-KULeuven/Shared_data_studium/Sources - Yann/draft_sources_tables/md_lecture_notes_sources.csv', na = "0")



alt_title = caa_246 %>% group_by(RecordID) %>% 
  summarise(Title = paste0(`246_a`, collapse = "/"))

df = caa_245 %>% 
  left_join(caa_100) %>% 
  select( RecordID, title_1 = `245_a`) %>%  
  left_join(alt_title) %>% 
  mutate(Title = coalesce(Title, title_1)) %>% select(-title_1) %>% 
  left_join(caa_264) %>%  
  separate(`264_c`, into = c('date1', 'date2'), sep = '-') %>% 
  select(RecordID, Title, `sourcesPublication_to_places::name` = `264_a`,date1, date2) %>%
  mutate(d1 = 0, m1 = 0, y1 = str_extract(date1, "[0-9]{4}"))%>%
  mutate(d2 = 0, m2 = 0, y2 = str_extract(date2, "[0-9]{4}")) %>% 
  mutate(publ_date_uncertain = ifelse(str_detect(date1, "\\?"), 'yes', 'no')) %>% 
  left_join(caa_300 %>% filter(!is.na(`300_c`))) %>% 
  select(RecordID, Title, `sourcesPublication_to_places::name`, d1, m1, y1, d2, m2, y2, publ_date_uncertain, page_numbers = `300_a`, dim = `300_c`) %>% 
  mutate(h_w = str_extract_all(dim, "[0-9]{3}")) %>%
  mutate(height = h_w[[1]][1], width = h_w[[1]][2]) %>% 
  left_join(caa_340 %>% filter(!is.na(`340_e`))) %>% 
  rename(material = `340_e`) %>% select(-`340_a`, -dim, -h_w) %>% mutate(page_numbers = str_extract(page_numbers, "[0-9]{1,}")) %>% 
  left_join(caa_655) %>% rename(type = `655_v`) %>% 
  mutate(y2 = coalesce(y2, y1)) %>% 
  left_join(caa_546) %>% 
  rename(language = `546_a`) %>% 
  mutate(date_concat = ifelse(y1 != y2, paste0(y1, " - ", y2), y1))


df_list = list(sources_table, df)

caa_notes_sources = df_list %>% data.table::rbindlist(use.names = TRUE, fill = TRUE) %>% select(-RecordID) 


caa_notes_sources$Source_ID_pk[14:nrow(caa_notes_sources)] = 1571:(1571 + (nrow(caa_notes_sources)-14))

caa_notes_sources  %>% write_csv('/Users/Yann/Library/CloudStorage/OneDrive-KULeuven/Shared_data_studium/Sources - Yann/draft_sources_tables/caa_sources.csv')

alt_title = theses_246 %>% group_by(RecordID) %>% 
  summarise(Title = paste0(`246_a`, collapse = "/"))

df = theses_245 %>% 
  select( RecordID, title_1 = `245_a`) %>%  
  left_join(alt_title) %>% 
  mutate(Title = coalesce(Title, title_1)) %>% select(-title_1) %>% 
  left_join(theses_264) %>%  
  separate(`264_c`, into = c('date1', 'date2'), sep = '-') %>% 
  select(RecordID, Title, `sourcesPublication_to_places::name` = `264_a`,date1, date2) %>%
  mutate(d1 = 0, m1 = 0, y1 = str_extract(date1, "[0-9]{4}"))%>%
  mutate(d2 = 0, m2 = 0, y2 = str_extract(date2, "[0-9]{4}")) %>% 
  mutate(publ_date_uncertain = ifelse(str_detect(date1, "\\?"), 'yes', 'no')) %>% 
  left_join(theses_300 %>% filter(!is.na(`300_c`))) %>% 
  select(RecordID, Title, `sourcesPublication_to_places::name`, d1, m1, y1, d2, m2, y2, publ_date_uncertain, page_numbers = `300_a`, dim = `300_c`) %>% 
  mutate(h_w = str_extract_all(dim, "[0-9]{3}")) %>%
  mutate(height = h_w[[1]][1], width = h_w[[1]][2]) %>% 
  left_join(theses_340 %>% filter(!is.na(`340_e`))) %>% 
  rename(material = `340_e`) %>% select(-`340_a`, -dim, -h_w) %>% mutate(page_numbers = str_extract(page_numbers, "[0-9]{1,}")) %>% 
  left_join(theses_655) %>% rename(type = `655_v`) %>% 
  mutate(y2 = coalesce(y2, y1)) %>% 
  left_join(theses_546) %>% 
  rename(language = `546_a`) 


df_list = list(sources_table, df)

theses_notes_sources = df_list %>% data.table::rbindlist(use.names = TRUE, fill = TRUE) %>% select(-RecordID) 


theses_notes_sources$Source_ID_pk[14:nrow(theses_notes_sources)] = 1571:(1571 + (nrow(theses_notes_sources)-14))

theses_notes_sources  %>% write_csv('/Users/Yann/Library/CloudStorage/OneDrive-KULeuven/Shared_data_studium/Sources - Yann/draft_sources_tables/theses_lecture_notes_sources.csv')

