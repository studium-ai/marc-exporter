library(tidyverse)

lov_omeka_file = read_csv('/Users/ghum-m-ae231206/Library/CloudStorage/OneDrive-KULeuven/Shared_data_studium/5 Collectio Academica Antiqua (through ALMA in stead of the fraction of Digitized items on Lovaniensia)/20230728_Omeka_Lovaniensia.csv')

l = read_csv('2023-12-20_lov_sources_table.csv')
s = read_csv('2023-12-20_lov_sources_links_table.csv')

lov_links = lov_omeka_file %>% 
  select(`Item URI`,`Item Type Metadata:MMS ID`)


lovaniensia_links = lov_links %>% mutate(external_id = as.character(lov_omeka_file$`Item Type Metadata:MMS ID`))%>% 
  mutate(fm_id = l$fm_id) %>%
  filter(!is.na(external_id)) %>% 
  mutate(link = paste0("https://lib.is/alma", external_id)) %>% 
  mutate(project_id = 23) %>%
  mutate(link = trimws(link)) %>% select(source_id = fm_id, project_id, link, external_id) %>% mutate(import = "Imported from 20230728_Omeka_Lovaniensia.csv")

lovaniensia_links %>% write_csv(paste0(Sys.Date(), '_lov_libis_links_table.csv'))


lov_links_export = read_csv('source_links_lov.csv', col_names = FALSE)

lov_links_export = lov_links_export %>% 
  select(X1) %>% 
  mutate(source_id = lovaniensia_links$source_id, 
         project_id = lovaniensia_links$project_id,
         link = lovaniensia_links$link, 
         external_id =lovaniensia_links$external_id,
         )


lov_links_export %>% write_csv('lov_links_for_import_fm.csv')

caa_csv = read_csv('/Users/ghum-m-ae231206/Library/CloudStorage/OneDrive-SharedLibraries-KULeuven/Suchismita Ghosh - Internship_KUL DH/caa_source_data_new.csv')

caa_lang = caa_041 %>% pivot_longer(starts_with("041")) %>% filter(!is.na(value)) %>%
  mutate(name = str_extract(name, "041_a|041_h")) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  mutate(`041_a` = as.character(`041_a`)) %>% 
  mutate(`041_h` = as.character(`041_h`)) %>% 
  select(-`NA`) %>% separate(`041_a`, into = paste0('original_language', 1:6), sep = ',') %>% 
  separate(`041_h`, into = paste0('target_language', 1:3), sep = ',') %>% 
  mutate(original_language1 = str_extract(original_language1, "[a-z]{3}"))%>% 
  mutate(original_language2 = str_extract(original_language2, "[a-z]{3}"))%>% 
  mutate(original_language3 = str_extract(original_language3, "[a-z]{3}"))%>% 
  mutate(original_language4 = str_extract(original_language4, "[a-z]{3}"))%>% 
  mutate(original_language5 = str_extract(original_language5, "[a-z]{3}"))%>% 
  mutate(original_language6 = str_extract(original_language6, "[a-z]{3}")) %>% 
  mutate(target_language1 = str_extract(target_language1, "[a-z]{3}"))%>% 
  mutate(target_language2 = str_extract(target_language2, "[a-z]{3}"))%>% 
  mutate(target_language3 = str_extract(target_language3, "[a-z]{3}")) %>% 
  mutate(target_language1  = ifelse(target_language1 == 'NULL', NA, target_language1)) 


caa_csv %>% mutate(RecordID = as.character(RecordID)) %>%
  left_join(caa_lang) %>% 
  write_csv('/Users/ghum-m-ae231206/Library/CloudStorage/OneDrive-SharedLibraries-KULeuven/Suchismita Ghosh - Internship_KUL DH/caa_source_data_new_withlanguage.csv')
