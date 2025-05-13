caa_sources = read_csv('2024-02-06_caa_sources_table.csv')
caa_mms_id = read_csv('2024-02-15_caa_record_id_add_table.csv')

caa_mms_id = caa_mms_id %>% mutate(RecordID = as.character(RecordID))

caa_libis_links = caa_mms_id%>% 
  mutate(link = paste0("https://lib.is/alma", RecordID)) %>% mutate(project_id = 23)

caa_libis_links %>% write_csv(paste0(Sys.Date(), "_caa_libis_links_table.csv"))
                              