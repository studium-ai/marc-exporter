url_stem = "https://www.kuleuven.be/lectio/magisterdixit/solr-search?q=%2A&page="

library(jsonlite)
library(httr)
library(rvest)

scraper = function(url_stem, num){
  
  url = paste0(url_stem,num)
  
  website1 <- read_html(url)
  
  links = website1 %>%
    html_nodes(".result-title") %>%
    html_attr("href")
  
  return(links)
}

scraper2 = function(url){
  
  website1 <- read_html(url)
  
  links = website1 %>%
    html_nodes("a") %>% html_attrs()
  
  links = links[[6]][2]
  
  return(links)
}


all_links = map(1:57, scraper, url_stem = url_stem)

all_items = all_links %>% unlist() 

all_items = paste0("https://www.kuleuven.be", all_items)

all_iiif = map(all_items, scraper2)

all_iiif_u = unlist(all_iiif)

names(all_iiif_u) = all_items

all_iiif_u_df = all_iiif_u %>% 
  as_tibble() %>% 
  mutate(url = all_items)

all_iiif_u_df = all_iiif_u_df %>% mutate(id = str_extract(value, "IE[0-9]{1,}"))

md_concordance = readxl::read_excel('MD_concordance.xlsx')



md_concordance %>% 
  distinct(`IE PID`, .keep_all =  TRUE) %>% 
  select(`MMS ID`, `IE PID`) %>% distinct(`MMS ID`,`IE PID`) %>% 
  left_join(all_iiif_u_df, by = c('IE PID' = 'id')) %>% select(-`IE PID`) %>% filter(is.na(value)) %>% pull(`MMS ID`)
  write_csv(paste0(Sys.Date(), "md_mms_omeka_ids.csv"))
  
md_concordance %>% 
  distinct(`IE PID`, .keep_all =  TRUE) %>% 
  select(`MMS ID`, `IE PID`) %>%  
#left_join(all_iiif_u_df, by = c('IE PID' = 'id')) %>%  select(-`IE PID`) %>% 
  full_join(md_001008, by = c('MMS ID' = 'tag_001')) %>% View()


md_source_links = read_csv('2024-02-15_md_source_links_table.csv')

md_mms_omeka = md_001008 %>% distinct(tag_001) %>% left_join(md_concordance %>% 
  distinct(`IE PID`, .keep_all =  TRUE) %>% 
  select(`MMS ID`, `IE PID`) %>% 
    distinct(`MMS ID`,`IE PID`), by = c('tag_001' = 'MMS ID'))  %>%
  left_join(all_iiif_u_df, by = c('IE PID' = 'id'), na_matches = 'never') 


md_mms_omeka %>% 
  write_csv(paste0(Sys.Date(), "md_mms_omeka_ids.csv"))

md_source_links = md_source_links %>% mutate(RecordID = as.character(RecordID))

md_source_links_table = md_source_links %>% left_join(md_mms_omeka, by = c('RecordID' = 'tag_001')) %>% select(fm_id, url) %>% mutate(project =1)
  

md_source_links_table %>% 
  mutate(import = paste0(Sys.Date(), "_fm_id_md_mms_omeka_ids.csv")) %>% 
  write_csv(paste0(Sys.Date(), "_fm_id_md_mms_omeka_ids.csv")) 

# 570 Omeka IDs
# 576 MMS IDs in concordance
# 569 Omeka IDs found in the concordance through IE numbers.
# 1 Omeka ID is not in concordance: 998151430101488
# 7 in concordance which don't have Omeka IDs:
# 599 MMS IDs in MARC21 records
# 30 in MARC records not in Omeka
# 23 in MARC records not in concordance

