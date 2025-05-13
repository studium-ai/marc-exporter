d1 = imported_no_place_found %>% select(fm_place_id = place_fm_id_found, wikidata_ur)

d2 = imported_place_extracted_not_found %>% select(fm_place_id, wikidata_ur = uri)

rbind(d1, d2) %>% 
  filter(!is.na(wikidata_ur)) %>% 
  distinct(fm_place_id, wikidata_ur)
