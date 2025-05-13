library(ggalluvial)


df = tibble(x = c('all', 'all', 'all'), next_x = c('found', 'missing', 'missing'), 
            node = c(NA, 'found2', 'still_missing'), next_node = c('2200', '500', '300'))

ggplot(df, aes(x = df, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node))) +
  ggsankey::geom_sankey()


ggplot(df,
       aes(y = freq,
           axis1 = result1, axis2 = result2)) +
  geom_alluvium(aes(fill = result1)) +
  geom_stratum(width = 1/12, fill = "black", color = "grey")


correct %>% select(RecordID, place_fm_id_found) %>%
  inner_join(imported_correct %>% mutate(RecordID = as.character(RecordID)) %>% 
              select(RecordID, place_fm_id_found))
