next_namvar_id = 81460

namvar_last = read_csv('namvar.csv', col_names = FALSE) %>% mutate(type_for_join = 'last')
namvar_given = read_csv('namvar_given.csv', col_names = FALSE)%>% mutate(type_for_join = 'given')

namvar_given = namvar_given %>% distinct(X3, .keep_all = TRUE)

namvar_given %>% 
  mutate(X3 = trimws(X3)) %>% 
  count(X3) %>% arrange(desc(n)) %>% View()



all_namvar = namvar_last %>% rbind(namvar_given)

last = theses_descriptors_for_export %>% 
  filter(type == 'last')  %>%
  ungroup() %>% 
  mutate(value = trimws(value)) %>% 
  distinct(value)

last %>% 
  left_join(namvar, by = c('value' = 'X3')) %>% 
  filter(is.na(X2)) %>% select(value)

given = theses_descriptors_for_export %>% 
  filter(type %in% c('first', 'middle'))

given %>% 
  left_join(namvar_given, by = c('value' = 'X3')) %>% View()
  filter(is.na(X2)) %>% 
  count(psatt_id) %>% arrange(desc(n))
  
  
theses_descriptors_for_export %>% 
  mutate(type_for_join = ifelse(type == 'last', 'last',
                                ifelse(type %in% c('first', 'middle'), 'given', 'other'))) %>%
  left_join(all_namvar, by = c(c('value' = 'X3'), 'type_for_join')) %>% View()



missing_given = given %>% ungroup() %>% 
  left_join(namvar_given, by = c('value' = 'X3')) %>% 
filter(is.na(X2)) %>% distinct(value) %>% mutate(type = 'given')

missing_last = last %>% 
  left_join(namvar, by = c('value' = 'X3')) %>% 
  filter(is.na(X2)) %>% distinct(value) %>% mutate(type = 'last')

namvar_new = rbind(missing_given, missing_last) %>% distinct(value, type) %>% 
  mutate(namvar_id = next_namvar_id:((next_namvar_id-1) + nrow(.))) %>% mutate(nam_id = 0) %>% 
  select(X1 = namvar_id, X2 = nam_id, X3 = value, type_for_join = type)

# this gets uploaded as new NamVar

all_namvar = rbind(all_namvar, namvar_new)
