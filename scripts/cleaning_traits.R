library(stringr)

traits_raw <- gs_read(ss = MA_sheet, ws=1) %>% 
  filter(!is.na(scientific_name))

traits_raw %>% 
  head()

sort(traits_raw$scientific_name)

raw <- traits_raw$scientific_name

#lower cap
low_cap <- tolower(raw)

# remove underscore

rem_und <- str_replace(low_cap, "_", " ")

# trim ends

trim_removed <- str_trim(rem_und)

# collapse genus-species

collaped_gen <- str_replace(trim_removed, " ", "")

## finding unique species

dictionary <- data_frame(scientific_name = raw, clean_name = trim_removed, collapsed_name = collaped_gen)

unique_genus_species <- unique(collaped_gen)

###

traits_raw$clean_name <- trim_removed
traits_raw$collapsed_name <- collaped_gen


duplicated(traits_raw$collapsed_name)


traits_raw %>% 
  select(collapsed_name)

exm <- traits_raw %>% 
  select(clean_name, collapsed_name, mean.hra.m2, dispersal_km, Migration_km, Mass_kg) %>% 
  filter(collapsed_name == 'alcesalces')


#### Migration #####

## migration check duplicates ###

species_to_check_migration <- traits_raw %>% 
  select(collapsed_name, Migration_km) %>% 
  mutate(Migration_km = as.numeric(Migration_km)) %>% 
  filter(!is.na(Migration_km)) %>% 
  unique() %>% 
  group_by(collapsed_name) %>% 
  summarise(n = n()) %>% 
  filter(n>1) %>%  as.data.frame()

conflict_migration <- traits_raw %>%
  select(scientific_name, collapsed_name, Taxon, Migration_km, Migration_source) %>% 
  filter(collapsed_name %in% species_to_check_migration$collapsed_name) %>% 
  arrange(collapsed_name) 

### assume we will pick the most recent database for each conflict 

conflict_filter_unique_source <- conflict_migration %>%
  filter(!(Migration_source %in% c("Wennerberg_2001", "Arzel_2015", "Trumbo_et_al_2012"))) 

year <- as.numeric(str_extract(conflict_filter_unique_source$Migration_source, "\\d+"))

conflict_resolved_migration <- conflict_filter_unique_source %>% 
  mutate(source_year = year) %>% 
  filter(!is.na(Migration_km) & !is.na(Migration_source)) %>%
  group_by(collapsed_name) %>%
  do( data.frame(with(data=., .[order(source_year),] )) ) %>%
  slice(n()) %>% 
  ungroup() %>% 
  select(-source_year) %>% 
  mutate(Migration_km = as.numeric(Migration_km))
  
## unique migration table

species_unique_migration <- traits_raw %>% 
  select(collapsed_name, Migration_km) %>% 
  mutate(Migration_km = as.numeric(Migration_km)) %>% 
  filter(!is.na(Migration_km)) %>% 
  unique() %>% 
  group_by(collapsed_name) %>% 
  summarise(n = n()) %>% 
  filter(n == 1) %>%  as.data.frame()

migration_unique <- traits_raw %>%
  select(scientific_name, collapsed_name, Taxon, Migration_km, Migration_source) %>% 
  filter(collapsed_name %in% species_unique_migration$collapsed_name) %>% 
  mutate(Migration_km = as.numeric(Migration_km)) %>% 
  filter(!is.na(Migration_km)) %>% 
  unique()

migration_traits <- bind_rows(conflict_resolved_migration, migration_unique)


######## home range size ##########

### homerange check duplicates

species_to_check_home_range <- traits_raw %>% 
  select(collapsed_name, mean.hra.m2) %>% 
  mutate(mean.hra.m2 = as.numeric(mean.hra.m2)) %>% 
  filter(!is.na(mean.hra.m2)) %>% 
  unique() %>% 
  group_by(collapsed_name) %>% 
  summarise(n = n()) %>% 
  filter(n>1) 

conflict_home_range <- traits_raw %>% 
  select(scientific_name, collapsed_name, Taxon, mean.hra.m2, source_hra) %>% 
  filter(collapsed_name %in% species_to_check_home_range$collapsed_name) %>% 
  arrange(collapsed_name) %>% 
  mutate(source_hra = ifelse(source_hra == 'Trochet_etal', 'Trochet_etal_2014', source_hra))

year <- as.numeric(str_extract(conflict_home_range$source_hra, "\\d+"))

### for Perry_garland we will average them. 

Perry_garland_home_range <- conflict_home_range %>% 
  filter(!is.na(mean.hra.m2) & !is.na(source_hra)) %>% 
  filter(source_hra == "Perry_Garland_2002") %>% 
  mutate(mean.hra.m2 = as.numeric(mean.hra.m2)) %>% 
  group_by(scientific_name, collapsed_name, Taxon, source_hra) %>% 
  summarise(mean.hra.m2 = mean(mean.hra.m2)) %>% 
  mutate(source_year = 2002)

conflict_resolved_home_range <- conflict_home_range %>% 
  mutate(source_year =  year) %>% 
  mutate(mean.hra.m2 = as.numeric(mean.hra.m2)) %>% 
  filter(!is.na(mean.hra.m2) & !is.na(source_hra)) %>% 
  filter(!(collapsed_name %in% Perry_garland_home_range$collapsed_name)) %>% 
  bind_rows(Perry_garland_home_range) %>%
  group_by(collapsed_name) %>%
  do( data.frame(with(data=., .[order(source_year),] )) ) %>%
  slice(n()) %>% 
  ungroup() %>% 
  select(-source_year)
  

## unique species home range


species_unique_home_range <- traits_raw %>% 
  select(collapsed_name, mean.hra.m2) %>% 
  mutate(mean.hra.m2 = as.numeric(mean.hra.m2)) %>% 
  filter(!is.na(mean.hra.m2)) %>% 
  unique() %>% 
  group_by(collapsed_name) %>% 
  summarise(n = n()) %>% 
  filter(n==1) 

home_range_unique <- traits_raw %>%
  select(scientific_name, collapsed_name, Taxon, mean.hra.m2, source_hra) %>% 
  filter(collapsed_name %in% species_unique_home_range$collapsed_name) %>% 
  mutate(mean.hra.m2 = as.numeric(mean.hra.m2)) %>% 
  filter(!is.na(mean.hra.m2)) %>% 
  unique()

home_range_traits <- bind_rows(conflict_resolved_home_range, home_range_unique)

######## dispersal ########

### dispersal check duplicates

species_to_check_dispersal <- traits_raw %>% 
  select(collapsed_name, dispersal_km) %>% 
  mutate(dispersal_km = as.numeric(dispersal_km)) %>% 
  filter(!is.na(dispersal_km)) %>% 
  unique() %>% 
  group_by(collapsed_name) %>% 
  summarise(n = n()) %>% 
  filter(n>1) 

conflict_dispersal <- traits_raw %>% 
  select(scientific_name, collapsed_name, Taxon, dispersal_km, source_dispersal) %>% 
  filter(collapsed_name %in% species_to_check_dispersal$collapsed_name) %>% 
  arrange(collapsed_name) %>% 
  filter(!is.na(dispersal_km) & !is.na(source_dispersal)) 

#selecting whotmee and orme which are the second reference separated by ;

clean_ref_1 <- sapply(str_split(conflict_dispersal$source_dispersal, ";"), FUN = function(x) x[length(x)])

# adding 2008 to prugh referece 

clean_ref_2 <- str_replace(clean_ref_1, "Prugh", "Prugh_2008")

table(clean_ref_2)

year <- sapply(str_extract_all(clean_ref_2, "\\d+"), FUN = function(x) max(as.numeric(x)))

conflict_resolved_dispersal <- conflict_dispersal %>% 
  mutate(source_year = year) %>% 
  mutate(dispersal_km = as.numeric(dispersal_km)) %>% 
  filter(!is.na(dispersal_km) & !is.na(source_dispersal)) %>% 
  group_by(collapsed_name) %>%
  do( data.frame(with(data=., .[order(source_year),] )) ) %>%
  slice(n()) %>% 
  ungroup() %>% 
  select(-source_year)
  


## unique species dispersal


species_unique_dispersal <- traits_raw %>% 
  select(collapsed_name, dispersal_km) %>% 
  mutate(dispersal_km = as.numeric(dispersal_km)) %>% 
  filter(!is.na(dispersal_km)) %>% 
  unique() %>% 
  group_by(collapsed_name) %>% 
  summarise(n = n()) %>% 
  filter(n==1) 

dispersal_unique <- traits_raw %>%
  select(scientific_name, collapsed_name, Taxon, dispersal_km, source_dispersal) %>% 
  filter(collapsed_name %in% species_unique_dispersal$collapsed_name) %>% 
  mutate(dispersal_km = as.numeric(dispersal_km)) %>% 
  filter(!is.na(dispersal_km)) %>% 
  unique() %>% 
  group_by(collapsed_name) %>% 
  slice(1) %>% 
  ungroup()

dispersal_traits <- bind_rows(conflict_resolved_dispersal, dispersal_unique)

#### Body mass (kg) ####

### dispersal check duplicates

species_to_check_bodymass <- traits_raw %>% 
  select(collapsed_name, Mass_kg) %>% 
  mutate(Mass_kg = as.numeric(Mass_kg)) %>% 
  filter(!is.na(Mass_kg)) %>% 
  unique() %>% 
  group_by(collapsed_name) %>% 
  summarise(n = n()) %>% 
  filter(n>1) 

conflict_bodymass <- traits_raw %>% 
  select(scientific_name, collapsed_name, Taxon, Mass_kg) %>% 
  filter(collapsed_name %in% species_to_check_bodymass$collapsed_name) %>% 
  arrange(collapsed_name) %>% 
  filter(!is.na(Mass_kg)) 

# average body mass measures

conflict_resolved_bodymass <- conflict_bodymass %>% 
  mutate(Mass_kg = as.numeric(Mass_kg)) %>% 
  filter(!is.na(Mass_kg)) %>% 
  group_by(collapsed_name) %>%
  summarize(Mass_kg = mean(Mass_kg))


## unique species body mass

species_unique_bodymass <- traits_raw %>% 
  select(collapsed_name, Mass_kg) %>% 
  mutate(Mass_kg = as.numeric(Mass_kg)) %>% 
  filter(!is.na(Mass_kg)) %>% 
  unique() %>% 
  group_by(collapsed_name) %>% 
  summarise(n = n()) %>% 
  filter(n==1) 

bodymass_unique <- traits_raw %>%
  select(scientific_name, collapsed_name, Taxon, Mass_kg) %>% 
  filter(collapsed_name %in% species_unique_bodymass$collapsed_name) %>% 
  mutate(Mass_kg = as.numeric(Mass_kg)) %>% 
  filter(!is.na(Mass_kg)) %>% 
  unique() %>% 
  group_by(collapsed_name) %>% 
  slice(1) %>% 
  ungroup()

bodymass_traits <- bind_rows(conflict_resolved_bodymass, bodymass_unique)

##### merge three traits ##########

full_traits <- dispersal_traits %>% 
  select(scientific_name, collapsed_name, dispersal_km, source_dispersal) %>% 
  full_join(migration_traits, by = "collapsed_name") %>% 
  select(-Taxon, -scientific_name.y) %>% 
  full_join(home_range_traits,  by = "collapsed_name") %>% 
  select(-scientific_name, -Taxon) %>% 
  full_join(bodymass_traits,  by = "collapsed_name") %>% 
  select(-scientific_name, -Taxon) 


number_traits_completed <- (!is.na(full_traits$dispersal_km)) + 
  (!is.na(full_traits$Migration_km)) + 
  (!is.na(full_traits$mean.hra.m2))+ 
  (!is.na(full_traits$Mass_kg))

full_traits$number_traits_completed <- number_traits_completed

length(which(full_traits$number_traits_completed == 4))

export_traits <- full_traits %>% 
  arrange(desc(number_traits_completed))

write.csv(export_traits, "../data/clean_traits.csv", row.names = FALSE)
