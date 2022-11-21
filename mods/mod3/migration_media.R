library(brms)
library(stringr)
library(dplyr)

### load traits
######## googlesheets sucks now :(

traits_clean <- read.csv("../data/SUPs_traits - clean_traits.csv", fileEncoding="latin1")

## load phylogeny

one_phylo <- readRDS("../data/final_phylo.rds")

#one_phylo

all_class <- read.csv("../data/classes.csv")

traits_phylo <- traits_clean %>% 
  dplyr::filter(number_traits_completed ==4) %>% 
  left_join(all_class, by = c("scientific_name.x" = "scientific_name")) %>% 
  filter(!(class %in% "Insecta")) %>% 
  #bind_rows(shark_1) %>% 
  dplyr::filter(str_replace(scientific_name.x, " ", "_") %in% one_phylo$tip.label) %>% 
  select(scientific_name.x, dispersal_km, Migration_km, mean.hra.m2, Mass_kg, class, diet_broadest_cat, media_simplified, geographic_range_km2) %>% 
  mutate(scientific_name.x = str_replace(scientific_name.x, " ", "_")) %>% 
  mutate(dispersal_km = dispersal_km + 0.0001) %>% 
  mutate(mean.hra.m2 = as.numeric(as.character(mean.hra.m2))) %>% 
  mutate(Migration_km = as.numeric(as.character(Migration_km))) %>% 
  mutate(media_simplified = as.factor(as.character(media_simplified)))

traits_phylo$class <- as.character(traits_phylo$class)
which(is.na(traits_phylo$class)) #42, Lontra canadensis
traits_phylo$class[42] <- "Mammalia"

# trim down factor levels
traits_phylo$media_migration[which(traits_phylo$media_migration =="water")] <- "aquatic"
traits_mig<- traits_phylo %>% 
  filter(media_migration == "air" | media_migration == "land" | media_migration == "aquatic"| media_migration == "marine")
levels(traits_mig$media_migration)
traits_mig$media_migration <- factor(traits_mig$media_migration, levels = c("air", "aquatic", "land", "marine"))


bf_migration <- bf(Migration_km ~ log(Mass_kg) + (log(Mass_kg)|p|media_migration)) + hurdle_gamma(link = 'log')
migration_media <- brm(bf_migration, data = traits_mig,
                       control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 3000)
saveRDS(migration_media, "migration_media_new.rds")

