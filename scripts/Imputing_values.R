library(mice)
library(dplyr)

traits_clean <- read.csv("data/SUPs_traits - clean_traits.csv")

## load phylogeny

one_phylo <- readRDS("data/one_phylo.rds")

one_phylo

all_class <- read.csv("data/classes.csv")

traits_phylo <- traits_clean %>% 
  dplyr::filter(number_traits_completed %in% c(3,4)) %>% 
  left_join(all_class, by = c("scientific_name.x" = "scientific_name")) %>% 
  dplyr::filter(str_replace(scientific_name.x, " ", "_") %in% one_phylo$tip.label) %>% 
  select(scientific_name.x, dispersal_km, Migration_km, mean.hra.m2, Mass_kg, class) %>% 
  mutate(scientific_name.x = str_replace(scientific_name.x, " ", "_")) %>% 
  mutate(dispersal_km = dispersal_km + 0.0001) %>% 
  mutate(mean.hra.m2 = as.numeric(as.character(mean.hra.m2))) %>% 
  mutate(Migration_km = as.numeric(as.character(Migration_km))) %>% 
  filter(!is.na(class))


traits_phylo %>% 
  filter(is.na(dispersal_km))

traits_phylo %>% 
  filter(is.na(Migration_km))

traits_phylo %>% 
  filter(is.na(mean.hra.m2))

imp <- mice(traits_phylo, m = 5, print = FALSE, method = "cart")

imp
