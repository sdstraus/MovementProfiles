library(googlesheets)
library(dplyr)
library(stringr)
library(taxize)
library(purrr)

MA_sheet<-gs_title(x = "SUPs_traits")

traits_clean <- gs_read(ss = MA_sheet, ws=5) 

traits_clean <- read.csv("data/SUPs_traits - clean_traits.csv")

sci_names <- traits_clean %>% 
  filter(number_traits_completed %in% c(3,4)) %>% 
  select(scientific_name.x) %>% unique() %>% unlist() %>% as.character()

taxon_sps <- list()

for(i in 2:length(sci_names)){
  
  if(i %in% seq(2,length(sci_names), 2)) Sys.sleep(2)
  
  class_sp <- tax_name(query = sci_names[i], get = "class", db = "ncbi")
  
  taxon_sps[[i]] <- class_sp
}

non_na_taxa <- taxon_sps %>%
  map_df(~as.data.frame(.x)) %>% 
  filter(!is.na(class))
  
na_taxa <- taxon_sps %>%
  map_df(~as.data.frame(.x)) %>% 
  filter(is.na(class)) %>% 
  select(query) %>% unlist()

taxon_sps_nas <- list()

for(i in 1:length(na_taxa)){
  
  if(i %in% seq(2,length(na_taxa), 2)) Sys.sleep(2)
  
  class_sp <- tax_name(query = na_taxa[i], get = "class", db = "itis")
  
  taxon_sps_nas[[i]] <- class_sp
}

taxa_sp2 <- taxon_sps_nas %>%
  map_df(~as.data.frame(.x)) %>% 
  mutate(class = ifelse(query == "Lacerta agilis", "Reptilia", class)) %>%
  mutate(class = ifelse(query == "Carinascincus microlepidotus", "Reptilia", class)) %>% 
  mutate(class = ifelse(query == "Rhincodontypus", "Chondrichthyes", class)) %>% 
  mutate(class = ifelse(query == "Zootoca vivipara", "Reptilia", class)) %>% 
  select(scientific_name = query, class)

taxa_sp2

all_classes <- non_na_taxa %>% 
  select(scientific_name = query, class) %>% 
  bind_rows(taxa_sp2)


write.csv(all_classes, "data/classes.csv", row.names = FALSE)


all_classes %>% 
  arrange(class) %>% 
  group_by(class) %>% 
  summarise(n())

all_classes %>% 
  select(scientific_name) %>% unlist()

