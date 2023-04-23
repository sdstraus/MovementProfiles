library(dplyr)
library(stringr)
library(taxize)
library(purrr)


traits_clean <- read.csv("/Users/sam/github/SUPSmods/data/cleaned_traits.csv", fileEncoding="latin1")

sci_names <- traits_clean %>% 
  filter(number_traits_completed %in% c(3,4)) %>% 
  select(scientific_name.x) %>% unique() %>% unlist() %>% as.character()

taxon_sps_class <- list()

for(i in 2:length(sci_names)){
  
  if(i %in% seq(2,length(sci_names), 2)) Sys.sleep(10)
  
  class_sp <- tax_name(query = sci_names[i], get = "class", db = "ncbi")
  
  taxon_sps_class[[i]] <- class_sp
}

taxon_sps_order <- list()
for(i in 2:length(sci_names)){
  
  if(i %in% seq(2,length(sci_names), 2)) Sys.sleep(5)
  
  order_sp <- tax_name(query = sci_names[i], get = "order", db = "ncbi")
  
  taxon_sps_order[[i]] <- order_sp
}

non_na_taxa_class <- taxon_sps_class %>%
  map_df(~as.data.frame(.x)) %>% 
  filter(!is.na(class))
  
na_taxa_class <- taxon_sps_class %>%
  map_df(~as.data.frame(.x)) %>% 
  filter(is.na(class)) %>% 
  select(query) %>% unlist()

taxon_sps_nas_class <- list()

for(i in 1:length(na_taxa_class)){
  
  if(i %in% seq(2,length(na_taxa_class), 2)) Sys.sleep(2)
  
  class_sp <- tax_name(query = na_taxa_class[i], get = "class", db = "itis")
  
  taxon_sps_nas_class[[i]] <- class_sp
}

taxa_sp2_class <- taxon_sps_nas_class %>%
  map_df(~as.data.frame(.x)) %>% 
  mutate(class = ifelse(query == "Lacerta agilis", "Reptilia", class)) %>%
  mutate(class = ifelse(query == "Carinascincus microlepidotus", "Reptilia", class)) %>% 
  mutate(class = ifelse(query == "Rhincodontypus", "Chondrichthyes", class)) %>% 
  mutate(class = ifelse(query == "Zootoca vivipara", "Reptilia", class)) %>% 
  select(scientific_name = query, class)

taxa_sp2_class

all_classes <- non_na_taxa_class %>% 
  select(scientific_name = query, class) %>% 
  bind_rows(taxa_sp2_class)



non_na_taxa_order <- taxon_sps_order %>%
  map_df(~as.data.frame(.x)) %>% 
  filter(!is.na(class))

na_taxa_order <- taxon_sps_order %>%
  map_df(~as.data.frame(.x)) %>% 
  filter(is.na(order)) %>% 
  select(query) %>% unlist()

taxon_sps_nas_order <- list()

for(i in 1:length(na_taxa_order)){
  
  if(i %in% seq(2,length(na_taxa_order), 2)) Sys.sleep(2)
  
  order_sp <- tax_name(query = na_taxa_order[i], get = "class", db = "itis")
  
  taxon_sps_order[[i]] <- order_sp
}


all_orders <- non_na_taxa_order %>% 
  select(scientific_name = query, order) %>% 
  bind_rows(na_taxa_order)

all_orders$query1 <- NULL
all_orders$query2 <- NULL

which(all_classes$scientific_name == "Nerodia sipeodon")#390
all_classes$class[390] <- "Reptilia"


which(all_orders$scientific_name == "Nerodia sipeodon")#202
all_orders$order[202] <- "Squamata"

which(all_orders$scientific_name == "Rhincodontypus")#369
all_orders$order[369] <- "Orectolobiformes"


all_classes_orders <- full_join(all_classes, all_orders, by = "scientific_name")

which(all_classes_orders$scientific_name == "Sylvia atricapilla")#394
all_classes_orders$class[394] <- "Aves"

write.csv(all_classes_orders, "data/classes.csv", row.names = FALSE)


all_classes %>% 
  arrange(class) %>% 
  group_by(class) %>% 
  summarise(n())

all_classes %>% 
  select(scientific_name) %>% unlist()

