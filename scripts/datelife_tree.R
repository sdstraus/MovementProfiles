library(datelife)
library(dplyr)
library(stringr)
library(phytools)

### input taxa with class ###

all_taxa <- read.csv("data/classes.csv")


#### search names and do 1 tree with all taxa using the median method


traits_clean <- read.csv("/Users/sam/github/SUPSmods/data/cleaned_traits.csv", fileEncoding="latin1")


fish <- traits_clean %>% 
  filter(number_traits_completed %in% c(3)) %>%
  select(scientific_name = scientific_name.x) %>% 
  left_join(all_taxa) %>% 
  filter((class %in% "Actinopteri")) %>% 
  slice(1) %>% 
  select(scientific_name) %>% unlist() %>% as.character()

species_with_mig <- traits_clean %>% 
  filter(number_traits_completed %in% c(4)) %>%
  select(scientific_name = scientific_name.x) %>% 
  left_join(all_taxa) %>% 
  filter(!(class %in% "Insecta")) %>% 
  #bind_rows(shark_1) %>% 
  mutate(scientific_name = ifelse(scientific_name == "Capra hircus", "Capra falconeri", scientific_name)) %>%  ## sister species
  mutate(scientific_name = ifelse(scientific_name == "Hoplocephalus bungaroides", "Hoplocephalus bitorquatus", scientific_name)) %>% ## sister species
  mutate(scientific_name = ifelse(scientific_name == "Poecile atricapillus", "Poecile carolinensis", scientific_name)) %>% ## sister species 
  mutate(scientific_name = ifelse(scientific_name == "Mustela furo", "Mustela putorius", scientific_name)) %>% ## furo is sub-species of Mustela putorius (syn name) 
  mutate(scientific_name = ifelse(scientific_name == "Emydoidea blandingii", "Emys orbicularis", scientific_name)) %>% ## sister species 
  mutate(scientific_name = ifelse(scientific_name == "Carinascincus microlepidotus", "Proablepharus reginae", scientific_name)) %>% ## sister species 
  mutate(scientific_name = ifelse(scientific_name == "Linaria cannabina", "Acanthis flammea", scientific_name)) %>% ## sister species 
  select(scientific_name) %>% unlist() %>% as.character() %>% c(fish)

sort(species_with_mig)

all_res <- get_datelife_result(
  input = species_with_mig,
  partial = TRUE,
  use_tnrs = TRUE,
  approximate_match = TRUE,
  update_cache = FALSE,
  cache = get("opentree_chronograms"),
  get_spp_from_taxon = FALSE,
  verbose = FALSE
)


one_phylo <- datelife_result_median(all_res)

not_found <- species_with_mig[!(str_replace(species_with_mig, " ", "_") %in% one_phylo$tip.label)] %>% sort()

not_found

phylo_no_fish_4 <- ape::drop.tip(one_phylo, "Esox_lucius")

#phylo_no_fish_1 <- phytools::bind.tip(phylo_no_fish, tip.label="Cervus_canadensis", 
#                                     where=which(phylo_no_fish$tip.label=="Cervus_elaphus"), edge.length = 2)

#phylo_no_fish_2 <- phytools::bind.tip(phylo_no_fish_1, tip.label="Grus_canadensis", 
#                                      where=which(phylo_no_fish_1$tip.label=="Grus_americana"), edge.length = 2)

#phylo_no_fish_3 <- phytools::bind.tip(phylo_no_fish_2, tip.label="Deirochelys_reticularia", 
#                                      where=which(phylo_no_fish_2$tip.label=="Chrysemys_picta"), edge.length = 2)

#phylo_no_fish_4 <- phytools::bind.tip(phylo_no_fish_3, tip.label="Macropus_rufogriseus", 
#                                      where=which(phylo_no_fish_3$tip.label=="Macropus_fuliginosus"), edge.length = 2)


### renaming changed tips

phylo_no_fish_4$tip.label[phylo_no_fish_4$tip.label == "Capra_falconeri"] <- "Capra_hircus"
phylo_no_fish_4$tip.label[phylo_no_fish_4$tip.label == "Hoplocephalus_bitorquatus"] <- "Hoplocephalus_bungaroides"
phylo_no_fish_4$tip.label[phylo_no_fish_4$tip.label == "Poecile_carolinensis"] <- "Poecile_atricapillus"
phylo_no_fish_4$tip.label[phylo_no_fish_4$tip.label == "Mustela_putorius"] <- "Mustela_furo"
phylo_no_fish_4$tip.label[phylo_no_fish_4$tip.label == "Emys_orbicularis"] <- "Emydoidea_blandingii"
phylo_no_fish_4$tip.label[phylo_no_fish_4$tip.label == "Proablepharus_reginae"] <- "Carinascincus_microlepidotus"
phylo_no_fish_4$tip.label[phylo_no_fish_4$tip.label == "Acanthis_flammea"] <- "Linaria_cannabina"


phylo_no_fish_4$tip.label %>% sort()

plot_phylo_all(phylo_no_fish_4 , write = "pdf", file = "one_phylo")

A <- ape::vcv.phylo(phylo_no_fish_4)

saveRDS(phylo_no_fish_4, "data/final_phylo.rds")


