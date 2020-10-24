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

A <- ape::vcv.phylo(one_phylo)



bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (1|scientific_name.x)) + Gamma(link = "log")
bf_home_range <- bf(mean.hra.m2 ~ log(Mass_kg) + (1|scientific_name.x)) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) + (1|scientific_name.x)) + hurdle_gamma(link = 'log')


mod4_prior <- get_prior(bf_dispersal + bf_home_range + bf_migration + set_rescor(FALSE), cov_ranef = list(scientific_name.x = A),
                        data = traits_phylo)

mod4_prior$prior[mod4_prior$prior == "gamma(0.01, 0.01)"] <- "gamma(0.1, 0.1)"

mod4_prior$prior[mod4_prior$class == "Intercept"][2] <- "normal(2,20)"

mod4_prior$prior[mod4_prior$class == "Intercept"][3] <- "normal(11,20)"

mod4_prior$prior[mod4_prior$class == "Intercept"][4] <- "normal(1,20)"

mod4_prior$prior[mod4_prior$prior == "student_t(3, 0, 10)"] <- "student_t(3, 0, 5)"

mod4.2 <- brm(
  bf_dispersal + bf_home_range + bf_migration + set_rescor(FALSE), cov_ranef = list(scientific_name.x = A),
  data = traits_phylo, chains = 4, cores = 4, iter = 100,
  control = list(adapt_delta = 0.995, max_treedepth = 15)
)

saveRDS(mod4.2, "mod4.2.rds")