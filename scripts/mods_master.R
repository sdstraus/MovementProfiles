library(brms)
library(stringr)
library(dplyr)
library(rstan)

### load traits
######## googlesheets sucks now :(
# setwd("/Users/sam/github/SUPSmods/scripts")
# traits_clean <- read.csv("../data/Traits_Final.csv", fileEncoding="latin1")
# 
# ## load phylogeny
# 
# one_phylo <- readRDS("../data/final_phylo.rds")
# 
# #one_phylo
# 
# all_class <- read.csv("../data/classes.csv")
# 
# traits_phylo <- traits_clean %>% 
#   dplyr::filter(number_traits_completed ==4) %>% 
#   left_join(all_class, by = c("scientific_name.x" = "scientific_name")) %>% 
#   filter(!(class %in% "Insecta")) %>% 
#   #bind_rows(shark_1) %>% 
#   dplyr::filter(str_replace(scientific_name.x, " ", "_") %in% one_phylo$tip.label) %>% 
#   select(scientific_name.x, dispersal_km, Migration_km, mean.hra.m2, Mass_kg, 
#          class, order, diet_broadest_cat, media_simplified, media_dispersal, 
#          media_migration, media_foraging) %>% 
#   mutate(scientific_name.x = str_replace(scientific_name.x, " ", "_")) %>% 
#   mutate(dispersal_km = dispersal_km + 0.0001) %>% 
#   mutate(mean.hra.m2 = as.numeric(as.character(mean.hra.m2))) %>% 
#   mutate(hr.radius = sqrt((mean.hra.m2/1000000)/pi)) %>% 
#   mutate(Migration_km = as.numeric(as.character(Migration_km))) %>% 
#   mutate(media_simplified = as.factor(as.character(media_simplified))) %>% 
#   mutate(media_foraging = as.factor(media_foraging)) %>% 
#   mutate(media_dispersal = as.factor(media_dispersal)) %>% 
#   mutate(media_migration = as.factor(media_migration))
# 
# traits_phylo$class <- as.character(traits_phylo$class)
# 
# 
# 
# which(is.na(traits_phylo$class)) #1, 	Scapanus_townsendii
# traits_phylo$class[1] <- "Mammalia" 
# traits_phylo$order[1] <- "Eulipotyphla" 
# 
# rows <- which(traits_phylo$class == "Lepidosauria") # this is a subclass
# traits_phylo$class[rows] <- "Reptilia"
# 

#A <- ape::vcv.phylo(one_phylo)

traits_phylo <- read.csv("data/Traits_final_cleaned.csv", fileEncoding="latin1")

######## mod 1 - NULL ########
bf_dispersal <- bf(dispersal_km ~ 1) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ 1) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ 1) + hurdle_gamma(link = 'log')


library(parallel)
mod1 <- brm(bf_dispersal + bf_home_range + bf_migration, data = traits_phylo, 
                cores = 4)
# mod1 <- mod1.new
loo1 <- loo(mod1)
mod1 <- add_criterion(mod1, "waic")
loo(mod1, reloo = T)
summary(mod1)
bayes_R2(mod1)

saveRDS(mod1, "../mods/mod1/mod1.rds")
mod1 <- readRDS("../mods/mod1/mod1.rds")
loo(mod1, mod1.new, reloo=TRUE)


####### mod 2 - add mass ########
bf_dispersal <- bf(dispersal_km ~ log(Mass_kg)) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ log(Mass_kg)) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) ) + hurdle_gamma(link = 'log')


mod2 <- brm(bf_dispersal + bf_home_range + bf_migration, data = traits_phylo, cores = 4)
# loo --> higher ELPD is better fit, lower looic indicate better fit
# loo2 <- loo(mod2) #use with log+1
loo(mod2, reloo = T) #use with gamma
summary(mod2)
bayes_R2(mod2)
# mod2 <- (add_criterion(mod2, "loo"))

loo(mod1, mod2, reloo = T) #gamma
loo(mod2) #log
# 
# loo_compare(mod1, mod2, criterion = "loo")
saveRDS(mod2, "../mods/mod2/mod2.rds")
mod2 <- readRDS("../mods/mod2/mod2.rds")
summary(mod2)


######### mod 4 - niche conservatism: by class #########
# mod 4.1
# run on compute canada
bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|p|class) + dispersal_year) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ log(Mass_kg)+ (log(Mass_kg)|p|class) + foraging_year) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) + (log(Mass_kg)|p|class) + migration_year) + hurdle_gamma(link = 'log')

mod4.1 <- brm(bf_dispersal + bf_home_range + bf_migration, data = traits_phylo, 
    control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 10000, cores = 4)
coef(mod4.1)
saveRDS(mod4.1, "../mods/mod4.1/mod4_1.rds")
# mod4.1 <- readRDS("../mods/mod4.1/mod4_1.rds")
loo4.1 <- loo(mod4.1, reloo = TRUE)
saveRDS(loo4.1, "../mods/mod4.1/mod4_1_loo.rds")
read.loo4.1 <- readRDS("../mods/mod4.1/mod4_1.rds")


bayes_R2(mod4.1)

loo(mod4.2, mod4.1)
coef(mod4.1)

## mod 4.3 - order
# can't nest order within class bc not all orders present in all classes
bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|p|order)) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ log(Mass_kg) + (log(Mass_kg)|p|order))+ Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) + (log(Mass_kg)|p|order)) + hurdle_gamma(link = 'log')

mod4.3 <- brm(bf_dispersal + bf_home_range + bf_migration, data = traits_phylo, 
              control = list(adapt_delta = 0.995), iter = 5000)
saveRDS(mod4.3, "../mods/mod4.1/mod4_3_orderonly.rds")
loo(mod4.1, mod4.3)

bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|p|order) + (log(Mass_kg)|q|class)) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ log(Mass_kg) + (log(Mass_kg)|p|order) + (log(Mass_kg)|q|class))+ Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) + (log(Mass_kg)|p|order) + (log(Mass_kg)|q|class)) + hurdle_gamma(link = 'log')

mod4.3_2 <- brm(bf_dispersal + bf_home_range + bf_migration, data = traits_phylo, 
              control = list(adapt_delta = 0.995), iter = 10000)
summary(mod4.3_2)
coef(mod4.3_2)

saveRDS(mod4.3_2, "../mods/mod4.1/mod4_3.2.rds")
loo(mod4.1, mod4.3, mod4.3_2 )

######### mod 4 - niche conservatism: by order #########

## look at orders within each class ##
# birds
aves <- traits_phylo %>% filter(class == "Aves")

bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|p|order)) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ log(Mass_kg) + (log(Mass_kg)|p|order))+ Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) + (log(Mass_kg)|p|order)) + hurdle_gamma(link = 'log')

mod4.3_aves <- brm(bf_dispersal + bf_home_range + bf_migration, data = aves, 
              control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 5000, cores = 4)
summary(mod4.3_aves)
coef(mod4.3_aves)

saveRDS(mod4.3_aves, "../mods/mod4.3/mod4.3_aves.rds")

# mammals
mam <- traits_phylo %>% filter(class == "Mammalia")

bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|p|order)) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ log(Mass_kg) + (log(Mass_kg)|p|order))+ Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) + (log(Mass_kg)|p|order)) + hurdle_gamma(link = 'log')

mod4.3_mam <- brm(bf_dispersal + bf_home_range + bf_migration, data = mam, 
                   control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 5000, cores = 4)
saveRDS(mod4.3_mam, "../mods/mod4.3/mod4.3_mam.rds")
summary(mod4.3_mam)
coef(mod4.3_mam)


# amphib
amph <- traits_phylo %>% filter(class == "Amphibia")

bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|p|order)) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ log(Mass_kg) + (log(Mass_kg)|p|order))+ Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) + (log(Mass_kg)|p|order)) + hurdle_gamma(link = 'log')

mod4.3_amph <- brm(bf_dispersal + bf_home_range + bf_migration, data = amph, 
                  control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 5000, cores = 4)
saveRDS(mod4.3_amph, "../mods/mod4.3/mod4.3_amph.rds")
summary(mod4.3_amph)
coef(mod4.3_amph)

# reptiles
rep <- traits_phylo %>% filter(class == "Reptilia")

bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|p|order)) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ log(Mass_kg) + (log(Mass_kg)|p|order))+ Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) + (log(Mass_kg)|p|order)) + hurdle_gamma(link = 'log')

mod4.3_rep <- brm(bf_dispersal + bf_home_range + bf_migration, data = rep, 
                   control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 5000, cores = 4)
saveRDS(mod4.3_rep, "../mods/mod4.3/mod4.3_rep.rds")
summary(mod4.3_rep)
coef(mod4.3_rep)

########## mod 5 - diet ###########
traits_phylo$diet_broadest_cat <- as.factor(traits_phylo$diet_broadest_cat)
traits_phylo$diet_broadest_cat <- droplevels(traits_phylo$diet_broadest_cat)
levels(traits_phylo$diet_broadest_cat)

# same slope
bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (1|p|diet_broadest_cat)) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ log(Mass_kg)+ (1|p|diet_broadest_cat)) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) + (1|p|diet_broadest_cat)) + hurdle_gamma(link = 'log')

# test year as fixed effect
bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (1|p|diet_broadest_cat) + dispersal_year) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ log(Mass_kg)+ (1|p|diet_broadest_cat) + foraging_year) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) + (1|p|diet_broadest_cat) + migration_year) + hurdle_gamma(link = 'log')

mod5 <- brm(bf_dispersal + bf_home_range + bf_migration, data = traits_phylo, 
            control = list(adapt_delta = 0.99, max_treedepth=15), iter = 3000, cores =4)
saveRDS(mod5, "../mods/mod5_1.rds")

options(loo.cores = 4)
loo5.1 <- loo(mod5, reloo=TRUE)
saveRDS(loo5.1,"../mods/mod5_1_loo.rds" )

# diff slopes
bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|p|diet_broadest_cat) + dispersal_year) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ log(Mass_kg)+ (log(Mass_kg)|p|diet_broadest_cat)) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) + (log(Mass_kg)|p|diet_broadest_cat)) + hurdle_gamma(link = 'log')

mod5.2.disp <- brm(bf_dispersal, data = traits_phylo, 
            control = list(adapt_delta = 0.995, max_treedepth=20), iter = 4000, cores = 4)

mod5.2.for <- brm(bf_home_range, data = traits_phylo, 
              control = list(adapt_delta = 0.99, max_treedepth=20), iter = 4000, cores = 4)

mod5.2.mig <- brm(bf_migration, data = traits_phylo, 
                  control = list(adapt_delta = 0.95, max_treedepth=15), iter = 3000, cores = 4)

summary(mod5.2)
coef(mod5.2)
mod5.2_loo <- loo(mod5.2, reloo=TRUE) # higher elpd values indicate better fit, mod5.2
saveRDS(mod5.2, "../mods/mod5_2.rds")
saveRDS(mod5.2_loo,"../mods/mod5_2_loo.rds" )
mod5 <- readRDS("../mods/mod5_2.rds")
mod5_loo <- readRDS("../mods/mod5_2_loo.rds")
bayes_R2(mod5)

summary(mod5)
coef(mod5)

loo(mod1, mod2, mod4.1, mod5)
plot(mod3)



######## media for amphibians only ########
amphibia <- traits_phylo %>% filter(class == "Amphibia")
bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|p|media_dispersal)) + Gamma(link = "log")
amphibia.dispersal <- brm(bf_dispersal, data = amphibia,
                          control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 5000)
summary(amphibia.dispersal)
saveRDS(amphibia.dispersal, "../mods/amphibia.dispersal.rds")

bf_home_range <- bf(hr.radius ~ log(Mass_kg) + (log(Mass_kg)|p|media_foraging)) + Gamma(link = "log")
amphibia.foraging <- brm(bf_home_range, data = amphibia,
                          control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 5000)
saveRDS(amphibia.foraging, "../mods/amphibia.foraging.rds")

bf_migration <- bf(Migration_km ~ log(Mass_kg) + (log(Mass_kg)|p|media_migration)) + hurdle_gamma(link = 'log')
amphibia.migration <- brm(bf_migration, data = amphibia,
                         control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 5000)
saveRDS(amphibia.dispersal, "../mods/amphibia.migration.rds")


####### media diff for each SUP #######
# create subset df with only SUPs with all three media ##142
levels(traits_phylo$media_dispersal)
levels(traits_phylo$media_dispersal) <- c(levels(traits_phylo$media_dispersal), "aquatic")
traits_phylo$media_dispersal[which(traits_phylo$media_dispersal =="water")] <- "aquatic"

traits_disp <- traits_phylo %>% 
  filter(media_dispersal == "air" | media_dispersal == "land" | media_dispersal == "aquatic"| media_dispersal == "marine")

levels(traits_disp$media_dispersal)
traits_disp$media_dispersal <- factor(traits_disp$media_dispersal, levels = c("air", "aquatic", "land", "marine"))

traits_phylo$media_foraging[which(traits_phylo$media_foraging =="water")] <- "aquatic"
traits_for <- traits_phylo %>% 
  filter(media_foraging == "air" | media_foraging == "land" | media_foraging == "aquatic"| media_foraging == "marine")
levels(traits_for$media_foraging)
traits_for$media_foraging <- droplevels(traits_for$media_foraging)

traits_phylo$media_migration[which(traits_phylo$media_migration =="water")] <- "aquatic"
traits_mig<- traits_phylo %>% 
  filter(media_migration == "air" | media_migration == "land" | media_migration == "aquatic"| media_migration == "marine")
levels(traits_mig$media_migration)
traits_mig$media_migration <- factor(traits_mig$media_migration, levels = c("air", "aquatic", "land", "marine"))

# data = filter(traits_phylo, !is.na(media_simplified)
bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|p|media_dispersal)) + Gamma(link = "log")
dispersal_media <- brm(bf_dispersal, data = traits_disp,
                    control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 5000, cores = 4)

bf_dispersal_null <- bf(dispersal_km ~ log(Mass_kg)) + Gamma(link = "log")
dispersal_media_null <- brm(bf_dispersal_null, data = traits_disp,
                       control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 5000, cores = 4)

summary(dispersal_media)
saveRDS(dispersal_media, "../mods/dispersal_media.rds")
saveRDS(dispersal_media_null, "../mods/dispersal_media_null.rds")
#dispersal_media <- readRDS("../mods/dispersal_media.rds")
loo_med_disp <- loo(dispersal_media, dispersal_media_null)

bf_foraging <- bf(hr.radius ~ log(Mass_kg) + (log(Mass_kg)|p|media_foraging)) + Gamma(link = "log")
foraging_media <- brm(bf_foraging, data = traits_for,
                       control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 2000, cores = 4)
summary(foraging_media)
saveRDS(foraging_media, "../mods/foraging_media.rds")
foraging_media <- readRDS("../mods/foraging_media.rds")


bf_foraging_null <- bf(hr.radius ~ log(Mass_kg)) + Gamma(link = "log")
foraging_media_null <- brm(bf_foraging_null, data = traits_for,
                            control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 5000, cores = 4)
saveRDS(foraging_media_null, "../mods/foraging_media_null.rds")


loo_med_for <- loo(foraging_media, foraging_media_null)


bf_migration <- bf(Migration_km ~ log(Mass_kg) + (log(Mass_kg)|p|media_migration)) + hurdle_gamma(link = 'log')
migration_media <- brm(bf_migration, data = traits_mig,
                      control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 3000, cores = 4)
summary(migration_media)
saveRDS(migration_media, "../mods/mod3/migration_media.rds")

bf_migration_null <- bf(Migration_km ~ log(Mass_kg)) + hurdle_gamma(link = 'log')
migration_media_null <- brm(bf_migration_null, data = traits_mig,
                       control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 3000, cores = 4)
saveRDS(migration_media_null, "../mods/migration_media_null.rds")


loo_med_mig <- loo(migration_media, migration_media_null)

coef(migration_media)
bayes_R2(migration_media)
loo_mig_med <- loo(migration_media, reloo= TRUE)


migration_media <- readRDS(file.choose())
coef(migration_media)

dispersal_media <- readRDS(file.choose())

########### Migrators only ###############
traits_migrators <- traits_phylo %>% filter(Migration_km > 0)

### null - migrators ####
bf_dispersal <- bf(dispersal_km ~ 1) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ 1) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ 1) + Gamma(link = "log")

library(parallel)
mod0.migrators <- brm(bf_dispersal + bf_home_range + bf_migration, data = traits_migrators, 
            cores = 4)

saveRDS(mod0.migrators, "../mods/mod1/mod0_migrators.rds")

### mass - migrators ####
bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) ) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ log(Mass_kg)) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) ) + Gamma(link = "log")

mod_mass_migrators <- brm(bf_dispersal + bf_home_range + bf_migration, data = traits_migrators, cores = 4)

saveRDS(mod_mass_migrators, "../mods/mod2/mod_mass_migrators.rds")


### media - migrators, diff med for each ####
# dispersal 
traits_disp_migrators <- traits_disp %>% filter(Migration_km > 0)

bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|p|media_dispersal)) + Gamma(link = "log")
dispersal_media_migrators <- brm(bf_dispersal, data = traits_disp_migrators,
                       control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 5000, cores = 4)

bf_dispersal_null <- bf(dispersal_km ~ log(Mass_kg)) + Gamma(link = "log")
dispersal_media_null_migrators <- brm(bf_dispersal_null, data = traits_disp_migrators,
                            control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 5000, cores = 4)

summary(dispersal_media)
saveRDS(dispersal_media_migrators, "../mods/dispersal_media_migrators.rds")
saveRDS(dispersal_media_null_migrators, "../mods/dispersal_media_null_migrators.rds")

# foraging
traits_for_migrators <- traits_for %>% filter(Migration_km > 0)

bf_foraging <- bf(hr.radius ~ log(Mass_kg) + (log(Mass_kg)|p|media_foraging)) + Gamma(link = "log")
foraging_media_migrators <- brm(bf_foraging, data = traits_for_migrators,
                      control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 5000, cores = 4)


bf_foraging_null <- bf(hr.radius ~ log(Mass_kg)) + Gamma(link = "log")
foraging_media_null_migrators <- brm(bf_foraging_null, data = traits_for_migrators,
                           control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 5000, cores = 4)

saveRDS(foraging_media_migrators, "../mods/foraging_media_migrators.rds")
saveRDS(foraging_media_null_migrators, "../mods/foraging_media_null_migrators.rds")

# migration
traits_mig_migrators <- traits_mig %>% filter(Migration_km > 0)

bf_migration <- bf(Migration_km ~ log(Mass_kg) + (log(Mass_kg)|p|media_migration)) + Gamma(link = "log")
migration_media_migrators <- brm(bf_migration, data = traits_mig_migrators,
                       control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 7000, cores = 4)


bf_migration_null <- bf(Migration_km ~ log(Mass_kg)) + Gamma(link = "log")
migration_media_null_migrators <- brm(bf_migration_null, data = traits_mig_migrators,
                            control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 3000, cores = 4)

saveRDS(migration_media_migrators, "../mods/mod3/migration_media_migrators.rds")
saveRDS(migration_media_null_migrators, "../mods/migration_media_null_migrators.rds")

### Taxonomy ####
bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|p|class)) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ log(Mass_kg)+ (log(Mass_kg)|p|class)) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) + (log(Mass_kg)|p|class)) + Gamma(link = "log")

mod_class_migrators <- brm(bf_dispersal + bf_home_range + bf_migration, data = traits_migrators, 
              control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 7000, cores = 4)

saveRDS(mod_class_migrators, "../mods/mod4.1/mod_class_migrators.rds")
coef(mod_class_migrators)

### trophic guild ####
bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|p|diet_broadest_cat)) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ log(Mass_kg)+ (log(Mass_kg)|p|diet_broadest_cat)) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) + (log(Mass_kg)|p|diet_broadest_cat)) + Gamma(link = "log")

mod_trophic_migrators <- brm(bf_dispersal + bf_home_range + bf_migration, data = traits_migrators, 
            control = list(adapt_delta = 0.99, max_treedepth=15), iter = 3000, cores =4)

saveRDS(mod_trophic_migrators, "../mods/mod_trophic_migrators.rds")



########### Non-migrators only ###############
traits_non_migrators <- traits_phylo %>% filter(Migration_km == 0)

### null - migrators ####
bf_dispersal <- bf(dispersal_km ~ 1) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ 1) + Gamma(link = "log")
#bf_migration <- bf(Migration_km ~ 1) + Gamma(link = "log")

library(parallel)
mod0.non_migrators <- brm(bf_dispersal + bf_home_range, data = traits_non_migrators, 
                      cores = 4)

saveRDS(mod0.non_migrators, "../mods/mod1/mod0_migrators.rds")

### mass - migrators ####
bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) ) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ log(Mass_kg)) + Gamma(link = "log")

mod_mass_nonmigrators <- brm(bf_dispersal + bf_home_range, data = traits_non_migrators, cores = 4)

saveRDS(mod_mass_nonmigrators, "../mods/mod2/mod_mass_nonmigrators.rds")

### media - migrators, diff med for each ####
# dispersal 
traits_disp_nonmigrators <- traits_disp %>% filter(Migration_km == 0)

bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|p|media_dispersal)) + Gamma(link = "log")
dispersal_media_nonmigrators <- brm(bf_dispersal, data = traits_disp_nonmigrators,
                                 control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 5000, cores = 4)

bf_dispersal_null <- bf(dispersal_km ~ log(Mass_kg)) + Gamma(link = "log")
dispersal_media_null_nonmigrators <- brm(bf_dispersal_null, data = traits_disp_nonmigrators,
                                      control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 5000, cores = 4)

summary(dispersal_media)
saveRDS(dispersal_media_nonmigrators, "../mods/dispersal_media_nonmigrators.rds")
saveRDS(dispersal_media_null_nonmigrators, "../mods/dispersal_media_null_nonmigrators.rds")

# foraging
traits_for_nonmigrators <- traits_for %>% filter(Migration_km == 0)

bf_foraging <- bf(hr.radius ~ log(Mass_kg) + (log(Mass_kg)|p|media_foraging)) + Gamma(link = "log")
foraging_media_nonmigrators <- brm(bf_foraging, data = traits_for_nonmigrators,
                                control = list(adapt_delta = 0.999, max_treedepth = 15), iter = 5000, cores = 4)


bf_foraging_null <- bf(hr.radius ~ log(Mass_kg)) + Gamma(link = "log")
foraging_media_null_nonmigrators <- brm(bf_foraging_null, data = traits_for_nonmigrators,
                                     control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 5000, cores = 4)

saveRDS(foraging_media_nonmigrators, "../mods/foraging_media_nonmigrators.rds")
saveRDS(foraging_media_null_nonmigrators, "../mods/foraging_media_null_nonmigrators.rds")

### Taxonomy ####
bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|p|class)) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ log(Mass_kg)+ (log(Mass_kg)|p|class)) + Gamma(link = "log")

mod_class_non_migrators <- brm(bf_dispersal + bf_home_range, data = traits_non_migrators, 
                           control = list(adapt_delta = 0.999, max_treedepth = 15), iter = 7000, cores = 4)

saveRDS(mod_class_non_migrators, "../mods/mod4.1/mod_class_nonmigrators.rds")
summary(mod_class_non_migrators)

### trophic guild ####
bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|p|diet_broadest_cat)) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ log(Mass_kg)+ (log(Mass_kg)|p|diet_broadest_cat)) + Gamma(link = "log")

mod_trophic_nonmigrators <- brm(bf_dispersal + bf_home_range, data = traits_non_migrators, 
                             control = list(adapt_delta = 0.999, max_treedepth=15), iter = 3000, cores =4)

saveRDS(mod_trophic_nonmigrators, "../mods/mod_trophic_nonmigrators.rds")


