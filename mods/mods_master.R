library(brms)
library(stringr)
library(dplyr)

### load traits
######## googlesheets sucks now :(
setwd("/Users/samstraus/github/SUPSmods/scripts")
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
  select(scientific_name.x, dispersal_km, Migration_km, mean.hra.m2, Mass_kg, 
         class, diet_broadest_cat, media_simplified, media_dispersal, 
         media_migration, media_foraging) %>% 
  mutate(scientific_name.x = str_replace(scientific_name.x, " ", "_")) %>% 
  mutate(dispersal_km = dispersal_km + 0.0001) %>% 
  mutate(mean.hra.m2 = as.numeric(as.character(mean.hra.m2))) %>% 
  mutate(hr.radius = sqrt((mean.hra.m2/1000000)/pi)) %>% 
  mutate(Migration_km = as.numeric(as.character(Migration_km))) %>% 
  mutate(media_simplified = as.factor(as.character(media_simplified)))

traits_phylo$class <- as.character(traits_phylo$class)



which(is.na(traits_phylo$class)) #1, 	Scapanus_townsendii
traits_phylo$class[1] <- "Mammalia" 
traits_phylo$order[1] <- "Eulipotyphla" 

rows <- which(traits_phylo$class == "Lepidosauria") # this is a subclass
traits_phylo$class[rows] <- "Reptilia"

A <- ape::vcv.phylo(one_phylo)

######## mod 1 - NULL ########
bf_dispersal <- bf(dispersal_km ~ 1) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ 1) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ 1) + hurdle_gamma(link = 'log')
#OR
# bf_dispersal <- bf(log10(dispersal_km+1) ~ 1)
# bf_home_range <- bf(log10(mean.hra.m2+1) ~ 1)
# bf_migration <- bf(log10(Migration_km+1) ~ 1)

mod1 <- brm(bf_dispersal + bf_home_range + bf_migration, data = traits_phylo)
loo1 <- loo(mod1)
mod1 <- add_criterion(mod1, "waic")
loo(mod1, reloo = T)
summary(mod1)
bayes_R2(mod1)

saveRDS(mod1, "../mods/mod1/mod1.rds")
mod1 <- readRDS("mods/mod1/mod1.rds")

# mod 1.2
# rows <- which(traits_phylo$media_simplified == "")
# traits_phylo$media_simplified[rows] <- NA
# traits_phylo$media_simplified <- droplevels(traits_phylo$media_simplified)
# 
# traits_media <- filter(traits_phylo, !is.na(media_simplified))
# bf_dispersal <- bf(dispersal_km ~ 1) + Gamma(link = "log")
# bf_home_range <- bf(hr.radius ~ 1) + Gamma(link = "log")
# bf_migration <- bf(Migration_km ~ 1) + hurdle_gamma(link = 'log')
# 
# mod1.2 <- brm(bf_dispersal + bf_home_range + bf_migration, data = traits_media)

####### mod 2 - add mass ########
bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) ) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ log(Mass_kg)) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) ) + hurdle_gamma(link = 'log')
#OR
# bf_dispersal <- bf(log10(dispersal_km+1) ~ log(Mass_kg))
# bf_home_range <- bf(log10(mean.hra.m2+1) ~ log(Mass_kg))
# bf_migration <- bf(log10(Migration_km+1) ~ log(Mass_kg))

mod2 <- brm(bf_dispersal + bf_home_range + bf_migration, data = traits_phylo)
# loo --> higher ELPD is better fit, lower looic indicate better fit
loo2 <- loo(mod2) #use with log+1
loo(mod2, reloo = T) #use with gamma
summary(mod2)
bayes_R2(mod2)
# mod2 <- (add_criterion(mod2, "loo"))

loo(mod1, mod2, reloo = T) #gamma
loo(mod1, mod2) #log
# 
# loo_compare(mod1, mod2, criterion = "loo")
saveRDS(mod2, "../mods/mod2/mod2.rds")
mod2 <- readRDS("../mods/mod2/mod2.rds")
summary(mod2)
# #mod 2.2
# bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) ) + Gamma(link = "log")
# bf_home_range <- bf(hr.radius ~ log(Mass_kg)) + Gamma(link = "log")
# bf_migration <- bf(Migration_km ~ log(Mass_kg) ) + hurdle_gamma(link = 'log')
# 
# mod2.2 <- brm(bf_dispersal + bf_home_range + bf_migration, data = traits_media)
# saveRDS(mod2.2, "../mods/mod2/mod2.2.rds")
######### mod 3 - media ###########
rows <- which(traits_phylo$media_simplified == "")
traits_phylo$media_simplified[rows] <- NA
traits_phylo$media_simplified <- droplevels(traits_phylo$media_simplified)


# traits_phylo$media_simplified <- as.character(traits_phylo$media_simplified)

# remove potential outlier
traits_phylo <- traits_phylo %>% 
  filter(scientific_name.x != "Apus_apus")

# traits_media <- filter(traits_phylo, !is.na(media_simplified))

# bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (1|p|media_simplified) + (1|q|class)) + Gamma(link = "log")
# bf_home_range <- bf(hr.radius ~ log(Mass_kg)+ (1|p|media_simplified) + (1|q|class)) + Gamma(link = "log")
# bf_migration <- bf(Migration_km ~ log(Mass_kg) + (1|p|media_simplified) + (1|q|class)) + hurdle_gamma(link = 'log')
# 
# mod3 <- brm(bf_dispersal + bf_home_range + bf_migration, data = traits_phylo, 
#             control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 5000)
saveRDS(mod3, "../mods/mod3.rds")
mod3.2 <- readRDS("mod3_2.rds")
loo(mod3)
summary(mod3)
loo(mod1.2, mod2,  mod3)


bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|p|media_simplified)) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ log(Mass_kg)+ (log(Mass_kg)|p|media_simplified)) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) + (log(Mass_kg)|p|media_simplified)) + hurdle_gamma(link = 'log')

mod3.2 <- brm(bf_dispersal + bf_home_range + bf_migration, data = filter(traits_phylo, !is.na(media_simplified)),
                          control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 5000)

plot(mod3.2)
saveRDS(mod3.2, "../mods/mod3_2_outlier.rds")
mod3.2 <- readRDS("../mods/mod3_2.rds") # best one
loo(mod1, mod2,  mod3, mod4.1)
######### mod 4 - niche conservatism #########
# mod 4.1
# run on compute canada
bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|p|class)) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ log(Mass_kg)+ (log(Mass_kg)|p|class)) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) + (log(Mass_kg)|p|class)) + hurdle_gamma(link = 'log')

mod4.1 <- brm(bf_dispersal + bf_home_range + bf_migration, data = traits_phylo, 
    control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 10000)
summary(mod4.1)
saveRDS(mod4.1, "../mods/mod4.1/mod4_1.rds")
mod4.1 <- readRDS("../mods/mod4.1/mod4_1.rds")
loo(mod4.1, reloo = T)
loo(mod1, mod2, mod4.1)

mod4.1 <- readRDS("../mods/mod4.1/mod4_1.2.rds")
mod4.2 <- readRDS("../mod4.2/mod4.2.rds")
loo(mod1, mod2, mod4.1)
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
              control = list(adapt_delta = 0.995), iter = 5000)
saveRDS(mod4.3_2, "../mods/mod4.1/mod4_3.2.rds")
loo(mod4.1, mod4.3, mod4.3_2 )
########## mod 5 - diet ###########

traits_phylo$diet_broadest_cat <- droplevels(traits_phylo$diet_broadest_cat)
levels(traits_phylo$diet_broadest_cat)

# same slope
bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (1|p|diet_broadest_cat)) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ log(Mass_kg)+ (1|p|diet_broadest_cat)) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) + (1|p|diet_broadest_cat)) + hurdle_gamma(link = 'log')

mod5 <- brm(bf_dispersal + bf_home_range + bf_migration, data = traits_phylo, 
            control = list(adapt_delta = 0.99), iter = 3000)
saveRDS(mod5, "../mods/mod5.rds")
# diff slopes
bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|p|diet_broadest_cat)) + Gamma(link = "log")
bf_home_range <- bf(hr.radius ~ log(Mass_kg)+ (log(Mass_kg)|p|diet_broadest_cat)) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) + (log(Mass_kg)|p|diet_broadest_cat)) + hurdle_gamma(link = 'log')

mod5.2 <- brm(bf_dispersal + bf_home_range + bf_migration, data = traits_phylo, 
            control = list(adapt_delta = 0.99), iter = 3000)
summary(mod5.2)
coef(mod5.2)
loo(mod5, mod5.2) # higher elpd values indicate better fit, mod5.2
saveRDS(mod5.2, "../mods/mod5_2.rds")

mod5 <- readRDS("mod5.rds")

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


###### media diff for each SUP #######
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
                    control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 5000)
summary(dispersal_media)
saveRDS(dispersal_media, "../mods/dispersal_media.rds")
dispersal_media <- readRDS("../mods/dispersal_media.rds")

bf_foraging <- bf(hr.radius ~ log(Mass_kg) + (log(Mass_kg)|p|media_foraging)) + Gamma(link = "log")
foraging_media <- brm(bf_foraging, data = traits_for,
                       control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 2000)
summary(foraging_media)
saveRDS(foraging_media, "../mods/foraging_media.rds")
foraging_media <- readRDS("../mods/foraging_media.rds")

bf_migration <- bf(Migration_km ~ log(Mass_kg) + (log(Mass_kg)|p|media_migration)) + hurdle_gamma(link = 'log')
migration_media <- brm(bf_migration, data = traits_mig,
                      control = list(adapt_delta = 0.995, max_treedepth = 15), iter = 3000)
summary(migration_media)
saveRDS(migration_media, "../mods/migration_media.rds")



# bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|p|media_dispersal)) + Gamma(link = "log")
# bf_home_range <- bf(hr.radius ~ log(Mass_kg)+ (log(Mass_kg)|p|media_foraging)) + Gamma(link = "log")
# bf_migration <- bf(Migration_km ~ log(Mass_kg) + (log(Mass_kg)|p|media_migration)) + hurdle_gamma(link = 'log')
# media_mods <- brm(bf_dispersal + bf_home_range + bf_migration, data = traits_media, 
#               control = list(adapt_delta = 0.995), iter = 5000)
# summary(mod4.3)
