library(brms)
library(stringr)
library(dplyr)

### load traits
######## googlesheets sucks now :(

traits_clean <- read.csv("../../data/SUPs_traits - clean_traits.csv")

## load phylogeny

one_phylo <- readRDS("../../data/final_phylo.rds")

#one_phylo

all_class <- read.csv("../../data/classes.csv")

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

A <- ape::vcv.phylo(one_phylo)

######## mod 1 - NULL ########
bf_dispersal <- bf(dispersal_km ~ 1) + Gamma(link = "log")
bf_home_range <- bf(mean.hra.m2 ~ 1) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ 1) + hurdle_gamma(link = 'log')
#OR
bf_dispersal <- bf(log10(dispersal_km+1) ~ 1)
bf_home_range <- bf(log10(mean.hra.m2+1) ~ 1)
bf_migration <- bf(log10(Migration_km+1) ~ 1)

mod1 <- brm(bf_dispersal + bf_home_range + bf_migration, data = traits_phylo)
loo1 <- loo(mod1)
mod1 <- add_criterion(mod1, "waic")
loo(mod1, reloo = T)
summary(mod1)
bayes_R2(mod1)

saveRDS(mod1, "mod1.rds")
####### mod 2 - add mass ########
bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) ) + Gamma(link = "log")
bf_home_range <- bf(mean.hra.m2 ~ log(Mass_kg)) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) ) + hurdle_gamma(link = 'log')
#OR
bf_dispersal <- bf(log10(dispersal_km+1) ~ log(Mass_kg))
bf_home_range <- bf(log10(mean.hra.m2+1) ~ log(Mass_kg))
bf_migration <- bf(log10(Migration_km+1) ~ log(Mass_kg))

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
saveRDS(mod2, "mod2.rds")

######### mod 3 - media ###########
rows <- which(traits_phylo$media_simplified == "")
traits_phylo$media_simplified[rows] <- NA
traits_phylo$media_simplified <- droplevels(traits_phylo$media_simplified)

traits_media <- filter(traits_phylo, !is.na(media_simplified))

bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (1|p|media_simplified)) + Gamma(link = "log")
bf_home_range <- bf(mean.hra.m2 ~ log(Mass_kg)+ (1|p|media_simplified)) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) + (1|p|media_simplified)) + hurdle_gamma(link = 'log')

mod3 <- brm(bf_dispersal + bf_home_range + bf_migration, data = traits_phylo, 
            control = list(adapt_delta = 0.995, max_treedepth = 15))
saveRDS(mod3, "mod3.rds")
loo(mod3)
summary(mod3)
loo(mod1, mod3)

######### mod 4 - niche conservatism #########
# mod 4.1
# run on compute canada
bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (1|p|class)) + Gamma(link = "log")
bf_home_range <- bf(mean.hra.m2 ~ log(Mass_kg)+ (1|p|class)) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) + (1|p|class)) + hurdle_gamma(link = 'log')

brm(bf_dispersal + bf_home_range + bf_migration, data = traits_phylo, 
    control = list(adapt_delta = 0.995))

mod4.1 <- readRDS("mod4_1.rds")
loo(mod4.1, reloo = T)
loo(mod1, mod2, mod4.1)

########## mod 5 - diet ###########
bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (1|p|diet_broadest_cat)) + Gamma(link = "log")
bf_home_range <- bf(mean.hra.m2 ~ log(Mass_kg)+ (1|p|diet_broadest_cat)) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) + (1|p|diet_broadest_cat)) + hurdle_gamma(link = 'log')

mod5 <- brm(bf_dispersal + bf_home_range + bf_migration, data = traits_phylo, 
            control = list(adapt_delta = 0.995, max_treedepth = 15))
saveRDS(mod5, "mod5.rds")
loo(mod5)
summary(mod5)

loo(mod1, mod2, mod5)
plot(mod3)
# ########## mod 6 - geographic extent ###########
# bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (1|p|geographic_range_km2)) + Gamma(link = "log")
# bf_home_range <- bf(mean.hra.m2 ~ log(Mass_kg)+ (1|p|geographic_range_km2)) + Gamma(link = "log")
# bf_migration <- bf(Migration_km ~ log(Mass_kg) + (1|p|geographic_range_km2)) + hurdle_gamma(link = 'log')
# 
# mod6 <- brm(bf_dispersal + bf_home_range + bf_migration, data = traits_phylo, 
#             control = list(adapt_delta = 0.995, max_treedepth = 15))
# saveRDS(mod6, "mod6.rds")
# loo(mod6)
# summary(mod6)

