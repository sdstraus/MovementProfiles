library(brms)
library(stringr)
library(dplyr)
library(rstan)

### load traits

traits_phylo <- read.csv("data/Traits_final_cleaned.csv", fileEncoding="latin1")

traits_phylo$diet_broadest_cat <- as.factor(traits_phylo$diet_broadest_cat)
traits_phylo$diet_broadest_cat <- droplevels(traits_phylo$diet_broadest_cat)
levels(traits_phylo$diet_broadest_cat)

##### Trophic Guild #####

# random slope, no year
bf_dispersal_trophic_noyear <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|p|diet_broadest_cat)) + Gamma(link = "log")
bf_home_range_trophic_noyear <- bf(hr.radius ~ log(Mass_kg)+ (log(Mass_kg)|p|diet_broadest_cat)) + Gamma(link = "log")
bf_migration_trophic_noyear <- bf(Migration_km ~ log(Mass_kg) + (log(Mass_kg)|p|diet_broadest_cat)) + hurdle_gamma(link = 'log')


trophic.disp_noyear <- brm(bf_dispersal_trophic_noyear, data = traits_phylo, 
                   control = list(adapt_delta = 0.995, max_treedepth=20), iter = 4000, cores = 4)

trophic.for_noyear <- brm(bf_home_range_trophic_noyear, data = traits_phylo, 
                  control = list(adapt_delta = 0.99, max_treedepth=20), iter = 4000, cores = 4)

trophic.mig_noyear <- brm(bf_migration_trophic_noyear, data = traits_phylo, 
                  control = list(adapt_delta = 0.95, max_treedepth=15), iter = 3000, cores = 4)


saveRDS(trophic.disp_noyear, "../mods/mod4.1/trophic.disp_noyear.rds")
saveRDS(trophic.for_noyear, "../mods/mod4.1/trophic.for_noyear.rds")
saveRDS(trophic.mig_noyear, "../mods/mod4.1/trophic.mig_noyear.rds")


# random slope, with year
bf_dispersal_trophic_withyear <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|p|diet_broadest_cat) + dispersal_year) + Gamma(link = "log")
bf_home_range_trophic_withyear <- bf(hr.radius ~ log(Mass_kg)+ (log(Mass_kg)|p|diet_broadest_cat) + foraging_year) + Gamma(link = "log")
bf_migration_trophic_withyear <- bf(Migration_km ~ log(Mass_kg) + (log(Mass_kg)|p|diet_broadest_cat) + migration_year) + hurdle_gamma(link = 'log')


trophic.disp_withyear <- brm(bf_dispersal_trophic_withyear, data = traits_phylo, 
                          control = list(adapt_delta = 0.995, max_treedepth=20), iter = 4000, cores = 4)

trophic.for_withyear <- brm(bf_home_range_trophic_withyear, data = traits_phylo, 
                         control = list(adapt_delta = 0.99, max_treedepth=20), iter = 4000, cores = 4)

trophic.mig_withyear <- brm(bf_migration_trophic_withyear, data = traits_phylo, 
                         control = list(adapt_delta = 0.95, max_treedepth=15), iter = 3000, cores = 4)



##### Class #####

# random slope, no year
bf_dispersal_class_noyear <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|p|class)) + Gamma(link = "log")
bf_home_range_class_noyear <- bf(hr.radius ~ log(Mass_kg)+ (log(Mass_kg)|p|class)) + Gamma(link = "log")
bf_migration_class_noyear <- bf(Migration_km ~ log(Mass_kg) + (log(Mass_kg)|p|class)) + hurdle_gamma(link = 'log')


class.disp_noyear <- brm(bf_dispersal_class_noyear, data = traits_phylo, 
                          control = list(adapt_delta = 0.995, max_treedepth=20), iter = 4000, cores = 4)

class.for_noyear <- brm(bf_home_range_class_noyear, data = traits_phylo, 
                         control = list(adapt_delta = 0.99, max_treedepth=20), iter = 4000, cores = 4)

class.mig_noyear <- brm(bf_migration_class_noyear, data = traits_phylo, 
                         control = list(adapt_delta = 0.95, max_treedepth=15), iter = 3000, cores = 4)


# random slope, with year
bf_dispersal_class_withyear <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|p|class) + dispersal_year) + Gamma(link = "log")
bf_home_range_class_withyear <- bf(hr.radius ~ log(Mass_kg)+ (log(Mass_kg)|p|class) + foraging_year) + Gamma(link = "log")
bf_migration_class_withyear <- bf(Migration_km ~ log(Mass_kg) + (log(Mass_kg)|p|class) + migration_year) + hurdle_gamma(link = 'log')


class.disp_withyear <- brm(bf_dispersal_class_withyear, data = traits_phylo, 
                         control = list(adapt_delta = 0.995, max_treedepth=20), iter = 4000, cores = 4)

class.for_withyear <- brm(bf_home_range_class_withyear, data = traits_phylo, 
                        control = list(adapt_delta = 0.99, max_treedepth=20), iter = 4000, cores = 4)

class.mig_withyear <- brm(bf_migration_class_withyear, data = traits_phylo, 
                        control = list(adapt_delta = 0.95, max_treedepth=15), iter = 3000, cores = 4)


