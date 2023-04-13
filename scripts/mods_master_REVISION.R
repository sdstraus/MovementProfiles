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
bf_dispersal_trophic_noyear <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|diet_broadest_cat)) + lognormal(link = "identity")
bf_home_range_trophic_noyear <- bf(hr.radius ~ log(Mass_kg)+ (log(Mass_kg)|diet_broadest_cat)) + lognormal(link = "identity")
bf_migration_trophic_noyear <- bf(Migration_km ~ log(Mass_kg) + (log(Mass_kg)|diet_broadest_cat)) + hurdle_lognormal(link = 'identity')

trophic_noyear_prior <- prior(normal(0,5),class = 'b', coef = logMass_kg) +
  prior(student_t(4, 0, 3), class = 'Intercept') +
  prior(student_t(6, 0, 2.9), class = 'sd') +
  prior(student_t(4, 0, 2.9), class = 'sigma')

## this model is good
trophic.disp_noyear <- brm(bf_dispersal_trophic_noyear, data = traits_phylo, 
                   control = list(adapt_delta = 0.995, max_treedepth=20), iter = 2000, cores = 4,
                   prior = trophic_noyear_prior)

## this model is good
trophic.for_noyear <- brm(bf_home_range_trophic_noyear, data = traits_phylo, 
                  control = list(adapt_delta = 0.995, max_treedepth=20), iter = 2000, cores = 4,
                  prior = trophic_noyear_prior)

trophic_noyear_prior_mig <- prior(normal(0,4),class = 'b', coef = logMass_kg) +
  prior(student_t(3, 0, 3), class = 'Intercept') +
  prior(student_t(6, 0, 2.9), class = 'sd') +
  prior(student_t(3, 0, 2.9), class = 'sigma')

## 1 divergent transitions, see if we can fix that 

## filtered is na migration data in order to compare with the year model 
trophic.mig_noyear <- brm(bf_migration_trophic_noyear, data = traits_phylo, 
                  control = list(adapt_delta = 0.995, max_treedepth=20), iter = 3000, cores = 4, 
                  prior = trophic_noyear_prior_mig)

trophic.mig_noyear <- add_criterion(trophic.mig_noyear, "loo")

saveRDS(trophic.disp_noyear, "mods/mod4.1/trophic.disp_noyear.rds")
saveRDS(trophic.for_noyear, "mods/mod4.1/trophic.for_noyear.rds")
saveRDS(trophic.mig_noyear, "mods/mod4.1/trophic.mig_noyear.rds")


# random slope, with year
bf_dispersal_trophic_withyear <- bf(dispersal_km ~ log(Mass_kg) + (log(Mass_kg)|diet_broadest_cat) + dispersal_year) + lognormal(link = "identity")
bf_home_range_trophic_withyear <- bf(hr.radius ~ log(Mass_kg)+ (log(Mass_kg)|diet_broadest_cat) + foraging_year) + lognormal(link = "identity")
bf_migration_trophic_withyear <- bf(Migration_km ~ log(Mass_kg) + (log(Mass_kg)|diet_broadest_cat) + migration_year) + hurdle_lognormal(link = 'identity')


trophic_year_prior_disp <- prior(normal(0,5),class = 'b', coef = logMass_kg) +
  prior(normal(0,5),class = 'b', coef = dispersal_year) +
  prior(student_t(5, 0, 3), class = 'Intercept') +
  prior(student_t(6, 0, 2.9), class = 'sd') +
  prior(student_t(5, 0, 2.9), class = 'sigma')

## 1 divergent transition 
trophic.disp_withyear <- brm(bf_dispersal_trophic_withyear, data = traits_phylo, 
                          control = list(adapt_delta = 0.995, max_treedepth=20), iter = 2000, cores = 4,
                          prior = trophic_year_prior_disp)

trophic_year_prior_for <- prior(normal(0,5),class = 'b', coef = logMass_kg) +
  prior(normal(0,5),class = 'b', coef = foraging_year) +
  prior(student_t(5, 0, 3), class = 'Intercept') +
  prior(student_t(6, 0, 2.9), class = 'sd') +
  prior(student_t(5, 0, 2.9), class = 'sigma')

# 1 divergent transitions
trophic.for_withyear <- brm(bf_home_range_trophic_withyear, data = traits_phylo, 
                         control = list(adapt_delta = 0.995, max_treedepth=20), iter = 2000, cores = 4,
                         prior = trophic_year_prior_for)


##1 divergent transitions 
trophic_year_prior_mig <- prior(normal(0,2),class = 'b', coef = logMass_kg) +
  prior(normal(0,2),class = 'b', coef = migration_year) +
  prior(student_t(5, 0, 3), class = 'Intercept') +
  prior(student_t(4, 0, 2.9), class = 'sd') +
  prior(student_t(4, 0, 2.9), class = 'sigma') 


trophic.mig_withyear <- brm(bf_migration_trophic_withyear, data = traits_phylo, 
                         control = list(adapt_delta = 0.995, max_treedepth=20), iter = 5000, cores = 4,
                         prior = trophic_year_prior_mig)

saveRDS(trophic.disp_withyear, "mods/mod4.1/trophic.disp_withyear.rds")
saveRDS(trophic.for_withyear, "mods/mod4.1/trophic.for_withyear.rds")
saveRDS(trophic.mig_withyear, "mods/mod4.1/trophic.mig_withyear.rds")

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


