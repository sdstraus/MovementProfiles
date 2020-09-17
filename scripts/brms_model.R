library(brms)
library(stringr)
library(dplyr)
library(ggplot2)


### load traits
######## googlesheets sucks now :(

traits_clean <- read.csv("data/SUPs_traits - clean_traits.csv")

## load phylogeny

one_phylo <- readRDS("data/final_phylo.rds")

one_phylo

all_class <- read.csv("data/classes.csv")

traits_phylo <- traits_clean %>% 
  dplyr::filter(number_traits_completed ==4) %>% 
  left_join(all_class, by = c("scientific_name.x" = "scientific_name")) %>% 
  filter(!(class %in% "Insecta")) %>% 
  #bind_rows(shark_1) %>% 
  dplyr::filter(str_replace(scientific_name.x, " ", "_") %in% one_phylo$tip.label) %>% 
  select(scientific_name.x, dispersal_km, Migration_km, mean.hra.m2, Mass_kg, class) %>% 
  mutate(scientific_name.x = str_replace(scientific_name.x, " ", "_")) %>% 
  mutate(dispersal_km = dispersal_km + 0.0001) %>% 
  mutate(mean.hra.m2 = as.numeric(as.character(mean.hra.m2))) %>% 
  mutate(Migration_km = as.numeric(as.character(Migration_km)))

### easy model

fit1 <- brm(dispersal_km ~ Mass_kg, data = traits_phylo, cores = 4)
summary(fit1)


## start adding priors

traits_phylo %>% 
  ggplot(aes(x = Mass_kg, y = dispersal_km)) + geom_point() +
  scale_x_log10() + scale_y_log10()

fit1 <- brm(log(dispersal_km) ~ log(Mass_kg), data = traits_phylo, 
            prior = c(
              prior(normal(0, 10), "b"),
              prior(normal(0, 50), "Intercept")))

## add log link function

fit1 <- brm(dispersal_km ~ log(Mass_kg), data = traits_phylo,
            family = Gamma(link = "log"),
            prior = c(
              prior(normal(0, 10), "b"),
              prior(normal(0, 50), "Intercept"),
              prior(gamma(0.01, 0.01), "shape")))


### add phylogeny

all(A >= 0)

A <- ape::vcv.phylo(one_phylo)

traits_phylo %>% 
  ggplot(aes(x = Mass_kg, y = dispersal_km, colour = class)) + geom_point() +
  scale_x_log10() + scale_y_log10()

fit1 <- brm(dispersal_km ~ log(Mass_kg) + (1|class), data = traits_phylo, cov_ranef = list(phylo = A),
            family = Gamma(link = "log"),
            prior = c(
              prior(normal(0, 10), "b"),
              prior(normal(0, 50), "Intercept"),
              prior(gamma(0.01, 0.01), "shape")))

### model just for home range size 

traits_phylo %>% 
  ggplot(aes(x = Mass_kg, y = mean.hra.m2, colour = class)) + geom_point() +
  scale_x_log10() + scale_y_log10()

fit1 <- brm(mean.hra.m2 ~ log(Mass_kg) + (1|class), data = traits_phylo, cov_ranef = list(phylo = A),
            family = Gamma(link = "log"),
            prior = c(
              prior(normal(0, 10), "b"),
              prior(normal(0, 50), "Intercept"),
              prior(gamma(0.01, 0.01), "shape")))


### fit a hurdle model for migration only

traits_phylo %>% 
  ggplot(aes(x = Mass_kg, y = Migration_km+1, colour = class)) + geom_point() +
  scale_x_log10() + scale_y_log10()

fit1 <- brm(Migration_km ~ log(Mass_kg) + (1|class) + (1|scientific_name.x), data = traits_phylo, 
            cov_ranef = list(scientific_name.x = A),
            family = hurdle_gamma(link = 'log'),
            prior = c(
              prior(normal(0, 10), "b"),
              prior(normal(0, 50), "Intercept"),
              prior(gamma(0.01, 0.01), "shape")), cores = 4) 

### add multivariate structure

bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (1|p|class)) + Gamma(link = "log")
bf_home_range <- bf(mean.hra.m2 ~ log(Mass_kg) + (1|p|class)) + Gamma(link = "log")
  
fit1 <- brm(
  bf_dispersal + bf_home_range + set_rescor(FALSE), 
  data = traits_phylo, chains = 2, cores = 2,
  control = list(adapt_delta = 0.95)
)


bf_migration <- bf(Migration_km ~ log(Mass_kg) + (1|class)) + hurdle_gamma(link = 'log')

fit1 <- brm(
  bf_dispersal + bf_home_range + bf_migration + set_rescor(FALSE), 
  data = traits_phylo, chains = 2, cores = 2,
  control = list(adapt_delta = 0.95)
)



########## Models to test ##############


######### full phylogenetic and multivariate model####

A <- ape::vcv.phylo(one_phylo)

bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (1|class) + (1|scientific_name.x)) + Gamma(link = "log")
bf_home_range <- bf(mean.hra.m2 ~ log(Mass_kg) + (1|class) + (1|scientific_name.x)) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) + (1|class) + (1|scientific_name.x)) + hurdle_gamma(link = 'log')

mod0 <- brm(
  bf_dispersal + bf_home_range + bf_migration + set_rescor(FALSE), cov_ranef = list(scientific_name.x = A),
  data = traits_phylo, chains = 2, cores = 2,
  control = list(adapt_delta = 0.99)
)

summary(mod0)
bayes_R2(mod0)

######## mod 1 = mass, random = phylogeny #######

A <- ape::vcv.phylo(one_phylo)

bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (1|scientific_name.x)) + Gamma(link = "log")
bf_home_range <- bf(mean.hra.m2 ~ log(Mass_kg) + (1|scientific_name.x)) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) + (1|scientific_name.x)) + hurdle_gamma(link = 'log')

mod1 <- brm(
  bf_dispersal + bf_home_range + bf_migration + set_rescor(FALSE), cov_ranef = list(scientific_name.x = A),
  data = traits_phylo, chains = 4, cores = 4,
  control = list(adapt_delta = 0.8)
)

summary(mod1)
bayes_R2(mod1)

########## mod 2 = mass, random = species ########


A <- ape::vcv.phylo(one_phylo)

bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (1|scientific_name.x)) + Gamma(link = "log")
bf_home_range <- bf(mean.hra.m2 ~ log(Mass_kg) + (1|scientific_name.x)) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) + (1|scientific_name.x)) + hurdle_gamma(link = 'log')

mod2 <- brm(
  bf_dispersal + bf_home_range + bf_migration + set_rescor(FALSE), 
  data = traits_phylo, chains = 2, cores = 2,
  control = list(adapt_delta = 0.99)
)

######### mod 3 = mass, random = class/species ############

A <- ape::vcv.phylo(one_phylo)

bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (1|class)) + Gamma(link = "log")
bf_home_range <- bf(mean.hra.m2 ~ log(Mass_kg) + (1|class)) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) + (1|class)) + hurdle_gamma(link = 'log')

mod3 <- brm(
  bf_dispersal + bf_home_range + bf_migration + set_rescor(FALSE), 
  data = traits_phylo, chains = 2, cores = 2,
  control = list(adapt_delta = 0.99)
)


####### mod 4 = intercept only, random = phylogeny #########

A <- ape::vcv.phylo(one_phylo)

bf_dispersal <- bf(dispersal_km ~  (1|scientific_name.x)) + Gamma(link = "log")
bf_home_range <- bf(mean.hra.m2 ~  (1|scientific_name.x)) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~  (1|scientific_name.x)) + hurdle_gamma(link = 'log')

mod4 <- brm(
  bf_dispersal + bf_home_range + bf_migration + set_rescor(FALSE), cov_ranef = list(scientific_name.x = A),
  data = traits_phylo, chains = 2, cores = 2,
  control = list(adapt_delta = 0.99)
)



#########  mod 5 = intercept only, random = species ########


A <- ape::vcv.phylo(one_phylo)

bf_dispersal <- bf(dispersal_km ~ log(Mass_kg) + (1|scientific_name.x)) + Gamma(link = "log")
bf_home_range <- bf(mean.hra.m2 ~ log(Mass_kg) + (1|scientific_name.x)) + Gamma(link = "log")
bf_migration <- bf(Migration_km ~ log(Mass_kg) + (1|scientific_name.x)) + hurdle_gamma(link = 'log')

mod5 <- brm(
  bf_dispersal + bf_home_range + bf_migration + set_rescor(FALSE), 
  data = traits_phylo, chains = 2, cores = 2,
  control = list(adapt_delta = 0.99)
)


