### check the difference: coef(model) vs spread_draws slope

library(ggplot2)
library(tidybayes)
library(modelr)
library(brms)
library(stringr)
library(dplyr)
library(tidyr)
library(ggpubr)
library(viridis)
library(ggdist)
library(broom)        # Convert model objects to data frames
library(broom.mixed)  # Convert brms model objects to data frames



## traits_phylo

### load traits


# load model
mod4.1 <- readRDS("../mods/mod4.1/mod4_1.rds")

tidy(mod4.1) %>% View()

## summary info

summary(mod4.1)
mod4.1




samples1 <- posterior_samples(mod4.1)

nrow(samples1)
apply(samples1[, c(4:6)], 2, FUN = function(x) c(mean(x), quantile(x, c(0.025, 0.5,0.975))))

## 20k posterior samples

samples1 %>%
  select(`b_dispersalkm_logMass_kg`:`b_Migrationkm_logMass_kg`) %>% 
  pivot_longer(everything(),names_to = "slope", values_to = "posterior_samples") %>% 
  ggplot(aes(x = posterior_samples)) +
  facet_wrap(~slope, ncol = 1) +
  geom_histogram() +
  geom_vline(xintercept = 0, colour = 'red')


## what is happening with coef

coef(mod4.1)

column_names_samples <- colnames(samples1)

columns_to_get <- column_names_samples[str_detect(column_names_samples, "r_class__dispersalkm\\[[A-Z, a-z]*,logM")]

apply(samples1[, columns_to_get], 2, FUN = function(x) c(mean(x), quantile(x, c(0.025, 0.5,0.975))))

coef(mod4.1, summary = TRUE)


coef_no_summary <- coef(mod4.1, summary = FALSE)

coef_no_summary$class %>% dimnames()

# 1 = iteration, #2 class #3 slopes 
coef_no_summary$class[,,] %>% dim()

apply(coef_no_summary$class[,,], c(2,3), FUN = function(x) c(mean(x), quantile(x, c(0.025, 0.5,0.975))))

## posterior distribution

samples1[, columns_to_get] %>% 
  pivot_longer(everything(),names_to = "slope", values_to = "posterior_samples") %>% 
  ggplot(aes(x = posterior_samples)) +
  facet_wrap(~slope, ncol = 1) +
  geom_histogram() +
  geom_vline(xintercept = 0, colour = 'red')

## this one matches what's in table 2
coef_no_summary$class[,,2] %>% 
  as.data.frame() %>% 
  pivot_longer(everything(),names_to = "slope", values_to = "posterior_samples") %>% 
  ggplot(aes(x = posterior_samples)) +
  facet_wrap(~slope, ncol = 1) +
  geom_histogram(stat = "bin", bins = 100) +
  geom_vline(xintercept = 0, colour = 'red') +
  theme_cowplot()


fixef(mod4.1)

ranef(mod4.1)

#
coef_no_summary$class[,,2]

rowSums(samples1[, c("b_dispersalkm_logMass_kg", "r_class__dispersalkm[Amphibia,logMass_kg]")]) %>% 
  mean()


# extract draws
get_variables(mod4.1)

# summarizing in a couple diff ways
mod4.1 %>%
  spread_draws(r_class__dispersalkm[Class, Intercept]) %>%
  head(10)

mod4.1 %>%
  spread_draws(r_class__dispersalkm[condition,]) %>%
  summarise_draws()

mod4.1 %>% 
  spread_draws(b_dispersalkm_logMass_kg, r_class__dispersalkm[condition,]) %>%
  head(10)

#### creating figs for posterior mean slopes for each condition #######
# #### Media ####
# # foraging
# foraging_media <- readRDS("../mods/mod3/foraging_media.rds")
# 
# 
# coef_no_summary <- coef(foraging_media, summary = FALSE)
# coef_no_summary$media_foraging %>% dimnames()
# # 1 = iteration, #2 class #3 slopes 
# coef_no_summary$media_foraging[,,] %>% dim()
# apply(coef_no_summary$media_foraging[,,], c(2,3), FUN = function(x) c(mean(x), quantile(x, c(0.025, 0.5,0.975))))
# ## posterior distribution
# 
# media_foraging_slope <- coef_no_summary$media_foraging[,,2] %>% 
#   as.data.frame() %>% 
#   pivot_longer(everything(),names_to = "slope", values_to = "posterior_samples") %>% 
#   ggplot(aes(x = posterior_samples)) +
#   facet_wrap(~slope, ncol = 1) +
#   geom_histogram(stat = "bin", bins = 100) +
#   geom_vline(xintercept = 0, colour = 'red') +
#   theme_cowplot()+
#   theme(strip.background = element_rect(fill = "white"))+
#   xlab("Posterior mean slope")
# 
# # dispersal
# dispersal_media <- readRDS("../mods/mod3/dispersal_media.rds")
# 
# 
# coef_no_summary <- coef(dispersal_media, summary = FALSE)
# coef_no_summary$media_dispersal %>% dimnames()
# # 1 = iteration, #2 class #3 slopes 
# coef_no_summary$media_dispersal[,,] %>% dim()
# apply(coef_no_summary$media_dispersal[,,], c(2,3), FUN = function(x) c(mean(x), quantile(x, c(0.025, 0.5,0.975))))
# ## posterior distribution
# 
# media_dispersal_slope <- coef_no_summary$media_dispersal[,,2] %>% 
#   as.data.frame() %>% 
#   pivot_longer(everything(),names_to = "slope", values_to = "posterior_samples") %>% 
#   ggplot(aes(x = posterior_samples)) +
#   facet_wrap(~slope, ncol = 1) +
#   geom_histogram(stat = "bin", bins = 100) +
#   geom_vline(xintercept = 0, colour = 'red') +
#   theme_cowplot()+
#   theme(strip.background = element_rect(fill = "white"))+
#   xlab("Posterior mean slope")
# 
# 
# # migration
# migration_media <- readRDS("../mods/mod3/migration_media.rds")
# 
# 
# coef_no_summary <- coef(migration_media, summary = FALSE)
# coef_no_summary$media_migration %>% dimnames()
# # 1 = iteration, #2 class #3 slopes 
# coef_no_summary$media_migration[,,] %>% dim()
# apply(coef_no_summary$media_migration[,,], c(2,3), FUN = function(x) c(mean(x), quantile(x, c(0.025, 0.5,0.975))))
# ## posterior distribution
# 
# media_migration_slope <- coef_no_summary$media_migration[,,2] %>% 
#   as.data.frame() %>% 
#   pivot_longer(everything(),names_to = "slope", values_to = "posterior_samples") %>% 
#   ggplot(aes(x = posterior_samples)) +
#   facet_wrap(~slope, ncol = 1) +
#   geom_histogram(stat = "bin", bins = 100) +
#   geom_vline(xintercept = 0, colour = 'red') +
#   theme_cowplot()+
#   theme(strip.background = element_rect(fill = "white"))+
#   xlab("Posterior mean slope")
# 
# media_post_mean_slopes <- ggarrange(media_foraging_slope, media_dispersal_slope, media_migration_slope, 
#                                     labels = c("Foraging", "Dispersal", "Migration"),
#                                     nrow = 1)
# 
# jpeg(filename = "../figures/media_posterior_means_slopes.jpeg", width = 9, height = 6, units = 'in', res = 1000)
# media_post_mean_slopes
# dev.off()


#### Class ####
# foraging
coef_no_summary <- coef(mod4.1, summary = FALSE)
coef_no_summary$class %>% dimnames()
# 1 = iteration, #2 class #3 slopes 
coef_no_summary$class[,,] %>% dim()
apply(coef_no_summary$class[,,], c(2,3), FUN = function(x) c(mean(x), quantile(x, c(0.025, 0.5,0.975))))
## posterior distribution

class_foraging_slope <- coef_no_summary$class[,,4] %>% 
  as.data.frame() %>% 
  pivot_longer(everything(),names_to = "slope", values_to = "posterior_samples") %>% 
  ggplot(aes(x = posterior_samples)) +
  facet_wrap(~slope, ncol = 1) +
  geom_histogram(stat = "bin", bins = 100) +
  geom_vline(xintercept = 0, colour = 'red') +
  theme_cowplot()+
  theme(strip.background = element_rect(fill = "white"))+
  xlab("Posterior mean slope")


# dispersal
class_dispersal_slope <- coef_no_summary$class[,,2] %>% 
  as.data.frame() %>% 
  pivot_longer(everything(),names_to = "slope", values_to = "posterior_samples") %>% 
  ggplot(aes(x = posterior_samples)) +
  facet_wrap(~slope, ncol = 1) +
  geom_histogram(stat = "bin", bins = 100) +
  geom_vline(xintercept = 0, colour = 'red') +
  theme_cowplot()+
  theme(strip.background = element_rect(fill = "white"))+
  xlab("Posterior mean slope")

# migration
class_migration_slope <- coef_no_summary$class[,,6] %>% 
  as.data.frame() %>% 
  pivot_longer(everything(),names_to = "slope", values_to = "posterior_samples") %>% 
  ggplot(aes(x = posterior_samples)) +
  facet_wrap(~slope, ncol = 1) +
  geom_histogram(stat = "bin", bins = 100) +
  geom_vline(xintercept = 0, colour = 'red') +
  theme_cowplot()+
  theme(strip.background = element_rect(fill = "white"))+
  xlab("Posterior mean slope")

class_post_mean_slopes <- ggarrange(class_foraging_slope, class_dispersal_slope, class_migration_slope, 
          labels = c("Foraging", "Dispersal", "Migration"),
          nrow = 1)

jpeg(filename = "../figures/class_posterior_means_slopes.jpeg", width = 9, height = 7, units = 'in', res = 1000)
class_post_mean_slopes
dev.off()


#### Trophic Guild ####
mod5.2 <- readRDS("../mods/mod5_2.rds")

# foraging
coef_no_summary <- coef(mod5.2, summary = FALSE)
coef_no_summary$diet_broadest_cat %>% dimnames()
# 1 = iteration, #2 class #3 slopes 
coef_no_summary$diet_broadest_cat[,,] %>% dim()
apply(coef_no_summary$diet_broadest_cat[,,], c(2,3), FUN = function(x) c(mean(x), quantile(x, c(0.025, 0.5,0.975))))
## posterior distribution

diet_foraging_slope <- coef_no_summary$diet_broadest_cat[,,4] %>% 
  as.data.frame() %>% 
  pivot_longer(everything(),names_to = "slope", values_to = "posterior_samples") %>% 
  ggplot(aes(x = posterior_samples)) +
  facet_wrap(~slope, ncol = 1) +
  geom_histogram(stat = "bin", bins = 100) +
  geom_vline(xintercept = 0, colour = 'red') +
  theme_cowplot()+
  theme(strip.background = element_rect(fill = "white"))+
  xlab("Posterior mean slope")


# dispersal
diet_dispersal_slope <- coef_no_summary$diet_broadest_cat[,,2] %>% 
  as.data.frame() %>% 
  pivot_longer(everything(),names_to = "slope", values_to = "posterior_samples") %>% 
  ggplot(aes(x = posterior_samples)) +
  facet_wrap(~slope, ncol = 1) +
  geom_histogram(stat = "bin", bins = 100) +
  geom_vline(xintercept = 0, colour = 'red') +
  theme_cowplot()+
  theme(strip.background = element_rect(fill = "white"))+
  xlab("Posterior mean slope")

# migration
diet_migration_slope <- coef_no_summary$diet_broadest_cat[,,6] %>% 
  as.data.frame() %>% 
  pivot_longer(everything(),names_to = "slope", values_to = "posterior_samples") %>% 
  ggplot(aes(x = posterior_samples)) +
  facet_wrap(~slope, ncol = 1) +
  geom_histogram(stat = "bin", bins = 100) +
  geom_vline(xintercept = 0, colour = 'red') +
  theme_cowplot()+
  theme(strip.background = element_rect(fill = "white"))+
  xlab("Posterior mean slope")

diet_post_mean_slopes <- ggarrange(diet_foraging_slope, diet_dispersal_slope, diet_migration_slope, 
                                    labels = c("Foraging", "Dispersal", "Migration"),
                                    nrow = 1)

jpeg(filename = "../figures/diet_posterior_means_slopes.jpeg", width = 9, height = 7, units = 'in', res = 1000)
diet_post_mean_slopes
dev.off()
