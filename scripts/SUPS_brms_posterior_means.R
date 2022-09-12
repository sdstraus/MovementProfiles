# try to visualize #
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


## traits_phylo

### load traits
######## googlesheets sucks now :(
setwd("/Users/sam/github/SUPSmods/scripts")
traits_clean <- read.csv("../data/Traits_Final.csv", fileEncoding="latin1")

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
         class, order, diet_broadest_cat, media_simplified, media_dispersal, 
         media_migration, media_foraging) %>% 
  mutate(scientific_name.x = str_replace(scientific_name.x, " ", "_")) %>% 
  mutate(dispersal_km = dispersal_km + 0.0001) %>% 
  mutate(mean.hra.m2 = as.numeric(as.character(mean.hra.m2))) %>% 
  mutate(hr.radius = sqrt((mean.hra.m2/1000000)/pi)) %>% 
  mutate(Migration_km = as.numeric(as.character(Migration_km))) %>% 
  mutate(media_simplified = as.factor(as.character(media_simplified))) %>% 
  mutate(media_foraging = as.factor(media_foraging)) %>% 
  mutate(media_dispersal = as.factor(media_dispersal)) %>% 
  mutate(media_migration = as.factor(media_migration))

traits_phylo$class <- as.character(traits_phylo$class)



which(is.na(traits_phylo$class)) #1, 	Scapanus_townsendii
traits_phylo$class[1] <- "Mammalia" 
traits_phylo$order[1] <- "Eulipotyphla" 

rows <- which(traits_phylo$class == "Lepidosauria") # this is a subclass
traits_phylo$class[rows] <- "Reptilia"

# load model
mod4.1 <- readRDS("../mods/mod4.1/mod4_1.rds")

# summary info
loo(mod4.1)
summary(mod4.1)
coef(mod4.1)

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


mod4.1 %>%
  spread_draws(`b_dispersalkm_logMass_kg`, r_class__dispersalkm[condition,]) %>%
  mutate(condition_mean = b_dispersalkm_logMass_kg + r_class__dispersalkm) %>%
  median_qi(condition_mean, .width = c(.95))  # three prob levels: .95, .8, .5


# basic plot
mod4.1 %>%
  spread_draws(b_dispersalkm_logMass_kg, r_class__dispersalkm[condition,]) %>%
  median_qi(condition_mean = b_dispersalkm_logMass_kg + r_class__dispersalkm, .width = c(.95, .66)) %>%
  ggplot(aes(y = condition, x = condition_mean, xmin = .lower, xmax = .upper)) +
  geom_pointinterval()


# density plot
mod4.1 %>%
  spread_draws(b_dispersalkm_logMass_kg, r_class__dispersalkm[condition,]) %>%
  mutate(condition_mean = b_dispersalkm_logMass_kg + r_class__dispersalkm) %>%
  ggplot(aes(y = condition, x = condition_mean)) +
  stat_halfeye()+
  scale_x_log10()+
  labs(x = "Dispersal slope mean")


# posterior means and predictions
traits_phylo %>%
  group_by(class) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 322)) %>%
  add_epred_draws(mod4.1) %>%
  head(10)

pred.class <- traits_phylo %>%
  group_by(class) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 322)) %>%
  add_epred_draws(mod4.1)

#plot (uses stat_pointinterval instead of geom_pointinterval) - takes a couple min
traits_phylo %>%
  group_by(class) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 322)) %>%
  add_epred_draws(mod4.1) %>%
  ggplot(aes(x = log10(.epred), y = class)) +
  stat_pointinterval(.width = c(.66, .95))

pred.class %>% 
  ggplot(aes(x = log10(.epred), y = class)) +
  stat_pointinterval(.width = c(.66, .95))


# pred.class %>% 
#   ggplot(aes(y = class, x = .prediction)) +
#   stat_interval(.width = c(.50, .80, .95, .99)) +
#   geom_point(aes(x = response), data = ABC) +
#   # +scale_x_log10() +
#   scale_color_brewer()


traits_phylo %>%
  group_by(class) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 322)) %>%
  add_predicted_draws(mod4.1) %>%
  ggplot(aes(y = class, x = .prediction)) +
  stat_interval(.width = c(.50, .80, .95, .99)) +
  geom_point(aes(x = log10(dispersal_km)), data = traits_phylo) +
  scale_color_brewer()

traits_phylo %>%
  ggplot(aes(y = class, x = dispersal_km)) +
  # stat_interval(aes(x = (.prediction)), data = preds) +
  stat_pointinterval(aes(x = log10(.epred)), data = means, .width = c(.66, .95), position = position_nudge(y = -0.3)) +
  geom_point() +
  scale_x_log10()+
  scale_color_brewer()

# put it all together
grid <-  traits_phylo %>%
  group_by(class) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 322))

means <-  grid %>%
  add_epred_draws(mod4.1)

preds <-  grid %>%
  add_predicted_draws(mod4.1)

traits_phylo %>%
  ggplot(aes(y = class, x = log10(dispersal_km))) +
  stat_interval(aes(x = log10(.prediction)), data = preds[preds$.category=='dispersalkm'&(preds$.prediction>0),]) +
  stat_pointinterval(aes(x = log10(.epred)), data = means, .width = c(.66, .95), position = position_nudge(y = -0.3)) +
  geom_point() +
  scale_color_brewer()


### contigency analysis for clas by trophic

class_diet <- table(traits_phylo$class, traits_phylo$diet_broadest_cat)
chisq.test(class_diet, simulate.p.value = TRUE)

class_media <- table(traits_phylo$class, traits_phylo$media_simplified)
chisq.test(class_media, simulate.p.value = TRUE)

media_diet <- table(traits_phylo$media_simplified, traits_phylo$diet_broadest_cat)
chisq.test(media_diet, simulate.p.value = TRUE)



