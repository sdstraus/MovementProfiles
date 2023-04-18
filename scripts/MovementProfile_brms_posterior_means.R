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

#install.packages(c("ggplot2", "tidybayes", "modelr", "brms", "stringr", 
                   # "dplyr", "ggpubr", "viridis", "ggdist", "knitr"))

library(kableExtra)

### load traits


# load model
mod4.1 <- readRDS("mods/class_multivar_withyear.rds")

class_dispersal <- readRDS("mods/class.disp_withyear.rds")

# summary info
loo(class_dispersal)
summary(mod4.1)
coef(class_dispersal)


tidy(coef(class_dispersal)) %>% 
  filter(grepl(term, pattern = '^b')) %>% 
  # mutate(term = c('', 'Math', 'Male', 'General', 'Academic')) %>% 
  rename_all(str_to_title) %>% 
  kable(digits = 2)

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
  data_grid(Mass_kg = seq_range(Mass_kg, n = 320), 
            dispersal_year = seq_range(dispersal_year, n = 320),
            foraging_year = seq_range(foraging_year, n = 320),
            migration_year = seq_range(migration_year, n = 320)) %>%
  add_epred_draws(class_dispersal) %>%
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

### class ####
grid <-  traits_phylo %>%
  group_by(class) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 320))

means <-  grid %>%
  add_epred_draws(mod4.1)

preds <-  grid %>%
  add_predicted_draws(mod4.1)

class_for <- traits_phylo %>%
  ggplot(aes(y = class, x = log10(hr.radius))) +
  stat_interval(aes(x = log10(.prediction)), data = preds[preds$.category=='hrradius'&(preds$.prediction>0),]) +
  stat_pointinterval(aes(x = log10(.epred)), data = means[means$.category=='hrradius'&(means$.epred>0),], .width = c(.66, .95), position = position_nudge(y = -0.3)) +
  geom_point() +
  scale_color_brewer()+
  cowplot::theme_cowplot()+
  labs(y = "Taxonomic class")

class_disp <- traits_phylo %>%
  ggplot(aes(y = class, x = log10(dispersal_km))) +
  stat_interval(aes(x = log10(.prediction)), data = preds[preds$.category=='dispersalkm'&(preds$.prediction>0),]) +
  stat_pointinterval(aes(x = log10(.epred)), data = means[means$.category=='dispersalkm'&(means$.epred>0),], .width = c(.66, .95), position = position_nudge(y = -0.3)) +
  geom_point() +
  scale_color_brewer()+
  cowplot::theme_cowplot()+
  labs(y = "Taxonomic class")

class_mig <- traits_phylo %>%
  ggplot(aes(y = class, x = log10(Migration_km))) +
  stat_interval(aes(x = log10(.prediction)), data = preds[preds$.category=='Migrationkm'&(preds$.prediction>0),]) +
  stat_pointinterval(aes(x = log10(.epred)), data = means[means$.category=='Migrationkm'&(means$.epred>0),], .width = c(.66, .95), position = position_nudge(y = -0.3)) +
  geom_point() +
  scale_color_brewer()+
  cowplot::theme_cowplot()+
  labs(y = "Taxonomic class")



class_figs <- ggarrange(class_for, class_disp, class_mig, nrow = 1, 
                        common.legend = T, legend = "right", labels = c("d", "e", "f"))

jpeg(filename = "../figures/class_figs_posteriormeans.jpeg", width = 10, height = 3, units = 'in', res = 1000)
class_figs
dev.off()

###diet ####

mod5 <- readRDS("../mods/mod5_2.rds")

summary(mod5)

grid <-  traits_phylo %>%
  group_by(diet_broadest_cat) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 322))

means <-  grid %>%
  add_epred_draws(mod5)

preds <-  grid %>%
  add_predicted_draws(mod5)

guild_for <- traits_phylo %>%
  ggplot(aes(y = diet_broadest_cat, x = log10(hr.radius))) +
  stat_interval(aes(x = log10(.prediction)), data = preds[preds$.category=='hrradius'&(preds$.prediction>0),]) +
  stat_pointinterval(aes(x = log10(.epred)), data = means[means$.category=='hrradius'&(means$.epred>0),], .width = c(.66, .95), position = position_nudge(y = -0.3)) +
  geom_point() +
  scale_color_brewer()+
  cowplot::theme_cowplot()+
  labs(y = "Trophic guild")

guild_disp <- traits_phylo %>%
  ggplot(aes(y = diet_broadest_cat, x = log10(dispersal_km))) +
  stat_interval(aes(x = log10(.prediction)), data = preds[preds$.category=='dispersalkm'&(preds$.prediction>0),]) +
  stat_pointinterval(aes(x = log10(.epred)), data = means[means$.category=='dispersalkm'&(means$.epred>0),], .width = c(.66, .95), position = position_nudge(y = -0.3)) +
  geom_point() +
  scale_color_brewer()+
  cowplot::theme_cowplot()+
  labs(y = "Trophic guild")

guild_mig <- traits_phylo %>%
  ggplot(aes(y = diet_broadest_cat, x = log10(Migration_km))) +
  stat_interval(aes(x = log10(.prediction)), data = preds[preds$.category=='Migrationkm'&(preds$.prediction>0),]) +
  stat_pointinterval(aes(x = log10(.epred)), data = means[means$.category=='Migrationkm'&(means$.epred>0),], .width = c(.66, .95), position = position_nudge(y = -0.3)) +
  geom_point() +
  scale_color_brewer()+
  cowplot::theme_cowplot()+
  labs(y = "Trophic guild")

guild_figs <- ggarrange(guild_for, guild_disp, guild_mig, nrow = 1, 
                        common.legend = T, legend = "right", labels = c("g", "h", "i"))

jpeg(filename = "../figures/diet_figs_posteriormeans.jpeg", width = 10, height = 3, units = 'in', res = 1000)
guild_figs
dev.off()

### media ####
### each SUP with own media 
foraging_media <- readRDS("../mods/mod3/foraging_media.rds")
dispersal_media <- readRDS("../mods/mod3/dispersal_media.rds")
migration_media <- readRDS("../mods/mod3/migration_media.rds")

# foraging
grid <-  traits_for %>%
  group_by(media_foraging) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 322))

means <-  grid %>%
  add_epred_draws(foraging_media)

preds <-  grid %>%
  add_predicted_draws(foraging_media)

media_for <- traits_for %>%
  ggplot(aes(y = media_foraging, x = log10(hr.radius))) +
  stat_interval(aes(x = log10(.prediction)), data = preds) +
  stat_pointinterval(aes(x = log10(.epred)), data = means, .width = c(.66, .95), position = position_nudge(y = -0.3)) +
  geom_point() +
  scale_color_brewer()+
  cowplot::theme_cowplot()+
  labs(y = "Foraging media")


# dispersal
grid <-  traits_disp %>%
  group_by(media_dispersal) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 322))

means <-  grid %>%
  add_epred_draws(dispersal_media)

preds <-  grid %>%
  add_predicted_draws(dispersal_media)

media_disp <- traits_disp %>%
  ggplot(aes(y = media_dispersal, x = log10(dispersal_km))) +
  stat_interval(aes(x = log10(.prediction)), data = preds) +
  stat_pointinterval(aes(x = log10(.epred)), data = means, .width = c(.66, .95), position = position_nudge(y = -0.3)) +
  geom_point() +
  scale_color_brewer()+
  cowplot::theme_cowplot()+
  labs(y = "Dispersal media")


# migration
grid <-  traits_mig %>%
  group_by(media_migration) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 322))

means <-  grid %>%
  add_epred_draws(migration_media)

preds <-  grid %>%
  add_predicted_draws(migration_media)

media_mig <- traits_mig %>%
  ggplot(aes(y = media_migration, x = log10(Migration_km))) +
  stat_interval(aes(x = log10(.prediction)), data = preds[preds$.prediction>0,]) +
  stat_pointinterval(aes(x = log10(.epred)), data = means[means$.epred>0,], .width = c(.66, .95), position = position_nudge(y = -0.3)) +
  geom_point() +
  scale_color_brewer()+
  cowplot::theme_cowplot()+
  labs(y = "Migration media")

media_figs <- ggarrange(media_for, media_disp, media_mig, nrow = 1, 
                        common.legend = T, legend = "right", labels = c("a", "b", "c"))
#ggsave(media_figs, filename = "../figures/media_figs_posteriormeans.jpeg", dpi = 'retina', width = 10, height = 3, units = 'in')

jpeg(filename = "../figures/media_figs_posteriormeans.jpeg", width = 10, height = 3, units = 'in', res = 1000)
media_figs
dev.off()


### contigency analysis ####

class_diet <- table(traits_phylo$class, traits_phylo$diet_broadest_cat)
chisq.test(class_diet, simulate.p.value = TRUE, B = 10000)

traits_media <- traits_phylo[traits_phylo$media_simplified != "", ]
traits_media$media_simplified <- droplevels(traits_media$media_simplified)

# traits_media <- left_join(traits_disp, traits_for)
# traits_media <- left_join(traits_media, traits_mig)
# rows <- which(traits_media$media_dispersal == "marine")
# traits_media$media_simplified[rows] <- "marine"

class_media <- table(traits_media$class, traits_media$media_simplified)
chisq.test(class_media, simulate.p.value = TRUE, B = 10000)

media_diet <- table(traits_phylo$media_simplified, traits_phylo$diet_broadest_cat)
chisq.test(media_diet, simulate.p.value = TRUE, B = 10000)

# reviewer comment on 77000x larger
aves <- traits_phylo %>% 
  filter(class == "Aves") %>% 
  mutate(mig.hr.ratio = (Migration_km/hr.radius))

mean(aves$mig.hr.ratio)
max(aves$mig.hr.ratio)

## reviwer comment - specific example of small mammals migrating farther in air than on land
air.under5 <- traits_phylo %>% 
  filter(media_migration == "air") %>% 
  filter(Mass_kg < 5)

mean(air.under5$Migration_km) #3657.252

land.under5 <- traits_phylo %>% 
  filter(media_migration == "land") %>% 
  filter(Mass_kg < 5)

mean(land.under5$Migration_km) #4.69654

