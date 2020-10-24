# try to visualize #
library(ggplot2)
library(tidybayes)
library(modelr)
library(brms)
library(stringr)
library(dplyr)
library(tidyr)
library(ggpubr)

# setwd("/Users/sam/SUPS_meta_cluster/mods/")

######## old mods ########
## mod0 ###
mod0_logn <- readRDS("mod0_3/mod0_logn.rds")
## checking mod0_logn.rds
loo0 <- loo(mod0_logn)
plot(loop)
plot(mod0_logn)
conditional_effects(mod0_logn, "Mass_kg", resp = "dispersalkm", 
                    method = "fitted", points = T)
marginal_effects(mod0_logn)
pp_check(mod0_logn, resp = "dispersalkm")
bayes_R2(mod0_logn)


mod0_dispersal <- traits_phylo %>%
  group_by(class) %>% 
  data_grid(Mass_kg = seq_range(Mass_kg, n = 322)) %>%
  add_fitted_draws(mod0_logn, allow_new_levels = TRUE) %>%
  ggplot(aes(x = Mass_kg, y = dispersal_km, color = ordered(class))) +
  geom_line(aes(y = .value, group = paste(class, .draw)), alpha = .01) +
  geom_point(data = traits_phylo) +
  scale_color_brewer(palette = "Dark2")+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()

ggsave(mod0_dispersal, filename = "mod0_dispersal.jpg", dpi = 'retina', width = 6, height = 4, unit = "in")


mod0_migration <- traits_phylo %>%
  group_by(class) %>% 
  data_grid(Mass_kg = seq_range(Mass_kg, n = 322)) %>%
  add_fitted_draws(mod0_logn, allow_new_levels = TRUE) %>%
  ggplot(aes(x = Mass_kg, y = Migration_km, color = ordered(class))) +
  geom_line(aes(y = .value, group = paste(class, .draw)), alpha = .01) +
  geom_point(data = traits_phylo) +
  scale_color_brewer(palette = "Dark2")+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()

ggsave(mod0_migration, filename = "mod0_migration.jpg", dpi = 'retina', width = 6, 
       height = 4, unit = "in")


mod0_homerange <- traits_phylo %>%
  group_by(class) %>% 
  data_grid(Mass_kg = seq_range(Mass_kg, n = 322)) %>%
  add_fitted_draws(mod0_logn, allow_new_levels = TRUE) %>%
  ggplot(aes(x = Mass_kg, y = mean.hra.m2, color = ordered(class))) +
  geom_line(aes(y = .value, group = paste(class, .draw)), alpha = .01) +
  geom_point(data = traits_phylo) +
  scale_color_brewer(palette = "Dark2")+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()

ggsave(mod0_homerange, filename = "mod0_homerange.jpg", dpi = 'retina', width = 6, height = 4, unit = "in")

## mod0 ##
mod1_logn <- readRDS("mod1/mod1_logn.rds")
summary(mod1_logn)
loo1 <- loo(mod1_logn)

loo_compare(loo0, loo1)
kfold1 <- kfold(mod1_logn, K = 10)

mod1_dispersal <- traits_phylo %>%
  group_by(class) %>% 
  data_grid(Mass_kg = seq_range(Mass_kg, n = 322)) %>%
  add_fitted_draws(mod1_logn, allow_new_levels = TRUE) %>%
  ggplot(aes(x = Mass_kg, y = dispersal_km, color = ordered(class))) +
  geom_line(aes(y = .value, group = paste(class, .draw)), alpha = .01) +
  geom_point(data = traits_phylo) +
  scale_color_brewer(palette = "Dark2")+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()

ggsave(mod1_dispersal, filename = "mod1_dispersal.jpg", dpi = 'retina', width = 6, height = 4, unit = "in")

mod1_migration <- traits_phylo %>%
  group_by(class) %>% 
  data_grid(Mass_kg = seq_range(Mass_kg, n = 322)) %>%
  add_fitted_draws(mod1_logn, allow_new_levels = TRUE) %>%
  ggplot(aes(x = Mass_kg, y = Migration_km, color = ordered(class))) +
  geom_line(aes(y = .value, group = paste(class, .draw)), alpha = .01) +
  geom_point(data = traits_phylo) +
  scale_color_brewer(palette = "Dark2")+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()

ggsave(mod1_migration, filename = "mod1_migration.jpg", dpi = 'retina', width = 6, 
       height = 4, unit = "in")


mod1_homerange <- traits_phylo %>%
  group_by(class) %>% 
  data_grid(Mass_kg = seq_range(Mass_kg, n = 322)) %>%
  add_fitted_draws(mod1_logn, allow_new_levels = TRUE) %>%
  ggplot(aes(x = Mass_kg, y = mean.hra.m2, color = ordered(class))) +
  geom_line(aes(y = .value, group = paste(class, .draw)), alpha = .01) +
  geom_point(data = traits_phylo) +
  scale_color_brewer(palette = "Dark2")+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()

ggsave(mod1_homerange, filename = "mod1_homerange.jpg", dpi = 'retina', width = 6, height = 4, unit = "in")

## mod 4 ##
mod4 <- readRDS("mod4/mod4_better_model_des.rds")
loo4 <- loo(mod4)
loo(mod0, mod4)
summary(mod4)

mod4_dispersal <- traits_phylo %>%
  group_by(class) %>% 
  data_grid(Mass_kg = seq_range(Mass_kg, n = 322)) %>%
  add_fitted_draws(mod4, allow_new_levels = TRUE) %>%
  ggplot(aes(x = Mass_kg, y = dispersal_km, color = ordered(class))) +
  geom_line(aes(y = .value, group = paste(class, .draw)), alpha = .01) +
  geom_point(data = traits_phylo) +
  scale_color_brewer(palette = "Dark2")+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()

ggsave(mod4_dispersal, filename = "mod4_dispersal.jpg", dpi = 'retina', width = 6, height = 4, unit = "in")


######## new mods #########
######## mod 1 - NULL ########
mod1 <- readRDS("mods/mod1/mod1.rds")
summary(mod1)

pred_mod1 <- traits_phylo %>%
  group_by(class) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 322)) %>%
  add_predicted_draws(mod1)

mod1_hr <- ggplot() +
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = mean.hra.m2, color = ordered(class))) +
  stat_lineribbon(data = pred_mod1[pred_mod1$.category=='meanhram2',], aes(y = .prediction, x = Mass_kg, color = ordered(class)), .width = c(.99, .95, .8, .5), alpha = 0.25) +
  scale_color_brewer(palette = 'Dark2')+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()

mod1_disp <- ggplot() +
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = dispersal_km, color = ordered(class))) +
  stat_lineribbon(data = pred_mod1[pred_mod1$.category=='dispersalkm',], aes(y = .prediction, x = Mass_kg, color = ordered(class)), .width = c(.99, .95, .8, .5), alpha = 0.25) +
  scale_color_brewer(palette = 'Dark2')+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()

mod1_mig <- ggplot() +
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = Migration_km, color = ordered(class))) +
  stat_lineribbon(data = pred_mod1[pred_mod1$.category=='Migrationkm',], aes(y = .prediction, x = Mass_kg, color = ordered(class)), .width = c(.99, .95, .8, .5), alpha = 0.25) +
  scale_color_brewer(palette = 'Dark2')+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()

mod1_figs <- ggarrange(mod1_hr, mod1_disp, mod1_mig)
ggsave(mod1_figs, filename = "mod1_figs.jpg", dpi = 'retina', width = 10, height = 6, units = 'in')

############ mod 2 #############
mod2 <- readRDS("mods/mod2/mod2.rds")
summary(mod2)

# traits_phylo %>%
#   group_by(class) %>% 
#   data_grid(Mass_kg = seq_range(Mass_kg, n = 322)) %>%
#   add_predicted_draws(mod2) %>%
#   ggplot(aes(x = Mass_kg, y = dispersal_km, color = ordered(class))) +
#   # geom_line(aes(y = .prediction, group = paste(class, .draw)), alpha = .01) +
#   stat_lineribbon(aes(y = .prediction), .width = c(.99, .95, .8, .5), alpha = 0.25) +
#   geom_point(data = traits_phylo) +
#   scale_color_brewer(palette = "Dark2")+
#   scale_x_log10()+
#   scale_y_log10()+
#   cowplot::theme_cowplot()

pred_mod2 <- traits_phylo %>%
  group_by(class) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 322)) %>%
  add_predicted_draws(mod2)

mod2_hr <- ggplot() +
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = mean.hra.m2, color = ordered(class))) +
  stat_lineribbon(data = pred_mod2[pred_mod2$.category=='meanhram2',], aes(y = .prediction, x = Mass_kg, color = ordered(class)), .width = c(.99, .95, .8, .5), alpha = 0.25) +
  scale_color_brewer(palette = 'Dark2')+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()

mod2_disp <- ggplot() +
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = dispersal_km, color = ordered(class))) +
  stat_lineribbon(data = pred_mod2[pred_mod2$.category=='dispersalkm',], aes(y = .prediction, x = Mass_kg, color = ordered(class)), .width = c(.99, .95, .8, .5), alpha = 0.25) +
  scale_color_brewer(palette = 'Dark2')+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()

mod2_mig <- ggplot() +
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = Migration_km, color = ordered(class))) +
  stat_lineribbon(data = pred_mod2[pred_mod2$.category=='Migrationkm',], aes(y = .prediction, x = Mass_kg, color = ordered(class)), .width = c(.99, .95, .8, .5), alpha = 0.25) +
  scale_color_brewer(palette = 'Dark2')+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()

mod2_figs <- ggarrange(mod2_hr, mod2_disp, mod2_mig)
ggsave(mod2_figs, filename = "mod2_figs.jpg", dpi = 'retina', width = 10, height = 6, units = 'in')

########### mod 4 ###############
mod4.1 <- readRDS("../mods/mod4.1/mod4_1.rds")
mod4.1.2 <- readRDS("../mods/mod4.1/mod4_1.2.rds")
loo(mod4.1.2, mod4.2)
summary(mod4.1.2)

pred_mod4.1.2 <- traits_phylo %>%
  group_by(class) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 322)) %>%
  add_predicted_draws(mod4.1.2)

mod4.1_hr <- ggplot() +
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = mean.hra.m2, color = ordered(class))) +
  stat_lineribbon(data = pred_mod4.1.2[pred_mod4.1.2$.category=='meanhram2',], aes(y = .prediction, x = Mass_kg, color = ordered(class)), .width = c(.99, .95, .8, .5), alpha = 0.25) +
  scale_color_brewer(palette = 'Dark2')+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()

mod4.1_disp <- ggplot() +
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = dispersal_km, color = ordered(class))) +
  stat_lineribbon(data = pred_mod4.1.2[pred_mod4.1.2$.category=='dispersalkm',], aes(y = .prediction, x = Mass_kg, color = ordered(class)), .width = c(.99, .95, .8, .5), alpha = 0.25) +
  scale_color_brewer(palette = 'Dark2')+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()

mod4.1_mig <- ggplot() +
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = Migration_km, color = ordered(class))) +
  stat_lineribbon(data = pred_mod4.1.2[pred_mod4.1.2$.category=='Migrationkm',], aes(y = .prediction, x = Mass_kg, color = ordered(class)), .width = c(.99, .95, .8, .5), alpha = 0.25) +
  scale_color_brewer(palette = 'Dark2')+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()

mod4.1_figs <- ggarrange(mod4.1_hr, mod4.1_disp, mod4.1_mig, nrow=1)
ggsave(mod4.1_figs, filename = "mod4.1_figs.jpg", dpi = 'retina', width = 18, height = 6, units = 'in')

########## mod 3 - media ############
# mod4.1 <- readRDS("mods/mod4.1/mod4_1.rds")
summary(mod3)

pred_mod3.2 <- traits_phylo %>%
  group_by(media_simplified) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 322)) %>%
  add_predicted_draws(mod3.2)

mod3_hr <- ggplot() +
  geom_point(data = traits_media, aes(x = Mass_kg, y = mean.hra.m2, color = ordered(media_simplified))) +
  stat_lineribbon(data = pred_mod3.2[pred_mod3.2$.category=='meanhram2',], aes(y = .prediction, x = Mass_kg, color = ordered(media_simplified)), .width = c(.99, .95, .8, .5), alpha = 0.25) +
  scale_color_brewer(palette = 'Dark2')+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()

mod3_disp <- ggplot() +
  geom_point(data = traits_media, aes(x = Mass_kg, y = dispersal_km, color = ordered(media_simplified))) +
  stat_lineribbon(data = pred_mod3.2[pred_mod3.2$.category=='dispersalkm',], aes(y = .prediction, x = Mass_kg, color = ordered(media_simplified)), .width = c(.99, .95, .8, .5), alpha = 0.25) +
  scale_color_brewer(palette = 'Dark2')+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()

mod3_mig <- ggplot() +
  geom_point(data = traits_media, aes(x = Mass_kg, y = Migration_km, color = ordered(media_simplified))) +
  stat_lineribbon(data = pred_mod3.2[pred_mod3.2$.category=='Migrationkm',], aes(y = .prediction, x = Mass_kg, color = ordered(media_simplified)), .width = c(.99, .95, .8, .5), alpha = 0.25) +
  scale_color_brewer(palette = 'Dark2')+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()

mod3_figs <- ggarrange(mod3_hr, mod3_disp, mod3_mig, nrow=1)
ggsave(mod3_figs, filename = "mod3_figs.jpg", dpi = 'retina', width = 18, height = 6, units = 'in')

######## mod 5 - diet ##########
summary(mod5)

pred_mod5 <- traits_media %>%
  group_by(diet_broadest_cat) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 322)) %>%
  add_predicted_draws(mod5)

mod5_hr <- ggplot() +
  geom_point(data = traits_media, aes(x = Mass_kg, y = mean.hra.m2, color = ordered(diet_broadest_cat))) +
  stat_lineribbon(data = pred_mod5[pred_mod5$.category=='meanhram2',], aes(y = .prediction, x = Mass_kg, color = ordered(diet_broadest_cat)), .width = c(.99, .95, .8, .5), alpha = 0.25) +
  scale_color_brewer(palette = 'Dark2')+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()

mod5_disp <- ggplot() +
  geom_point(data = traits_media, aes(x = Mass_kg, y = dispersal_km, color = ordered(diet_broadest_cat))) +
  stat_lineribbon(data = pred_mod5[pred_mod5$.category=='dispersalkm',], aes(y = .prediction, x = Mass_kg, color = ordered(diet_broadest_cat)), .width = c(.99, .95, .8, .5), alpha = 0.25) +
  scale_color_brewer(palette = 'Dark2')+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()

mod5_mig <- ggplot() +
  geom_point(data = traits_media, aes(x = Mass_kg, y = Migration_km, color = ordered(diet_broadest_cat))) +
  stat_lineribbon(data = pred_mod5[pred_mod5$.category=='Migrationkm',], aes(y = .prediction, x = Mass_kg, color = ordered(diet_broadest_cat)), .width = c(.99, .95, .8, .5), alpha = 0.25) +
  scale_color_brewer(palette = 'Dark2')+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()

mod5_figs <- ggarrange(mod5_hr, mod5_disp, mod5_mig)
ggsave(mod5_figs, filename = "mod5_figs.jpg", dpi = 'retina', width = 12, height = 6, units = 'in')
