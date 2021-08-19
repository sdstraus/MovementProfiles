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

setwd("/Users/samstraus/github/SUPSmods/")

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
  ggplot(aes(x = Mass_kg, y = hr.radius, color = ordered(class))) +
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
  ggplot(aes(x = Mass_kg, y = hr.radius, color = ordered(class))) +
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
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = hr.radius, color = ordered(class))) +
  stat_lineribbon(data = pred_mod1[pred_mod1$.category=='hrradius',], aes(y = .prediction, x = Mass_kg, color = ordered(class)), .width = c(.99, .95, .8, .5), alpha = 0.25) +
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
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = hr.radius, color = ordered(class))) +
  stat_lineribbon(data = pred_mod2[pred_mod2$.category=='hrradius',], aes(y = .prediction, x = Mass_kg, color = ordered(class)), .width = c(.5), alpha = 0.25) +
  scale_color_brewer(palette = 'Dark2')+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()

mod2_disp <- ggplot() +
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = dispersal_km, color = ordered(class))) +
  stat_lineribbon(data = pred_mod2[pred_mod2$.category=='dispersalkm',], aes(y = .prediction, x = Mass_kg, color = ordered(class)), .width = c(.5), alpha = 0.25) +
  scale_color_brewer(palette = 'Dark2')+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()

mod2_mig <- ggplot() +
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = Migration_km, color = ordered(class))) +
  stat_lineribbon(data = pred_mod2[pred_mod2$.category=='Migrationkm',], aes(y = .prediction, x = Mass_kg, color = ordered(class)), .width = c(.5), alpha = 0.25) +
  # scale_color_brewer(palette = 'Dark2')+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()

mod2_figs <- ggarrange(mod2_hr, mod2_disp, mod2_mig, nrow =1, common.legend = T, legend = "right")
ggsave(mod2_figs, filename = "mod2_figs.jpg", dpi = 'retina', width = 10, height = 6, units = 'in')


########## mod 3 - media ############
### each SUP with own media 
foraging_media <- readRDS("mods/foraging_media.rds")
dispersal_media <- readRDS("mods/dispersal_media.rds")
migration_media <- readRDS("mods/migration_media.rds")

summary(foraging_media)$coefficients

## hex codes for viridis colours
scales::show_col(viridis_pal()(4))
## air == #440154FF
## aquatic == #FDE725FF
## land == #5DC863FF
## marine == #31688EFF

dispersal <- traits_disp %>%
  group_by(media_dispersal) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 311)) %>%
  add_predicted_draws(dispersal_media)

dispersal_plot <- ggplot() +
  geom_point(data = traits_disp, aes(x = Mass_kg, y = dispersal_km, color = ordered(media_dispersal))) +
  stat_lineribbon(data = dispersal, aes(y = .prediction, x = Mass_kg, color = ordered(media_dispersal)), .width = c(.5), alpha = 0.25) +
  # scale_color_viridis(discrete = T, option ="D", name ="Medium of\n movement")+
  scale_color_manual(values = c("air" = "#440154FF","aquatic" = "#FDE725FF", 
                                "land" = "#5DC863FF", "marine" = "#31688EFF"), name ="Medium of\n movement")+
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Dispersal distance (km)")

foraging <- traits_for %>%
  group_by(media_foraging) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 273)) %>%
  add_predicted_draws(foraging_media)

foraging_plot <- ggplot() +
  geom_point(data = traits_for, aes(x = Mass_kg, y = hr.radius, color = ordered(media_foraging))) +
  stat_lineribbon(data = foraging, aes(y = .prediction, x = Mass_kg, color = ordered(media_foraging)), .width = c(.5), alpha = 0.25) +
  scale_color_manual(values = c("air" = "#440154FF","aquatic" = "#FDE725FF", 
                                "land" = "#5DC863FF", "marine" = "#31688EFF"), name ="Medium of\n movement")+  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Home range radius (km)")

migration <- traits_mig %>%
  group_by(media_migration) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 142)) %>%
  add_predicted_draws(migration_media)

migration_plot <- ggplot() +
  geom_point(data = traits_mig, aes(x = Mass_kg, y = Migration_km, color = ordered(media_migration))) +
  stat_lineribbon(data = migration, aes(y = .prediction, x = Mass_kg, color = ordered(media_migration)), .width = c(.5), alpha = 0.25) +
  scale_color_manual(values = c("air" = "#440154FF","aquatic" = "#FDE725FF", 
                                "land" = "#5DC863FF", "marine" = "#31688EFF"), name ="Medium of\n movement")+  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Migration distance (km)")

# disp_for_figs <- ggarrange(foraging_plot, dispersal_plot, nrow = 1, common.legend = T, legend = "right")
media_figs <- ggarrange(foraging_plot, dispersal_plot, migration_plot, nrow = 1, common.legend = T, legend = "right")
ggsave(media_figs, filename = "media_figs.jpeg", dpi = 'retina', width = 10, height = 3, units = 'in')


# mod3.2 <- readRDS("../mods/mod3_2.rds")
# summary(mod3.2)
# 
# (str(filter(traits_phylo, !is.na(media_simplified))))
# 
# pred_mod3.2 <- filter(traits_phylo, !is.na(media_simplified)) %>%
#   group_by(media_simplified) %>%
#   data_grid(Mass_kg = seq_range(Mass_kg, n = 153)) %>% ## CHANGE THIS
#   add_predicted_draws(mod3.2)
# 
# 
# mod3_hr <- ggplot() +
#   geom_point(data = filter(traits_phylo, !is.na(media_simplified)), aes(x = Mass_kg, y = hr.radius, color = ordered(media_simplified))) +
#   stat_lineribbon(data = pred_mod3.2[pred_mod3.2$.category=='hrradius',], aes(y = .prediction, x = Mass_kg, color = ordered(media_simplified)), .width = c(.5), alpha = 0.25) +
#   scale_color_brewer(palette = 'Dark2')+
#   scale_x_log10()+
#   scale_y_log10()+
#   cowplot::theme_cowplot()
# 
# mod3_disp <- ggplot() +
#   geom_point(data = filter(traits_phylo, !is.na(media_simplified)), aes(x = Mass_kg, y = dispersal_km, color = ordered(media_simplified))) +
#   stat_lineribbon(data = pred_mod3.2[pred_mod3.2$.category=='dispersalkm',], aes(y = .prediction, x = Mass_kg, color = ordered(media_simplified)), .width = c(.5), alpha = 0.25) +
#   scale_color_brewer(palette = 'Dark2')+
#   scale_x_log10()+
#   scale_y_log10()+
#   cowplot::theme_cowplot()
# 
# mod3_mig <- ggplot() +
#   geom_point(data = filter(traits_phylo, !is.na(media_simplified)), aes(x = Mass_kg, y = Migration_km, color = ordered(media_simplified))) +
#   stat_lineribbon(data = pred_mod3.2[pred_mod3.2$.category=='Migrationkm',], aes(y = .prediction, x = Mass_kg, color = ordered(media_simplified)), .width = c(.5), alpha = 0.25) +
#   scale_color_brewer(palette = 'Dark2')+
#   scale_x_log10()+
#   scale_y_log10()+
#   cowplot::theme_cowplot()
# 
# mod3_figs <- ggarrange(mod3_hr, mod3_disp, mod3_mig, nrow=1, common.legend = T, legend = "right")
# ggsave(mod3_figs, filename = "mod3_figs_outlierNAremoved.jpg", dpi = 'retina', width = 12, height = 3, units = 'in')
# 
# 
# 
# #### air media is doing something weird###
# air <- traits_phylo %>% filter(media_simplified == "air")
# air.media <- lm(log10(hr.radius) ~ log10(Mass_kg), data = air)
# plot(log10(hr.radius) ~ log10(Mass_kg), data = air)
# abline(air.media)


########### mod 4 ###############
mod4.1 <- readRDS("../mods/mod4.1/mod4_1.rds")
mod4.1.2 <- readRDS("../mods/mod4.1/mod4_1.2.rds")
mod4.3.2 <- readRDS("../mods/mod4.1/mod4_3.2.rds")
loo(mod4.1.2, mod4.2)
summary(mod4.1)

pred_mod4.1 <- traits_phylo %>%
  group_by(class) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 322)) %>%
  add_predicted_draws(mod4.1)

#pred_mod4.1 <- filter(traits_phylo, class != "Aves" & class != "Mammalia") %>%
# mammals 117
# aves 159
# other 46
str(filter(traits_phylo, class != "Aves" & class != "Mammalia"))

levels(as.factor(traits_phylo$class))
## hex codes for viridis colours
scales::show_col(viridis_pal()(5))
# "Amphibia" == #440154FF      
# "Aves" == #3B528BFF         
# "Chondrichthyes" == #21908CFF 
# "Mammalia"== #5DC863FF       
# "Reptilia" == FDE725FF
 

class_hr <- ggplot() +
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = hr.radius, color = ordered(class))) +
  stat_lineribbon(data = pred_mod4.1[pred_mod4.1$.category=='hrradius',], aes(y = .prediction, x = Mass_kg, color = ordered(class)), .width = c(.5), alpha = 0.25) +
  scale_color_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
                                "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), name ="Class")+  
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Home range radius (km)")

class_disp <- ggplot() +
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = dispersal_km, color = ordered(class))) +
  stat_lineribbon(data = pred_mod4.1[pred_mod4.1$.category=='dispersalkm',], aes(y = .prediction, x = Mass_kg, color = ordered(class)), .width = c(.5), alpha = 0.25) +
  scale_color_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
                                "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), name ="Class")+  
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Dispersal distance (km)")

class_mig <- ggplot() +
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = Migration_km, color = ordered(class))) +
  stat_lineribbon(data = pred_mod4.1[pred_mod4.1$.category=='Migrationkm',], aes(y = .prediction, x = Mass_kg, color = ordered(class)), .width = c(.5), alpha = 0.25) +
  scale_color_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
                                "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), name ="Class")+  
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Migration distance (km)")

other_figs <- ggarrange(mam_hr, mam_disp, mam_mig, nrow=1, common.legend = TRUE, legend = "right")
Mega_fig <- ggarrange(aves_figs, mam_figs, other_figs, nrow=3)

class_figs <- ggarrange(class_hr, class_disp, class_mig, nrow=1, common.legend = TRUE, legend = "right")

ggsave(class_figs, filename = "mod4.1_figs.jpg", dpi = 'retina', width = 10, height = 3, units = 'in')



######## mod 5 - diet ##########
summary(mod5)
coef(mod5)
pred_mod5 <- traits_phylo %>%
  group_by(diet_broadest_cat) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 322)) %>%
  add_predicted_draws(mod5.2)

levels(traits_phylo$diet_broadest_cat)
## hex codes for viridis colours
scales::show_col(viridis_pal()(4))
## Carnivore == #440154FF
## Herbivore == #35B779FF
## Invertivore == #FDE725FF
## Omnivore == #31688EFF

mod5_hr <- ggplot() +
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = hr.radius, color = ordered(diet_broadest_cat))) +
  stat_lineribbon(data = pred_mod5[pred_mod5$.category=='hrradius',], aes(y = .prediction, x = Mass_kg, color = ordered(diet_broadest_cat)), .width = c(.5), alpha = 0.25) +
  scale_color_manual(values = c("Carnivore" = "#440154FF","Herbivore" = "#35B779FF", 
                                "Invertivore" = "#FDE725FF", "Omnivore" = "#31688EFF"), name ="Trophic level")+  
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Home range radius (km)")

mod5_disp <- ggplot() +
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = dispersal_km, color = ordered(diet_broadest_cat))) +
  stat_lineribbon(data = pred_mod5[pred_mod5$.category=='dispersalkm',], aes(y = .prediction, x = Mass_kg, color = ordered(diet_broadest_cat)), .width = c(.5), alpha = 0.25) +
  scale_color_manual(values = c("Carnivore" = "#440154FF","Herbivore" = "#35B779FF", 
                                "Invertivore" = "#FDE725FF", "Omnivore" = "#31688EFF"), name ="Trophic level")+  
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Dispersal distance (km)")

mod5_mig <- ggplot() +
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = Migration_km, color = ordered(diet_broadest_cat))) +
  stat_lineribbon(data = pred_mod5[pred_mod5$.category=='Migrationkm',], aes(y = .prediction, x = Mass_kg, color = ordered(diet_broadest_cat)), .width = c(.5), alpha = 0.25) +
  scale_color_manual(values = c("Carnivore" = "#440154FF","Herbivore" = "#35B779FF", 
                                "Invertivore" = "#FDE725FF", "Omnivore" = "#31688EFF"), name ="Trophic level")+   
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Migration distance (km)")

mod5_figs <- ggarrange(mod5_hr, mod5_disp, mod5_mig, nrow = 1, common.legend = T, legend = "right")
ggsave(mod5_figs, filename = "mod5_figs.jpg", dpi = 'retina', width = 10, height = 3, units = 'in')



library(BayesianTools)
bayesianSetup = createBayesianSetup(
  likelihood = generateTestDensityMultiNormal(sigma = "no correlation"), 
  lower = rep(-10, 3), upper = rep(10, 3))

out <- runMCMC(bayesianSetup = bayesianSetup, sampler = "Metropolis", 
               settings = list(iterations = 2000, message = FALSE))

getVolume(out, prior = TRUE)

bayesianSetup = createBayesianSetup(
  likelihood = generateTestDensityMultiNormal(sigma = "strongcorrelation"), 
  lower = rep(-10, 3), upper = rep(10, 3))

out <- runMCMC(bayesianSetup = bayesianSetup, sampler = "Metropolis", 
               settings = list(iterations = 2000, message = FALSE))

getVolume(out, prior = TRUE)
