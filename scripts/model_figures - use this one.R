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


pred1_disp <- pred_mod1[pred_mod1$.category=='dispersalkm',]

mod1_disp <- ggplot()+
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = dispersal_km, color = ordered(class))) +
  stat_lineribbon(data =pred4.1_disp, 
                  aes(y = .prediction, x = Mass_kg, color = ordered(class), fill = ordered(class)), 
                  .width = c(.5), alpha = 0.3) +
  # geom_line(data = pred4.1_disp, aes(y=.prediction, x=Mass_kg))+
  scale_color_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
                                "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), 
                     name ="Class")+  
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Dispersal distance (km)")

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
mod2 <- readRDS("../mods/mod2/mod2.rds")
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
foraging_media <- readRDS("../mods/mod3/foraging_media.rds")
dispersal_media <- readRDS("../mods/mod3/dispersal_media.rds")
migration_media <- readRDS("../mods/mod3/migration_media.rds")

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
  stat_lineribbon(data = dispersal, aes(y = .prediction, x = Mass_kg, color = ordered(media_dispersal), fill = ordered(media_dispersal)), 
                  .width = c(.5), alpha = 0.5) +
  # scale_color_viridis(discrete = T, option ="D", name ="Medium of\n movement")+
  scale_color_manual(values = c("air" = "#440154FF","aquatic" = "#FDE725FF", 
                                "land" = "#5DC863FF", "marine" = "#31688EFF"), name ="Medium of\n movement")+
  scale_fill_manual(values = c("air" = "#440154FF","aquatic" = "#FDE725FF", 
                                "land" = "#5DC863FF", "marine" = "#31688EFF"), name ="Medium of\n movement")+
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                   labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                   labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Dispersal distance (km)")

foraging <- traits_for %>%
  group_by(media_foraging) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 273)) %>%
  add_predicted_draws(foraging_media)


foraging_plot <- ggplot() +
  geom_point(data = traits_for, aes(x = Mass_kg, y = hr.radius, color = ordered(media_foraging))) +
  stat_lineribbon(data = foraging, aes(y = .prediction, x = Mass_kg, color = ordered(media_foraging), fill = ordered(media_foraging)), 
                  .width = c(.5), alpha = 0.4) +
  scale_color_manual(values = c("air" = "#440154FF","aquatic" = "#FDE725FF", 
                                "land" = "#5DC863FF", "marine" = "#31688EFF"), name ="Medium of\n movement")+  
  scale_fill_manual(values = c("air" = "#440154FF","aquatic" = "#FDE725FF", 
                               "land" = "#5DC863FF", "marine" = "#31688EFF"), name ="Medium of\n movement")+ 
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Foraging radius (km)")


migration <- traits_mig %>%
  group_by(media_migration) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 142)) %>%
  add_predicted_draws(migration_media)

migration_plot <- ggplot() +
  geom_point(data = traits_mig, aes(x = Mass_kg, y = Migration_km, color = ordered(media_migration))) +
  stat_lineribbon(data = filter(migration, .prediction>0), aes(y = .prediction, x = Mass_kg, color = ordered(media_migration),  fill = ordered(media_migration)), 
                  #=='Migrationkm')&(pred_mod5$.prediction>0),],
                  .width = c(.5), alpha = 0.4) +
  scale_color_manual(values = c("air" = "#440154FF","aquatic" = "#FDE725FF", 
                                "land" = "#5DC863FF", "marine" = "#31688EFF"), name ="Medium of\n movement")+  
  scale_fill_manual(values = c("air" = "#440154FF","aquatic" = "#FDE725FF", 
                                "land" = "#5DC863FF", "marine" = "#31688EFF"), name ="Medium of\n movement")+ 
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Migration distance (km)")

# disp_for_figs <- ggarrange(foraging_plot, dispersal_plot, nrow = 1, common.legend = T, legend = "right")
media_figs <- ggarrange(foraging_plot, dispersal_plot, migration_plot, nrow = 1, 
                        common.legend = T, legend = "right", labels = c("a", "b", "c"))
ggsave(media_figs, filename = "../figures/media_figs.jpeg", dpi = 'retina', width = 10, height = 3, units = 'in')



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


########### mod 4 - class ###############
mod4.1 <- readRDS("../mods/mod4.1/mod4_1.rds")
# mod4.2 <- readRDS("../mods/mod4.2/mod4.2.rds")
# mod4.1.2 <- readRDS("../mods/mod4.1/mod4_1.2.rds")
# mod4.3.2 <- readRDS("../mods/mod4.1/mod4_3.2.rds")
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
 

class_foraging <- ggplot() +
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = hr.radius, color = ordered(class))) +
  stat_lineribbon(data = pred_mod4.1[pred_mod4.1$.category=='hrradius',], 
                  aes(y = .prediction, x = Mass_kg, color = ordered(class), fill = ordered(class)), 
                  .width = c(.5), alpha = 0.4) +
  scale_color_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
                                "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", 
                                "Reptilia" = "#FDE725FF"), name ="Class")+  
  scale_fill_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
                                "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", 
                                "Reptilia" = "#FDE725FF"), name ="Class")+  
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Foraging radius (km)")

pred4.1_disp <- pred_mod4.1[pred_mod4.1$.category=='dispersalkm',]
#pred4.1_summ <- point_interval(pred4.1_disp, .width = c(0.95, 0.8, 0.5, 0.25))

class_dispersal <- ggplot()+
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = dispersal_km, color = ordered(class))) +
  stat_lineribbon(data =pred4.1_disp,
                  aes(y = .prediction, x = Mass_kg,
                      color = ordered(class), fill = ordered(class)),
                  .width = 0.5, alpha = 0.4)+
  scale_color_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
                                "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), 
                     name ="Class")+  
  scale_fill_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
                                "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), 
                     name ="Class")+  
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Dispersal distance (km)")

#ggsave(class_disp, filename = "../figures/mod4.1_disp.jpg", dpi = 'retina', width = 4, height = 4, units = 'in')


class_migration <- ggplot() +
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = Migration_km, color = ordered(class))) +
  stat_lineribbon(data = pred_mod4.1[(pred_mod4.1$.category=='Migrationkm')&(pred_mod4.1$.prediction>0),], 
                  aes(y = .prediction, x = Mass_kg, color = ordered(class),fill =ordered(class)), 
                  .width = c(.5), alpha = 0.4) +
  scale_color_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
                                "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), 
                     name ="Class")+  
  scale_fill_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
                               "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), 
                    name ="Class")+  
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Migration distance (km)")

# class_mig <- ggplot() +
#   geom_point(data = traits_phylo, aes(x = Mass_kg, y = Migration_km, color = ordered(class))) +
#   stat_lineribbon(data = pred_mod4.1[(pred_mod4.1$.category=='Migrationkm'),], aes(y = .prediction, x = Mass_kg, color = ordered(class)), .width = c(.5), alpha = 0.25) +
#   scale_color_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
#                                 "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), name ="Class")+  
#   scale_x_log10()+
#   scale_y_log10()+
#   cowplot::theme_cowplot()+
#   xlab("Body mass (kg)")+
#   ylab("Migration distance (km)")

other_figs <- ggarrange(mam_hr, mam_disp, mam_mig, nrow=1, common.legend = TRUE, legend = "right")
Mega_fig <- ggarrange(aves_figs, mam_figs, other_figs, nrow=3)

class_figs <- ggarrange(class_foraging, class_dispersal, class_migration, nrow=1, 
                        common.legend = TRUE, legend = "right", labels = c("d", "e", "f"))

ggsave(class_figs, filename = "../figures/class_figs.jpg", dpi = 'retina', width = 10, height = 3, units = 'in')

######## mod 4 - order ########
# birds
mod4.3_aves <- readRDS("../mods/mod4.3/mod4.3_aves.rds")
pred_mod4.3_aves <- aves %>%
  group_by(order) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 159)) %>%
  add_predicted_draws(mod4.3_aves)


order_hr_aves <- ggplot() +
  geom_point(data = aves, aes(x = Mass_kg, y = hr.radius, color = ordered(order))) +
  stat_lineribbon(data = pred_mod4.3_aves[pred_mod4.3_aves$.category=='hrradius',], 
                  aes(y = .prediction, x = Mass_kg, color = ordered(order), fill = ordered(order)), 
                  .width = c(.5), alpha = 0.4) +
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Foraging radius (km)")

order_disp_aves <- ggplot() +
  geom_point(data = aves, aes(x = Mass_kg, y = dispersal_km, color = ordered(order))) +
  stat_lineribbon(data = pred_mod4.3_aves[pred_mod4.3_aves$.category=='dispersalkm',], 
                  aes(y = .prediction, x = Mass_kg, color = ordered(order), fill = ordered(order)), .width = c(.5), 
                  alpha = 0.4) +
 # scale_color_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
  #                              "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), name ="Class")+  
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Dispersal distance (km)")

order_mig_aves <- ggplot() +
  geom_point(data = aves, aes(x = Mass_kg, y = Migration_km, color = ordered(order))) +
  stat_lineribbon(data = pred_mod4.3_aves[(pred_mod4.3_aves$.category=='Migrationkm')&(pred_mod4.3_aves$.prediction>0),],
                  aes(y = .prediction, x = Mass_kg, color = ordered(order), fill = ordered(order)), 
                  .width = c(.5), alpha = 0.4) +
  # scale_color_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
  #                               "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), name ="Class")+  
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Migration distance (km)")

order_aves_figs <- ggarrange(order_hr_aves, order_disp_aves, order_mig_aves, nrow=1, common.legend = TRUE, legend = "right")
ggsave(order_aves_figs, filename = "../figures/mod4.3_aves_figs.jpg", dpi = 'retina', width = 10, height = 5, units = 'in')

## mammals
mod4.3_mam <- readRDS("../mods/mod4.3/mod4.3_mam.rds")

pred_mod4.3_mam <- mam %>%
  group_by(order) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 117)) %>%
  add_predicted_draws(mod4.3_mam)


order_hr_mam <- ggplot() +
  geom_point(data = mam, aes(x = Mass_kg, y = hr.radius, color = ordered(order))) +
  stat_lineribbon(data = pred_mod4.3_mam[pred_mod4.3_mam$.category=='hrradius',], aes(y = .prediction, x = Mass_kg, color = ordered(order), fill = ordered(order)), 
                  .width = c(.5), alpha = 0.4) +
  # scale_color_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
  #                               "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), name ="Class")+  
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Foraging radius (km)")

order_disp_mam <- ggplot() +
  geom_point(data = mam, aes(x = Mass_kg, y = dispersal_km, color = ordered(order))) +
  stat_lineribbon(data = pred_mod4.3_mam[pred_mod4.3_mam$.category=='dispersalkm',],
                  aes(y = .prediction, x = Mass_kg, color = ordered(order), fill = ordered(order)), 
                  .width = c(.5), alpha = 0.4) +
  # scale_color_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
  #                              "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), name ="Class")+  
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Dispersal distance (km)")

order_mig_mam <- ggplot() +
  geom_point(data = mam, aes(x = Mass_kg, y = Migration_km, color = ordered(order))) +
  stat_lineribbon(data = pred_mod4.3_mam[(pred_mod4.3_mam$.category=='Migrationkm') & (pred_mod4.3_mam$.prediction>0),], 
                  aes(y = .prediction, x = Mass_kg, color = ordered(order), fill = ordered(order)), 
                  .width = c(.5), alpha = 0.4) +
  # scale_color_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
  #                               "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), name ="Class")+  
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Migration distance (km)")

order_mam_figs <- ggarrange(order_hr_mam, order_disp_mam, order_mig_mam, nrow=1, common.legend = TRUE, legend = "right")
ggsave(order_mam_figs, filename = "../figures/mod4.3_mam_figs.jpg", dpi = 'retina', width = 10, height = 5, units = 'in')


## amphibs
mod4.3_amph <- readRDS("../mods/mod4.3/mod4.3_amph.rds")

pred_mod4.3_amph <- amph %>%
  group_by(order) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 21)) %>%
  add_predicted_draws(mod4.3_amph)


order_hr_amph <- ggplot() +
  geom_point(data = amph, aes(x = Mass_kg, y = hr.radius, color = ordered(order))) +
  stat_lineribbon(data = pred_mod4.3_amph[pred_mod4.3_amph$.category=='hrradius',], aes(y = .prediction, x = Mass_kg, color = ordered(order), fill = ordered(order)), 
                  .width = c(.5), alpha = 0.4) +
  # scale_color_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
  #                               "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), name ="Class")+  
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Foraging radius (km)")

order_disp_amph <- ggplot() +
  geom_point(data = amph, aes(x = Mass_kg, y = dispersal_km, color = ordered(order))) +
  stat_lineribbon(data = pred_mod4.3_amph[pred_mod4.3_amph$.category=='dispersalkm',],
                  aes(y = .prediction, x = Mass_kg, color = ordered(order), fill = ordered(order)), 
                  .width = c(.5), alpha = 0.4) +
  # scale_color_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
  #                              "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), name ="Class")+  
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Dispersal distance (km)")

order_mig_amph <- ggplot() +
  geom_point(data = amph, aes(x = Mass_kg, y = Migration_km, color = ordered(order))) +
  stat_lineribbon(data = pred_mod4.3_amph[(pred_mod4.3_amph$.category=='Migrationkm') & (pred_mod4.3_amph$.prediction>0),], 
                  aes(y = .prediction, x = Mass_kg, color = ordered(order), fill = ordered(order)), 
                  .width = c(.5), alpha = 0.4) +
  # scale_color_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
  #                               "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), name ="Class")+  
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Migration distance (km)")

order_amph_figs <- ggarrange(order_hr_amph, order_disp_amph, order_mig_amph, nrow=1, common.legend = TRUE, legend = "right")
ggsave(order_amph_figs, filename = "../figures/mod4.3_amph_figs.jpg", dpi = 'retina', width = 10, height = 5, units = 'in')

## reptiles
mod4.3_rep <- readRDS("../mods/mod4.3/mod4.3_rep.rds")
pred_mod4.3_rep <- rep %>%
  group_by(order) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 20)) %>%
  add_predicted_draws(mod4.3_rep)


order_hr_rep <- ggplot() +
  geom_point(data = rep, aes(x = Mass_kg, y = hr.radius, color = ordered(order))) +
  stat_lineribbon(data = pred_mod4.3_rep[pred_mod4.3_rep$.category=='hrradius',], aes(y = .prediction, x = Mass_kg, color = ordered(order), fill = ordered(order)), 
                  .width = c(.5), alpha = 0.4) +
  # scale_color_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
  #                               "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), name ="Class")+  
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Foraging radius (km)")

order_disp_rep <- ggplot() +
  geom_point(data = rep, aes(x = Mass_kg, y = dispersal_km, color = ordered(order))) +
  stat_lineribbon(data = pred_mod4.3_rep[pred_mod4.3_rep$.category=='dispersalkm',],
                  aes(y = .prediction, x = Mass_kg, color = ordered(order), fill = ordered(order)), 
                  .width = c(.5), alpha = 0.4) +
  # scale_color_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
  #                              "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), name ="Class")+  
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Dispersal distance (km)")

order_mig_rep <- ggplot() +
  geom_point(data = rep, aes(x = Mass_kg, y = Migration_km, color = ordered(order))) +
  stat_lineribbon(data = pred_mod4.3_rep[(pred_mod4.3_rep$.category=='Migrationkm') & (pred_mod4.3_rep$.prediction>0),], 
                  aes(y = .prediction, x = Mass_kg, color = ordered(order), fill = ordered(order)), 
                  .width = c(.5), alpha = 0.4) +
  # scale_color_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
  #                               "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), name ="Class")+  
  scale_x_log10()+
  scale_y_log10()+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Migration distance (km)")

order_rep_figs <- ggarrange(order_hr_rep, order_disp_rep, order_mig_rep, nrow=1, common.legend = TRUE, legend = "right")
ggsave(order_rep_figs, filename = "../figures/mod4.3_rep_figs.jpg", dpi = 'retina', width = 10, height = 5, units = 'in')


order_all_figs <- ggarrange(order_aves_figs, order_mam_figs, order_amph_figs, order_rep_figs, 
                            nrow=4, labels=c("birds", "mammals", "amphibians", "reptiles"), 
                            label.x = 0.05, label.y=1, common.legend = FALSE)
ggsave(order_all_figs, filename = "../figures/mod4.3_order_figs.jpg", dpi = 'retina', width = 10, height = 12, units = 'in')




######## mod 5 - diet ##########
mod5 <- readRDS("../mods/mod5_2.rds")
summary(mod5)
coef(mod5)

pred_mod5 <- traits_phylo %>%
  group_by(diet_broadest_cat) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 322)) %>%
  add_predicted_draws(mod5)

levels(traits_phylo$diet_broadest_cat)
## hex codes for viridis colours
scales::show_col(viridis_pal()(4))
## Carnivore == #440154FF
## Herbivore == #35B779FF
## Invertivore == #FDE725FF
## Omnivore == #31688EFF

mod5_foraging <- ggplot() +
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = hr.radius, color = ordered(diet_broadest_cat))) +
  stat_lineribbon(data = pred_mod5[pred_mod5$.category=='hrradius',], 
                  aes(y = .prediction, x = Mass_kg, color = ordered(diet_broadest_cat), fill = ordered(diet_broadest_cat)),
                  .width = c(.5), alpha = 0.4) +
  scale_color_manual(values = c("Carnivore" = "#440154FF","Herbivore" = "#35B779FF", 
                                "Invertivore" = "#FDE725FF", "Omnivore" = "#31688EFF"), name ="Trophic level")+  
  scale_fill_manual(values = c("Carnivore" = "#440154FF","Herbivore" = "#35B779FF", 
                                "Invertivore" = "#FDE725FF", "Omnivore" = "#31688EFF"), name ="Trophic level")+ 
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Foraging radius (km)")

mod5_dispersal <- ggplot() +
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = dispersal_km, color = ordered(diet_broadest_cat))) +
  stat_lineribbon(data = pred_mod5[pred_mod5$.category=='dispersalkm',], 
                  aes(y = .prediction, x = Mass_kg, color = ordered(diet_broadest_cat), fill = ordered(diet_broadest_cat)), 
                  .width = c(.5), alpha = 0.4) +
  scale_color_manual(values = c("Carnivore" = "#440154FF","Herbivore" = "#35B779FF", 
                                "Invertivore" = "#FDE725FF", "Omnivore" = "#31688EFF"), name ="Trophic level")+  
  scale_fill_manual(values = c("Carnivore" = "#440154FF","Herbivore" = "#35B779FF", 
                                "Invertivore" = "#FDE725FF", "Omnivore" = "#31688EFF"), name ="Trophic level")+
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Dispersal distance (km)")

mod5_migration <- ggplot() +
  geom_point(data = traits_phylo, aes(x = Mass_kg, y = Migration_km, color = ordered(diet_broadest_cat))) +
  stat_lineribbon(data = pred_mod5[(pred_mod5$.category=='Migrationkm')&(pred_mod5$.prediction>0),],
                  aes(y = .prediction, x = Mass_kg, color = ordered(diet_broadest_cat), fill = ordered(diet_broadest_cat)), 
                  .width = c(.5), alpha = 0.4) +
  scale_color_manual(values = c("Carnivore" = "#440154FF","Herbivore" = "#35B779FF", 
                                "Invertivore" = "#FDE725FF", "Omnivore" = "#31688EFF"), name ="Trophic level")+  
  scale_fill_manual(values = c("Carnivore" = "#440154FF","Herbivore" = "#35B779FF", 
                                "Invertivore" = "#FDE725FF", "Omnivore" = "#31688EFF"), name ="Trophic level")+
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Migration distance (km)")

mod5_figs <- ggarrange(mod5_foraging, mod5_dispersal, mod5_migration, nrow = 1, 
                       common.legend = T, legend = "right", labels = c("g", "h", "i"))
ggsave(mod5_figs, filename = "../figures/mod5_figs.jpg", dpi = 'retina', width = 10, height = 3, units = 'in')


mega_fig <- ggarrange(media_figs, class_figs, mod5_figs, nrow=3, common.legend=FALSE)
ggsave(mega_fig, filename = "../figures/Figure5_07Jan21.jpeg",
       dpi = "retina", width = 11, height = 9, units = "in")

# library(BayesianTools)
# bayesianSetup = createBayesianSetup(
#   likelihood = generateTestDensityMultiNormal(sigma = "no correlation"), 
#   lower = rep(-10, 3), upper = rep(10, 3))
# 
# out <- runMCMC(bayesianSetup = bayesianSetup, sampler = "Metropolis", 
#                settings = list(iterations = 2000, message = FALSE))
# 
# getVolume(out, prior = TRUE)
# 
# bayesianSetup = createBayesianSetup(
#   likelihood = generateTestDensityMultiNormal(sigma = "strongcorrelation"), 
#   lower = rep(-10, 3), upper = rep(10, 3))
# 
# out <- runMCMC(bayesianSetup = bayesianSetup, sampler = "Metropolis", 
#                settings = list(iterations = 2000, message = FALSE))
# 
# getVolume(out, prior = TRUE)


########## Migrators only ##########

#### Media ####
foraging_media_migrators <- readRDS( "../mods/foraging_media_migrators.rds")
dispersal_media_migrators <- readRDS("../mods/dispersal_media_migrators.rds")
migration_media_migrators <- readRDS("../mods/mod3/migration_media_migrators.rds")


foraging <- traits_for_migrators %>%
  group_by(media_foraging) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 273)) %>%
  add_predicted_draws(foraging_media_migrators)


foraging_plot <- ggplot() +
  geom_point(data = traits_for_migrators, aes(x = Mass_kg, y = hr.radius, color = ordered(media_foraging))) +
  stat_lineribbon(data = foraging, aes(y = .prediction, x = Mass_kg, color = ordered(media_foraging), fill = ordered(media_foraging)), 
                  .width = c(.5), alpha = 0.4) +
  scale_color_manual(values = c("air" = "#440154FF","aquatic" = "#FDE725FF", 
                                "land" = "#5DC863FF", "marine" = "#31688EFF"), name ="Medium of\n movement")+  
  scale_fill_manual(values = c("air" = "#440154FF","aquatic" = "#FDE725FF", 
                               "land" = "#5DC863FF", "marine" = "#31688EFF"), name ="Medium of\n movement")+ 
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Foraging radius (km)")



dispersal <- traits_disp_migrators %>%
  group_by(media_dispersal) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 180)) %>%
  add_predicted_draws(dispersal_media_migrators)

dispersal_plot <- ggplot() +
  geom_point(data = traits_disp_migrators, aes(x = Mass_kg, y = dispersal_km, color = ordered(media_dispersal))) +
  stat_lineribbon(data = dispersal, aes(y = .prediction, x = Mass_kg, color = ordered(media_dispersal), fill = ordered(media_dispersal)), 
                  .width = c(.5), alpha = 0.5) +
  # scale_color_viridis(discrete = T, option ="D", name ="Medium of\n movement")+
  scale_color_manual(values = c("air" = "#440154FF","aquatic" = "#FDE725FF", 
                                "land" = "#5DC863FF", "marine" = "#31688EFF"), name ="Medium of\n movement")+
  scale_fill_manual(values = c("air" = "#440154FF","aquatic" = "#FDE725FF", 
                               "land" = "#5DC863FF", "marine" = "#31688EFF"), name ="Medium of\n movement")+
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Dispersal distance (km)")


migration <- traits_mig_migrators %>%
  group_by(media_migration) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 180)) %>%
  add_predicted_draws(migration_media_migrators)

migration_plot <- ggplot() +
  geom_point(data = traits_mig_migrators, aes(x = Mass_kg, y = Migration_km, color = ordered(media_migration))) +
  stat_lineribbon(data = filter(migration, .prediction>0), aes(y = .prediction, x = Mass_kg, color = ordered(media_migration),  fill = ordered(media_migration)), 
                  #=='Migrationkm')&(pred_mod5$.prediction>0),],
                  .width = c(.5), alpha = 0.4) +
  scale_color_manual(values = c("air" = "#440154FF","aquatic" = "#FDE725FF", 
                                "land" = "#5DC863FF", "marine" = "#31688EFF"), name ="Medium of\n movement")+  
  scale_fill_manual(values = c("air" = "#440154FF","aquatic" = "#FDE725FF", 
                               "land" = "#5DC863FF", "marine" = "#31688EFF"), name ="Medium of\n movement")+ 
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Migration distance (km)")

# disp_for_figs <- ggarrange(foraging_plot, dispersal_plot, nrow = 1, common.legend = T, legend = "right")
media_figs <- ggarrange(foraging_plot, dispersal_plot, migration_plot, nrow = 1, 
                        common.legend = T, legend = "right", labels = c("a", "b", "c"))
ggsave(media_figs, filename = "../figures/migrators_media_figs.jpeg", dpi = 'retina', width = 10, height = 3, units = 'in')


#### Class ####

mod_class_migrators <- readRDS("../mods/mod4.1/mod_class_migrators.rds")

pred_class_migrators <- traits_migrators %>%
  group_by(class) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 184)) %>%
  add_predicted_draws(mod_class_migrators)


class_foraging <- ggplot() +
  geom_point(data = traits_migrators, aes(x = Mass_kg, y = hr.radius, color = ordered(class))) +
  stat_lineribbon(data = pred_class_migrators[pred_class_migrators$.category=='hrradius',], 
                  aes(y = .prediction, x = Mass_kg, color = ordered(class), fill = ordered(class)), 
                  .width = c(.5), alpha = 0.4) +
  scale_color_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
                                "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", 
                                "Reptilia" = "#FDE725FF"), name ="Class")+  
  scale_fill_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
                               "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", 
                               "Reptilia" = "#FDE725FF"), name ="Class")+  
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Foraging radius (km)")


pred_class_migrators_disp <- pred_class_migrators[pred_class_migrators$.category=='dispersalkm',]

class_dispersal <- ggplot()+
  geom_point(data = traits_migrators, aes(x = Mass_kg, y = dispersal_km, color = ordered(class))) +
  stat_lineribbon(data =pred_class_migrators_disp,
                  aes(y = .prediction, x = Mass_kg,
                      color = ordered(class), fill = ordered(class)),
                  .width = 0.5, alpha = 0.4)+
  scale_color_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
                                "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), 
                     name ="Class")+  
  scale_fill_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
                               "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), 
                    name ="Class")+  
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Dispersal distance (km)")



class_migration <- ggplot() +
  geom_point(data = traits_migrators, aes(x = Mass_kg, y = Migration_km, color = ordered(class))) +
  stat_lineribbon(data = pred_class_migrators[(pred_class_migrators$.category=='Migrationkm')&(pred_class_migrators$.prediction>0),], 
                  aes(y = .prediction, x = Mass_kg, color = ordered(class),fill =ordered(class)), 
                  .width = c(.5), alpha = 0.4) +
  scale_color_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
                                "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), 
                     name ="Class")+  
  scale_fill_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
                               "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), 
                    name ="Class")+  
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Migration distance (km)")

class_figs <- ggarrange(class_foraging, class_dispersal, class_migration, nrow=1, 
                        common.legend = TRUE, legend = "right", labels = c("a", "b", "c"))


ggsave(class_figs, filename = "../figures/migrators_class_figs.jpeg", dpi = 'retina', width = 10, height = 3, units = 'in')

#### Diet ####

mod_trophic_migrators <- readRDS("../mods/mod_trophic_migrators.rds")

pred_trophic_migrators <- traits_migrators %>%
  group_by(diet_broadest_cat) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 184)) %>%
  add_predicted_draws(mod_trophic_migrators)

trophic_foraging <- ggplot() +
  geom_point(data = traits_migrators, aes(x = Mass_kg, y = hr.radius, color = ordered(diet_broadest_cat))) +
  stat_lineribbon(data = pred_trophic_migrators[pred_trophic_migrators$.category=='hrradius',], 
                  aes(y = .prediction, x = Mass_kg, color = ordered(diet_broadest_cat), fill = ordered(diet_broadest_cat)),
                  .width = c(.5), alpha = 0.4) +
  scale_color_manual(values = c("Carnivore" = "#440154FF","Herbivore" = "#35B779FF", 
                                "Invertivore" = "#FDE725FF", "Omnivore" = "#31688EFF"), name ="Trophic level")+  
  scale_fill_manual(values = c("Carnivore" = "#440154FF","Herbivore" = "#35B779FF", 
                               "Invertivore" = "#FDE725FF", "Omnivore" = "#31688EFF"), name ="Trophic level")+ 
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Foraging radius (km)")

trophic_dispersal <- ggplot() +
  geom_point(data = traits_migrators, aes(x = Mass_kg, y = dispersal_km, color = ordered(diet_broadest_cat))) +
  stat_lineribbon(data = pred_trophic_migrators[pred_trophic_migrators$.category=='dispersalkm',], 
                  aes(y = .prediction, x = Mass_kg, color = ordered(diet_broadest_cat), fill = ordered(diet_broadest_cat)), 
                  .width = c(.5), alpha = 0.4) +
  scale_color_manual(values = c("Carnivore" = "#440154FF","Herbivore" = "#35B779FF", 
                                "Invertivore" = "#FDE725FF", "Omnivore" = "#31688EFF"), name ="Trophic level")+  
  scale_fill_manual(values = c("Carnivore" = "#440154FF","Herbivore" = "#35B779FF", 
                               "Invertivore" = "#FDE725FF", "Omnivore" = "#31688EFF"), name ="Trophic level")+
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Dispersal distance (km)")

trophic_migration <- ggplot() +
  geom_point(data = traits_migrators, aes(x = Mass_kg, y = Migration_km, color = ordered(diet_broadest_cat))) +
  stat_lineribbon(data = pred_trophic_migrators[(pred_trophic_migrators$.category=='Migrationkm')&(pred_trophic_migrators$.prediction>0),],
                  aes(y = .prediction, x = Mass_kg, color = ordered(diet_broadest_cat), fill = ordered(diet_broadest_cat)), 
                  .width = c(.5), alpha = 0.4) +
  scale_color_manual(values = c("Carnivore" = "#440154FF","Herbivore" = "#35B779FF", 
                                "Invertivore" = "#FDE725FF", "Omnivore" = "#31688EFF"), name ="Trophic level")+  
  scale_fill_manual(values = c("Carnivore" = "#440154FF","Herbivore" = "#35B779FF", 
                               "Invertivore" = "#FDE725FF", "Omnivore" = "#31688EFF"), name ="Trophic level")+
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Migration distance (km)")

trophic_figs <- ggarrange(trophic_foraging, trophic_dispersal, trophic_migration, nrow = 1, 
                       common.legend = T, legend = "right", labels = c("g", "h", "i"))
ggsave(trophic_figs, filename = "../figures/trophic_figs.jpg", dpi = 'retina', width = 10, height = 3, units = 'in')


############# Non-migrators #############

#### Media ####
foraging_media_nonmigrators <- readRDS("../mods/foraging_media_nonmigrators.rds")
dispersal_media_nonmigrators <- readRDS("../mods/dispersal_media_nonmigrators.rds")


foraging <- traits_for_nonmigrators %>%
  group_by(media_foraging) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 130)) %>%
  add_predicted_draws(foraging_media_nonmigrators)


foraging_plot <- ggplot() +
  geom_point(data = traits_for_nonmigrators, aes(x = Mass_kg, y = hr.radius, color = ordered(media_foraging))) +
  stat_lineribbon(data = foraging, aes(y = .prediction, x = Mass_kg, color = ordered(media_foraging), fill = ordered(media_foraging)), 
                  .width = c(.5), alpha = 0.4) +
  scale_color_manual(values = c("air" = "#440154FF","aquatic" = "#FDE725FF", 
                                "land" = "#5DC863FF", "marine" = "#31688EFF"), name ="Medium of\n movement")+  
  scale_fill_manual(values = c("air" = "#440154FF","aquatic" = "#FDE725FF", 
                               "land" = "#5DC863FF", "marine" = "#31688EFF"), name ="Medium of\n movement")+ 
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Foraging radius (km)")


dispersal <- traits_disp_nonmigrators %>%
  group_by(media_dispersal) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 134)) %>%
  add_predicted_draws(dispersal_media_nonmigrators)

dispersal_plot <- ggplot() +
  geom_point(data = traits_disp_nonmigrators, aes(x = Mass_kg, y = dispersal_km, color = ordered(media_dispersal))) +
  stat_lineribbon(data = dispersal, aes(y = .prediction, x = Mass_kg, color = ordered(media_dispersal), fill = ordered(media_dispersal)), 
                  .width = c(.5), alpha = 0.5) +
  # scale_color_viridis(discrete = T, option ="D", name ="Medium of\n movement")+
  scale_color_manual(values = c("air" = "#440154FF","aquatic" = "#FDE725FF", 
                                "land" = "#5DC863FF", "marine" = "#31688EFF"), name ="Medium of\n movement")+
  scale_fill_manual(values = c("air" = "#440154FF","aquatic" = "#FDE725FF", 
                               "land" = "#5DC863FF", "marine" = "#31688EFF"), name ="Medium of\n movement")+
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Dispersal distance (km)")


media_figs <- ggarrange(foraging_plot, dispersal_plot, nrow = 1, 
                        common.legend = T, legend = "right", labels = c("a", "b"))
ggsave(media_figs, filename = "../figures/nonmigrators_media_figs.jpeg", dpi = 'retina', width = 6.7, height = 3, units = 'in')

#### Class ####

mod_class_non_migrators <- readRDS("../mods/mod4.1/mod_class_nonmigrators.rds")

pred_class_nonmigrators <- traits_non_migrators %>%
  group_by(class) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 137)) %>%
  add_predicted_draws(mod_class_non_migrators)


class_foraging <- ggplot() +
  geom_point(data = traits_non_migrators, aes(x = Mass_kg, y = hr.radius, color = ordered(class))) +
  stat_lineribbon(data = pred_class_nonmigrators[pred_class_nonmigrators$.category=='hrradius',], 
                  aes(y = .prediction, x = Mass_kg, color = ordered(class), fill = ordered(class)), 
                  .width = c(.5), alpha = 0.4) +
  scale_color_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
                                "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", 
                                "Reptilia" = "#FDE725FF"), name ="Class")+  
  scale_fill_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
                               "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", 
                               "Reptilia" = "#FDE725FF"), name ="Class")+  
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Foraging radius (km)")


pred_class_nonmigrators_disp <- pred_class_nonmigrators[pred_class_nonmigrators$.category=='dispersalkm',]

class_dispersal <- ggplot()+
  geom_point(data = traits_non_migrators, aes(x = Mass_kg, y = dispersal_km, color = ordered(class))) +
  stat_lineribbon(data =pred_class_nonmigrators_disp,
                  aes(y = .prediction, x = Mass_kg,
                      color = ordered(class), fill = ordered(class)),
                  .width = 0.5, alpha = 0.4)+
  scale_color_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
                                "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), 
                     name ="Class")+  
  scale_fill_manual(values = c("Amphibia" = "#440154FF","Aves" = "#3B528BFF", 
                               "Chondrichthyes" = "#21908CFF", "Mammalia" = "#5DC863FF", "Reptilia" = "#FDE725FF"), 
                    name ="Class")+  
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Dispersal distance (km)")



class_figs <- ggarrange(class_foraging, class_dispersal, nrow=1, 
                        common.legend = TRUE, legend = "right", labels = c("a", "b", "c"))


ggsave(class_figs, filename = "../figures/nonmigrators_class_figs.jpeg", dpi = 'retina', width = 6.7, height = 3, units = 'in')


#### Diet ####

mod_trophic_nonmigrators <- readRDS("../mods/mod_trophic_nonmigrators.rds")

pred_trophic_nonmigrators <- traits_non_migrators %>%
  group_by(diet_broadest_cat) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 137)) %>%
  add_predicted_draws(mod_trophic_nonmigrators)

trophic_foraging <- ggplot() +
  geom_point(data = traits_non_migrators, aes(x = Mass_kg, y = hr.radius, color = ordered(diet_broadest_cat))) +
  stat_lineribbon(data = pred_trophic_nonmigrators[pred_trophic_nonmigrators$.category=='hrradius',], 
                  aes(y = .prediction, x = Mass_kg, color = ordered(diet_broadest_cat), fill = ordered(diet_broadest_cat)),
                  .width = c(.5), alpha = 0.4) +
  scale_color_manual(values = c("Carnivore" = "#440154FF","Herbivore" = "#35B779FF", 
                                "Invertivore" = "#FDE725FF", "Omnivore" = "#31688EFF"), name ="Trophic level")+  
  scale_fill_manual(values = c("Carnivore" = "#440154FF","Herbivore" = "#35B779FF", 
                               "Invertivore" = "#FDE725FF", "Omnivore" = "#31688EFF"), name ="Trophic level")+ 
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Foraging radius (km)")

trophic_dispersal <- ggplot() +
  geom_point(data = traits_non_migrators, aes(x = Mass_kg, y = dispersal_km, color = ordered(diet_broadest_cat))) +
  stat_lineribbon(data = pred_trophic_nonmigrators[pred_trophic_nonmigrators$.category=='dispersalkm',], 
                  aes(y = .prediction, x = Mass_kg, color = ordered(diet_broadest_cat), fill = ordered(diet_broadest_cat)), 
                  .width = c(.5), alpha = 0.4) +
  scale_color_manual(values = c("Carnivore" = "#440154FF","Herbivore" = "#35B779FF", 
                                "Invertivore" = "#FDE725FF", "Omnivore" = "#31688EFF"), name ="Trophic level")+  
  scale_fill_manual(values = c("Carnivore" = "#440154FF","Herbivore" = "#35B779FF", 
                               "Invertivore" = "#FDE725FF", "Omnivore" = "#31688EFF"), name ="Trophic level")+
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Body mass (kg)")+
  ylab("Dispersal distance (km)")



trophic_figs <- ggarrange(trophic_foraging, trophic_dispersal, nrow = 1, 
                          common.legend = T, legend = "right", labels = c("a", "b"))
ggsave(trophic_figs, filename = "../figures/nonmig_trophic_figs.jpg", dpi = 'retina', width = 6.7, height = 3, units = 'in')








