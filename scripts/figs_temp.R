
#Class posterior means, generated for Figure S4

```{r Fig S4 - class posterior means}
# class_dispersal <- readRDS("../mods/class.disp_withyear.rds")
# class_foraging <- readRDS("../mods/class.for_withyear.rds")
# class_migration <- readRDS("../mods/class.mig_withyear.rds")

grid_foraging <-  traits %>%
  group_by(class) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 320), 
            foraging_year = seq(min(foraging_year, na.rm = TRUE), max(foraging_year, na.rm = TRUE), by = 7))

grid_dispersal <-  traits %>%
  group_by(class) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 320), 
            dispersal_year = seq(min(dispersal_year, na.rm = TRUE), max(dispersal_year, na.rm = TRUE), by = 7))

grid_migration <-  traits %>%
  group_by(class) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 320), 
            migration_year = seq(min(migration_year, na.rm = TRUE), max(migration_year, na.rm = TRUE), by = 7))
# foraging
means_foraging <-  grid_foraging %>%
  add_epred_draws(class_foraging)

preds_foraging <-  grid_foraging %>%
  add_predicted_draws(class_foraging)

#dispersal
means_dispersal <-  grid_dispersal %>%
  add_epred_draws(class_dispersal)

preds_dispersal <-  grid_dispersal %>%
  add_predicted_draws(class_dispersal)

#migration
means_migration <-  grid_migration %>%
  add_epred_draws(class_migration)

preds_migration <-  grid_migration %>%
  add_predicted_draws(class_migration)


class_for <- traits %>%
  ggplot(aes(y = class, x = log10(hr.radius))) +
  stat_interval(aes(x = log10(.prediction)), data = preds_dispersal[(preds_dispersal$.prediction>0),]) +
  stat_pointinterval(aes(x = log10(.epred)), data = means_foraging[(means_foraging$.epred>0),], .width = c(.66, .95), position = position_nudge(y = -0.3)) +
  geom_point() +
  scale_color_brewer()+
  cowplot::theme_cowplot()+
  labs(y = "Taxonomic class")

class_disp <- traits %>%
  ggplot(aes(y = class, x = log10(dispersal_km))) +
  stat_interval(aes(x = log10(.prediction)), data = preds_dispersal[(preds_dispersal$.prediction>0),]) +
  stat_pointinterval(aes(x = log10(.epred)), data = means_dispersal[(means_dispersal$.epred>0),], .width = c(.66, .95), position = position_nudge(y = -0.3)) +
  geom_point() +
  scale_color_brewer()+
  cowplot::theme_cowplot()+
  labs(y = "Taxonomic class")

class_mig <- traits %>%
  ggplot(aes(y = class, x = log10(Migration_km))) +
  stat_interval(aes(x = log10(.prediction)), 
                data = preds_migration[(preds_migration$.prediction>0),]) +
  stat_pointinterval(aes(x = log10(.epred)), 
                     data = means_migration[(means_migration$.epred>0),], 
                     .width = c(.66, .95), position = position_nudge(y = -0.3)) +
  geom_point() +
  scale_color_brewer()+
  cowplot::theme_cowplot()+
  labs(y = "Taxonomic class")



class_figs_posterior_means <- ggarrange(class_for, class_disp, class_mig, nrow = 1, 
                                        common.legend = T, legend = "right", labels = c("a", "b", "c"))


while (!is.null(dev.list()))  dev.off()
jpeg(filename = "../figures/class_figs_posteriormeans.jpeg", width = 12, height = 3, units = 'in', res = 1000)
class_figs_posterior_means
dev.off()
```


#Trophic guild posterior means, generated for Figure S4

```{r Fig S4 - trophic guild posterior means}
# trophic_foraging <- readRDS("../mods/trophic.for_withyear.rds")
# trophic_dispersal <- readRDS("../mods/trophic.disp_withyear.rds")
# trophic_migration <- readRDS("../mods/trophic.mig_withyear.rds")

grid_foraging_trophic <-  traits %>%
  group_by(diet_broadest_cat) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 320), 
            foraging_year = seq(min(foraging_year, na.rm = TRUE), max(foraging_year, na.rm = TRUE), by = 7))

grid_dispersal_trophic <-  traits %>%
  group_by(diet_broadest_cat) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 320), 
            dispersal_year = seq(min(dispersal_year, na.rm = TRUE), max(dispersal_year, na.rm = TRUE), by = 7))

grid_migration_trophic <-  traits %>%
  group_by(diet_broadest_cat) %>%
  data_grid(Mass_kg = seq_range(Mass_kg, n = 320), 
            migration_year = seq(min(migration_year, na.rm = TRUE), max(migration_year, na.rm = TRUE), by = 7))
# foraging
means_foraging_trophic <-  grid_foraging_trophic %>%
  add_epred_draws(trophic_foraging)

preds_foraging_trophic <-  grid_foraging_trophic %>%
  add_predicted_draws(trophic_foraging)

#dispersal
means_dispersal_trophic <-  grid_dispersal_trophic %>%
  add_epred_draws(trophic_dispersal)

preds_dispersal_trophic <-  grid_dispersal_trophic %>%
  add_predicted_draws(trophic_dispersal)

#migration
means_migration_trophic <-  grid_migration_trophic %>%
  add_epred_draws(trophic_migration)

preds_migration_trophic <-  grid_migration_trophic %>%
  add_predicted_draws(trophic_migration)


trophic_for <- traits %>%
  ggplot(aes(y = diet_broadest_cat, x = log10(hr.radius))) +
  stat_interval(aes(x = log10(.prediction)), 
                data = preds_foraging_trophic[(preds_foraging_trophic$.prediction>0),]) +
  stat_pointinterval(aes(x = log10(.epred)), data = means_foraging_trophic[(means_foraging_trophic$.epred>0),], .width = c(.66, .95), position = position_nudge(y = -0.3)) +
  geom_point() +
  scale_color_brewer()+
  cowplot::theme_cowplot()+
  labs(y = "Taxonomic class")

trophic_disp <- traits %>%
  ggplot(aes(y = diet_broadest_cat, x = log10(dispersal_km))) +
  stat_interval(aes(x = log10(.prediction)), 
                data = preds_dispersal_trophic[(preds_dispersal_trophic$.prediction>0),]) +
  stat_pointinterval(aes(x = log10(.epred)), data = means_dispersal_trophic[(means_dispersal_trophic$.epred>0),],
                     .width = c(.66, .95), position = position_nudge(y = -0.3)) +
  geom_point() +
  scale_color_brewer()+
  cowplot::theme_cowplot()+
  labs(y = "Taxonomic class")

trophic_mig <- traits %>%
  ggplot(aes(y = diet_broadest_cat, x = log10(Migration_km))) +
  stat_interval(aes(x = log10(.prediction)), 
                data = preds_migration_trophic[(preds_migration_trophic$.prediction>0),]) +
  stat_pointinterval(aes(x = log10(.epred)), 
                     data = means_migration_trophic[(means_migration_trophic$.epred>0),], 
                     .width = c(.66, .95), position = position_nudge(y = -0.3)) +
  geom_point() +
  scale_color_brewer()+
  cowplot::theme_cowplot()+
  labs(y = "Taxonomic class")



trophic_figs_posterior_means <- ggarrange(trophic_for, trophic_disp, trophic_mig, nrow = 1, 
                                          common.legend = T, legend = "right", labels = c("d", "e", "f"))


while (!is.null(dev.list()))  dev.off()
jpeg(filename = "../figures/trophic_figs_posteriormeans.jpeg", width = 12, height = 3, units = 'in', res = 1000)
trophic_figs_posterior_means
dev.off()

```