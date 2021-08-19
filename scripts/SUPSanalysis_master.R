library(tidyverse)
library(googlesheets)
library(cowplot)
library(ggplot2)
library(lme4)
library(scatterplot3d)
library(VennDiagram)
library(scales)
library(gridExtra)
library(ggpubr)
library(rredlist)
library(quantreg)
library(viridis)
library(dplyr)
library(tidyr)
library(car)
library(plot3D)

#source("code/multiplot.R")
traits_clean <- read.csv("../data/Traits_Final.csv", fileEncoding="latin1")

# ## load phylogeny
# 
# one_phylo <- readRDS("../data/final_phylo.rds")

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


####### histograms ########
traits <- traits_phylo
show_col(scales::viridis_pal(option = "D")(4))

media_cols <- c(air = "#440154FF", land = "#35B779FF", aquatic = "#31688EFF", marine = "#FDE725FF")

# hist by media
traits$media_foraging[which(traits$media_foraging =="water")] <- "aquatic"
p1 <- traits %>% 
  filter(media_foraging == "air" | media_foraging == "land" | 
           media_foraging == "aquatic"| media_foraging == "marine") %>% 
  ggplot(aes(x = (hr.radius))) +
  geom_histogram(aes(color = media_foraging, fill = media_foraging),
                 position = "identity",alpha = 0.2, bins = 30)+
  scale_color_manual(values = media_cols, name ="Medium of\n movement")+
  scale_fill_manual(values = media_cols, name ="Medium of\n movement") +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Home range radius (km)")

traits$media_dispersal[which(traits$media_dispersal =="water")] <- "aquatic"
p2 <- traits %>% 
  filter(media_dispersal == "air" | media_dispersal == "land" | 
           media_dispersal == "aquatic"| media_dispersal == "marine") %>% 
  ggplot(aes(x = (dispersal_km))) +
  geom_histogram(aes(color = media_dispersal, fill = media_dispersal),
                 position = "identity",alpha = 0.2, bins = 30)+
  scale_color_manual(values = media_cols, name ="Medium of\n movement")+
  scale_fill_manual(values = media_cols, name ="Medium of\n movement") +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Dispersal distance (km)")


traits$media_migration[which(traits$media_migration =="water")] <- "aquatic"
p3 <- traits %>% 
  filter(media_migration == "air" | media_migration == "land" | 
           media_migration == "aquatic"| media_migration == "marine") %>% 
  ggplot(aes(x = (Migration_km + 1))) +
  geom_histogram(aes(color = media_migration, fill = media_migration),
                 position = "identity",alpha = 0.2, bins = 30)+
  scale_color_manual(values = media_cols, name ="Medium of\n movement")+
  scale_fill_manual(values = media_cols, name ="Medium of\n movement") +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Migration distance (km + 1)")
hist.by.media <- ggarrange(p1,p2,p3, nrow = 1, common.legend = T, legend = "right")

# class
show_col(scales::viridis_pal(option = "D")(5))
class_cols <- c(Amphibia = "#440154FF", Aves = "#3B528BFF", Chondricthyes = "#21908CFF", 
                Mammalia = "#5DC863FF", Reptilia = "#FDE725FF")


p1 <- ggplot(traits, aes(x = (hr.radius))) +
  geom_histogram(aes(color = class, fill = class),
                 position = "identity",alpha = 0.2, bins = 30)+
  scale_color_manual(values = class_cols, name = "Class")+
  scale_fill_manual(values = class_cols, name = "Class") +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Home range radius (km)")

p2 <- ggplot(traits, aes(x = (dispersal_km))) +
  geom_histogram(aes(color = class, fill = class),
                 position = "identity",alpha = 0.2, bins = 30)+
  scale_color_manual(values = class_cols, name = "Class")+
  scale_fill_manual(values = class_cols, name = "Class") +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Dispersal distance (km)")

p3 <- ggplot(traits, aes(x = (Migration_km + 1))) +
  geom_histogram(aes(color = class, fill = class),
                 position = "identity",alpha = 0.2, bins = 30)+
  scale_color_manual(values = class_cols, name = "Class")+
  scale_fill_manual(values = class_cols, name = "Class") +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Migration distance (km + 1)")

hist.by.class <- ggarrange(p1,p2,p3, nrow = 1, common.legend = T, legend = "right")


# by diet
show_col(scales::viridis_pal(option = "D")(4))
diet_cols <- c(Carnivore = "#440154FF", Herbivore = "#31688EFF", 
                Invertivore = "#35B779FF", Omnivore = "#FDE725FF")

p1 <-  traits %>% 
  filter(diet_broadest_cat != "") %>%
  ggplot(aes(x = (hr.radius))) +
  geom_histogram(aes(color = diet_broadest_cat, fill = diet_broadest_cat),
                 position = "identity",alpha = 0.2, bins = 30)+
  scale_color_manual(values = diet_cols, name = "Trophic level")+
  scale_fill_manual(values = diet_cols, name = "Trophic level") +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Home range radius (km)")

p2 <- traits %>% 
  filter(diet_broadest_cat != "") %>% 
  ggplot(aes(x = (dispersal_km))) +
  geom_histogram(aes(color = diet_broadest_cat, fill = diet_broadest_cat),
                 position = "identity",alpha = 0.2, bins = 30)+
  scale_color_manual(values = diet_cols, name = "Trophic level")+
  scale_fill_manual(values = diet_cols, name = "Trophic level") +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Dispersal distance (km)")

p3 <- traits %>% 
  filter(diet_broadest_cat != "") %>% 
  ggplot(aes(x = (Migration_km + 1))) +
  geom_histogram(aes(color = diet_broadest_cat, fill = diet_broadest_cat),
                 position = "identity",alpha = 0.2, bins = 30)+
  scale_color_manual(values = diet_cols, name = "Trophic level")+
  scale_fill_manual(values = diet_cols, name = "Trophic level") +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Migration distance (km + 1)")

hist.by.diet <- ggarrange(p1,p2,p3, nrow = 1, common.legend = T, legend = "right")



hist.all <- ggarrange(hist.by.media, hist.by.class, hist.by.diet, nrow =3)
ggsave(hist.all, filename = "../figures/figure2_17Aug.jpg", dpi = "retina", units = "in", height = 6, width = 10)



####### 3d scatterplot #######
traits.all.posmig <- traits[-c(which(traits$Migration_km == 0)),]


fit <- lm(log10(dispersal_km) ~log10(mean.hra.m2)+ log10(Migration_km), data = traits.all.posmig)
car::Anova(fit, type = 3)
summary(fit)

plot3D::scatter3D(x = log10(traits.all.posmig$dispersal_km), 
          y=log10(traits.all.posmig$Migration_km), 
          z=log10(traits.all.posmig$mean.hra.m2), 
          colvar = log10(traits.all.posmig$Mass_kg),
          col = ramp.col(c("#35B779FF", "#26828EFF", "#440154FF")),
          phi = 20, theta = 20,
          pch=20, cex =2.5, ticktype = "detailed",
          type = "h",
          xlab = "Dispersal (km)",
          ylab = "Migration (km)",
          zlab = "Foraging (km)",
          clab = c("Body Mass",
                   "(kg)"))
          # ,
          # surf = list(x = x.pred, y = y.pred, z = z.pred,  
          #             facets = NA, fit = fitpoints))

#use theta =, phi = to change orientation of cube, default values 40

#### including body size as point size, migration as colour
F2a <- traits %>% 
  filter(hr.radius > 0, Migration_km >0, !is.na(class)) %>% 
  mutate(fit=predict(f2a.fit)) %>% 
  ggplot(aes(x = (meanmedian_km), y = log10(hr.radius)))+
  geom_point(aes(color = log10(Migration_km), size = log10(Mass_kg)))+
  geom_smooth(method = "lm", aes(y = fit), se = F)+
  xlab("Dispersal scale (km)")+
  ylab("Foraging scale (km)")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x)))+
  scale_color_viridis(option ="magma")+
  labs(color = "Migration (km)", size = "Body mass (kg)")+
  theme_classic()+
  theme(legend.key = element_rect(colour = "white", fill = "white"))+
  theme(axis.text = element_text(size = 25, color = "black"),
        axis.title = element_text(size = 25),
        legend.text=element_text(size=18),
        legend.title = element_text(size=16),
        axis.line = element_line(size = 1.3))
F2a

##### pairwise correlations btw spatial use properties ####
par(mar=c(5,6,4,1)+.1)
plot(log10(mean.hra.km2) ~ log10(Migration_km), traits)
plot(log10(mean.hra.km2) ~ log10(dispersal_km), traits)
plot(log10(Migration_km) ~ log10(dispersal_km), traits)

plot(log(Migration_km) ~ log(Mass_kg), traits, col = traits$class)
legend(7,4.3,unique(traits$class),col=1:length(traits$class),pch=1)

plot(log(dispersal_km) ~ log(Mass_kg), traits)
plot(log(mean.hra.km2) ~ log(Mass_kg), traits)
plot(mean.hra.km2 ~ Mass_kg, data = traits, log = 'xy')

########## Figure 1 #########
# migration and home range

# #test for heteroscedasticity
# require(car)
# require(ncvTest)
# ncvTest(lm1)
# lm1 <- lm(from_body_size_log ~ to_body_size_log, int_bs)

traits.mig.hr <- traits[(which(is.na(traits$Migration_km) == F & is.na(traits$mean.hra.km2) == F)),]
traits.mig.hr$class <- factor(traits.mig.hr$class)
traits.mig.hr <- traits.mig.hr[-c(which(traits.mig.hr$Migration_km == 0)),]

traits.mig.pos <- traits[-c(which(traits$Migration_km == 0)),]

f2a.fit <- lmer(log10(mean.hra.km2) ~ log10(Migration_km) + (1|class), data = traits.mig.hr)
Anova(f2a.fit)
summary(f2a.fit)
f2a.fit2 <- lm(log10(mean.hra.km2) ~ log10(Migration_km), data = traits.mig.hr)
Anova(f2a.fit2)
anova(f2a.fit, f2a.fit2)

# qs <- c(0.025, 0.5, 0.975)
# f2a.qr <- rq(log10(mean.hra.km2) ~ log10(Migration_km), data = traits.mig.hr, tau = qs)
# summary(f2a.qr)

###
###
f2a.fit <- lmer(log10(hr.radius) ~ log10(Migration_km) + (1|class), data = filter(traits, hr.radius > 0, Migration_km >0, !is.na(class)))
f2a.fit2 <- lmer(log10(hr.radius) ~ log10(Migration_km) + (log10(Migration_km)|class), data = filter(traits, hr.radius > 0, Migration_km >0, !is.na(class)))

Anova(f2a.fit)
Anova(f2a.fit2)
summary(f3a.fit2)

AIC(f2a.fit, f2a.fit2) #fit 1 better (but AIC diff. less than 3)
anova(f2a.fit, f2a.fit2)
summary(f3b.fit2)

###
###

F2a <- traits %>% 
  filter(hr.radius > 0, Migration_km >0, !is.na(class)) %>% 
  mutate(fit=predict(f2a.fit)) %>% 
  ggplot(aes(x = (Migration_km), y = log10(hr.radius)))+
  geom_point(size = 4, aes(color = class, fill = class, shape = class))+
  geom_smooth(method = "lm", aes(y = fit, colour = class, group = class), size = 2, se = F)+
  xlab("Migration scale (km)")+
  ylab("Foraging scale (km)")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x)))+
  theme_classic()+
  # scale_shape_manual(values = c(21,22,23))+
  # scale_color_manual(values = c("AMPHIBIA" = "#440154FF","AVES" = "#2A788EFF", "MAMMALIA" = "#7AD151FF"))+
  # scale_fill_manual(values = c("AMPHIBIA" = "#440154FF","AVES" = "#2A788EFF", "MAMMALIA" = "#7AD151FF"))+
  theme(legend.key = element_rect(colour = "white", fill = "white"))+
  theme(axis.text = element_text(size = 25, color = "black"),
        axis.title = element_text(size = 25),
        legend.text=element_text(size=18),
        legend.title = element_text(size=16),
        axis.line = element_line(size = 1.3))+
  theme(legend.key.width=unit(3,"line"))
F2a

# dipsersal and home range
traits.disp.hr <- traits[(which(is.na(traits$dispersal_km) == F & is.na(traits$mean.hra.km2) == F)),]
traits.disp.hr$class <- factor(traits.disp.hr$class)

f2b.fit <- lmer(log10(mean.hra.km2) ~ log10(dispersal_km) + (1|class), data = traits.disp.hr)
Anova(f2b.fit)
summary(f2b.fit)
f2b.fit2 <- lm(log10(mean.hra.km2) ~ log10(dispersal_km), data = traits.disp.hr)
Anova(f2b.fit2)
anova(f2b.fit, f2b.fit2)

###
###
f2b.fit <- lmer(log10(hr.radius) ~ log10(dispersal_km) + (1|class), data = filter(traits, hr.radius > 0, dispersal_km >0, !is.na(class)))
f2b.fit2 <- lmer(log10(hr.radius) ~ log10(dispersal_km) + (log10(dispersal_km)|class), data = filter(traits, hr.radius > 0, dispersal_km >0, !is.na(class)))

Anova(f2b.fit)
Anova(f2b.fit2)
anova(f2b.fit, f2b.fit2)
summary(f2b.fit2)

AIC(f2b.fit, f2b.fit2) #fit 2 better (but AIC diff. less than 10)
summary(f3b.fit2)
###
###

F2b <- traits %>% 
  filter(hr.radius >0, dispersal_km >0, !is.na(class)) %>% 
  mutate(fit=predict(f2b.fit2)) %>% 
  ggplot(aes(x = (dispersal_km), y = log10(hr.radius)))+
  geom_point(size = 4, aes(color = class, fill = class, shape = class))+
  geom_smooth(method = "lm", aes(y = fit, colour = class, group = class), size = 2, se =F)+
  xlab("Dispersal scale (km)")+
  ylab("Foraging scale (km)")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x)))+
  theme_classic()+
  # scale_shape_manual(values = c(21,22,23))+
  # scale_color_manual(values = c("AMPHIBIA" = "#440154FF","AVES" = "#2A788EFF", "MAMMALIA" = "#7AD151FF"))+
  # scale_fill_manual(values = c("AMPHIBIA" = "#440154FF","AVES" = "#2A788EFF", "MAMMALIA" = "#7AD151FF"))+
  theme(legend.key = element_rect(colour = "white", fill = "white"))+
  theme(axis.text = element_text(size = 25, color = "black"),
        axis.title = element_text(size = 25),
        legend.text=element_text(size=18),
        legend.title = element_text(size=16),
        axis.line = element_line(size = 1.3))+
  theme(legend.key.width=unit(3,"line"))
F2b

# migration and dispersal
#length(which(is.na(traits$Migration_km) == F & is.na(traits$meanmedian_km) == F))
traits.mig.disp <- traits[(which(is.na(traits$Migration_km) == F & is.na(traits$dispersal_km) == F)),]
traits.mig.disp$class <- factor(traits.mig.disp$class)
traits.mig.disp <- traits.mig.disp[-c(which(traits.mig.disp$Migration_km == 0)),]

f2c.fit <- lmer(log10(Migration_km) ~ log10(dispersal_km) + (1|class), data = traits.mig.disp)
Anova(f2c.fit)
summary(f2c.fit)

###
###
f2c.fit <- lmer(log10(Migration_km) ~ log10(dispersal_km) + (1|class), data = filter(traits, Migration_km > 0, dispersal_km >0, !is.na(class), !class %in% "INSECTA"))
f2c.fit2 <- lmer(log10(Migration_km) ~ log10(dispersal_km) + (log10(dispersal_km)|class), data = filter(traits, Migration_km > 0, dispersal_km >0, !is.na(class), !class %in% "INSECTA"))

Anova(f2c.fit)
Anova(f2c.fit2)
anova(f2c.fit, f2b.fit2)
summary(f2c.fit)

AIC(f2c.fit, f2c.fit2) #fit 1 better (but AIC diff. less than 3)
summary(f3c.fit2)
###
###

F2c <- traits %>% 
  filter(Migration_km >0, dispersal_km >0, !is.na(class), !class %in% "INSECTA") %>% 
  mutate(fit=predict(f2c.fit)) %>% 
  ggplot(aes(x = (dispersal_km), y = log10(Migration_km)))+
  geom_point(size = 4, aes(color = class, fill = class, shape = class))+
  geom_smooth(method = "lm", aes(y = fit, colour = class, group = class), size = 2, se = F)+
  xlab("Dispersal scale (km)")+
  ylab("Migration scale (km)")+
  theme_classic()+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x)))+
  # scale_shape_manual(values = c(21,22,23))+
  # scale_color_manual(values = c("AMPHIBIA" = "#440154FF","AVES" = "#2A788EFF", "MAMMALIA" = "#7AD151FF"))+
  # scale_fill_manual(values = c("AMPHIBIA" = "#440154FF","AVES" = "#2A788EFF", "MAMMALIA" = "#7AD151FF"))+
  theme(legend.key = element_rect(colour = "white", fill = "white"))+
  theme(axis.text = element_text(size = 25, color = "black"),
        axis.title = element_text(size = 25),
        legend.text=element_text(size=18),
        legend.title = element_text(size=16),
        axis.line = element_line(size = 1.3))+
  theme(legend.key.width=unit(3,"line"))
F2c

test <- ggplot(data = traits.mig.hr, aes(x = Migration_km, y = mean.hra.km2, color = class)) +  geom_point(aes(color = class))
leg <- get_legend(test)
legend <- as_ggplot(leg)

multiplot(F2a, F2b, F2c, cols = 2)

####### Figure 2 #########
f3a.fit <- lmer(log10(dispersal_km) ~ log10(Mass_kg) + (1|class), data = traits)
Anova(f3a.fit)
summary(f3a.fit)

###
f3a.fit <- lmer(log10(dispersal_km) ~ log10(Mass_kg) + (1|class), data = filter(traits, dispersal_km > 0, Mass_kg >0, !is.na(class), !class %in% "INSECTA"))
f3a.fit2 <- lmer(log10(dispersal_km) ~ log10(Mass_kg) + (log10(Mass_kg)|class), data = filter(traits, dispersal_km > 0, Mass_kg >0, !is.na(class), !class %in% "INSECTA"))

Anova(f3a.fit)
Anova(f3a.fit2)
summary(f3a.fit2)

AIC(f3a.fit, f3a.fit2) #fit 2 better
anova(f3a.fit, f3a.fit2)
summary(f3b.fit2)
length(fitted(f3a.fit))
###

# dispersal and body size
show_col(viridis_pal()(10))

# ampbhibian = #440154FF, birds = #2A788EFF, mammals = #7AD151FF
F3a <- traits %>% 
  filter(dispersal_km >0, Mass_kg >0, !is.na(class), !class %in% "INSECTA") %>% 
  mutate(fit=predict(f3a.fit2)) %>% 
  ggplot(size = 4, aes(x = (Mass_kg), y = log10(dispersal_km)))+
  geom_point(size = 4, aes(colour = class, fill = class, shape = class))+
  geom_smooth(method = "lm", aes(y = fit, colour = class, group = class), size = 2, se = F)+
  xlab("Body size (kg)")+
  ylab("Dispersal scale (km)")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x)))+
  theme_classic()+
  # scale_shape_manual(values = c(21,22,23))+
  # scale_color_manual(values = c("AMPHIBIA" = "#440154FF","AVES" = "#2A788EFF", "MAMMALIA" = "#7AD151FF"))+
  # scale_fill_manual(values = c("AMPHIBIA" = "#440154FF","AVES" = "#2A788EFF", "MAMMALIA" = "#7AD151FF"))+
  theme(legend.key = element_rect(colour = "white", fill = "white"))+
  theme(axis.text = element_text(size = 25, color = "black"),
        axis.title = element_text(size = 25),
        legend.text=element_text(size=18),
        legend.title = element_text(size=16),
        axis.line = element_line(size = 1.3))+
  theme(legend.key.width=unit(3,"line"))
F3a

# migration and body size
f3b.fit <- lmer(log10(Migration_km) ~ log10(Mass_kg) + (1|class), data = filter(traits, Migration_km >0, Mass_kg >0, !is.na(class), !class %in% "INSECTA", !class %in% "CEPHALASPIDOMORPHI"))
f3b.fit2 <- lmer(log10(Migration_km) ~ log10(Mass_kg) + (log10(Mass_kg)|class), data = filter(traits, Migration_km >0, Mass_kg >0, !is.na(class), !class %in% "INSECTA", !class %in% "CEPHALASPIDOMORPHI"))

Anova(f3b.fit)
Anova(f3b.fit2)
summary(f3b.fit2)

AIC(f3b.fit, f3b.fit2) #fit 2 much better
summary(f3b.fit2)
length(fitted(f3b.fit))

# migration and body size
show_col(viridis_pal()(6))

# ampbhibian = #440154FF, birds = #2A788EFF, mammals = #7AD151FF, "ACTINOPTERYGII" = "#414487FF", 
# "CHONDRICHTHYES" = "#22A884FF"

F3b <- traits %>% 
  filter(Migration_km >0, Mass_kg >0, !is.na(class), !class %in% "INSECTA", !class %in% "CEPHALASPIDOMORPHI") %>% 
  mutate(fit=predict(f3b.fit2)) %>% 
  ggplot(aes(x = (Mass_kg), y = log10(Migration_km)))+
  geom_point(size = 4, aes(color = class, fill = class, shape = class))+
  geom_smooth(method = "lm", aes(y = fit, colour = class, group = class),size = 2, se = F)+
  xlab("Body size (kg)")+
  ylab("Migration scale (km)")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x)))+
  theme_classic()+
  theme_classic()+
  scale_shape_manual(values = c(21,22,23, 24, 25))+
  scale_color_manual(values = c("AMPHIBIA" = "#440154FF","AVES" = "#2A788EFF", "MAMMALIA" = "#7AD151FF","ACTINOPTERYGII" = "#414487FF", "CHONDRICHTHYES" = "#22A884FF"))+
  scale_fill_manual(values = c("AMPHIBIA" = "#440154FF","AVES" = "#2A788EFF", "MAMMALIA" = "#7AD151FF","ACTINOPTERYGII" = "#414487FF", "CHONDRICHTHYES" = "#22A884FF"))+
  theme(legend.key = element_rect(colour = "white", fill = "white"))+
  theme(axis.text = element_text(size = 25, color = "black"),
        axis.title = element_text(size = 25),
        legend.text=element_text(size=18),
        legend.title = element_text(size=16),
        axis.line = element_line(size = 1.3))+
  theme(legend.key.width=unit(3,"line"))
F3b

# home range and body size
f3c.fit <- lmer(log10(mean.hra.km2) ~ log10(Mass_kg) + (1|class), data = traits)
Anova(f3c.fit)
summary(f3c.fit)

###
f3c.fit <- lmer(log10(hr.radius) ~ log10(Mass_kg) + (1|class), data = filter(traits, hr.radius > 0, Mass_kg >0, !is.na(class)))
f3c.fit2 <- lmer(log10(hr.radius) ~ log10(Mass_kg) + (log10(Mass_kg)|class), data = filter(traits, mean.hra.km2 > 0, Mass_kg >0, !is.na(class)))

Anova(f3c.fit)
Anova(f3c.fit2)
summary(f3c.fit2)

AIC(f3c.fit, f3c.fit2) #no diff, fit 1 barely smaller aic
summary(f3c.fit2)
length(fitted(f3c.fit))
###

# migration and body size
show_col(viridis_pal()(6))

# ampbhibian = #440154FF, birds = #2A788EFF, mammals = #7AD151FF, "ACTINOPTERYGII" = "#414487FF", 
# "CHONDRICHTHYES" = "#22A884FF", "REPTILIA" = "#FDE725FF"

F3c <- traits %>% 
  filter(hr.radius >0, Mass_kg >0, !is.na(class)) %>% 
  mutate(fit=predict(f3c.fit)) %>% 
  ggplot(aes(x = (Mass_kg), y = log10(mean.hra.km2)))+
  geom_point(size = 4, aes(color = class, fill = class, shape = class))+
  geom_smooth(method = "lm", aes(y = fit, colour = class, group = class), size = 2, se = F)+
  xlab("Body size (kg)")+
  ylab("Foraging scale (m)")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x)))+
  theme_classic()+
  scale_shape_manual(values = c(21,22,23,24))+
  scale_color_manual(values = c("AMPHIBIA" = "#440154FF","AVES" = "#2A788EFF", "MAMMALIA" = "#7AD151FF", "REPTILIA" = "#FDE725FF"))+
  scale_fill_manual(values = c("AMPHIBIA" = "#440154FF","AVES" = "#2A788EFF", "MAMMALIA" = "#7AD151FF", "REPTILIA" = "#FDE725FF"))+
  theme(legend.key = element_rect(colour = "white", fill = "white"))+
  theme(axis.text = element_text(size = 25, color = "black"),
        axis.title = element_text(size = 25),
        legend.text=element_text(size=18),
        legend.title = element_text(size=16),
        axis.line = element_line(size = 1.3))+
  theme(legend.key.width=unit(3,"line"))
F3c

fit <- ggplot(data = traits.mig.pos, aes(x = Mass_kg, y = Migration_km))+
  geom_point(aes(color = class))
leg <- get_legend(fit)
legend <- as_ggplot(leg)

multiplot(F3a, F3b, F3c, cols = 2)



########### Covariance Matrix + heatmap ###########
library(stats)
library(gplots)

name <- traits.all$scientific_name
home.range <- log10(traits.all$mean.hra.m2)
dispersal <- log10(traits.all$meanmedian_km)
migration <- log10(traits.all$Migration_km)
mass <- log10(traits.all$Mass_kg)

cov.mat <- data.frame(home.range, dispersal, migration, mass)
matrix <- cov(cov.mat)
# remove cases where migration is zero
cov.mat2 <- cov.mat[-c(which(cov.mat$migration == "-Inf")),]
cov.mat2 <- cov.mat2[-c(is.na(cov.mat2$mass == T)),]

# matrix2 <- cov(cov.mat2, use = "na.or.complete")
matrix3 <- cov(cov.mat2, use = "pairwise.complete.obs") #go with this one
# diag(matrix2) <- NA
diag(matrix3) <- NA
View(matrix3)


as.data.frame(matrix3) %>% 
  mutate(movement2 = rownames(matrix3)) %>% 
  gather(key = "movement", value = cov, -movement2) %>% 
  ggplot(aes(x = movement, y = movement2, fill = cov))+
  geom_tile()+
  scale_fill_viridis()

par(mar=c(5,6,4,1)+.1)
heatmap(matrix2, symm = T)


 ########## PCA #########
library(vegan)
mydata <- traits
# mydata <- mydata[,-3]

mydata$log.mass <- log10(mydata$Mass_kg)
mydata$log.mig <- log10(mydata$Migration_km)
mydata$log.disp <- log10(mydata$dispersal_km)
mydata$log.hr <- log10(mydata$hr.radius)
 

mydata <- mydata[-which(mydata$log.mig == -Inf),]
mydata <- mydata[which(!is.na(mydata$log.hr)),]
mydata <- mydata[which(!is.na(mydata$log.disp)),]



names(mydata)
test <- decostand(mydata[, 15:17], method = "standardize")

v <- var(test)
View(v)
diag(v) #check variance of traits in same order of magnitude (else need to transform)

library(devtools)
# install_github("vqv/ggbiplot")
library(ggbiplot)
theme_set(theme_classic())

z <- prcomp(test)   
biplot(z, choices = c(1,2), cex = 0.7)
ggbiplot(z, choices = c(1,2))
ggbiplot(z, ellipse = TRUE, choices = c(1,2), groups = mydata$class)

disp.bs <- lm(log.disp ~ log.mass, data = mydata)
plot(log.disp ~ log.mass, data = mydata)
mydata$disp.resid <- resid(disp.bs)

mig.bs <- lm(log.mig ~ log.mass, data = mydata)
plot(log.mig ~ log.mass, data = mydata)
mydata$mig.resid <- resid(mig.bs)

hr.bs <- lm(log.hr ~ log.mass, data = mydata)
plot(log.hr ~ log.mass, data = mydata)
mydata$hr.resid <- resid(hr.bs)

test2 <- decostand(mydata[, 18:20], method = "standardize")

v2 <- var(test2)
View(v2)

z2 <- prcomp(test2)   
ggbiplot(z2, ellipse = TRUE, choices = c(1,2), groups = mydata$class)
