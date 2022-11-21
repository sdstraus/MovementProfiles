library(tidyverse)
library(cowplot)
library(ggplot2)
library(lme4)
library(scatterplot3d)
library(scales)
library(gridExtra)
library(ggpubr)
library(quantreg)
library(viridis)
library(dplyr)
library(tidyr)
library(car)
library(plot3D)
library(stringr)

#source("code/multiplot.R")
traits_clean <- read.csv("/Users/sam/github/SUPSmods/data/cleaned_traits.csv", fileEncoding="latin1")
# 
# ## load phylogeny
#
one_phylo <- readRDS("/Users/sam/github/SUPSmods/data/final_phylo.rds")

#one_phylo

all_class <- read.csv("/Users/sam/github/SUPSmods/data/classes.csv")

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

traits <- traits_phylo

write.csv(traits, "/Users/sam/github/SUPSmods/3D-app/3Dplot/Traits_final.csv")

####### histograms ########

show_col(scales::viridis_pal(option = "D")(4))

media_cols <- c(air = "#440154FF", land = "#5DC863FF", aquatic = "#3B528BFF", marine = "#21908CFF")

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
  xlab("Foraging radius (km)")

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
hist.by.media <- ggarrange(p1,p2,p3, nrow = 1, common.legend = T, legend = "right", labels = c("a", "b","c"))

# class
show_col(scales::viridis_pal(option = "D")(5))
traits$class <- ordered(traits$class, levels = c("Aves", "Mammalia", "Amphibia", "Reptilia", "Chondrichthyes"))

class_cols <- c(Aves = "#440154FF", Mammalia = "#3B528BFF", Amphibia = "#21908CFF", 
                Reptilia = "#5DC863FF", Chondrichthyes = "#FDE725FF")

p1 <- ggplot(traits, aes(x = (hr.radius))) +
  geom_histogram(aes(color = class, fill = class),
                 position = "identity",alpha = 0.2, bins = 30)+
  scale_color_manual(values = class_cols, name = "Class")+
  scale_fill_manual(values = class_cols, name = "Class") +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  cowplot::theme_cowplot()+
  xlab("Foraging radius (km)")

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

hist.by.class <- ggarrange(p1,p2,p3, nrow = 1, common.legend = T, legend = "right", 
                           labels = c("d", "e", "f"))


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
  xlab("Foraging radius (km)")

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

hist.by.diet <- ggarrange(p1,p2,p3, nrow = 1, common.legend = T, legend = "right",
                          labels = c("g", "h", "i"))



hist.all <- ggarrange(hist.by.media, hist.by.class, hist.by.diet, nrow =3)
ggsave(hist.all, filename = "../figures/figure2_24April22.jpg", dpi = "retina", units = "in", height = 6, width = 10)



####### 3d scatterplot #######

library(plot3D)
library(rgl)
library(magick)
# colour by mass
scatter3D(x = log10(traits$dispersal_km), 
          y=log10(traits$Migration_km+1), 
          z=log10(traits$hr.radius), 
          colvar = log10(traits$Mass_kg),
          col = ramp.col(c("#35B779FF", "#26828EFF", "#440154FF")),
          phi = 20, theta = 20, #20, 20 looks good
          pch=20, cex =2, ticktype = "detailed",
          type = "h",
          xlab = "Dispersal (km)",
          ylab = "Migration (km)",
          zlab = "Foraging (km)",
          clab = c("Body Mass",
                   "(kg)"))

# colour by class

colvar <- as.numeric(as.factor(traits$class))
col <-levels(as.factor(traits$class))

scatter3D(x = log10(traits$dispersal_km), 
          y=log10(traits$Migration_km + 1), 
          z=log10(traits$hr.radius), 
          colvar = colvar,
          # col = ramp.col(c("#35B779FF", "#26828EFF", "#440154FF")),
          phi = 90, theta = 0, #20, 20 looks good
          pch=20, cex =2, ticktype = "detailed",
          type = "h",
          xlab = "Dispersal (km)",
          ylab = "Migration (km)",
          zlab = "Foraging (km)",
          clab = col)

library(plot3Drgl)
plotrgl()

play3d( spin3d( axis = c(0, 0, 1), rpm = 5), duration = 10 )

movie3d(
  movie="3dAnimatedScatterplot_nozeromig", 
  spin3d( axis = c(0, 0, 1), rpm = 5),
  duration = 10, 
  dir = "~/Desktop",
  type = "gif", 
  clean = TRUE
)


library(plotly)

show_col(scales::viridis_pal(option = "D")(5))
class_cols <- c(Amphibia = "#440154FF", Aves = "#3B528BFF", Chondricthyes = "#21908CFF", 
                Mammalia = "#5DC863FF", Reptilia = "#FDE725FF")
pal <- c("#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "#FDE725FF")

traits$class_color <- factor(traits$class, labels = c(Amphibia = "#440154FF", Aves = "#3B528BFF", Chondricthyes = "#21908CFF", 
                                                      Mammalia = "#5DC863FF", Reptilia = "#FDE725FF"))

fig <- plot_ly(traits, x = ~log10(dispersal_km), y = ~log10(Migration_km + 1), 
               z = ~log10(hr.radius), color = ~class, colors = pal)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Dispersal (km)'),
                                   yaxis = list(title = 'Migration (km + 1)'),
                                   zaxis = list(title = 'Foraging radius (km)')))
fig
htmlwidgets::saveWidget(as_widget(fig), "../figures/3Dplot.html")


########### Covariance Matrix + heatmap ###########
library(stats)
library(gplots)

traits <- traits_phylo

name <- traits$scientific_name
home.range <- log10(traits$hr.radius)
dispersal <- log10(traits$dispersal_km)
migration <- log10(traits$Migration_km)
mass <- log10(traits$Mass_kg)

cov.mat <- data.frame(home.range, dispersal, migration, mass)
matrix <- cov(cov.mat)

# remove cases where migration is zero, matrix only for migrators
cov.mat2 <- cov.mat[-c(which(cov.mat$migration == "-Inf")),]
#cov.mat2 <- cov.mat2[-c(is.na(cov.mat2$mass == T)),]

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


### non-migrators only ###
cov.mat.nonmig <- cov.mat %>% filter(migration == "-Inf")
# remove migration column
cov.mat.nonmig <- cov.mat.nonmig %>% select(-migration)

matrix.nonmig <- cov(cov.mat.nonmig, use = "pairwise.complete.obs") #go with this one
# diag(matrix2) <- NA
diag(matrix.nonmig) <- NA
View(matrix.nonmig)

 ########## PCA #########
library(vegan)
mydata <- traits
# mydata <- mydata[,-3]

mydata$log.mass <- log10(mydata$Mass_kg)
mydata$log.mig <- log10(mydata$Migration_km)
mydata$log.disp <- log10(mydata$dispersal_km)
mydata$log.for <- log10(mydata$hr.radius)
 

mydata <- mydata[-which(mydata$log.mig == -Inf),]
mydata <- mydata[which(!is.na(mydata$log.for)),]
mydata <- mydata[which(!is.na(mydata$log.disp)),]

mydata_med <- mydata %>% 
  filter(media_simplified != "")

names(mydata)
test <- decostand(mydata[, 15:17], method = "standardize")
test_med <- decostand(mydata_med[, 15:17], method = "standardize")

library(geometry)
blob <- convhulln(test, "FA")
obs.vol <- blob$vol

v <- var(test)
View(v)
diag(v) #check variance of traits in same order of magnitude (else need to transform)

library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
#theme_set(theme_classic())

# z <- prcomp(test)  
z <- prcomp(test)
loadings.raw <- z$rotation

z_med <- prcomp(test_med)
loadings.raw.med <- z_med$rotation
# biplot(z, choices = c(1,2), cex = 0.7)
# ggbiplot(z, choices = c(1,2))

show_col(scales::viridis_pal(option = "D")(6))

media_cols <- c(air = "#440154FF", land = "#5DC863FF", water = "#21908CFF")

class_cols <- c(Amphibia = "#440154FF", Aves = "#3B528BFF", Chondrichthyes = "#21908CFF", 
                Mammalia = "#5DC863FF", Reptilia = "#FDE725FF")

diet_cols <- c(Carnivore = "#440154FF", Herbivore = "#2A788EFF", 
               Invertivore = "#22A884FF", Omnivore = "#7AD151FF")




pca_med <- ggbiplot(z_med, ellipse = TRUE, choices = c(1,2), groups = mydata_med$media_simplified) +
  scale_color_manual(values = media_cols) + 
  xlab("PC1 (62.5%)")+
  ylab("PC2 (22.8%)")+
  theme_cowplot()

pca_class <- ggbiplot(z, ellipse = TRUE, choices = c(1,2), groups = mydata$class) +
  scale_color_manual(values = class_cols) + 
  xlab("PC1 (61.9%)")+
  ylab("PC2 (23.1%)")+
  theme_cowplot()

pca_diet <- ggbiplot(z, ellipse = TRUE, choices = c(1,2), groups = mydata$diet_broadest_cat) + 
  scale_color_manual(values = diet_cols)+
  xlab("PC1 (61.9%)")+
  ylab("PC2 (23.1%)")+
  theme_cowplot()


##  make the widths all the same
library(gtable)
library(grid)
library(gridExtra)

# Get the gtables
gA <- ggplotGrob(pca_med)
gB <- ggplotGrob(pca_class)
gC <- ggplotGrob(pca_diet)

gC$widths <- gA$widths
gB$widths <- gA$widths


pca1 <- ggarrange(pca_med, pca_class, pca_diet, nrow = 3, legend = "none", labels = c("a", "c", "e"))

## residuals
disp.bs <- lm(log.disp ~ log.mass, data = mydata)
disp.bs_med <- lm(log.disp ~ log.mass, data = mydata_med)
plot(log.disp ~ log.mass, data = mydata)
mydata$disp.resid <- resid(disp.bs)
mydata_med$disp.resid <- resid(disp.bs_med)

mig.bs <- lm(log.mig ~ log.mass, data = mydata)
mig.bs_med <- lm(log.mig ~ log.mass, data = mydata_med)
plot(log.mig ~ log.mass, data = mydata)
mydata$mig.resid <- resid(mig.bs)
mydata_med$mig.resid <- resid(mig.bs_med)

for.bs <- lm(log.for ~ log.mass, data = mydata)
for.bs_med <- lm(log.for ~ log.mass, data = mydata_med)
plot(log.for ~ log.mass, data = mydata)
mydata$for.resid <- resid(for.bs)
mydata_med$for.resid <- resid(for.bs_med)


test2 <- decostand(mydata[, 18:20], method = "standardize")
test2_med <- decostand(mydata_med[, 18:20], method = "standardize")

v2 <- var(test2)
View(v2)

z2 <- prcomp(test2)
loadings.z2 <- z2$rotation

z2_med <- prcomp(test2_med)

biplot(z2, choices = c(1,2), cex = 0.7)
ggbiplot(z2, choices = c(1,2), cex = 0.7)

pca_med_res <- ggbiplot(z2_med, ellipse = TRUE, choices = c(1,2), groups = mydata_med$media_simplified) +
  scale_color_manual(values = media_cols) + 
  xlab("PC1 (62.9%)")+
  ylab("PC2 (20.5%)")+
  theme_cowplot()

pca_class_res <- ggbiplot(z2, ellipse = TRUE, choices = c(1,2), groups = mydata$class) +
  scale_color_manual(values = class_cols) + 
  xlab("PC1 (61.4%)")+
  ylab("PC2 (20.5%)")+
  theme_cowplot()

pca_diet_res <- ggbiplot(z2, ellipse = TRUE, choices = c(1,2), groups = mydata$diet_broadest_cat) + 
  scale_color_manual(values = diet_cols)+
  xlab("PC1 (61.4%)")+
  ylab("PC2 (20.5%)")+
  theme_cowplot()

#pca2 <- ggarrange(pca_med_res, pca_class_res, pca_diet_res, nrow = 3, labels = c("b","d","f"))


# Get the gtables
gD <- ggplotGrob(pca_med_res)
gE <- ggplotGrob(pca_class_res)
gF <- ggplotGrob(pca_diet_res)

gD$widths <- gA$widths
gE$widths <- gA$widths
gF$widths <- gA$widths

# Arrange the two charts.
# The legend boxes are centered
grid.newpage()
jpeg(file = "figures/Figure3_18Nov22.jpeg", width = 12, height = 9, units = "in", res = 1000)
grid.arrange(gA, gC, gE, gB, gD, gF, nrow = 3, padding = 2)
dev.off()


# pca_fin <- ggarrange(pca1, pca2, nrow = 1)
# ggsave(pca_fin, filename = "../figures/PCA_plots.jpeg", dpi = "retina", 
#        units = "in", width = 8, height = 8)


jpeg(filename = "figures/PCA_plots.jpeg", width = 8, height = 8, units = 'in', res = 1000)
pca_fin
dev.off()
