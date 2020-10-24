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

MA_sheet<-gs_title(x = "SUPs_traits")
traits <- traits_phylo

# fix lontra canadensis class
which(traits$scientific_name.x == "Lontra_canadensis") #42
traits$class[42] <- "Mammalia"
# ## INTIAL CLEANING FROM RAW DATA, DO NOT REPEAT ##
# traits <- gs_read(ss = MA_sheet,ws = 1)
traits$mean.hra.m2 <- as.numeric(traits$mean.hra.m2)
traits$Migration_km <- as.numeric(traits$Migration_km)
traits$Mass_kg <- as.numeric(traits$Mass_kg)
traits$mean.hra.km2<-traits$mean.hra.m2/1000000
# area = pi*r^2 ==> sqrt(area/pi) = r
traits$hr.radius <- sqrt(traits$mean.hra.km2/pi)
#row <- which(traits$mean.hra.km2 < 1e-10)
#traits <- traits[-row,]
# 
# # sometimes only have max dispersal..replace max for meanmeadian if only measure
# for(x in 1:length(traits$dispersal_km)){
#   if(is.na(traits$dispersal_km[x]) == T && is.na(traits$maxdispersal_km[x]) == F){
#     traits$dispersal_km[x] <- traits$maxdispersal_km[x]
#   }
# }
# # 
# 
# traits2 <- traits %>% 
#   group_by(scientific_name)
# 
# traits$scientific_name <- gsub("_", " ", traits$scientific_name)
# 
# # # use body_mass_dispersal for Mass_kg column
# # for(x in 1:length(traits$Mass_kg)){
# #   if(is.na(traits$Mass_kg[x]) == T && is.na(traits$body_mass_dispersal_stage_kg[x]) == F){
# #     traits$Mass_kg[x] <- traits$body_mass_dispersal_stage_kg[x]
# #   }
# # }


ggplot(traits, aes(x = log10(dispersal_km))) +
  geom_histogram(aes(color = class), fill = "white",
                 position = "identity", bins = 30)) 

####### for VennDiagram #########
# how many trait values for each sup?
length(which(is.na(traits$mean.hra.m2) == F)) #165 
length(which(is.na(traits$Migration_km) == F)) #277
length(which(is.na(traits$dispersal_km) == F)) #178

# how many species have all 3?
traits.all <- traits %>% 
  filter(number_traits_completed == 4)
length(traits.all$scientific_name.x) #336

length(which(is.na(traits$Migration_km) == F & is.na(traits$meanmedian_km) == F)) #86
length(which(is.na(traits$Migration_km) == F & is.na(traits$mean.hra.km2) == F)) #110
length(which(is.na(traits$meanmedian_km) == F & is.na(traits$mean.hra.km2) == F)) #83
#all together have 71


# 1 = hr, 2 = mig, 3 = disp
dev.off()
draw.triple.venn(area1 = 165, area2 = 277, area3 = 178, n12 = 110, n13 = 83, n23 = 86, n123 = 73,
                 category = c("Home Range", "Migration", "Dispersal"))
dev.off()



####### 3d scatterplot #######
par(mfrow=c(1,1))
traits.all.posmig <- traits[-c(which(traits$Migration_km == 0)),]
# traits.all.posmig$Migration_km <- as.numeric(traits.all.posmig$Migration_km )
# traits.all.posmig$mean.hra.km2 <- as.numeric(traits.all.posmig$mean.hra.km2 )

s3d <- scatterplot3d(x=log10(traits.all.posmig$dispersal_km),
              y=log10(traits.all.posmig$Migration_km),
              z=log10(traits.all.posmig$mean.hra.km2),
              pch=16,
              xlab="Dispersal scale",
              ylab="Migration scale",
              zlab="Foraging scale",
              grid=T,
              cex.symbols = 1,
              type="h",
              highlight.3d = T)

fit <- lm(log10(dispersal_km) ~log10(mean.hra.km2)+ log10(Migration_km), data = traits.all.posmig)
Anova(fit, type = 3)
s3d$plane3d(fit)

write.csv(traits.all, "traits.all.csv")

#library(plot3D)
# # add plane
# x <- log10(traits.all.posmig$meanmedian_km)
# y <- log10(traits.all.posmig$Migration_km)
# z <- log10(traits.all.posmig$mean.hra.km2)
# 
# # Compute the linear regression (z = ax + by + d)
# fit <- lm(z ~ x + y)
# # predict values on regular xy grid
# grid.lines = 26
# x.pred <- seq(min(x), max(x), length.out = grid.lines)
# y.pred <- seq(min(y), max(y), length.out = grid.lines)
# xy <- expand.grid( x = x.pred, y = y.pred)
# z.pred <- matrix(predict(fit, newdata = xy), 
#                  nrow = grid.lines, ncol = grid.lines)
# # fitted points for droplines to surface
# fitpoints <- predict(fit)

library(plot3D)
library(rgl)
library(magick)
# colour by mass
scatter3D(x = log10(traits.all.posmig$dispersal_km), 
          y=log10(traits.all.posmig$Migration_km), 
          z=log10(traits.all.posmig$mean.hra.km2), 
          colvar = log10(traits.all.posmig$Mass_kg),
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
          z=log10(traits$mean.hra.km2), 
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

fig <- plot_ly(traits, x = ~log10(dispersal_km), y = ~log10(Migration_km + 1), 
               z = ~log10(mean.hra.km2), color = ~class)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Dispersal (km)'),
                                   yaxis = list(title = 'Migration (km + 1)'),
                                   zaxis = list(title = 'Foraging radius (km)')))

fig
htmlwidgets::saveWidget(as_widget(fig), "3Dplot.html")

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

###### get taxonomic class #######
# # do not repeat #
# # function to see if result in defined (NULL)
is.defined = function(x)!is.null(x)
#is.defined(x$result[[1]]$class)
is.defined(rl_search("Puma concolor",
                     key = "ad69d87efd6b5f6b0c91bc101b7d99acc52ca908b7a8bd77e74dd10af729f471",
                     parse=F)$result[[1]]$class)

traits$class <- rep("", times = length(traits$scientific_name))

#this step takes ~10 minutes, output is saved as new csv so i don't have to do this every time
for(i in 1:length(traits$scientific_name)){
  if(length(rl_search(traits$scientific_name[i],
                    key = "ad69d87efd6b5f6b0c91bc101b7d99acc52ca908b7a8bd77e74dd10af729f471",
                    parse=F)$result) == 0){
    traits$class[i] <- "NA"}
  else if(is.defined(rl_search(traits$scientific_name[i],
                          key = "ad69d87efd6b5f6b0c91bc101b7d99acc52ca908b7a8bd77e74dd10af729f471",
                          parse=F)$result[[1]]$class) == FALSE){
    traits$class[i] <- "NA"}
  else{
    traits$class[i] <- rl_search(traits$scientific_name[i],
                                 key = "ad69d87efd6b5f6b0c91bc101b7d99acc52ca908b7a8bd77e74dd10af729f471",
                                 parse=F)$result[[1]]$class
}}

traits$class <- as.factor(traits$class)
write.csv(traits, "traits_with_class.csv")
traits <- read.csv("traits_with_class.csv")

##### pairwise correlations btw spatial use properties ####
par(mar=c(5,6,4,1)+.1)
plot(log10(mean.hra.km2) ~ log10(Migration_km), traits)
plot(log10(mean.hra.km2) ~ log10(meanmedian_km), traits)
plot(log10(Migration_km) ~ log10(meanmedian_km), traits)

plot(log(Migration_km) ~ log(Mass_kg), traits, col = traits$class)
legend(7,4.3,unique(traits$class),col=1:length(traits$class),pch=1)

plot(log(meanmedian_km) ~ log(Mass_kg), traits)
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
  scale_shape_manual(values = c(21,22,23))+
  scale_color_manual(values = c("AMPHIBIA" = "#440154FF","AVES" = "#2A788EFF", "MAMMALIA" = "#7AD151FF"))+
  scale_fill_manual(values = c("AMPHIBIA" = "#440154FF","AVES" = "#2A788EFF", "MAMMALIA" = "#7AD151FF"))+
  theme(legend.key = element_rect(colour = "white", fill = "white"))+
  theme(axis.text = element_text(size = 25, color = "black"),
        axis.title = element_text(size = 25),
        legend.text=element_text(size=18),
        legend.title = element_text(size=16),
        axis.line = element_line(size = 1.3))+
  theme(legend.key.width=unit(3,"line"))
F2a

# dipsersal and home range
traits.disp.hr <- traits[(which(is.na(traits$meanmedian_km) == F & is.na(traits$mean.hra.km2) == F)),]
traits.disp.hr$class <- factor(traits.disp.hr$class)

f2b.fit <- lmer(log10(mean.hra.km2) ~ log10(meanmedian_km) + (1|class), data = traits.disp.hr)
Anova(f2b.fit)
summary(f2b.fit)
f2b.fit2 <- lm(log10(mean.hra.km2) ~ log10(meanmedian_km), data = traits.disp.hr)
Anova(f2b.fit2)
anova(f2b.fit, f2b.fit2)

###
###
f2b.fit <- lmer(log10(hr.radius) ~ log10(meanmedian_km) + (1|class), data = filter(traits, hr.radius > 0, meanmedian_km >0, !is.na(class)))
f2b.fit2 <- lmer(log10(hr.radius) ~ log10(meanmedian_km) + (log10(meanmedian_km)|class), data = filter(traits, hr.radius > 0, meanmedian_km >0, !is.na(class)))

Anova(f2b.fit)
Anova(f2b.fit2)
anova(f2b.fit, f2b.fit2)
summary(f2b.fit2)

AIC(f2b.fit, f2b.fit2) #fit 2 better (but AIC diff. less than 10)
summary(f3b.fit2)
###
###

F2b <- traits %>% 
  filter(hr.radius >0, meanmedian_km >0, !is.na(class)) %>% 
  mutate(fit=predict(f2b.fit2)) %>% 
  ggplot(aes(x = (meanmedian_km), y = log10(hr.radius)))+
  geom_point(size = 4, aes(color = class, fill = class, shape = class))+
  geom_smooth(method = "lm", aes(y = fit, colour = class, group = class), size = 2, se =F)+
  xlab("Dispersal scale (km)")+
  ylab("Foraging scale (km)")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x)))+
  theme_classic()+
  scale_shape_manual(values = c(21,22,23))+
  scale_color_manual(values = c("AMPHIBIA" = "#440154FF","AVES" = "#2A788EFF", "MAMMALIA" = "#7AD151FF"))+
  scale_fill_manual(values = c("AMPHIBIA" = "#440154FF","AVES" = "#2A788EFF", "MAMMALIA" = "#7AD151FF"))+
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
traits.mig.disp <- traits[(which(is.na(traits$Migration_km) == F & is.na(traits$meanmedian_km) == F)),]
traits.mig.disp$class <- factor(traits.mig.disp$class)
traits.mig.disp <- traits.mig.disp[-c(which(traits.mig.disp$Migration_km == 0)),]

f2c.fit <- lmer(log10(Migration_km) ~ log10(meanmedian_km) + (1|class), data = traits.mig.disp)
Anova(f2c.fit)
summary(f2c.fit)

###
###
f2c.fit <- lmer(log10(Migration_km) ~ log10(meanmedian_km) + (1|class), data = filter(traits, Migration_km > 0, meanmedian_km >0, !is.na(class), !class %in% "INSECTA"))
f2c.fit2 <- lmer(log10(Migration_km) ~ log10(meanmedian_km) + (log10(meanmedian_km)|class), data = filter(traits, Migration_km > 0, meanmedian_km >0, !is.na(class), !class %in% "INSECTA"))

Anova(f2c.fit)
Anova(f2c.fit2)
anova(f2c.fit, f2b.fit2)
summary(f2c.fit)

AIC(f2c.fit, f2c.fit2) #fit 1 better (but AIC diff. less than 3)
summary(f3c.fit2)
###
###

F2c <- traits %>% 
  filter(Migration_km >0, meanmedian_km >0, !is.na(class), !class %in% "INSECTA") %>% 
  mutate(fit=predict(f2c.fit)) %>% 
  ggplot(aes(x = (meanmedian_km), y = log10(Migration_km)))+
  geom_point(size = 4, aes(color = class, fill = class, shape = class))+
  geom_smooth(method = "lm", aes(y = fit, colour = class, group = class), size = 2, se = F)+
  xlab("Dispersal scale (km)")+
  ylab("Migration scale (km)")+
  theme_classic()+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x)))+
  scale_shape_manual(values = c(21,22,23))+
  scale_color_manual(values = c("AMPHIBIA" = "#440154FF","AVES" = "#2A788EFF", "MAMMALIA" = "#7AD151FF"))+
  scale_fill_manual(values = c("AMPHIBIA" = "#440154FF","AVES" = "#2A788EFF", "MAMMALIA" = "#7AD151FF"))+
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
f3a.fit <- lmer(log10(meanmedian_km) ~ log10(Mass_kg) + (1|class), data = traits)
Anova(f3a.fit)
summary(f3a.fit)

###
f3a.fit <- lmer(log10(meanmedian_km) ~ log10(Mass_kg) + (1|class), data = filter(traits, meanmedian_km > 0, Mass_kg >0, !is.na(class), !class %in% "INSECTA"))
f3a.fit2 <- lmer(log10(meanmedian_km) ~ log10(Mass_kg) + (log10(Mass_kg)|class), data = filter(traits, meanmedian_km > 0, Mass_kg >0, !is.na(class), !class %in% "INSECTA"))

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
  filter(meanmedian_km >0, Mass_kg >0, !is.na(class), !class %in% "INSECTA") %>% 
  mutate(fit=predict(f3a.fit2)) %>% 
  ggplot(size = 4, aes(x = (Mass_kg), y = log10(meanmedian_km)))+
  geom_point(size = 4, aes(colour = class, fill = class, shape = class))+
  geom_smooth(method = "lm", aes(y = fit, colour = class, group = class), size = 2, se = F)+
  xlab("Body size (kg)")+
  ylab("Dispersal scale (km)")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x)))+
  theme_classic()+
  scale_shape_manual(values = c(21,22,23))+
  scale_color_manual(values = c("AMPHIBIA" = "#440154FF","AVES" = "#2A788EFF", "MAMMALIA" = "#7AD151FF"))+
  scale_fill_manual(values = c("AMPHIBIA" = "#440154FF","AVES" = "#2A788EFF", "MAMMALIA" = "#7AD151FF"))+
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

########### are there predator-prey pairs? ########
pp.pairs <- read.csv("predprey_pairs.csv")
pp.pairs$prey <- as.character(pp.pairs$prey)
pp.pairs$predator <- as.character(pp.pairs$predator)
library(Hmisc)
pp.pairs$prey <- capitalize(pp.pairs$prey)
pp.pairs$predator <- capitalize(pp.pairs$predator)

df <- inner_join(x=traits.all, y=pp.pairs, by=c("scientific_name" = "prey"))
df2 <- inner_join(x=traits.all, y=pp.pairs, by=c("scientific_name" = "predator"))

########### Covariance Matrix + heatmap ###########
library(stats)
library(gplots)

name <- traits$scientific_name
home.range <- log10(traits$mean.hra.m2)
dispersal <- log10(traits$dispersal_km)
migration <- log10(traits$Migration_km)
mass <- log10(traits$Mass_kg)

cov.mat <- data.frame(home.range, dispersal, migration, mass)
matrix <- cov(cov.mat)
# remove cases where migration is zero
cov.mat2 <- cov.mat[-c(which(cov.mat$migration == "-Inf")),]
#cov.mat2 <- cov.mat2[-c(is.na(cov.mat2$mass == T)),]

# matrix2 <- cov(cov.mat2, use = "na.or.complete")
matrix3 <- cov(cov.mat2, use = "pairwise.complete.obs") #go with this one
# diag(matrix2) <- NA
diag(matrix3) <- NA
View(matrix3)

# as.data.frame(matrix2) %>% 
#   mutate(movement2 = rownames(matrix2)) %>% 
#   gather(key = "movement", value = cov, -movement2) %>% 
#   ggplot(aes(x = movement, y = movement2, fill = cov))+
#   geom_tile()+
#   scale_fill_viridis()

as.data.frame(matrix3) %>% 
  mutate(movement2 = rownames(matrix3)) %>% 
  gather(key = "movement", value = cov, -movement2) %>% 
  ggplot(aes(x = movement, y = movement2, fill = cov))+
  geom_tile()+
  scale_fill_viridis()

par(mar=c(5,6,4,1)+.1)
heatmap(matrix2, symm = T)

########## IUCN status ###########
# plotting and models
# (y variable ~ x variable, data = traits)

#reordering factors
# traits$IUCN_status <- factor(traits$IUCN_status, levels = c("LC", "NT", "VU", "EN", "CR"))
# 
# traits$concern_status <- rep("", times = length(traits$IUCN_status))
# 
# 
# rows <- which(traits$IUCN_status == "LC")
# traits$concern_status[rows] <- "Not of concern"
# rows <- which(traits$IUCN_status == "NT")
# traits$concern_status[rows] <- "Not of concern"
# rows <- which(traits$IUCN_status == "VU")
# traits$concern_status[rows] <- "Of concern"
# rows <- which(traits$IUCN_status == "EN")
# traits$concern_status[rows] <- "Of concern"
# rows <- which(traits$IUCN_status == "CR")
# traits$concern_status[rows] <- "Of concern"
# rows <- which(is.na(traits$IUCN_status) == T)
# traits$concern_status[rows] <- NA
# # rows <- which(traits$IUCN_status == "NULL")
# # traits$concern_status[rows] <- "NA"
# traits$concern_status <- as.factor(traits$concern_status)
# 
# hist(log10(traits$Mass_kg))
# lm1 <- lm(log10(Mass_kg) ~ concern_status, data = traits)
# Anova(lm1)
# summary(lm1)
# 
# plot(log10(Mass_kg) ~ concern_status, data = traits)
# 
# lm2 <- lm(log10(Mass_kg) ~ IUCN_status, data = traits)
# Anova(lm2)
# summary(lm2)
# plot(log10(Mass_kg) ~ IUCN_status, data = traits)
# 
# pos.migration <- traits[which(traits$Migration_km > 0),]
# lm3 <- lm(log10(Migration_km) ~ concern_status, pos.migration)
# Anova(lm3)
# summary(lm3)
# plot(log10(Migration_km) ~ concern_status, data = pos.migration)
# 
# lm4 <- lm(log10(hr.radius)~concern_status, data = traits)
# Anova(lm4)
# plot(log10(hr.radius)~concern_status, data = traits)
# 
# lm5 <- lm(log(meanmedian_km) ~ concern_status, data = traits)
# Anova(lm5)
# plot(log(meanmedian_km) ~ concern_status, data = traits)
# 
# ############ histograms to show spread for each taxa #########
# ggplot(traits, aes(x=log10(meanmedian_km), color = class)) +
#   geom_histogram(fill = "white")
# 
# ggplot(traits, aes(x=log10(mean.hra.m2), color = class)) +
#   geom_histogram(fill = "white")
# 
# ggplot(traits, aes(x=log10(Migration_km), color = class)) +
#   geom_histogram(fill = "white")
# 
# 
 ########## PCA #########
library(vegan)
mydata <- traits
# mydata <- mydata[,-3]

mydata$log.mass <- log10(mydata$Mass_kg)
mydata$log.mig <- log10(mydata$Migration_km)
mydata$log.disp <- log10(mydata$dispersal_km)
mydata$log.hr <- log10(mydata$hr.radius)
mydata <- na.omit(mydata)
mydata <- mydata[-which(mydata$log.mig == -Inf),]
# mydata <- mydata[-which(mydata$log.disp == -Inf),]

# 
# mydata$sc.mass <- scale(mydata$log.mass)
# mydata$sc.mig <- scale(log10(mydata$log.mig))
# mydata$sc.disp <- scale(log10(mydata$log.disp))
# mydata$sc.hr <- scale(log10(mydata$log.hr))

test <- decostand(mydata[, 12:15], method = "standardize")

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

test2 <- decostand(mydata[, 16:18], method = "standardize")

v2 <- var(test2)
View(v2)

z2 <- prcomp(test2)   
ggbiplot(z2, ellipse = TRUE, choices = c(1,2), groups = mydata$class)


############ Phylo ###############
library(ape)
# 
# # trees here: https://datadryad.org/resource/doi:10.5061/dryad.11609
# tree <- read.tree(file.choose())
# 
# aves <- read.tree(file.choose())
# amphib <- read.tree(file.choose())
# mammals <- read.tree(file.choose())
# 
# library(phangorn)
# tree.2 <- superTree(tree)
# 
# tree.species <- tree.2$tip.label
# tree.species <- gsub("_", " ", tree.2$tip.label)
# 
# tree.species <- gsub("_", " ", mammals$tip.label)
# 
# tree.species <- gsub("_", " ", aves[[1]]$tip.label)
# 
# tree.species <- gsub("_", " ", amphib[[1]]$tip.label)
# 
# tree.species <- gsub("_", " ", tree[[1]]$tip.label)
# 
# length(tree.species)
# 
# 
# bird.species.all <- traits$scientific_name[which(traits$class == "AVES")]
# bird.species.3sups <- traits.all$scientific_name[which(traits.all$class == "AVES")]
# 
# mammal.species.all <- traits$scientific_name[which(traits$class == "MAMMALIA")]
# mammal.species.3sups <- traits.all$scientific_name[which(traits.all$class == "MAMMALIA")]
# 
# amphib.species.all <- traits$scientific_name[which(traits$class == "AMPHIBIA")]
# amphib.species.3sups <- traits.all$scientific_name[which(traits.all$class == "AMPHIBIA")]
# 
# tree.species.all <- traits$scientific_name
# tree.species.3sups <- traits.all$scientific_name
# 
# length(which(tree.species.all %in% tree.species)) #19 mammals, #69 birds, #63 amphibian, #153 total
# 
# length(which(tree.species %in% tree.species.3sups))# 6 birds, #17 mammals, #4, #27 total
# 
# common <- tree.species[which(tree.species %in% tree.species.3sups)]
# 
# not.common <- tree.species.3sups[which(!(tree.species.3sups %in% common))]
# length(which(!(tree.species.3sups %in% common))) #46 + 27 = 73, nice
# 
# 
# library(stringr)
# genus <- word(not.common, sep=fixed(" "))
# whole.tree.genus <- word(tree.species, sep = fixed(" "))
# 
# not.common[which(genus %in% whole.tree.genus)]
# length(not.common[which(genus %in% whole.tree.genus)]) #23/47 share a genus with something in the tree; total of 50 spp
# 
# no.genus <- not.common[which(!(genus %in% whole.tree.genus))] # these ones are not in three nor do they share a genus with anything in the tree

library(taxize)
# api key
Sys.setenv(ENTREZ_KEY="557250e4a741383f2d8a607a9a7ed2b02008")

####
class.1 <- classification(tree.species.3sups[1:10], db = "ncbi")
class.2 <- classification(tree.species.3sups[11:20], db = "ncbi")
class.3 <- classification(tree.species.3sups[21:30], db = "ncbi")
class.4 <- classification(tree.species.3sups[31:40], db = "ncbi")
class.5 <- classification(tree.species.3sups[41:50], db = "ncbi")
class.6 <- classification(tree.species.3sups[51:60], db = "ncbi")
class.7 <- classification(tree.species.3sups[61:73], db = "ncbi")

class.all <- c(class.1, class.2, class.3, class.4, class.5, class.6, class.7)
t <- class2tree(class.all)
plot(t)
t$phylo[2]

tree.final <- t$phylo
plot(tree.final)
edgelabels(tree.final$edge.length)

tree.species <- tree.final$tip.label
species.3sups <- traits.all$scientific_name
rows <- which(!(species.3sups %in% tree.species))
species.3sups[rows]

rows <- which(!(tree.species %in% species.3sups))
tree.species[rows]

write.tree(tree.final, file = "TreeforSUPS.tree")

## treeanotator package to collapse these 100 into one, download with BEAST (outside of R)
# 10 for Burnin (%), otherwise default settings -- this is first step
# Open tree of life, rotl package can submit list of species and spit back out a tree, # maybe lower quality trees
# library(rotl)
# 
# resolved.names <- tnrs_match_names(species.all, do_approximate_matching = F, context_name = "Animals")
# taxon_map <- structure(resolved.names$search_string, names=resolved.names$unique_name)
# taxon_map["Vulpes fulva"]
# 
# my_tree <- tol_induced_subtree(ott_ids=resolved.names$ott_id)
# plot(my_tree, no.margin = T)
# 
# write.tree(my_tree, file = "Tree_all3movements")
