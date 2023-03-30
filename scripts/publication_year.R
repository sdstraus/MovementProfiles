library(tidyverse)
library(stringr)

# read data
traits <- read.csv("/Users/sam/github/SUPSmods/3D-app/3Dplot/Traits_final.csv", fileEncoding="latin1")

# Find which studies have more than one entry, those are more likely to be the databases
dispersal_source_count <- traits %>% 
  group_by(source_dispersal) %>% 
  summarize(count = n())

migration_source_count <- traits %>% 
  group_by(Migration_source) %>% 
  summarize(count = n())

foraging_source_count <- traits %>% 
  group_by(source_hra) %>% 
  summarize(count = n())

## studies with more than one dispersal distance that we don't need to pull individual years from:
# Jenkins 2017
## Paradis 1998 also large dataset (Patterns of natal and breeding dispersal in birds)
# maybe better to use than Prugh??
## Prugh (https://doi.org/10.1073/pnas.0806080105) and Jenkins don't list data source

# differences between solo papers and db papers
# distance by year (and body size)
# spp with 1 or 2 movement types

# identify which dispersal sources are single papers or dbs
dispersal_single <- dispersal_source_count$source_dispersal[which(dispersal_source_count$count == 1)]
dispersal_db <- dispersal_source_count$source_dispersal[which(dispersal_source_count$count != 1)]

traits$dispersal_source_type <- ""

traits$dispersal_source_type[which(traits$source_dispersal %in% dispersal_single == TRUE)] <- "single"
traits$dispersal_source_type[which(traits$source_dispersal %in% dispersal_db == TRUE)] <- "db"

# repeat for foraging
foraging_single <- foraging_source_count$source_hra[which(foraging_source_count$count == 1)]
foraging_db <- foraging_source_count$source_hra[which(foraging_source_count$count != 1)]

traits$foraging_source_type <- ""

traits$foraging_source_type[which(traits$source_hra %in% foraging_single == TRUE)] <- "single"
traits$foraging_source_type[which(traits$source_hra %in% foraging_db == TRUE)] <- "db"


# repeat for migration
migration_single <- migration_source_count$Migration_source[which(migration_source_count$count == 1)]
migration_db <- migration_source_count$Migration_source[which(migration_source_count$count != 1)]

traits$migration_source_type <- ""

traits$migration_source_type[which(traits$Migration_source %in% migration_single == TRUE)] <- "single"
traits$migration_source_type[which(traits$Migration_source %in% migration_db == TRUE)] <- "db"

## pull out year from source
# author and year separated by _
# year always comes last

head(traits$source_dispersal)
#stringr::str_split(traits$source_dispersal, "_")
# last_element <- my_list[length(my_list)]
library(stringr)

traits$dispersal_year <- 0 #initialize as numeric
traits$foraging_year <- 0
traits$migration_year <- 0


for(i in 1:length(traits$source_dispersal)){
  temp_string1 <- str_split(traits$source_dispersal[i], "_")
  traits$dispersal_year[i] <- temp_string1[[1]][length(temp_string1[[1]])]
  
  temp_string2 <- str_split(traits$source_hra[i], "_")
  traits$foraging_year[i] <- temp_string2[[1]][length(temp_string2[[1]])]
  
  temp_string3 <- str_split(traits$Migration_source[i], "_")
  traits$migration_year[i] <- temp_string3[[1]][length(temp_string3[[1]])]
  
}

# some quick manual fixes
# dispersal is fine
unlist(traits$dispersal_year)


# foraging
unlist(traits$foraging_year)
traits$foraging_year[which(traits$foraging_year== "Penzhorn 1982")] <- 1982
traits$foraging_year[which(traits$foraging_year == 
                             "Global Invasive Species Database")] <- "GISD"

traits$foraging_year[which(traits$source_hra == "Pantheria")] <- 2008 #also this is a db even though we only have 1 entry
traits$foraging_source_type[which(traits$source_hra == "Pantheria")] <- "db" 

# migration needs more work
unlist(traits$migration_year)

# rows i can pull the publishing year from
traits$migration_year[which(traits$migration_year == "Lazenby-Cohen and Cockburn 1991")] <- 1991
traits$migration_year[which(traits$migration_year == "Szep et al. 2017")] <- 2017
traits$migration_year[which(traits$migration_year == "Cornell Birds of North America")] <- 2004 # publishing year of book
traits$migration_year[which(traits$migration_year == "http://www.sibr.com/mammals/M154.html")] <- 1999

# rows I can't
traits$migration_year[which(traits$migration_year == "AWD")] <- "ADW"
traits$migration_year[which(traits$migration_year == "Montana field guide")] <- "mt.gov"
traits$migration_year[which(traits$migration_year == "Montana FWP")] <- "mt.gov"
traits$migration_year[which(traits$migration_year == "http://birds.kz/v2taxon.php?l=en&s=85 + google earth")] <- "KZ Birds"

# make sure that these source types are correct based on edits above
traits$migration_source_type[which(traits$Migration_source == "Avibase")] <- "db" 
traits$migration_source_type[which(traits$migration_year == "mt.gov")] <- "db" 
traits$migration_source_type[which(traits$migration_year == "IUCN")] <- "db" 
traits$migration_source_type[which(traits$migration_year == "ADW")] <- "db" 

# tidy up
traits_num <- traits %>% 
  mutate(dispersal_year = as.numeric(dispersal_year)) %>% 
  mutate(foraging_year = as.numeric(migration_year)) %>% 
  mutate(migration_year = as.numeric(migration_year))


## basic exploration of diffs between groups

ggplot(traits, aes(x=log10(dispersal_km), color = dispersal_source_type)) + 
  geom_density()

Anova(lm(log10(dispersal_km) ~ dispersal_source_type, data = traits))

ggplot(traits, aes(x=log10(hr.radius), color = foraging_source_type)) + 
  geom_density()

Anova(lm(log10(hr.radius) ~ foraging_source_type, data = traits))


ggplot(traits, aes(x=log10(Migration_km), color = migration_source_type)) + 
  geom_density()
Anova(lm(log10(Migration_km) ~ migration_source_type, data = filter(traits, Migration_km != 0)))


## distance by year

ggplot(traits_num, aes(y=log10(dispersal_km), x = dispersal_year, color = dispersal_source_type)) + 
  geom_point()+
  geom_smooth(method = "lm")

ggplot(traits_num, aes(y=log10(hr.radius), x = foraging_year, color = foraging_source_type)) + 
  geom_point()+
  geom_smooth(method = "lm")

ggplot(traits_num, aes(y=log10(Migration_km+1), x = migration_year, color = migration_source_type)) + 
  geom_point()+
  geom_smooth(method = "lm")

ggplot(traits_num, aes(y=log10(Migration_km), x = migration_year, color = migration_source_type)) + 
  geom_point()+
  geom_smooth(method = "lm")


### distance ~ body size + source type
library(lme4)
library(car)

disp_null <- lm(log10(dispersal_km)~log10(Mass_kg), data = traits)
disp_test_random <- lmer(log10(dispersal_km)~log10(Mass_kg) + (1|dispersal_source_type), data = traits)
disp_test_fixed <- lm(log10(dispersal_km)~log10(Mass_kg) + dispersal_source_type, data = traits)
disp_test_fixed_interaction <- lm(log10(dispersal_km)~log10(Mass_kg) + dispersal_source_type + log10(Mass_kg)*dispersal_source_type, data = traits)
summary(disp_test_random) # 0.09478 variance
summary(disp_test_fixed) # type p-value = 0.002
summary(disp_test_fixed_interaction) # interaction p-value < 0.05
Anova(disp_test_fixed_interaction)
AIC(disp_null, disp_test) # AIC diff <5, null lower

traits$predicted_disp <- predict(disp_test_fixed_interaction)
ggplot(traits, aes(y=log10(dispersal_km), x = log10(Mass_kg), color = dispersal_source_type)) + 
  geom_point()+
  stat_smooth(method = "lm")

for_null <- lm(log10(hr.radius)~log10(Mass_kg), data = traits)
for_test_random <- lmer(log10(hr.radius)~log10(Mass_kg) + (1|foraging_source_type), data = traits)
for_test_fixed <- lm(log10(hr.radius)~log10(Mass_kg) + foraging_source_type, data = traits)
for_test_fixed_interaction <- lm(log10(hr.radius)~log10(Mass_kg) + foraging_source_type + log10(Mass_kg)*foraging_source_type, data = traits)
summary(for_test_random) # 0.02576 variance
summary(for_test_fixed) # type p-value = 0.006
summary(for_test_fixed_interaction) # interaction < 0.05
Anova(for_test_fixed_interaction)
AIC(for_null, for_test) # AIC diff = 7, null lower

ggplot(traits, aes(y=log10(hr.radius), x = log10(Mass_kg), color = foraging_source_type)) + 
  geom_point()+
  stat_smooth(method = "lm")

# mig_null <- lm(log10(Migration_km+1)~log10(Mass_kg), data = traits)
# mig_test <- lmer(log10(Migration_km+1)~log10(Mass_kg) + (1|migration_source_type), data = traits)
# summary(mig_test) # 0.4869 variance
# AIC(mig_null, mig_test) # AIC diff = 17, test lower

mig_null <- lm(log10(Migration_km)~log10(Mass_kg), data = filter(traits, Migration_km != 0))
mig_test_random <- lmer(log10(Migration_km)~log10(Mass_kg) + (1|migration_source_type), data = filter(traits, Migration_km != 0))
mig_test_fixed <- lm(log10(Migration_km)~log10(Mass_kg) + migration_source_type, data = filter(traits, Migration_km != 0))
mig_test_fixed_interaction <- lm(log10(Migration_km)~log10(Mass_kg) + migration_source_type +
                                   log10(Mass_kg)*migration_source_type, 
                                 data = filter(traits, Migration_km != 0))
mig_test_fixed_interaction2 <- lm(log10(Migration_km)~log10(Mass_kg) + migration_source_type +
                                   log10(Mass_kg)*migration_source_type + class*migration_source_type, 
                                 data = filter(traits, Migration_km != 0))
mig_test_fixed_interaction3 <- lm(log10(Migration_km)~log10(Mass_kg) + migration_source_type + 
                                    class*migration_source_type, 
                                  data = filter(traits, Migration_km != 0))
summary(mig_test_random) # 0.5381 variance
summary(mig_test_fixed) # source p-value is basically 0
summary(mig_test_fixed_interaction) # interaction p-value = 0.008
summary(mig_test_fixed_interaction) # interaction p-value = 0.008
Anova(mig_test_fixed_interaction2)
AIC(mig_test_fixed_interaction, mig_test_fixed_interaction2)
AIC(mig_null, mig_test) # AIC diff = 6, test lower

ggplot( filter(traits, Migration_km != 0), aes(y=log10(Migration_km), x = log10(Mass_kg), color = migration_source_type)) + 
  geom_point()+
  stat_smooth(method = "lm")


### distance ~ body size + year
disp_test2_fixed_interaction <- lm(log10(dispersal_km)~log10(Mass_kg) + dispersal_year + log10(Mass_kg)*dispersal_year, data = traits_num)
Anova(disp_test2_fixed_interaction) # interaction not sig


for_test2_fixed_interaction <- lm(log10(hr.radius)~log10(Mass_kg) + foraging_year + log10(Mass_kg)*foraging_year, data = traits_num)
Anova(for_test2_fixed_interaction) # interaction not sig

mig_test2_fixed_interaction <- lm(log10(Migration_km)~log10(Mass_kg) + migration_year +
                                   log10(Mass_kg)*migration_year, 
                                 data = filter(traits_num, Migration_km != 0))
Anova(mig_test2_fixed_interaction) # interaction p-value = 0.03


### class as birds, mammals, others
table(traits$class)
traits$class2 <- traits$class

traits$class2[which(traits$class == "Amphibia")] <- "other"
traits$class2[which(traits$class == "Reptilia")] <- "other"
traits$class2[which(traits$class == "Reptilia")] <- "other"
traits$class2[which(traits$class == "Chondrichthyes")] <- "other"


ggplot( filter(traits, Migration_km != 0), aes(y=log10(Migration_km), x = log10(Mass_kg), color = class2)) + 
  geom_point()+
  stat_smooth(method = "lm")



mig_test_fixed_interaction2 <- lm(log10(Migration_km)~log10(Mass_kg) + migration_source_type +
                                    log10(Mass_kg)*migration_source_type + class2*migration_source_type, 
                                  data = filter(traits, Migration_km != 0))
Anova(mig_test_fixed_interaction2)


mig_test_fixed_interaction2 <- lm(log10(Migration_km)~log10(Mass_kg) + migration_source_type + class2, 
                                  data = filter(traits, Migration_km != 0))
Anova(mig_test_fixed_interaction2)


### contigency analysis ####

class_type <- table(traits$class2, traits$migration_source_type)
chisq.test(class_type, simulate.p.value = TRUE, B = 10000)
graphics::mosaicplot(class_type)


## mass by year

ggplot(filter(traits, Migration_km != 0), aes(y=log10(Mass_kg), x = as.numeric(migration_year))) + 
  geom_point()+
  stat_smooth(method = "lm")

Anova(lm(log10(Mass_kg)~as.numeric(migration_year), data = filter(traits, Migration_km != 0)))

ggplot(filter(traits, dispersal_source_type != "db"), aes(y=log10(Mass_kg), x = as.numeric(dispersal_year))) + 
  geom_point()+
  stat_smooth(method = "lm")
Anova(lm(log10(Mass_kg)~as.numeric(dispersal_year), data = filter(traits, dispersal_source_type != "db")))

ggplot(filter(traits, foraging_source_type != "db"), aes(y=log10(Mass_kg), x = as.numeric(foraging_year), color = class2)) + 
  geom_point()+
  stat_smooth(method = "lm")
Anova(lm(log10(Mass_kg)~as.numeric(foraging_year), data = filter(traits, foraging_source_type != "db")))

