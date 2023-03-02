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

