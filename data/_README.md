# README

## Data used in the analysis of ***Macroecological constraints on species' 'movement profiles': body mass does not explain it all***

#### in alphabetical order, with data dictionaries:

**Description of:** `classes.csv` - file created from the `classes_for_sp.R` script

| Column Name     | Description                                 |
|-----------------|---------------------------------------------|
| scientific_name | binomial scientific name of *Genus species* |
| class           | taxonomic class of organism                 |
| order           | taxonomic order of organism                 |

**Description of:** cleaned_traits.csv - raw data of movement types per species

| Column Name              | Description                                            |
|--------------------------|--------------------------------------------------------|
| scientific_name.x        | Full scientific name in the format *Genus species*     |
| collapsed_name           | Scientific name in the format *Genusspecies*           |
| dispersal_km             | dispersal distance in kilometers                       |
| dispersal_checked        | initials of author who double checked entry            |
| source_dispersal         | author and year of the paper where we pulled the value |
| dispersal.original.value | original value if different                            |
| dispersal.original.units | original units if different than km                    |
| Migration_km             | migration distance in kilometers                       |
| migration_checked        | initials of author who double checked entry            |
| Migration_source         | author and year of the paper where we pulled the value |
| migration.original.value | original value if different                            |
| migration.original.units | original units if different than km                    |
| mean.hra.m2              | home range area in meters\^2                           |
| home_range_checked       | initials of author who double checked entry            |
| source_hra               | author and year of the paper where we pulled the value |
| hra.original.value       | original value if different                            |
| hra.original.unit        | original units if different than m\^2                  |
| Mass_kg                  | body mass in kg                                        |
| Mass_source              | initials of author who double checked entry            |
| mass checked             | author and year of the paper where we pulled the value |
| number_traits_completed  | number of movement types with complete data            |
| person                   | author who double checked for completion               |
| diet - fine scale        | description of diet                                    |
| diet_broadest_cat        | broader categories based on diet - fine scale column   |
| diet source              | source of diet data                                    |
| media_air_water_land     | description of movement media                          |
| media_simplified         | broader categories based on above column               |
| medium source_notes      | source for medium information                          |
| media_dispersal          | medium used for dispesal                               |
| media_migration          | medium used for migration                              |
| media_foraging           | medium used for foraging                               |
| geographic_range_km2     | geographic extent of the species                       |
| geographic unit source   | source of geographic extent value                      |
| IUCN                     | IUCN status                                            |

**Description of:** `final_phylo.rds` - file created from `datelife_tree.R` script, used in the taxonomy cleaning step of `00_Preprocessing.Rmd`

**Description of:** `Traits_final_cleaned.csv` - final file used for data analysis, created in `00_Preprocessing.Rmd`. In column headers, home range = foraging

| Column Name           | Description                                                                                                                          |
|-----------------------|--------------------------------------------------------------------------------------------------------------------------------------|
| scientific_name.x     | Full scientific name in the format *Genus_species*                                                                                   |
| dispersal_km          | dispersal distance in km                                                                                                             |
| Migration_km          | migration distance in km                                                                                                             |
| mean.hra.m2           | home range area in meters\^2                                                                                                         |
| Mass_kg               | mass in kg                                                                                                                           |
| class                 | taxonomic class                                                                                                                      |
| order                 | taxonomic order                                                                                                                      |
| diet_broadest_cat     | broad diet categories: carnivore, herbivore, omnivore, invertivore                                                                   |
| media_simplified      | movement medium simplified to: air, land, aquatic                                                                                    |
| media_dispersal       | movement medium used for dispersal                                                                                                   |
| media_migration       | movement medium used for migration                                                                                                   |
| media_foraging        | movement medium used for foraging                                                                                                    |
| source_dispersal      | source paper for dispersal                                                                                                           |
| Migration_source      | source paper for migration                                                                                                           |
| source_hra            | source paper for home range (foraging) area                                                                                          |
| hr.radius             | radius of home range area in km                                                                                                      |
| dispersal_source_type | source type for dispersal: database (db), where many species come from one paper, or single, where only 1 species comes from a paper |
| foraging_source_type  | source type for foraging                                                                                                             |
| migration_source_type | source type for migration                                                                                                            |
| dispersal_year        | publication year for dispersal source                                                                                                |
| foraging_year         | publication year for foraging source                                                                                                 |
| migration_year        | publication year for migration source                                                                                                |
