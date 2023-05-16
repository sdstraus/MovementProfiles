# README

## Scripts used in the analysis of ***Macroecological constraints on species\' \'movement profiles\': body mass does not explain it all***

#### in alphabetical order:

-   `00_Preprocessing.Rmd` - R Markdown script that runs our data cleaning and preprocessing steps, creates the `Traits_cleaned_final.csv` file that is used in downstream analyses and figures

-   `01_BRMS_models.Rmd` - R Markdown script that runs our Bayesian Regression Models (BRMS) and saves outputs to the mods folder as .rds files

-   `02_Figures.Rmd` - R Markdown script that generates figures presented in the manuscript and supplemental materials

-   `class_for_sp.R` - script that uses the taxize package to pull each class and order for vertebrates in our species list

-   `datelife_tree.R` - script that uses our list of classes to build a basic phylogeny, saved as an .rds file
