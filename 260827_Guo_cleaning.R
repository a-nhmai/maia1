#Knighton et al. (2025) Data Cleaning
#Exploring and cleaning data from the Guo et al. (2022) paper found at https://www.pnas.org/doi/10.1073/pnas.2026733119#executive-summary-abstract
#Anh Mai
#27 August 2026


#Formating shortcut
dashes <- "--------------------------------"

#Libraries--------------------------------
lib_ls <- c("dplyr",
            "ggplot2",
            "forcats",
            "janitor",
            "stringr",
            "cowplot")

lapply(lib_ls, library, character.only = TRUE)


#Data: loading--------------------------------
guo_rd <- read.delim("~/maia1/Data/Trait/Guo_et_al_2022.txt", sep = ",") |> select(-X)
selected_FIA <- REF_SPECIES <- read.csv("Data/FIA/REF_SPECIES.csv") |> select(SCIENTIFIC_NAME)

#Data: cleaning--------------------------------

guo_rd |> 
  select(-family) |>
  tidyr::pivot_longer(cols = VegMaxHeight:LA,
                      names_to = "trait_name",
                      values_to = "value") |> 
  mutate(trait_name = as.factor(trait_name), 
         trait_name = fct_recode(trait_name,
                                  "spp_name" = "species",
                                  "Plant height (maximum)" = "VegMaxHeight",
                                  "Leaf nitrogen concentration" = "leafN",
                                  "Leaf area" = "LA", #needs additional comment
                                  "Leaf phosphorous concentration" = "leafP",
                                  "Seed Dry Mass" = "SeedDryMass",
                                  "Leaf Dry Matter Content" ="LDMC",
                                  "Wood Density" = "WDensity")) |> #needs additional comment))
  mutate(species = str_replace(species, "_", " ")) |>
  inner_join(REF_SPECIES,
             by = join_by(species == SCIENTIFIC_NAME)) |>
  rename(spp_name = species) |>
  mutate(unit = NA,
         comment = NA,
         comment = ifelse(trait_name == "Leaf area", 
                   "Leaf area (in case of compound leaves: leaflet, petiole and rachis excluded)",
                   comment),
         comment = ifelse(trait_name == "Wood Density", 
                          "Stem specific density (SSD)",
                          comment),
         kind = "imputed using Bayesian hierarchical probabilistic matrix factorization",
         source = "Guo et al. (2022)") 
#still need to figure out the unites for these  
