#Knighton et al. (2025) Data Cleaning
#Exploring and cleaning data from the Knighton et al. (2025) dataset found at https://zenodo.org/records/15009207
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

#Data: reading in--------------------------------

knighton_rd <- readxl::read_excel("~/maia1/Data/Trait/Knighton_et_al_2025.xlsx")
fia_spp <- read.csv("~/maia1/Data/FIA/REF_SPECIES.csv") |> select(GENUS, SPECIES, SCIENTIFIC_NAME) |> clean_names()

glimpse(knighton_rd)
glimpse(fia_spp)

#Data: inner joining--------------------------------
knighton_clean <- knighton_rd |> 
                      inner_join(fia_spp,
                                 by = join_by("spec.name" == "scientific_name",
                                              "Genus" == "genus",
                                              "Species" == "species")) |> 
                      select(-Family, -Genus, -Species) |>
                      rename("Rooting Depth maximum" = "rdmax") |>
                      tidyr::pivot_longer(cols = gsmax:LeafN,
                                          names_to = "trait_name",
                                          values_to = "value") |>
                      rename("spp_name" = "spec.name") |>
                      mutate(unit = NA,
                             unit = ifelse(trait_name == "Rooting Depth maximum", "m", unit),
                             kind = "imputed, random tree with phylogenetic eigenvector maps (PEM)",
                             source = "Knighton et al. (2025)") 

#CSV: writing CSV--------------------------------
write.csv(knighton_clean, "~/maia1/Data/Trait/Knighton_et_al_2025_c.csv")


