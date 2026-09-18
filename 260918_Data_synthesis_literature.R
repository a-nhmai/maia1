#Vulnerability proj: Trait Network Setup
#Data synthesis from multiple literature data sources
#Anh Mai
#18 September 2026

#Libraries--------------------------------
lib_ls <- c("dplyr",
            "ggplot2",
            "forcats",
            "janitor",
            "stringr",
            "cowplot",
            "tidyr")

lapply(lib_ls, library, character.only = TRUE)

#Loading dfs--------------------------------
dir <- "~/Documents/05_Data/"
diaz_2022 <- readxl::read_excel(paste0(dir, "Diaz_et_al_2022_data/Species_mean_traits.xlsx")) |> clean_names()
jin_2016 <- read.csv(paste0(dir, "Jin_et_al_2026.csv")) |> clean_names()
lopez_martinez_2023 <- readxl::read_excel(paste0(dir, "Lopez-Martinez_et_al_2023.xlsx"), sheet = "DATA") |> clean_names()
lopez_martinez_2023_ids <- readxl::read_excel(paste0(dir, "Lopez-Martinez_et_al_2023.xlsx"), sheet = "Character States") |> clean_names()

col_order <- c("spp_name",	"trait_name",	"value",	"unit",	"kind",	"source",	"comment")

clean_dir <- "~/maia1/Data/Trait/"

#Cleaning: Diaz--------------------------------
colnames(diaz_2022)

diaz_trait_ls <- diaz_2022 |> colnames() |> c()
diaz_trait_select <- diaz_trait_ls[c(11, 15, 16:31)] 


diaz_trait_names<- diaz_2022 |>
  filter(phylogenetic_group_general == "Angiosperm" & growth_form == "tree") |> 
  select(1, 2, 11, 15, 16:31) |> #specific columns
  rename(spp_name = species_name_standardized_against_tpl) |> 
  mutate(across(leaf_type:ssd_combined_mg_mm3, as.character)) |>
  pivot_longer(cols = leaf_type:ssd_combined_mg_mm3,
               names_to = "trait_name",
               values_to = "value") |> 
  mutate(comment = paste0("try_30_acc_spp_id = ", try_30_acc_species_id, ", growth form = ", growth_form),
         source = "Diaz et al. (2022)",
         kind = ifelse(str_detect(trait_name, "imputed") == "TRUE", "imputation", "TRY mean values")) |> 
  filter(!is.na(value)) |>
  filter(!str_detect(trait_name, "_n_o")) |> distinct(trait_name) 

#Correcting the units and trying to get rid of the unit names in the trait names
diaz_units <- c(NA, "mm2", "mg g-1","g m-2", "mg mm-3", "mg mm-3", "m", "mg") 
diaz_units_ls <- bind_cols(diaz_trait_names, diaz_units) |> rename("unit" = "...2")

#getting vector of characters that need to be removed from trait names
trait_name_rm <- str_extract(diaz_trait_names$trait_name, "_.*\\b") |>
                        str_remove("[a-z]{4,8}_") 


#renaming trait names without units and cleaning up column order
diaz_2022_c <- diaz_2022 |>
  filter(phylogenetic_group_general == "Angiosperm" & growth_form == "tree") |> 
  select(1, 2, 11, 15, 16:31) |> #specific columns
  rename(spp_name = species_name_standardized_against_tpl) |> 
  mutate(across(leaf_type:ssd_combined_mg_mm3, as.character)) |>
  pivot_longer(cols = leaf_type:ssd_combined_mg_mm3,
               names_to = "trait_name",
               values_to = "value") |> 
  mutate(comment = paste0("try_30_acc_spp_id = ", try_30_acc_species_id, ", growth form = ", growth_form),
         source = "Diaz et al. (2022)",
         kind = ifelse(str_detect(trait_name, "imputed") == "TRUE", "imputation", "TRY mean values")) |> 
  filter(!is.na(value)) |>
  filter(!str_detect(trait_name, "_n_o")) |> 
  left_join(diaz_units_ls) |> 
  select(-1, -3) |>
  mutate(trait_name = str_replace_all(trait_name, "_", " "),
         trait_name = str_remove_all(trait_name, " [a-z|\\d]{1,3}\\b")) |> 
  relocate(col_order)

#writing csv
write.csv(diaz_2022_c, 
          paste0(clean_dir, "c_diaz_et_al_2022.csv"))

#Cleaning: Jin--------------------------------

jin_2026_c <- jin_2016[,c(6, 7, 9)] |>
      pivot_longer(cols = c(seed_dispersal_category),
                   names_to = "trait_name",
                   values_to = "value") |> 
      rename(source = seed_dispersal_data_source,
             spp_name = accepted_name) |>
      mutate(trait_name = str_replace_all(trait_name, "_", " "),
             source = paste0("Jin et al. (2016) | ", source),
             unit = NA,
             kind = "TRY data",
             comment = NA) |> 
      relocate(col_order)

write.csv(jin_2026_c, 
          paste0(clean_dir, "c_jin_et_al_2026.csv"))

#Cleaning: Lopez-Martinez--------------------------------

#filtered for only ovary, anther, sex, and ovule for now
lopez_martinez_2023_c <- lopez_martinez_2023 |> 
  select(taxon, androecium_merism:aperture_shape) |>
  rename(spp_name = taxon) |>
  pivot_longer(cols = androecium_merism:aperture_shape,
               names_to = "trait_name",
               values_to = "value") |>
  filter(!is.na(value)) |> 
  filter(str_detect(trait_name, "ovary|anther|ovaries|sex|ovule")) |> #filtered for only ovary, anther, sex, and ovule for now
  mutate(trait_name = str_replace_all(trait_name, "_", " "),
         spp_name = str_remove(spp_name, "X "),
         unit = NA,
         source = "Lopez-Martinez et al. (2023) | PROTEUS",
         kind = "ancestral reconstruction",
         comment = NA) |> 
  relocate(col_order)

write.csv(lopez_martinez_2023_c, 
          paste0(clean_dir, "c_lopez_martinez_2023.csv"))
  
  

