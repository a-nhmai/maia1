#Vulnerability proj: Trait Merge
#Merging trait dfs for prelim data and examing my species and trait coverage
#Anh Mai
#28 September 2026

#Libraries--------------------------------
#install.packages("BIEN")
lib_ls <- c("dplyr",
            "ggplot2",
            "forcats",
            "janitor",
            "stringr",
            "cowplot",
            "tidyr",
            "BIEN")

lapply(lib_ls, library, character.only = TRUE)

install.packages("MultiTraits")
library(MultiTraits)

#Data:loading--------------------------------
data_dir <- "~/maia1/Data/Trait/"

REF_SPECIES <- read.csv("~/maia1/Data/Cleaning/spp_highlighted.csv") |> select(1) #FIA + U.S.

col_order <- read.csv("~/maia1/Data/Cleaning/col_order.csv") 
spp_highlight <- read.csv("~/maia1/Data/Cleaning/spp_highlighted.csv") 

diaz <- read.csv(paste0(data_dir, "c_diaz_et_al_2022.csv")) |> select(-1) |> mutate(value = as.character(value))
guo <- read.csv(paste0(data_dir, "c_Guo_et_al_2022.csv")) |> relocate(col_order$x) |> mutate(value = as.character(value))
jin <- read.csv(paste0(data_dir, "c_jin_et_al_2026.csv")) |> select(-1) |> mutate(value = as.character(value))
lopez_martin <- read.csv(paste0(data_dir, "c_lopez_martinez_2023.csv")) |> select(-1) |> mutate(value = as.character(value))
try <- read.csv(paste0(data_dir, "c_TRY_angio_trait_v2.csv")) |> mutate(value = as.character(value)) |> select(-6)
bien <- read.csv("~/Documents/05_Data/BIEN/c_BIEN.csv")

#main df
df_main <- bind_rows(diaz, guo, jin, lopez_martin, try, bien)




#Data: overview--------------------------------

#What is the data coverage for species traits for my reference list?......................................
#findings:
#looking to see where the reference species list overlaps with the trait data coverage I have
#146 reference species
#143 species are already represented in the dataset so far
#3 species not represented in the dataset: Quercus graciliformis, Quercus tardifolia, Quercus robusta

#how many species in consideration?
REF_SPECIES #reference list: FIA, U.S. List, MN natural resource mentions, n = 146

#how many species overlap with the REF_SPECIES and the ones from the main trait df?
n_distinct(df_main$spp_name) #n = 44264 species in main trait df

ref_species_ls <- REF_SPECIES |> unlist(use.names = FALSE)

df_main |>
  filter(spp_name %in% ref_species_ls) |>
  nrow() #5026203 observations

REF_SPECIES |> inner_join(as.data.frame(unique(df_main$spp_name)), 
                          by = join_by(scientific_name == `unique(df_main$spp_name)`)) |>
  nrow() #144 out of 146 species represented

REF_SPECIES |> anti_join(as.data.frame(unique(df_main$spp_name)), 
                         by = join_by(scientific_name == `unique(df_main$spp_name)`)) #the 2 species not represented

#What does the dataset look like when I filter for only the 148ish species?......................................
df_filt <- df_main |>
  filter(spp_name %in% ref_species_ls) #5026203 observations

trait_ct <- df_filt |> #df where rows = species and columns = traits and cells = number of observations
  group_by(spp_name, trait_name) |>
  summarise(n_trait_ct = n()) |>
  arrange(desc(n_trait_ct)) |>
  pivot_wider(names_from = trait_name,
              values_from = n_trait_ct) 


#Filtering for only the traits I am interested in currently......................................
trait_names_ls <- colnames(trait_ct)

trait_names_ls <- trait_names_ls[c(2,3,4,5,8,9,10,12,18,21,22,23,24,25,26,29,31,32,36)] #selecting traits

trait_ct_filt <- trait_ct |> 
                      select(spp_name, trait_names_ls)
                    
NA_row_num <- nrow(trait_ct_filt) #number of species 

summary(trait_ct_filt) #validating NAs look right

#looking at NAs by traits......................................
NA_prop_traits <- colSums(is.na(trait_ct_filt[-1])) |> #proportion of NAs per trait in descending order at species level
  as.data.frame() |>
  rename(propNA = `colSums(is.na(trait_ct_filt[-1]))`) |>
  mutate(propNA = round(propNA/NA_row_num, 2)) |>
  arrange(desc(propNA))

NA_prop_traits 


#looking at NAs by species......................................
NA_prop_spp <- rowSums((is.na(trait_ct_filt[-1]))) |>
  as.data.frame() |>
  bind_cols(trait_ct[1]) |> relocate(spp_name) |>
  rename(propNA = `rowSums((is.na(trait_ct_filt[-1])))`) |>
  mutate(propNA = round(propNA/NA_row_num, 2)) |>
  arrange(desc(propNA))
  
NA_prop_spp
  

#To do still
#look for duplicates due to addition of carterau et al 2025



