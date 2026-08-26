#TRY Data Exploration
#Looking at missing data, identifying filtering strategies
#Anh Mai
#26 August 2026

#formating shortcut
dashes <- "--------------------------------"




#Libraries--------------------------------
lib_ls <- c("dplyr",
            "ggplot2",
            "forcats",
            "janitor",
            "stringr",
            "cowplot")

sapply(lib_ls, library, character.only = TRUE)

#Data: loading and cleaning--------------------------------
trait_df_raw <- read.csv("~/maia1/260807_FIA_exploration/try_angio_clean.csv") 
  #this df was cleaned from maia1/260807_FIA_exploration/260812_TRY_cleaning.R
  #only angiosperms that are listed in REF_SPECIES.csv from the FIA database are included
  #see bottom of file for original code 

trait_df <- trait_df_raw |>
                select(DatasetID, 
                       AccSpeciesName,
                       ObservationID,
                       TraitID,
                       TraitName,
                       DatasetID,
                       Dataset,
                       DataID,
                       OriglName,
                       OrigValueStr,
                       OrigUnitStr,
                       ValueKindName,
                       StdValue,
                       UnitName,
                       Comment) |>
                rename(value = StdValue,
                       spp_name = AccSpeciesName) |>
                clean_names() |> 
                mutate(across(c(dataset_id:data_id),
                              as.factor)) |>
                mutate(across(c(orig_value_str, value),
                              as.numeric))

#Cleaning: overview--------------------------------
glimpse(trait_df)

trait_df |>
  select(trait_name, trait_id, observation_id) |>
  group_by(trait_name) |>
  summarize(ids = n_distinct(observation_id))

#Cleaning: checking duplicated values--------------------------------
dup <- trait_df |>
          select(-observation_id, -value_kind_name) |>
          duplicated() |>
          unlist()

trait_df_dup <- trait_df |>
                    bind_cols(dup) |> 
                    rename(duplicated = ...15)

trait_df_dup |>
    filter(duplicated != TRUE) |> 
    select(trait_name, trait_id, observation_id) |>
    group_by(trait_name) |>
    summarize(ids = n_distinct(observation_id))

trait_df_cl <- trait_df_dup |>
                    filter(duplicated != TRUE)


#Cleaning: checking incongruence in original units and (new) units--------------------------------
    #places where the UNITS are different but the values are the same = suspicious
    #double mismatch is likely ok, probably a unit conversion
    #single mismatch is not ok, likely a conversion error
    #there should be an equal amount of single and double mismatches
    #if the difference between single and double is negative, then there are too many 

#double mismatch
mism_d <- trait_df_cl |>
        mutate(unit_mismatch = ifelse(orig_unit_str != unit_name, "TRUE", "FALSE"),
               value_mismatch = ifelse(orig_value_str != value, "TRUE", "FALSE"),
               double_mismatch = ifelse(unit_mismatch == value_mismatch, "TRUE", "FALSE"),
               across(unit_mismatch:double_mismatch, as.logical)) |> 
        group_by(trait_name) |>
        summarize(mismatches_d = sum(double_mismatch, na.rm = TRUE))


mism_s <- trait_df_cl |>
        mutate(unit_mismatch = ifelse(orig_unit_str != unit_name, "TRUE", "FALSE"),
               value_mismatch = ifelse(orig_value_str != value, "TRUE", "FALSE"),
               single_mismatch = ifelse(unit_mismatch != unit_name & orig_value_str == value, 
                                        "TRUE", 
                                        "FALSE"),
               across(unit_mismatch:single_mismatch, as.logical)) |> 
        group_by(trait_name) |>
        summarize(mismatches_s = sum(single_mismatch, na.rm = TRUE))

bind_cols(mism_s, mism_d[2]) |>
  mutate(diff = mismatches_s - mismatches_d,
         issue = ifelse(diff != 0, "TRUE", "FALSE"))



#CSV writing: saving the cleaned df without duplicates in desired format --------------------------------

trait_df_dup |> 
  filter(trait_id != 926) |> #filtering out Leaf area index (LAI) of a single plant because only 1 observation
  slice_sample(n = 5)

#General notes--------------------------------
#   - 28 different data ID/traits
#   - 2311 duplicates
#   - Leaf area index (LAI) of a single plant was filtered out as it only has one observation
#   - next to do is figure out the mismatches***








#Filter: completed in a different .R file--------------------------------

#Libraries--------------------------------
#install.packages("janitor")

lib_ls <- c("dplyr",
            "ggplot2",
            "forcats",
            "janitor",
            "stringr")

sapply(lib_ls, library, character.only = TRUE)

#Loading in dfs--------------------------------
try_df <- read.delim("~/Documents/Data/260812_TRY_data.txt")
try_df2 <- read.delim("~/Documents/Data/260813_TRY_data.txt") #vessel morphology traits that got left out

try_df <- try_df |> bind_rows(try_df2)

spp_ref <- read.csv("~/maia1/260807_FIA_exploration/REF_SPECIES.csv")

#Cleaning and filtering for only angiosperms/hardwoods--------------------------------
angio_ls <- spp_ref |>
  filter(SFTWD_HRDWD == "H") 
angio_ls <- angio_ls |>
  select(COMMON_NAME, GENUS, SPECIES, SCIENTIFIC_NAME) |>
  clean_names()

#Lists for filtering--------------------------------
angio_ls2 <- angio_ls[,4]
trait_ls <- c(4083, 3117,12, 59, 6, 21077, 23257, 1080, 926, 132, 3568, 18, 889, 773, 3507, 24, 169, 281, 1177)

#Filtering species and for specific traits--------------------------------
try_angio <- try_df |>
  filter(AccSpeciesName %in% angio_ls2) |>
  filter(TraitID %in% trait_ls) |>
  filter(str_detect(OrigValueStr, "[\\d]*[.]{1}")) 


#unique(try_angio$TraitName)

#Writing clean csv--------------------------------
write.csv(try_angio, "~/maia1/260807_FIA_exploration/try_angio_clean.csv")