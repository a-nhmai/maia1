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

#Data: loading and initial cleaning--------------------------------
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
                              as.numeric)) |>
                filter(trait_id != 926) #only one LAI of a single plant
    #mostly selecting relevant columns, cleaning up names, and making sure 
    #variables are the correct data type

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
                    filter(duplicated == FALSE)


#Cleaning: checking incongruence in original units and (new) units--------------------------------
    #places where the UNITS are different but the values are the same = suspicious
    #double mismatch is likely ok, probably a unit conversion
    #single mismatch is not ok, likely a conversion error
    #there should be an equal amount of single and double mismatches
    #if the difference between single and double is negative, then there are too many 

#Cleaning: ID'ing incongruence of mismatches--------------------------------
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

#Cleaning: filtering incongruence of mismatches--------------------------------

single_mismatches <- trait_df_cl |>
                         mutate(unit_mismatch = ifelse(orig_unit_str != unit_name, "TRUE", "FALSE"),
                         value_mismatch = ifelse(orig_value_str != value, "TRUE", "FALSE"),
                         single_mismatch = ifelse(unit_mismatch != unit_name & orig_value_str == value, 
                                                  "TRUE", 
                                                  "FALSE"),
                         across(unit_mismatch:single_mismatch, as.logical))

trait_mism <- trait_df_cl |>
                  bind_cols(single_mismatches$single_mismatch) |>
                  rename("mism_s" = "...16")

trait_mism |>
  filter(mism_s == TRUE) |>
  filter(orig_value_str == value & unit_name != orig_unit_str)

    #The mismatches are ok. Mostly months -> month and m2/g -> mm2 mg-1; i.e. one to one conversions

#CSV writing: saving the cleaned df without duplicates in desired format --------------------------------

cleaned_trait_df <- trait_df_cl |>
                        select(-duplicated, -orig_unit_str, -origl_name, -orig_value_str) |>
                        mutate(id_dfdo = str_c(dataset_id, "_" , data_id, "_", observation_id), #unique identifier for a specific DataFrame, Data, and Obs id
                               source = str_c("TRY.", dataset), #adding that the source is try data
                               comment = ifelse(str_detect(trait_df_cl$trait_name, "\\(specific leaf area(.)*")  == TRUE, #removing the big parentheses from trait_name in SLA and pasting in comment
                                                paste(comment, "|" , str_extract(trait_df_cl$trait_name, "\\(specific leaf area(.)*")), 
                                                comment),
                               comment = str_remove(comment, "x"), #cleaning up messy formating
                               comment = paste0(comment, ". Value Kind Name = ", value_kind_name, "| TRY TraitID = ", trait_id), #storying the measurement kind (i.e. mean, best estimate) in the comment
                               comment = ifelse(str_detect(trait_df_cl$trait_name, "Root length (.)*")  == TRUE, #cleaning up trait name and putting in comment
                                                paste(comment, str_extract(trait_df_cl$trait_name, "Root length (.)*")), 
                                                comment),
                               trait_name = fct_recode(trait_name, 
                                                       "Specific root length" = "Root length per root dry mass (specific root length, SRL)")) |> #renaming long name to SRL
                        select(-c(dataset_id, observation_id, data_id, dataset, trait_id, value_kind_name)) |> 
                        relocate(spp_name, trait_name, value, unit_name, source, id_dfdo, comment) 

write.csv(cleaned_trait_df, "~/maia1/Data/Trait/TRY_angio_trait.csv")

#General notes--------------------------------

#   - 2311 duplicates
#   - Leaf area index (LAI) of a single plant was filtered out as it only has one observation
#   - next to do is figure out the mismatches; ok. see line 134.








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