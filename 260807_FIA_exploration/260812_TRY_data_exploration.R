#TRY Data Acquisition and Cleaning 
#EDA of try_angio.csv
#Anh Mai
#12 August 2026

#Loading in libraries
lib_ls <- c("dplyr",
            "ggplot2",
            "forcats",
            "janitor",
            "stringr")

sapply(lib_ls, library, character.only = TRUE)


#Loading in df
trait_df_raw <- read.csv("~/maia1/260807_FIA_exploration/try_angio_clean.csv")
trait_df <- trait_df_raw |>
              select(DatasetID, 
                     SpeciesName,
                     ObservationID,
                     TraitID,
                     TraitName,
                     DataID,
                     OriglName,
                     OrigValueStr,
                     OrigUnitStr,
                     StdValue,
                     Comment) |>
              rename(value = StdValue) |>
              clean_names()




