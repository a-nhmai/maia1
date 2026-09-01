#TRY Data Acquisition and Cleaning 
#Cleaning Try Data download and cleaning for angiosperm list from FIA
#Anh Mai
#12 August 2026

#Libraries
    #install.packages("janitor")

lib_ls <- c("dplyr",
            "ggplot2",
            "forcats",
            "janitor",
            "stringr")

sapply(lib_ls, library, character.only = TRUE)

#Loading in dfs
    try_df <- read.delim("~/Documents/Data/TRY/260812_TRY_data.txt")
    try_df2 <- read.delim("~/Documents/Data/TRY/260813_TRY_data.txt") #vessel morphology traits that got left out
    
    try_df <- try_df |> bind_rows(try_df2)
    
  spp_ref <- read.csv("~/maia1/260807_FIA_exploration/REF_SPECIES.csv")

#Cleaning and filtering for only angiosperms/hardwoods
angio_ls <- spp_ref |>
              filter(SFTWD_HRDWD == "H") 
angio_ls <- angio_ls |>
              select(COMMON_NAME, GENUS, SPECIES, SCIENTIFIC_NAME) |>
              clean_names()

      #Lists for filtering
      angio_ls2 <- angio_ls[,4]
      trait_ls <- c(4083, 3117,12, 59, 6, 21077, 23257, 1080, 926, 132, 3568, 18, 889, 773, 3507, 24, 169, 281, 1177)
      
      #Filtering species and for specific traits
      try_angio <- try_df |>
                      filter(AccSpeciesName %in% angio_ls2) |>
                      filter(TraitID %in% trait_ls) |>
                      filter(str_detect(OrigValueStr, "[\\d]*[.]{1}")) 
      
      
            #unique(try_angio$TraitName)

#Writing clean csv
write.csv(try_angio, "~/maia1/Data/Trait/c_try_angio_clean_v1.csv", row.names = FALSE)

