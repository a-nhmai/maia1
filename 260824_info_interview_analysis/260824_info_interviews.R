#Informational Interviews conducted from May to July 2026 
#Creating tables of counts of concepts and species mentions from interviewing Nat Resources Folks
#Anh Mai
#24 August 2026

#Libraries
lib_ls <- c("dplyr",
            "ggplot2",
            "janitor",
            "stringr",
            "readxl")
sapply(lib_ls, library, character.only = TRUE)

#Loading in data
concept_df <- read_excel("~/Dropbox/AM_Dropbox/2026_SummerDropbox/2026_Interviews/260824_interview_concepts.xlsx") |>
  clean_names()
spp_df <- read_excel("~/Dropbox/AM_Dropbox/2026_SummerDropbox/2026_Interviews/260824_interview_spp.xlsx") |>
  clean_names()

#creating tables for reference later
concept_tab <- concept_df |> arrange(desc(count))

spp_df2 <- spp_df |> 
  mutate(genus = ifelse(str_detect(species, "Oak|oak") == "TRUE", "Quercus", NA),
         genus = ifelse(str_detect(species, "Maple|maple") == "TRUE", "Acer", genus),
         genus = ifelse(str_detect(species, "Basswood") == "TRUE", "Tilia", genus),
         genus = ifelse(str_detect(species, "Elm|elm") == "TRUE", "Ulmus", genus),
         genus = ifelse(str_detect(species, "Ash|ash") == "TRUE", "Fraxinus", genus),
         genus = ifelse(str_detect(species, "Aspen|aspen") == "TRUE", "Populus", genus),
         genus = ifelse(str_detect(species, "Spruce|spruce") == "TRUE", "Picea", genus),
         genus = ifelse(is.na(genus) == "TRUE", "Other", genus)
         ) 

spp_tab <- spp_df2 |>
              count(genus) |>
              arrange(desc(n))

#write tables for later reference
write.table(concept_tab, "~/maia1/260824_info_interview_analysis/concept_tab.txt")
write.table(spp_tab, "~/maia1/260824_info_interview_analysis/spp_tab.txt")

  
