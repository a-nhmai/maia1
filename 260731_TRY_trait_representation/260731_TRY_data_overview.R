#TRY Website Exploration
#Analysis of traits: distribution of representation by observation and species ct
#Anh Mai
#31 July 2026


#Libraries
lib_ls <- c("dplyr", "ggplot2", "stringr")
sapply(lib_ls, library, character.only = TRUE)

#Load in table
try_table <- read.delim("~/maia1/260731_TRY_trait_representation/260731_TRY_table", header=FALSE, comment.char="#")
head(try_table)
colnames(try_table) <- try_table[3,]
try_table <- try_table[-c(1:3), -6]
head(try_table)
try_table$Trait <- gsub("\\(|\\)", "", try_table$Trait)
try_table <- try_table |>
              mutate(across(ObsNum:AccSpecNum, as.numeric))

      #notes from TRY website about column names
          #ObsNum: Number of Observations
          #ObsGRNum: Number of geo-referenced Observations
          #AccSpecNum: Number of Accepted Species

#EDA
colnames(try_table)
str(try_table)

      #floral traits
          try_table |>
            select(-TraitID) |>
            mutate(Trait = str_to_lower(Trait)) |>
            filter(str_detect(Trait, "flower|floral")) |> 
            arrange(desc(ObsNum)) |>
            slice_max(ObsNum, n = 30)
    
      #root traits
          try_table |>
            select(-TraitID) |>
            mutate(Trait = str_to_lower(Trait)) |>
            filter(str_detect(Trait, "root")) |> 
            arrange(desc(ObsNum)) |>
            slice_max(ObsNum, n = 30)   

      #leaf traits
          try_table |>
            select(-TraitID) |>
            mutate(Trait = str_to_lower(Trait)) |>
            filter(str_detect(Trait, "leaf")) |> 
            arrange(desc(AccSpecNum)) |>
            slice_max(AccSpecNum, n = 30)   

  

         