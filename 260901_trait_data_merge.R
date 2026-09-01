#Merging Trait Data
#Data from Knighton et al. (2025), TRY, and Guo et al. (2022)
#1 September 2026
#Anh Mai

#Formating shortcut--------------------------------
dashes <- "--------------------------------"

#Libraries--------------------------------
lib_ls <- c("dplyr",
            "ggplot2",
            "forcats",
            "janitor",
            "stringr",
            "cowplot")

lapply(lib_ls, library, character.only = TRUE)


#Loading: Data--------------------------------
dir <- "~/maia1/Data/Trait/"

knighton <- read.csv(paste0(dir, "c_Knighton_et_al_2025.csv"))
try <- read.csv(paste0(dir, "c_try_angio_trait_v2.csv")) 
guo <- read.csv(paste0(dir, "c_Guo_et_al_2022"))



#Bindining Data--------------------------------

col_ls <- try |> colnames() |> c() #getting the correct order for columns
  col_ls <- col_ls[-6]

  guo <- guo |> relocate(col_ls)
  knighton <- knighton |> relocate(col_ls) 
  
summary(knighton)  
  
try |>
  bind_rows(guo) |>
  bind_rows(knighton) |> distinct(trait_name)
