#FIA exploration
#Simplifying at MN_TREE.csv from FIA website https://research.fs.usda.gov/products/dataandtools/fia-datamart
#Anh Mai
#07 August 2026


lib_ls <- c("dplyr",
            "ggplot2",
            "forcats",
            "janitor",
            "stringr")

sapply(lib_ls, library, character.only = TRUE)

#Data
dir <- "~/Documents/Data/FIA"
file_name <- list.files("~/Documents/Data/FIA") 
path <- paste0(dir, "/" , file_name)
fia_rd <- read.csv(path) #rd = raw data
                      #NOTE: MN_TREE file is too big to exist on git server but is in zip file in google drive 


###########
#FILTERING#
###########

#Proportion of NAs
fia_rd |>
  is.na() |>
  colSums()/nrow(fia_rd) 

#State variable; is contingent on dir, file_name, and path
file_name <- list.files("~/Documents/Data/FIA") 

state_name <- str_extract(file_name, "[A-Z]{2}")
state_name

fia_rd |>
  mutate(state = state_name)



























select_cols <- c("tree",
                 "spcd",
                 "spgrpcd",
                 "plot",
                 "subp",
                 "condid",
                 "unitcd",
                 "ht",
                 "htcd",
                 "actualht",
                 "totage",
                 "dia",
                 "bhage",
                 "countycd")




